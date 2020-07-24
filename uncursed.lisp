(in-package :uncursed)

(defclass cell ()
  ((style :initform *default-style*
          :accessor cell-style
          :type style)
   (string :initform (make-adjustable-string (string #\space))
           :accessor cell-string
           :type string
           :documentation "A grapheme cluster")))

(defmethod print-object ((cell cell) stream)
  (format stream "#<cell string:~a style:~a>" (cell-string cell) (cell-style cell)))

(defun cell/= (cell1 cell2)
  (or (style-difference (cell-style cell1) (cell-style cell2))
      (string/= (cell-string cell1) (cell-string cell2))))

(defun wide-cell-p (cell)
  (> (display-width (cell-string cell)) 1))

(deftype buffer () '(array cell))

(defclass tui (tui-base)
  ((%screen :initarg :screen
            :accessor screen
            :type buffer
            :documentation "The contents of the screen")
   (%canvas :initarg :canvas
            :accessor canvas
            :type buffer
            :documentation "The contents to be drawn to the screen")
   (%focused-window :initarg :focused-window
                    :initform nil
                    :accessor focused-window
                    :type window)
   (%event-handler :initform (error "must provide an event handler")
                   :initarg :event-handler
                   :accessor event-handler)))

(define-condition window-bounds-error (uncursed-error)
  ((coordinate :initarg :coordinate
               :reader window-bounds-error-coordinate
               :type integer)
   (bounds :initarg :bounds
           :reader window-bounds-error-bounds
           :type (cons integer integer))
   (window :initarg :window
           :reader window-bounds-error-window
           :type window))
  (:report (lambda (condition stream)
             (format stream "~d is not in bounds [~d,~d] of ~a"
                     (window-bounds-error-coordinate condition)
                     (car (window-bounds-error-bounds condition))
                     (cdr (window-bounds-error-bounds condition))
                     (window-bounds-error-window condition))))
  (:documentation "Signaled if an attempt is made to index outside a window's bounds"))

(define-condition wide-char-overwrite-error (uncursed-error)
  ((y :initarg :y
      :reader wide-char-overwrite-error-y)
   (x :initarg :x
      :reader wide-char-overwrite-error-x)
   (buffer :initarg :buffer
           :reader wide-char-overwrite-error-buffer))
  (:report (lambda (condition stream)
             (format stream "Coordinate intersects wide character: ~d,~d in ~a"
                     (wide-char-overwrite-error-y condition)
                     (wide-char-overwrite-error-x condition)
                     (wide-char-overwrite-error-buffer condition))))
  (:documentation "Signaled if an attempt is made to overwrite a wide character."))

(defgeneric handle-winch (tui))

(defclass standard-window (window)
  ())

(defgeneric handle-mouse-event (window type button y x controlp))
(defgeneric handle-key-event (window event))

(defvar *put-buffer*)
(defvar *put-window*)

(defun put (char line col &optional style (put-buffer *put-buffer*) (put-window *put-window*))
  (or (and put-buffer put-window)
      (error "PUT-BUFFER and PUT-WINDOW not both provided"))
  (check-type put-buffer buffer)
  (check-type put-window window)
  (check-type char character)
  (check-type style (or style null))
  (let ((dimensions (dimensions put-window))
        (width (character-width char)))
    (or (<= 1 line (rect-rows dimensions))
        (error 'window-bounds-error :coordinate line
                                    :bounds (cons 1 (rect-rows dimensions))
                                    :window put-window))
    (or (<= 1 col (- (rect-cols dimensions) (1- width)))
        (error 'window-bounds-error :coordinate col
                                    :bounds (cons 1 (rect-cols dimensions))
                                    :window put-window))
    (let* ((cell-y (+ (rect-y dimensions) (1- line)))
           (cell-x (+ (rect-x dimensions) (1- col)))
           (cell (aref put-buffer cell-y cell-x)))
      (and style (setf (cell-style cell) style))
      (if (zerop width)
          (vector-push-extend char (cell-string cell))
          (progn
            ;; clear previous wide character (if applicable)
            ;; [old][""] -> [" "][new]
            (unless (zerop cell-x)
              (let ((prev (aref put-buffer cell-y (1- cell-x))))
                (when (wide-cell-p prev)
                  (restart-case
                      (error 'wide-char-overwrite-error :y cell-y
                                                        :x (1- cell-x)
                                                        :buffer put-buffer)
                    (overwrite-char ()
                      :report "Overwrite the wide character"
                      (setf (cell-string prev) (make-adjustable-string (string #\space))))
                    (ignore-put ()
                      :report "Do nothing"
                      (return-from put t))))))
            ;; width > 1: clear next character unconditionally (we checked for room above)
            ;; and turn the next-next character into a space if the next was wide
            ;; [.][old][""] -> [new][""][ ] erases old character
            (when (> width 1)
              (let ((next (aref put-buffer cell-y (1+ cell-x))))
                (when (wide-cell-p next)
                  (restart-case
                      (error 'wide-char-overwrite-error :y cell-y
                                                        :x (1+ cell-x)
                                                        :buffer put-buffer)
                    (overwrite-char ()
                      :report "Overwrite the wide character"
                      (setf (cell-string (aref put-buffer cell-y (+ 2 cell-x)))
                            (make-adjustable-string (string #\space))))
                    (ignore-put ()
                      :report "Do nothing"
                      (return-from put t))))
                (setf (cell-string next) (make-adjustable-string))))
            ;; finally write the character into its cell
            (setf (cell-string cell) (make-adjustable-string (string char))))))
    width))

;; (defun put-string (string line col &optional style)
;;   ())

;; (defun put-style (style x y len)
;;   ())

(defun buffer-diff (old new)
  (assert (= (array-total-size old) (array-total-size new)))
  (loop :with diff = (make-array 0 :fill-pointer t :adjustable t)
        :with width = (array-dimension old 1)
        :for idx :below (array-total-size old)
        :for y = (truncate idx width)
        :for x = (mod idx width)
        :for ocell = (row-major-aref old idx)
        :for ncell = (row-major-aref new idx)
        :do (when (cell/= ocell ncell)
              (vector-push-extend (list* ncell y x) diff))
        :finally (return diff)))

(defmethod present :around ((window standard-window))
  (let ((*put-window* window))
    (call-next-method)))

(defmethod handle-winch ((tui tui))
  (with-accessors ((canvas canvas)
                   (screen screen)
                   (lines lines)
                   (columns columns))
      tui
    (let ((old-lines (array-dimension canvas 0))
          (old-columns (array-dimension canvas 1)))
      (setf canvas (adjust-array canvas (list lines columns)))
      (setf screen (adjust-array screen (list lines columns)))
      ;; fill empty columns in existing lines
      (loop :for line :below old-lines
            :do (loop :for column :from old-columns :below columns
                      :do (setf (aref canvas line column) (make-instance 'cell)
                                (aref screen line column) (make-instance 'cell))))
      ;; fill new lines
      (loop :for line :from old-lines :below lines
            :do (loop :for column :from 0 :below columns
                      :do (setf (aref canvas line column) (make-instance 'cell)
                                (aref screen line column) (make-instance 'cell)))))))

(defmethod redisplay ((tui tui))
  (with-accessors ((canvas canvas)
                   (screen screen))
      tui
    (let ((*put-buffer* canvas))
      (map () #'present (windows tui)))
    (set-style *default-style*)
    (loop :with diff = (buffer-diff screen canvas)
          :with current-style = *default-style*
          :with last-pos
          :with last-width
          :for (cell . pos) :across diff
          :do (or (and last-width last-pos (= (cdr pos) (+ (cdr last-pos) last-width)))
                  (set-cursor-position (car pos) (cdr pos)))
              (set-style-from-old current-style (cell-style cell))
              (setf current-style (cell-style cell))
              (write-string (cell-string cell))
              (setf last-pos pos
                    last-width (display-width (cell-string cell)))
          :finally (force-output)
                   (loop :for idx :below (array-total-size canvas)
                         :do (setf (row-major-aref screen idx) (row-major-aref canvas idx)
                                   (row-major-aref canvas idx) (make-instance 'cell))))))

(defun dispatch-mouse-event (window event)
  (destructuring-bind (type button col line . controlp) event
    (let* ((dimensions (dimensions window))
           (rel-y (- line (rect-y dimensions)))
           (rel-x (- col (rect-x dimensions))))
      (when (and (<= 1 rel-y (rect-rows dimensions))
                 (<= 1 rel-x (rect-cols dimensions)))
        (handle-mouse-event window type button rel-y rel-x controlp)))))

(defmethod start ((tui tui))
  (with-accessors ((canvas canvas)
                   (screen screen)
                   (lines lines)
                   (columns columns)
                   (focused-window focused-window)
                   (event-handler event-handler))
      tui
    (setf canvas (make-array (list lines columns)))
    (setf screen (make-array (list lines columns)))
    (loop :for idx :below (array-total-size canvas)
          :do (setf (row-major-aref canvas idx) (make-instance 'cell)
                    (row-major-aref screen idx) (make-instance 'cell)))
    (enable-alternate-screen)
    (set-mouse-shape :invisible)
    (enable-mouse)
    (clear-screen)
    (catch-sigwinch)
    (unwind-protect
         (catch 'tui-quit
           (loop
             (redisplay tui)
             (let ((event (read-event)))
               (when (got-winch tui)
                 (handle-winch tui))
               ;; serve the event to one window, or the TUI catchall
               (or (if (mouse-event-p event)
                       (loop :for window :in (windows tui)
                             :until (dispatch-mouse-event window event))
                       (handle-key-event focused-window event))
                   (funcall event-handler tui event)))))
      (disable-mouse)
      (set-mouse-shape :block)
      (disable-alternate-screen)
      (reset-sigwinch)
      (finish-output))))

(defmethod stop ((tui tui))
  (throw 'tui-quit nil))

;; (let ((horizontal-border-char #\─)
;;       (vertical-border-char #\│)
;;       (top-left-curved-border-char #\╭)
;;       (top-right-curved-border-char #\╮)
;;       (bottom-left-curved-border-char #\╰)
;;       (bottom-right-curved-border-char #\╯))
;;   (defun curved-box-border (window style)
;;     (check-type window window)
;;     (check-type style style)
;;     (set-style style)
;;     (let ((dimensions (dimensions window)))
;;       ;; horizontal borders and corners
;;       (let ((horizontal-border (make-string (- (rect-cols dimensions) 2)
;;                                             :initial-element horizontal-border-char)))
;;         (set-cursor-position (rect-y dimensions) (rect-x dimensions))
;;         (write-char top-left-curved-border-char)
;;         (write-string horizontal-border)
;;         (write-char top-right-curved-border-char)
;;         (set-cursor-position (+ (rect-y dimensions) (1- (rect-rows dimensions)))
;;                              (rect-x dimensions))
;;         (write-char bottom-left-curved-border-char)
;;         (write-string horizontal-border)
;;         (write-char bottom-right-curved-border-char))
;;       ;; vertical borders
;;       (loop :for line :from (1+ (rect-y dimensions))
;;             :repeat (- (rect-rows dimensions) 2)
;;             :do (set-cursor-position line (rect-x dimensions))
;;                 (write-char vertical-border-char))
;;       (loop :for line :from (1+ (rect-y dimensions))
;;             :repeat (- (rect-rows dimensions) 2)
;;             :do (set-cursor-position line
;;                                      (+ (rect-x dimensions) (- (rect-cols dimensions) 2) 1))
;;                 (write-char vertical-border-char)))
;;     (format t "~c[39m" #\esc)))
