(in-package :uncursed)

(defclass cell ()
  ((style :initform *default-style*
          :accessor cell-style
          :type style)
   (string :initform (make-string 1 :initial-element #\space)
           :accessor cell-string
           :type simple-string
           :documentation "A grapheme cluster")))

(defmethod print-object ((cell cell) stream)
  (format stream "#<cell string:~a style:~a>" (cell-string cell) (cell-style cell)))

(defun cell/= (cell1 cell2)
  (declare (optimize speed))
  (or (style-difference (cell-style cell1) (cell-style cell2))
      (string/= (the simple-string (cell-string cell1))
                (the simple-string (cell-string cell2)))))

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
                   :accessor event-handler)
   (%timers :initform (list)
            :accessor timers)))

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

(defgeneric handle-resize (tui))

(defclass standard-window (window)
  ())

(defgeneric handle-mouse-event (window type button line col controlp))
(defgeneric handle-key-event (window event))

(defvar *put-buffer*)
(defvar *put-window*)

(defun put (char line col &optional style (put-buffer *put-buffer*) (put-window *put-window*))
  (or (and put-buffer put-window) (error "PUT-BUFFER and PUT-WINDOW not both provided"))
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
      (and style (setf (cell-style cell) (copy-style style)))
      (if (zerop width)
          (let* ((string (cell-string cell))
                 (old-length (length string)))
            (setf (cell-string cell) (adjust-array string (1+ old-length))
                  (schar (cell-string cell) old-length) char))
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
                      (setf (cell-string prev) (make-string 1 :initial-element #\space)))
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
                            (make-string 1 :initial-element #\space)))
                    (ignore-put ()
                      :report "Do nothing"
                      (return-from put t))))
                (setf (cell-string next) (make-string 0))))
            ;; finally write the character into its cell
            (setf (cell-string cell) (make-string 1 :initial-element char)))))
    width))

;; (defun put-string (string line col &optional style)
;;   ())

(defun put-style (style rect &optional (put-buffer *put-buffer*) (put-window *put-window*))
  (or (and put-buffer put-window) (error "PUT-BUFFER and PUT-WINDOW not both provided"))
  (check-type put-buffer buffer)
  (check-type put-window window)
  (check-type rect rectangle)
  (check-type style style)
  (let ((dimensions (dimensions put-window)))
    ;; TODO not the most informative errors?
    (or (<= 0 (rect-y rect))
        (error 'window-bounds-error :coordinate (rect-y rect)
                                    :bounds (cons 0 (rect-rows dimensions))
                                    :window put-window))
    (or (<= (+ (rect-y rect) (rect-rows rect)) (rect-rows dimensions))
        (error 'window-bounds-error :coordinate (+ (rect-y rect) (rect-rows rect))
                                    :bounds (cons 0 (rect-rows dimensions))
                                    :window put-window))
    (or (<= 0 (rect-x rect))
        (error 'window-bounds-error :coordinate (rect-x rect)
                                    :bounds (cons 0 (rect-cols dimensions))
                                    :window put-window))
    (or (<= (+ (rect-x rect) (rect-cols rect)) (rect-cols dimensions))
        (error 'window-bounds-error :coordinate (+ (rect-x rect) (rect-cols rect))
                                    :bounds (cons 0 (rect-cols dimensions))
                                    :window put-window))
    (loop :with style = (copy-style style) ; to prevent mutability shenanigans
          :repeat (rect-rows rect)
          :for y :from (+ (rect-y dimensions) (rect-y rect))
          :do (loop :repeat (rect-cols rect)
                    :for x :from (+ (rect-x dimensions) (rect-x rect))
                    :do (setf (cell-style (aref put-buffer y x)) style)))))

(defun buffer-diff (old new)
  (declare (optimize speed)
           (type (simple-array cell 2) old new))
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

(defmethod handle-resize ((tui tui))
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
          :do (or (and last-width last-pos
                       (= (car pos) (car last-pos))
                       (= (cdr pos) (+ (cdr last-pos) last-width)))
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
           (relative-line (- line (rect-y dimensions)))
           (relative-column (- col (rect-x dimensions))))
      (when (and (<= 1 relative-line (rect-rows dimensions))
                 (<= 1 relative-column (rect-cols dimensions)))
        (handle-mouse-event window type button relative-line relative-column controlp)))))

(defclass timer ()
  ((callback :initarg :callback
             :accessor timer-callback
             :documentation "A function that is run when the timer expires. It is a function
of one argument, the TUI object it was scheduled with and is expected to return one value:
either the next timer expiry interval or NIL, meaning to cancel the timer.")
   (interval :initarg :interval
             :accessor timer-interval
             :type (real 0))))

(defun make-timer (interval callback)
  (make-instance 'timer :interval interval :callback callback))

(defmethod schedule-timer ((tui tui) timer)
  (let ((interval (timer-interval timer))
        (callback (timer-callback timer)))
    (check-type callback function)
    (check-type interval (real 0))
    (push timer (timers tui)) ; TODO maybe do this better
    (setf (timers tui) (stable-sort (timers tui) #'< :key #'timer-interval))))

(defmethod unschedule-timer ((tui tui) timer)
  (setf (timers tui) (delete timer (timers tui))))

(defmethod start ((tui tui))
  (with-accessors ((canvas canvas)
                   (screen screen)
                   (timers timers)
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
             (let* ((before-read (get-internal-real-time))
                    (next-timer (pop timers))
                    (next-timeout (when next-timer (timer-interval next-timer)))
                    (event (if next-timer
                               (read-event-timeout next-timeout)
                               (read-event))))
               (when (got-winch tui)
                 (handle-resize tui))
               (if event
                   ;; serve the event to one window, or the TUI catchall
                   (let ((now (get-internal-real-time)))
                     (push next-timer timers)
                     (map () #'(lambda (timer)
                                 (when timer ;TIMER=NIL?
                                   (setf (timer-interval timer)
                                         (max (- (timer-interval timer)
                                                 (/ (- now before-read)
                                                    internal-time-units-per-second))
                                              0))))
                          timers)
                     (or (if (mouse-event-p event)
                             (loop :for window :in (windows tui)
                                   :until (dispatch-mouse-event window event))
                             (handle-key-event focused-window event))
                         (funcall event-handler tui event)))
                   ;; process expired timer
                   (progn
                     (map () #'(lambda (timer)
                                 (decf (timer-interval timer) next-timeout))
                          timers)
                     (let ((next-interval (funcall (timer-callback next-timer) tui)))
                       (when next-interval
                         (setf (timer-interval next-timer) next-interval)
                         (schedule-timer tui next-timer))))))))
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
