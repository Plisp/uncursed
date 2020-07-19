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

(defmethod cell/= (cell1 cell2)
  (or (style-difference (cell-style cell1) (cell-style cell2))
      (string/= (cell-string cell1) (cell-string cell2))))

(defclass tui (tui-base)
  ((%screen :initarg :screen
            :accessor screen
            :type (array cell)
            :documentation "The contents of the screen")
   (%canvas :initarg :canvas
            :accessor canvas
            :type (array cell)
            :documentation "The contents to be drawn to the screen")
   (%focused-window :initarg :focused-window
                    :initform nil
                    :accessor focused-window
                    :type window)
   (%event-handler :initform (error "must provide an event handler")
                   :initarg :event-handler
                   :accessor event-handler)))

(defclass standard-window (window)
  ())

(defgeneric handle-mouse-event (window type button y x controlp))
(defgeneric handle-key-event (window event))

(defun handle-window-event (window event)
  (if (mouse-event-p event)
      (destructuring-bind (type button col line . controlp) event
        (handle-mouse-event window type button
                            (- line (win-y window))
                            (- col (win-x window))
                            controlp))
      (handle-key-event window event)))

(defvar *put-buffer*)
(defvar *put-window*)

(defun put (char line col &optional style (put-buffer *put-buffer*) (put-window *put-window*))
  (or (and put-buffer put-window)
      (error "PUT-BUFFER and PUT-WINDOW not both provided"))
  (check-type char character)
  (check-type style (or style null))
  (or (typep line `(integer 1 ,(win-lines put-window)))
      (error "LINE is not in bounds [1:~d]" (win-lines put-window)))
  (let ((width (character-width char)))
    (or (typep col `(integer 1 ,(- (win-cols put-window) (1- width))))
        (error "COL is not in bounds [1:~d]" (- (win-cols put-window) (1- width))))
    ;; logic
    (let ((cell (aref put-buffer (+ (1- line) (win-y put-window)) (+ (1- col) (win-x put-window)))))
      (and style (setf (cell-style cell) style))
      (if (zerop width)
          (error "zero-width characters not handled yet, this is where VECTOR-PUSH-EXTEND comes in")
          (progn ; first erase nearby cells, if wide character
            (when (> width 1)
              ;; clear previous wide character (if applicable)
              ;; [wold][""] -> [" "][wnew]
              (let ((prev (unless (zerop (+ (1- col) (win-x put-window)))
                            (aref put-buffer
                                  (+ (1- line) (win-y put-window))
                                  (+ (1- col) (win-x put-window) -1)))))
                (when (and prev (> (display-width (cell-string prev)) 1))
                  (setf (cell-string prev) (make-adjustable-string (string #\space)))))
              ;; clear next character unconditionally (we checked for room above)
              ;; and turn the next-next characater into a space if the next was wide
              ;; [.][wold][""] -> [wnew][""][ ] erases old character
              (let ((next (aref put-buffer
                                (+ (1- line) (win-y put-window))
                                (+ (1- col) (win-x put-window) 1))))
                (when (> (display-width (cell-string next)) 1)
                  (setf (cell-string (aref put-buffer
                                           (+ (1- line) (win-y put-window))
                                           (+ (1- col) (win-x put-window) 2)))
                        (make-adjustable-string (string #\space))))
                (setf (cell-string next) (make-adjustable-string))))
            (setf (cell-string cell) (make-adjustable-string (string char))))))))

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
                  (setf (cursor-position) pos))
              (set-style-from-old current-style (cell-style cell))
              (setf current-style (cell-style cell))
              (write-string (cell-string cell))
              (setf last-pos pos
                    last-width (display-width (cell-string cell)))
          :finally (force-output)
                   (loop :for idx :below (array-total-size canvas)
                         :do (setf (cell-string (row-major-aref screen idx))
                                   (cell-string (row-major-aref canvas idx))
                                   (cell-style (row-major-aref screen idx))
                                   (cell-style (row-major-aref canvas idx)))))))

(defmethod start ((tui tui))
  (with-accessors ((canvas canvas)
                   (screen screen)
                   (lines lines)
                   (columns columns)
                   (focused-window focused-window)
                   (event-handler event-handler))
      tui
    (setf canvas (make-array (list lines columns) :element-type 'cell))
    (setf screen (make-array (list lines columns) :element-type 'cell))
    (loop :for idx :below (array-total-size canvas)
          :do (setf (row-major-aref canvas idx) (make-instance 'cell)
                    (row-major-aref screen idx) (make-instance 'cell)))
    (enable-alternate-screen)
    (set-mouse-shape :invisible)
    (enable-mouse)
    (catch-sigwinch)
    (unwind-protect
         (catch 'tui-quit
           (loop
             (redisplay tui)
             (let ((event (read-event)))
               (handle-winch tui)
               (unless (handle-window-event focused-window event)
                 (funcall event-handler tui event)))))
      (disable-mouse)
      (set-mouse-shape :block)
      (disable-alternate-screen)
      (reset-sigwinch)
      (finish-output))))

(defmethod stop ((tui tui))
  (throw 'tui-quit nil))
