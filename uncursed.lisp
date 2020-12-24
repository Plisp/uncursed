(in-package :uncursed)

;;; basic definitions

(defclass window ()
  ((%dimensions :initarg :dimensions
                :initform (error "window dimensions not provided")
                :accessor dimensions
                :type rectangle)
   (%focused-p :initform nil
               :accessor win-focused-p))
  (:documentation "Pure data."))

(defgeneric present (window))

(defclass tui-base ()
  ((%lines :initarg :lines
           :accessor lines)
   (%columns :initarg :columns
             :accessor columns)
   (%windows :initarg :windows
             :initform (list)
             :accessor windows
             :documentation "Windows in drawing order.")
   (%termios :initform nil
             :accessor %termios)
   (%got-winch :initform nil
               :accessor got-winch)))

(defgeneric run (tui))
(defgeneric stop (tui)
  (:documentation "Causes the terminal to be restored to its original state immediately.
May only be called from within the dynamic-extent of a call to RUN."))

(defgeneric redisplay (tui))
(defgeneric handle-event (tui ev))

;; sigwinch handling - redrawn on next keypress.
;; TODO maybe implement the self-pipe hack

(defmethod got-winch :before ((tui tui-base))
  ;; window resized, update dimensions
  ;; this needs to happen after event is read (not in middle!),
  ;; so next event will repaint with updated dimensions
  (when sys::*got-sigwinch*
    (let ((dimensions (terminal-dimensions)))
      (setf (lines tui) (car dimensions)
            (columns tui) (cdr dimensions)
            (got-winch tui) t
            sys::*got-sigwinch* nil))))

(defmethod handle-winch :after ((tui tui-base))
  (setf (got-winch tui) nil))

;; our only responsibilities at this level

(defmethod run :around ((tui tui-base))
  (let (#+(or sbcl cmu) (*terminal-io* *standard-output*))
    (destructuring-bind (lines . columns)
        (terminal-dimensions)
      (setf (lines tui) lines
            (columns tui) columns))
    (ti:set-terminal (uiop:getenv "TERM"))
    (setf (%termios tui) (setup-terminal 0))
    (unwind-protect
         (call-next-method)
      (when (%termios tui)
        (restore-terminal (%termios tui) 0)
        (setf (%termios tui) nil)))))

(defclass cell ()
  ((%style :initarg :style
           :initform (copy-style *default-style*)
           :reader cell-style
           :type style)
   (%string :initarg :string
            :initform (string #\space)
            :accessor cell-string
            :type simple-string
            :documentation "Represents a grapheme cluster and its display style.
Note that setf-ing the style copies over the new attributes into the existing cell-style.")))

(defmethod (setf cell-style) (new-value (cell cell))
  (let ((style (cell-style cell)))
    (setf (fg style) (fg new-value)
          (bg style) (bg new-value)
          (boldp style) (boldp new-value)
          (italicp style) (italicp new-value)
          (reversep style) (reversep new-value)
          (underlinep style) (underlinep new-value))))

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

;;; main API

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
                    :accessor focused-window
                    :type window)
   (%event-handler :initform (error "must provide an event handler")
                   :initarg :event-handler
                   :accessor event-handler)
   (%timers :initform (list)
            :accessor timers)
   (%use-palette :initform nil
                 :initarg :use-palette
                 :accessor use-palette
                 :type boolean)))

(define-condition window-bounds-error (uncursed-error)
  ((coordinate :initarg :coordinate
               :reader window-bounds-error-coordinate
               :type integer)
   (bounds :initarg :bounds
           :reader window-bounds-error-bounds
           :type (or (eql :line) (eql :column)))
   (window :initarg :window
           :reader window-bounds-error-window
           :type window))
  (:report (lambda (condition stream)
             (format stream "~d is not a valid ~a for ~a"
                     (window-bounds-error-coordinate condition)
                     (window-bounds-error-bounds condition)
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
             (format stream "Coordinate ~d,~d intersects wide character in ~a"
                     (wide-char-overwrite-error-y condition)
                     (wide-char-overwrite-error-x condition)
                     (wide-char-overwrite-error-buffer condition))))
  (:documentation "Signaled if an attempt is made to overwrite a wide character."))

(defgeneric handle-resize (tui))

(defclass standard-window (window)
  ())

(defgeneric handle-mouse-event (window tui button state line col &key &allow-other-keys)
  (:documentation "Interface may change. Methods may optionally accept boolean keyword
arguments :shift, :alt, :control and :meta."))
(defgeneric handle-key-event (window tui event)
  (:documentation "Interface may change."))

(defvar *put-buffer*)
(defvar *put-window*)

;; note that triple-width is rare enough (e.g. three-em-dash) and terminal support is
;; so lacking that there's no point supporting it
(defun put (char line col &optional style (put-buffer *put-buffer*) (put-window *put-window*))
  (declare (optimize safety debug)
           (type (simple-array cell) put-buffer))
  (or (and put-buffer put-window) (error "PUT-BUFFER and PUT-WINDOW not both provided"))
  (check-type put-buffer buffer)
  (check-type put-window window)
  (check-type char character)
  (check-type style (or style null))
  (let ((dimensions (dimensions put-window))
        (width (character-width char)))
    (or (<= 1 line (rect-rows dimensions))
        (error 'window-bounds-error
               :coordinate line
               :bounds :line
               :window put-window))
    (or (<= 1 col (- (rect-cols dimensions) (max 0 (1- width))))
        (error 'window-bounds-error
               :coordinate col
               :bounds :column
               :window put-window))
    (let* ((cell-y (+ (rect-y dimensions) (1- line)))
           (cell-x (+ (rect-x dimensions) (1- col)))
           (cell (aref put-buffer cell-y cell-x)))
      (if (zerop width) ; this should not be common
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
                      (error 'wide-char-overwrite-error
                             :y cell-y
                             :x (1- cell-x)
                             :buffer put-buffer)
                    (overwrite-char ()
                      :report "Overwrite the wide character"
                      (setf (cell-string prev) (string #\space)))
                    (ignore-put ()
                      :report "Do nothing"
                      (return-from put))))))
            ;; width > 1: clear next character (we checked for room above)
            (when (> width 1)
              (let ((next (aref put-buffer cell-y (1+ cell-x))))
                (when (wide-cell-p next)
                  (restart-case
                      (error 'wide-char-overwrite-error
                             :y cell-y
                             :x (1+ cell-x)
                             :buffer put-buffer)
                    ;; turn the next-next character into a space if the next was wide
                    ;; [.][old][""] -> [new][""][ ] erases old character
                    (overwrite-char ()
                      :report "Overwrite the wide character"
                      (setf (cell-string (aref put-buffer cell-y (+ 2 cell-x))) (string #\space)))
                    (ignore-put ()
                      :report "Do nothing"
                      (return-from put))))
                (setf (cell-string next) (make-string 0))))
            ;; finally write the character into its cell
            (setf (cell-string cell) (string char))
            (and style (setf (cell-style cell) style)))))
    width))

(defun puts (string line col &optional style (put-buffer *put-buffer*) (put-window *put-window*))
  (or (and put-buffer put-window) (error "PUT-BUFFER and PUT-WINDOW not both provided"))
  (check-type put-buffer buffer)
  (check-type put-window window)
  (check-type string string)
  (check-type style (or style null))
  (let ((dimensions (dimensions put-window))
        (string-display-width (display-width string))
        (last-non-combining-char-pos (position-if-not #'zerop string
                                                      :key #'character-width
                                                      :from-end t)))
    (or (<= 1 line (rect-rows dimensions))
        (error 'window-bounds-error
               :coordinate line
               :bounds :line
               :window put-window))
    (or (plusp col)
        (error 'window-bounds-error
               :coordinate col
               :bounds :column
               :window put-window))
    (or (<= (+ (1- col) string-display-width) (rect-cols dimensions))
        (error 'window-bounds-error
               :coordinate (+ (1- col) string-display-width)
               :bounds :column
               :window put-window))
    (if last-non-combining-char-pos
        (let* ((last-non-combining-char (char string last-non-combining-char-pos))
               (last-non-combining-char-visual-offset
                 (reduce #'+ string :key #'character-width :end last-non-combining-char-pos))
               (first-non-combining-char-pos
                 (position-if-not #'zerop string :key #'character-width))
               (first-non-combining-char (char string first-non-combining-char-pos))
               (first-cell-y (+ (rect-y dimensions) (1- line)))
               (first-cell-x (+ (rect-x dimensions) (1- col)))
               (first-cell (aref put-buffer first-cell-y first-cell-x))
               first-to-overwrite)
          ;; signal overwrite error for first character early, *not writing to the buffer*
          (unless (zerop first-cell-x)
            (let ((prev (aref put-buffer first-cell-y (1- first-cell-x))))
              (when (wide-cell-p prev)
                (restart-case
                    (error 'wide-char-overwrite-error
                           :y first-cell-y
                           :x (1- first-cell-x)
                           :buffer put-buffer)
                  (overwrite-char ()
                    :report "Overwrite the wide character"
                    (setf first-to-overwrite prev))
                  (ignore-put ()
                    :report "Do nothing"
                    (return-from puts))))))
          ;; now attempt to put, allowing writing to the buffer since we've treated the first char
          ;; this may overwrite after the end of the string
          (or (put last-non-combining-char
                   line (+ col last-non-combining-char-visual-offset)
                   style
                   put-buffer put-window)
              ;; if IGNORE-PUT restart selected, abort here, before anything is written
              (return-from puts))
          ;; write first character after we've ascertained that the caller doesn't want to abort
          ;; via IGNORE-PUT. Also perform overwrite for first char if restart was selected earlier
          (unless (= first-non-combining-char-pos last-non-combining-char-pos)
            (setf (cell-string first-cell) (string first-non-combining-char))
            (and style (setf (cell-style first-cell) style))
            (when first-to-overwrite
              (setf (cell-string first-to-overwrite) (string #\space))))
          ;; put the rest normally, overwriting any previous contents unconditionally
          ;; leading combining characters are *discarded* (this is probably correct behavior?)
          (loop :with put-col = col
                :with last-width = (character-width first-non-combining-char)
                :for i :from (1+ first-non-combining-char-pos) :below last-non-combining-char-pos
                :for char = (char string i)
                :do (let ((width (character-width char)))
                      (when (plusp width)
                        (incf put-col last-width)
                        (setf last-width width)))
                    (handler-bind ((wide-char-overwrite-error
                                     (lambda (e)
                                       (declare (ignore e))
                                       (invoke-restart 'overwrite-char))))
                      (put char line put-col style put-buffer put-window)))
          ;; write trailing combining characters
          (loop :for i :from (1+ last-non-combining-char-pos) :below (length string)
                :do (put (char string i)
                         line (+ col last-non-combining-char-visual-offset)
                         style
                         put-buffer put-window)))
        ;; all combining characters, all fit at the index
        (loop :for char :across string
              :do (put char line col style put-buffer put-window)))
    string-display-width))

(defun put-style (style rect &optional (put-buffer *put-buffer*) (put-window *put-window*))
  (or (and put-buffer put-window) (error "PUT-BUFFER and PUT-WINDOW not both provided"))
  (check-type put-buffer buffer)
  (check-type put-window window)
  (check-type rect rectangle)
  (check-type style style)
  (let ((dimensions (dimensions put-window)))
    ;; TODO not the most informative errors?
    (or (<= 0 (rect-y rect))
        (error 'window-bounds-error
               :coordinate (rect-y rect)
               :bounds :line
               :window put-window))
    (or (<= (+ (rect-y rect) (rect-rows rect)) (rect-rows dimensions))
        (error 'window-bounds-error
               :coordinate (+ (rect-y rect) (rect-rows rect))
               :bounds :line
               :window put-window))
    (or (<= 0 (rect-x rect))
        (error 'window-bounds-error
               :coordinate (rect-x rect)
               :bounds :column
               :window put-window))
    (or (<= (+ (rect-x rect) (rect-cols rect)) (rect-cols dimensions))
        (error 'window-bounds-error
               :coordinate (+ (rect-x rect) (rect-cols rect))
               :bounds :column
               :window put-window))
    (loop :repeat (rect-rows rect)
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

(defmethod handle-resize :before ((tui tui))
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

(defmethod redisplay :around ((tui tui))
  ;; clear canvas of any junk from last time before drawing
  (loop :with canvas = (canvas tui)
        :for idx :below (array-total-size canvas)
        :do (setf (row-major-aref canvas idx) (make-instance 'cell)))
  (call-next-method))

(defmethod redisplay ((tui tui))
  (with-accessors ((canvas canvas)
                   (screen screen))
      tui
    ;; present windows
    (let ((*put-buffer* canvas))
      (map () #'present (windows tui)))
    ;; render diff to terminal
    (set-style *default-style*)
    (loop :with diff = (buffer-diff screen canvas)
          :with current-style = *default-style*
          :with last-pos
          :with last-width
          :for (cell . pos) :across diff
          :do (let ((cell-width (display-width (cell-string cell))))
                (or (and last-width last-pos
                         (= (car pos) (car last-pos))
                         (= (cdr pos) (+ (cdr last-pos) last-width)))
                    (set-cursor-position (car pos) (cdr pos)))
                (set-style-from-old current-style (cell-style cell) (use-palette tui))
                (setf current-style (cell-style cell))
                (write-string (cell-string cell))
                (setf last-pos pos
                      last-width cell-width))
          :finally (force-output))
    ;; swap buffers
    (rotatef screen canvas)))

(defun dispatch-mouse-event (window tui event)
  (destructuring-bind (button state line col &rest modifiers) event
    (let* ((dimensions (dimensions window))
           (relative-line (- line (rect-y dimensions)))
           (relative-column (- col (rect-x dimensions))))
      (when (and (<= 1 relative-line (rect-rows dimensions))
                 (<= 1 relative-column (rect-cols dimensions)))
        (let ((shift (member :shift modifiers))
              (alt (member :alt modifiers))
              (control (member :control modifiers))
              (meta (member :meta modifiers)))
          (handle-mouse-event window tui button state relative-line relative-column
                              :shift shift :alt alt :control control :meta meta))))))

(defclass timer ()
  ((%callback :initarg :callback
              :accessor timer-callback
              :documentation "A function that is run when the timer expires. It is a function
of one argument, the TUI object it was scheduled with and is expected to return one value:
either the next timer expiry interval or NIL, meaning to cancel the timer.")
   (%interval :initarg :interval
              :accessor timer-interval
              :type (real 0))))

(defun make-timer (interval callback)
  (make-instance 'timer :interval interval :callback callback))

(defmethod schedule-timer ((tui tui) timer)
  (let ((interval (timer-interval timer))
        (callback (timer-callback timer)))
    (check-type callback function)
    (check-type interval (real 0))
    (push timer (timers tui)) ; TODO maybe use a heap if needed
    (setf (timers tui) (stable-sort (timers tui) #'< :key #'timer-interval))))

(defmethod unschedule-timer ((tui tui) timer)
  (setf (timers tui) (delete timer (timers tui))))

(defmethod run ((tui tui))
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
    (clear-screen)
    (enable-mouse)
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
                     ;; TODO mechanism for closing windows and creating them during event handling
                     (or (if (mouse-event-p event)
                             (loop :for window :in (copy-list (windows tui))
                                   :thereis (dispatch-mouse-event window tui event))
                             (handle-key-event focused-window tui event))
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
      (set-cursor-shape :block)
      (disable-alternate-screen)
      (loop :while (read-event-timeout 0)) ; drain events
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
