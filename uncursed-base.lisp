(in-package :uncursed-sys)

(defclass window ()
  ((dimensions :initarg :dimensions
               :initform (error "window dimensions not provided")
               :accessor dimensions
               :type rectangle)
   (focused-p :initform nil
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

(defgeneric start (tui))
(defgeneric stop (tui)
  (:documentation "Causes the terminal to be restored to its original state immediately.
May only be called from within the dynamic-extent of a call to START."))

(defgeneric redisplay (tui))
(defgeneric handle-event (tui ev))

;;; implementation

;; sigwinch handling - redrawn on next keypress.
;; This is more reasonable than pulling in iolib/cffi for the self-pipe hack

(defmethod got-winch :before ((tui tui-base))
  ;; window resized, update dimensions
  ;; this needs to happen after event is read (not in middle!),
  ;; so next event will repaint with updated dimensions
  (when *got-sigwinch*
    (let ((dimensions (terminal-dimensions)))
      (setf (lines tui) (car dimensions)
            (columns tui) (cdr dimensions)
            (got-winch tui) t
            *got-sigwinch* nil))))

(defmethod handle-winch :after ((tui tui-base))
  (setf (got-winch tui) nil))

;; our only responsibilities at this level

(defmethod start :around ((tui tui-base))
  (let (#+(or sbcl cmu) (*terminal-io* *standard-output*))
    (destructuring-bind (lines . columns)
        (terminal-dimensions)
      (setf (lines tui) lines
            (columns tui) columns))
    (ti:set-terminal (uiop:getenv "TERM"))
    (setf (%termios tui) (setup-terminal 0))
    (call-next-method)))

(defmethod stop :around ((tui tui-base))
  (when (%termios tui)
    (unwind-protect
         (call-next-method)
      (restore-terminal (%termios tui) 0)
      (setf (%termios tui) nil))))
