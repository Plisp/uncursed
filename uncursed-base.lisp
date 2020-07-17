(in-package :uncursed-sys)

(defclass window ()
  ((y :initarg :y
      :accessor win-y) ; just use a package
   (x :initarg :x
      :accessor win-x)
   (lines :initarg :lines
          :accessor win-lines)
   (columns :initarg :columns
            :accessor win-cols)
   (focused-p :initform nil
              :accessor win-focused-p))
  (:documentation "Pure data."))

(defgeneric present (window))
(defgeneric handle-window-event (window ev))

(defclass tui-base ()
  ((%lines :initarg :lines
           :accessor tui-lines)
   (%columns :initarg :columns
             :accessor tui-columns)
   (%windows :initarg :windows
             :initform (list)
             :accessor windows
             :documentation "Windows in drawing order.")
   (%focused-window :initarg :focused-window
                    :initform nil
                    :accessor focused-window
                    :type window)
   (%termios :initform nil
             :accessor %termios)
   (%got-winch :initform nil
               :accessor %got-winch)))

(defgeneric start (tui))
(defgeneric stop (tui))
(defgeneric redisplay (tui))
(defgeneric handle-winch (tui))
(defgeneric handle-event (tui ev))

;;; implementation

(defmethod handle-event ((tui tui-base) ev)
  nil)

(defmethod handle-winch ((tui tui-base))
  ;; window resized, update dimensions
  ;; this needs to happen after event is read (not in middle!),
  ;; so next event will repaint with updated dimensions
  (when (%got-winch tui)
    (let ((dimensions (terminal-dimensions)))
      (setf (tui-lines tui) (car dimensions)
            (tui-columns tui) (cdr dimensions)
            (%got-winch tui) nil))))

(defmethod redisplay ((tui tui-base))
  (loop :for window :in (windows tui)
        :do (unless (eq window (focused-window tui))
              (present window))
        :finally (present (focused-window tui))
                 (force-output)))

;; sigwinch handling - redrawn on next keypress.
;; This is more reasonable than pulling in iolib/cffi for the self-pipe hack

(defconstant +sigwinch+ c-sigwinch
  "Signal number of SIGWINCH.")

(defvar *tui-list* nil)

(defvar *got-sigwinch* nil)
(let (#+ccl sigwinch-thread)
  (defun catch-sigwinch ()
    "Enables handling SIGWINCH. May fail silently."
    #+ccl (setf sigwinch-thread
                (bt:make-thread (lambda ()
                                  (loop
                                    (wait-for-signal 28 +sigwinch+)
                                    (setf *got-sigwinch* t)))))
    #+ecl (ext:set-signal-handler +sigwinch+ (lambda () (setf *got-sigwinch* t)))
    #+sbcl (sb-sys:enable-interrupt +sigwinch+
                                    (lambda (signo info ucontext)
                                      (declare (ignore signo info ucontext))
                                      (setf *got-sigwinch* t))))

  (defun reset-sigwinch ()
    #+ccl (bt:interrupt-thread sigwinch-thread (lambda () (return)))
    #+ecl (ext:set-signal-handler +sigwinch+ :default)
    #+sbcl (sb-sys:enable-interrupt +sigwinch+ :default)))

;; our only responsibilities at this level

(defmethod start ((tui tui-base))
  (ti:set-terminal (uiop:getenv "TERM"))
  (setf (%termios tui) (setup-terminal 0))
  (enable-mouse)
  (ti:tputs ti:enter-ca-mode)
  (ti:tputs ti:clear-screen)
  (force-output))

(defmethod stop ((tui tui-base))
  (when (%termios tui)
    (restore-terminal (%termios tui) 0)
    (setf (%termios tui) nil)
    (disable-mouse)
    (ti:tputs ti:orig-pair)
    (ti:tputs ti:exit-ca-mode)
    (force-output)))

(let ((horizontal-border-char #\─)
      (vertical-border-char #\│)
      (top-left-curved-border-char #\╭)
      (top-right-curved-border-char #\╮)
      (bottom-left-curved-border-char #\╰)
      (bottom-right-curved-border-char #\╯))
  (defun curved-box-border (window)
    (format t "~c[38;2;22;175;150m" #\esc)
    ;; horizontal borders
    (let ((horizontal-border
            (make-string (- (win-cols window) 2) :initial-element horizontal-border-char)))
      (ti:tputs ti:cursor-address (win-y window) (win-x window))
      (write-char top-left-curved-border-char)
      (write-string horizontal-border)
      (write-char top-right-curved-border-char)
      (ti:tputs ti:cursor-address (+ (win-y window) (1- (win-lines window))) (win-x window))
      (write-char bottom-left-curved-border-char)
      (write-string horizontal-border))
    (write-char bottom-right-curved-border-char)
    ;; vertical borders
    (loop :for line :from (1+ (win-y window))
          :repeat (- (win-lines window) 2)
          :do (ti:tputs ti:cursor-address line (win-x window))
              (write-char vertical-border-char))
    (loop :for line :from (1+ (win-y window))
          :repeat (- (win-lines window) 2)
          :do (ti:tputs ti:cursor-address line (+ (win-x window) (- (win-cols window) 2) 1))
              (write-char vertical-border-char))
    (format t "~c[39m" #\esc)))
