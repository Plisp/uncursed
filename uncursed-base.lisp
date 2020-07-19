(in-package :uncursed-sys)

(defclass window ()
  ((y :initarg :y
      :initform (error "window y position not provided")
      :accessor win-y) ; just use a package
   (x :initarg :x
      :initform (error "window x position not provided")
      :accessor win-x)
   (lines :initarg :lines
          :initform (error "window lines not provided")
          :accessor win-lines)
   (columns :initarg :columns
            :initform (error "window columns not provided")
            :accessor win-cols)
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
               :accessor %got-winch)))

(defgeneric start (tui))
(defgeneric stop (tui)
  (:documentation "Causes the terminal to be restored to its original state immediately.
May only be called from within the dynamic-extent of a call to START."))

(defgeneric redisplay (tui))
(defgeneric handle-winch (tui))
(defgeneric handle-event (tui ev))

;;; implementation

(defmethod handle-winch ((tui tui-base))
  ;; window resized, update dimensions
  ;; this needs to happen after event is read (not in middle!),
  ;; so next event will repaint with updated dimensions
  (when (%got-winch tui)
    (let ((dimensions (terminal-dimensions)))
      (setf (lines tui) (car dimensions)
            (columns tui) (cdr dimensions)
            (%got-winch tui) nil))))

;; sigwinch handling - redrawn on next keypress.
;; This is more reasonable than pulling in iolib/cffi for the self-pipe hack

(defconstant +sigwinch+ c-sigwinch
  "Signal number of SIGWINCH.")

(defvar *got-sigwinch* nil)
(let (#+ccl sigwinch-thread)
  (defun catch-sigwinch ()
    "Enables handling SIGWINCH. May fail silently."
    #+ccl (setf sigwinch-thread
                (ccl:process-run-function "sigwinch thread"
                                          (lambda ()
                                            (loop
                                              (ccl:wait-for-signal 28 +sigwinch+)
                                              (setf *got-sigwinch* t)))))
    #+ecl (ext:set-signal-handler +sigwinch+ (lambda () (setf *got-sigwinch* t)))
    #+sbcl (sb-sys:enable-interrupt +sigwinch+
                                    (lambda (signo info ucontext)
                                      (declare (ignore signo info ucontext))
                                      (setf *got-sigwinch* t))))

  (defun reset-sigwinch ()
    #+ccl (ccl:process-kill sigwinch-thread)
    #+ecl (ext:set-signal-handler +sigwinch+ :default)
    #+sbcl (sb-sys:enable-interrupt +sigwinch+ :default)))

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
