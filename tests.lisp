(defpackage #:uncursed-tests
  (:use #:cl)
  (:export #:run-tests))
(in-package #:uncursed-tests)

(defvar *now* 0)
(defvar *checks* 0)
(defun check (value)
  (incf *checks*)
  (assert value))
(defun advance (seconds)
  (incf *now* (* seconds internal-time-units-per-second)))
(defmacro with-functions (bindings &body body)
  (let ((saved (gensym)))
    `(let ((,saved (mapcar (lambda (name)
                             (cons name (when (fboundp name) (fdefinition name))))
                           ',(mapcar #'first bindings))))
       (unwind-protect
            (progn
              ,@(loop for (name value) in bindings
                      collect `(setf (fdefinition ',name) ,value))
              ,@body)
         (dolist (entry ,saved)
           (if (cdr entry) (setf (fdefinition (car entry)) (cdr entry))
               (fmakunbound (car entry))))))))

(defun test-accounting ()
  (let* ((ui (make-instance 'uncursed:tui))
         (old (uncursed:make-timer 10 (constantly nil)))
         (new (uncursed:make-timer 5 (constantly nil))))
    (uncursed:schedule-timer ui old)
    (uncursed::call-with-loop-timers
     ui (lambda ()
          (advance 2)
          (uncursed:schedule-timer ui new)
          (check (= 8 (uncursed::timer-interval old)))
          (check (= 5 (uncursed::timer-interval new)))
          (advance 1)
          (check (= 4 (uncursed::next-timer-interval ui)))))
    (check (null (uncursed::timer-clock ui)))))

(defun test-recursion ()
  (let* ((ui (make-instance 'uncursed:tui))
         (fired nil)
         (old (uncursed:make-timer 10 (constantly nil)))
         (new (uncursed:make-timer 3 (lambda (ui)
                                      (declare (ignore ui))
                                      (setf fired t)
                                      nil))))
    (uncursed:schedule-timer ui old)
    (catch 'leave-outer
      (uncursed::call-with-loop-timers
       ui (lambda ()
            (let ((clock (uncursed::timer-clock ui)))
              (advance 2)
              (catch 'leave-inner
                (uncursed::call-with-loop-timers
                 ui (lambda ()
                      (check (= clock (uncursed::timer-clock ui)))
                      (check (= 8 (uncursed::next-timer-interval ui)))
                      (uncursed:schedule-timer ui new)
                      (uncursed::call-with-loop-timers
                       ui (lambda ()
                            (advance 3)
                            (uncursed::process-expired-timer ui)
                            (check fired)))
                      (check (= 5 (uncursed::next-timer-interval ui)))
                      (advance 1)
                      (throw 'leave-inner nil))))
              (check (uncursed::timer-clock ui))
              (check (= 4 (uncursed::next-timer-interval ui)))
              (advance 1)
              (throw 'leave-outer nil)))))
    (check (null (uncursed::timer-clock ui)))
    (check (= 3 (uncursed::timer-interval old)))))

(defun test-recursive-timer-error ()
  (let ((ui (make-instance 'uncursed:tui)) (calls 0) (fired nil))
    (uncursed:schedule-timer
     ui (uncursed:make-timer 0 (lambda (ui)
                                (declare (ignore ui))
                                (incf calls)
                                (error "Timer failed"))))
    (uncursed:schedule-timer
     ui (uncursed:make-timer 1 (lambda (ui)
                                (declare (ignore ui))
                                (setf fired t)
                                nil)))
    (with-functions
        ((uncursed::run-event-loop
          (lambda (ui input)
            (declare (ignore input))
            (uncursed::process-expired-timer ui))))
      (catch 'abort-timer
        (handler-bind
            ((error (lambda (condition)
                      (declare (ignore condition))
                      (advance 1)
                      (uncursed:run-loop ui)
                      (throw 'abort-timer nil))))
          (uncursed:run-loop ui))))
    (check (= 1 calls))
    (check fired)
    (check (null (pileup:heap-top (uncursed::timers ui))))
    (check (null (uncursed::timer-clock ui)))))

(defun test-callback-time ()
  (let* ((ui (make-instance 'uncursed:tui))
         (other (uncursed:make-timer 10 (constantly nil)))
         (repeat (uncursed:make-timer 1 (lambda (ui)
                                          (declare (ignore ui))
                                          (advance 1/4)
                                          1))))
    (uncursed:schedule-timer ui other)
    (uncursed:schedule-timer ui repeat)
    (uncursed::call-with-loop-timers
     ui (lambda ()
          (advance 1)
          (uncursed::process-expired-timer ui)
          (check (= 1 (uncursed::next-timer-interval ui)))
          (check (= 35/4 (uncursed::timer-interval other)))))))

#+unix
(defun test-unix-loop ()
  (let ((ui (make-instance 'uncursed:tui)) (waits nil) (calls 0))
    (cffi:with-foreign-object (pipe :int 2)
      (setf (cffi:mem-aref pipe :int 0) 0
            (cffi:mem-aref pipe :int 1) 0
            (uncursed::%wakeup-pipe ui) pipe
            (uncursed::%winch-pipe ui) pipe)
      (uncursed:schedule-timer
       ui (uncursed:make-timer 1 (lambda (ui)
                                   (when (= (incf calls) 3) (uncursed:stop ui))
                                   1)))
      (with-functions
          ((uncursed:redisplay (constantly t))
           (uncursed-sys::select
            (lambda (n reads writes errors timeout)
              (declare (ignore n reads writes errors))
              (let ((seconds (cffi:foreign-slot-value
                              timeout '(:struct uncursed-sys::c-timeval)
                              'uncursed-sys::c-tv-sec)))
                (push seconds waits)
                (advance seconds))
              0)))
        (uncursed:run-loop ui)))
    (check (equal '(1 1 1) (reverse waits)))
    (check (= 1 (uncursed::counter ui)))
    (check (null (uncursed::timer-clock ui)))))

(defun test-generation ()
  (let ((ui (make-instance 'uncursed:tui)) (depth 0) (clock nil))
    (with-functions
        ((uncursed::run-event-loop
          (lambda (ui input)
            (declare (ignore input))
            (if (zerop depth)
                (progn
                  (incf depth)
                  (setf clock (uncursed::timer-clock ui))
                  (uncursed:run-loop ui)
                  (check (= clock (uncursed::timer-clock ui))))
                (check (= clock (uncursed::timer-clock ui)))))))
      (uncursed:run-loop ui))
    (check (= 2 (uncursed::counter ui)))
    (check (null (uncursed::timer-clock ui)))
    (let ((continued nil))
      (catch 'uncursed::next-redisplay
        (uncursed::with-loop-generation (ui)
          (incf (uncursed::counter ui)))
        (setf continued t))
      (check (not continued)))))

#+windows
(defun test-windows-loop ()
  (let* ((ui (make-instance 'uncursed:tui))
         (waits nil) (calls 0) (notifications 0))
    (setf (uncursed::%termios ui) (list (cffi:null-pointer))
          (uncursed::%wakeup-pipe ui) (cffi:null-pointer))
    (with-functions
        ((uncursed:redisplay (constantly t))
         (uncursed-sys::WaitForMultipleObjects
          (lambda (n handles all timeout)
            (declare (ignore n handles all))
            (push timeout waits)
            (if (= (incf calls) 1) 0 (uncursed:stop ui))))
         (uncursed-sys:win-events-left
          (lambda (handle)
            (declare (ignore handle))
            (incf notifications)
            '(t . 0)))
         (uncursed:handle-resize
          (lambda (ui) (incf (uncursed::counter ui)))))
      (uncursed:run-loop ui))
    (check (equal '(4294967295 4294967295) waits))
    ;; the recursive resize skips any further processing
    (check (= 1 notifications))))

#+unix
(defun test-buffered-input ()
  (let ((ui (make-instance 'uncursed:tui)) (received nil))
    (cffi:with-foreign-object (pipe :int 2)
      (setf (cffi:mem-aref pipe :int 0) 100
            (cffi:mem-aref pipe :int 1) 101
            (uncursed::%wakeup-pipe ui) pipe
            (uncursed::%winch-pipe ui) pipe)
      (with-functions
          ((uncursed:redisplay (constantly t))
           (uncursed-sys:read-event (lambda () (read-char)))
           (uncursed-sys::select (lambda (&rest args)
                                   (declare (ignore args))
                                   (error "Blocked despite buffered input")))
           (uncursed:dispatch-event
            (lambda (ui event) (setf received event) (uncursed:stop ui))))
        (with-input-from-string (*standard-input* "x") (uncursed:run-loop ui))))
    (check (eql #\x received))))

#+windows
(defun test-windows-timeout-rounding ()
  (let ((ui (make-instance 'uncursed:tui)) (waits nil) (calls 0))
    (setf (uncursed::%termios ui) (list (cffi:null-pointer))
          (uncursed::%wakeup-pipe ui) (cffi:null-pointer))
    (uncursed:schedule-timer
     ui (uncursed:make-timer 1/10000
                             (lambda (ui)
                               (when (= (incf calls) 3) (uncursed:stop ui))
                               1/10000)))
    (with-functions
        ((uncursed:redisplay (constantly t))
         (uncursed-sys::WaitForMultipleObjects
          (lambda (n handles all timeout)
            (declare (ignore n handles all))
            (push timeout waits)
            (advance (/ timeout 1000))
            #x102))
         (uncursed-sys:win-events-left (constantly '(nil . 0))))
      (uncursed:run-loop ui))
    (check (equal '(1 1 1) waits))))

#+windows
(defun test-windows-resize-consumption ()
  (let ((queue '(4 1 4 2 4)) (consumed nil))
    (with-functions
        ((uncursed-sys::GetNumberOfConsoleInputEvents
          (lambda (handle count)
            (declare (ignore handle))
            (setf (cffi:mem-ref count :uint32) (length queue)) t))
         (uncursed-sys::PeekConsoleInputW
          (lambda (handle records n count)
            (declare (ignore handle n))
            (loop for type in queue for i from 0
                  do (setf (cffi:foreign-slot-value
                            (cffi:mem-aptr records '(:struct uncursed-sys::input-record) i)
                            '(:struct uncursed-sys::input-record) 'uncursed-sys::event-type)
                           type))
            (setf (cffi:mem-ref count :uint32) (length queue)) t))
         (uncursed-sys::ReadConsoleInputW
          (lambda (handle records n count)
            (declare (ignore handle records))
            (dotimes (i n) (push (pop queue) consumed))
            (setf (cffi:mem-ref count :uint32) n) t)))
      (let ((events #'uncursed-sys:win-events-left))
        (check (equal '(t . 2) (funcall events nil)))
        ;; recursion cannot see the same notification, nor one behind device input
        (check (equal '(nil . 2) (funcall events nil)))
        (check (= 1 (pop queue)))
        (check (equal '(t . 1) (funcall events nil)))
        (check (= 2 (pop queue)))
        (check (equal '(t . 0) (funcall events nil)))
        (check (equal '(nil . 0) (funcall events nil)))
        (check (equal '(4 4 4) consumed))))))

(defun run-tests ()
  (let ((*now* 0) (*checks* 0))
    (with-functions ((uncursed::timer-time (lambda () *now*)))
      (test-accounting)
      (test-recursion)
      (test-recursive-timer-error)
      (test-callback-time)
      #+unix (test-unix-loop)
      (test-generation)
      #+windows (test-windows-loop)
      #+windows (test-windows-resize-consumption)
      #+windows (test-windows-timeout-rounding)
      #+unix (test-buffered-input))
    (format t "~d uncursed timer/reentrancy checks passed.~%" *checks*)))
