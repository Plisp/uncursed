(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads))

(defpackage #:uncursed-input
  (:use :cl)
  (:export #:main))
(in-package #:uncursed-input)

(defun main ()
  (bt:join-thread
   (bt:make-thread
    (lambda ()
      (let (termios)
        (unwind-protect
             (progn
               (setf termios (uncursed-sys:setup-terminal 0))
               (let ((*terminal-io* *standard-output*))
                 (uncursed:enable-mouse)
                 (force-output))
               (loop :for event = (print (uncursed-sys:read-event))
                     :until (eql event #\Q)
                     :do (princ #\return)
                         (force-output)))
          (princ #\return)
          (let ((*terminal-io* *standard-output*))
            (uncursed:disable-mouse)
            (force-output))
          (uncursed-sys:restore-terminal termios 0)))))))
