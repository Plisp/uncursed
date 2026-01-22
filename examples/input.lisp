(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads))

(defpackage #:uncursed-input
  (:use :cl)
  (:export #:main))
(in-package #:uncursed-input)

(defun main (&key hover)
  (bt:join-thread
   (bt:make-thread
    (lambda ()
      (let (termios)
        (unwind-protect
             (progn
               #+unix (setf termios (uncursed-sys:setup-terminal 0))
               #+windows (setf termios (uncursed-sys:setup-terminal))
               (let (#+sbcl (*terminal-io* *standard-output*))
                 (uncursed:enable-mouse :hover hover)
                 (force-output))
               (loop :for event = (print (uncursed-sys:read-event))
                     :until (and (equal (uncursed:event-kind event) #\c)
                                 (uncursed:event-controlp event))
                     :do (princ #\return)
                         (force-output)))
          (princ #\return)
          (let (#+sbcl (*terminal-io* *standard-output*))
            (uncursed:disable-mouse)
            (force-output))
          #+unix (uncursed-sys:restore-terminal termios 0)
          #+windows (uncursed-sys:restore-terminal termios)))))))
