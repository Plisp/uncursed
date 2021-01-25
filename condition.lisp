(in-package :uncursed-sys)

(define-condition uncursed-error (simple-error)
  ())

(define-condition syscall-error (uncursed-error)
  ())

(defun error-syscall-error (control &rest args)
  (error 'syscall-error
         :format-control #-sbcl control
                         #+sbcl (format nil "~a: ~a" control (sb-int:strerror))
         :format-arguments args))
