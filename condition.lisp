(in-package :uncursed-sys)

(define-condition uncursed-error (simple-error)
  ())

(define-condition syscall-error (uncursed-error)
  ())

(defun error-syscall-error (control &rest args)
  (error 'syscall-error
         :format-control control
         :format-arguments args))
