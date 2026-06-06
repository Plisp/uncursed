(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads)
  #+sbcl (require 'sb-concurrency))

(defpackage #:uncursed-layout
  (:use :cl)
  (:local-nicknames (#:tui :uncursed))
  (:export #:main))
(in-package #:uncursed-layout)

(defvar *log*)
(defun log* (o)
  #+(and sbcl slynk) (sb-concurrency:send-message *log* o)
  o)

(defclass ui (tui::elemental)
  ())

(defclass display ()
  ((string :initarg :string
           :accessor display-string)))

(defmethod tui::render ((view display) rect)
  (let* ((colors '(#x2aa198 #xfcba03))
         (cols (tui:display-width (display-string view)))
         (rect (tui:copy-rect rect :cols (+ 1 cols) :rows 3)))
    (tui:puts (make-string (+ 1 cols) :initial-element #\-) 1 1 rect)
    (tui:puts (display-string view) 2 2 rect (tui:make-style :fg (alexandria:random-elt colors)))
    (tui:puts (make-string (+ 1 cols) :initial-element #\-) 3 1 rect)
    (values (make-instance 'tui::view :rect rect) 42 #x33aa00)))

(defmethod tui::view-tree ((ui ui))
  (let ((root-rect (tui:make-rect :x 0 :y 0 :cols (tui:cols ui) :rows (tui:rows ui))))
    (tui::horizontal-container root-rect (list (make-instance 'display :string "a long string")
                                               (make-instance 'display :string "a long string")))))

(defvar *tui*)

(defun tui-main ()
  (let ((tui (make-instance 'ui)))
    (setf *tui* tui)
    (tui:run tui :redisplay-on-input t)
    #+sbcl
    (sb-concurrency:send-message *log* :stop)))

(defmethod tui:dispatch-event ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (log* event)
    (if (tui:mouse-event-p event)
        ()
        (cond ((and (equal (tui:event-kind event) #\c) (tui:event-controlp event))
               (tui:stop ui))))))

(defun main ()
  (if (member :slynk *features*)
      (progn
        (bt:make-thread (lambda () (tui-main)))
        #+sbcl
        (loop :initially (setf *log* (sb-concurrency:make-mailbox :name "log"))
              :for m = (sb-concurrency:receive-message *log*)
              :until (eq m :stop)
              :do (print m)
                  (force-output)))
      (tui-main)))
