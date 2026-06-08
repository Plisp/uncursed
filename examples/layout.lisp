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

(defclass ui (tui:elemental)
  ((state :initarg :state
          :accessor state)))

(defclass display ()
  ((string :initarg :string
           :accessor display-string)
   (focused :initform nil
            :accessor focused)))

(defclass horizontal ()
  ((things :initarg :things
           :accessor things)))
(defclass vertical ()
  ((things :initarg :things
           :accessor things)))
(defun horizontal (&rest things) (make-instance 'horizontal :things things))
(defun vertical (&rest things) (make-instance 'vertical :things things))

(defmethod tui:render ((thing display) rect)
  (when (< (tui:rect-rows rect) 3)
    (return-from tui:render))
  (let* ((colors '(#x2aa198 #xfcba03))
         (cols (tui:display-width (display-string thing)))
         (viewrect (tui:copy-rect rect :cols (+ 1 cols) :rows 3)))
    (tui:puts (make-string (+ 1 cols) :initial-element #\-) 1 1 viewrect)
    (tui:puts (display-string thing) 2 2 viewrect
              (tui:make-style :fg (if (focused thing)
                                      (first colors)
                                      (second colors))))
    (tui:puts (make-string (+ 1 cols) :initial-element #\-) 3 1 viewrect)
    (values (make-instance
             'tui:view
             :rect viewrect
             :mouse-handler (lambda (v e)
                              (let ((rect (tui:rect v)))
                                (setf (display-string thing)
                                      (format nil "was ~dx~d at (~d,~d)"
                                              (tui:rect-rows rect) (tui:rect-cols rect)
                                              (tui:rect-x rect) (tui:rect-y rect)))
                                (setf (focused thing)
                                      (tui:mouse-within (tui:event-kind e) rect)))))
            42 #xaa3300)))

(defmethod tui:render ((split horizontal) rect)
  (values (tui:horizontal-container rect (things split)) 1 #x33aa00))
(defmethod tui:render ((split vertical) rect)
  (values (tui:vertical-container rect (things split)) 1 #x3300aa))

(defmethod tui:render-state ((ui ui))
  (state ui))

(defvar *tui*)

(defun tui-main ()
  (let ((tui (make-instance
              'ui
              :state (vertical
                      (horizontal (make-instance 'display :string "click")
                                  (make-instance 'display :string "to"))
                      (horizontal (make-instance 'display :string "show")
                                  (make-instance 'display :string "dimensions"))))))
    (setf *tui* tui)
    (tui:run tui :redisplay-on-input t :mouse :hover)
    #+sbcl
    (sb-concurrency:send-message *log* :stop)))

(defmethod tui:dispatch-event :around ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (if (and (not (tui:mouse-event-p event))
             (equal (tui:event-kind event) #\c)
             (tui:event-controlp event))
        (tui:stop ui)
        (call-next-method))))

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
