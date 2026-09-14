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

(defclass display-view (tui:view)
  ((display :initarg :display
            :reader view-display)))

(defmethod render-state ((thing display) rect)
  (when (< (tui:rect-rows rect) 3)
    (return-from render-state))
  (let* ((cols (tui:display-width (display-string thing)))
         (viewrect (tui:copy-rect rect :cols (+ 1 cols) :rows 3)))
    (tui:puts (make-string (+ 1 cols) :initial-element #\-) 1 1 viewrect)
    (tui:puts (make-string (+ 1 cols) :initial-element #\-) 3 1 viewrect)
    (values (make-instance
             'display-view
             :display thing
             :rect viewrect
             :mouse-handler (lambda (v e)
                              (setf (focused thing)
                                    (tui:mouse-within (tui:event-kind e) (tui:rect v)))))
            42 #xaa3300)))

(defun label-sizes (view)
  "Writes the size computed by the last layout."
  (when (typep view 'display-view)
    (let ((rect (tui:rect view)))
      (tui:puts (format nil "~dx~d at (~d,~d)"
                        (tui:rect-rows rect) (tui:rect-cols rect)
                        (tui:rect-x rect) (tui:rect-y rect))
                2 2 rect
                (tui:make-style :fg (if (focused (view-display view)) #x2aa198 #xfcba03)))))
  (mapc #'label-sizes (tui:children view)))

(defmethod render-state ((split horizontal) rect)
  (values (tui:with-horizontal (rect)
            (dolist (thing (things split))
              (tui:place (rect) (render-state thing rect))))
          1 #x33aa00))
(defmethod render-state ((split vertical) rect)
  (values (tui:with-vertical (rect)
            (dolist (thing (things split))
              (tui:place (rect) (render-state thing rect))))
          1 #x3300aa))

(defmethod tui:render ((ui ui))
  (let ((root (render-state (state ui)
                            (tui:make-rect :x 0 :y 0 :rows (tui:rows ui) :cols (tui:cols ui)))))
    (label-sizes root)
    root))

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
    (unwind-protect (tui:run tui :redisplay-on-input t :mouse :hover)
      #+(and sbcl slynk)
      (sb-concurrency:send-message *log* :stop))))

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
