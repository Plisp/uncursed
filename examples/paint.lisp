(eval-when (:execute)
  (require 'bordeaux-threads))

(defpackage :uncursed-paint
  (:use :cl)
  (:local-nicknames (:tui :uncursed)))
(in-package :uncursed-paint)

;;; classes/methods

(defvar *paint-ui*)

(defclass paint-ui (tui:tui)
  ((draw-char :initform #\.
              :accessor draw-char)
   (draw-style :initform (tui:make-style :fg (tui:make-color :red 0 :green 255 :blue 100))
               :accessor draw-style)))

(defun tui-handle-event (tui ev)
  (cond ((and (characterp ev) (char= ev #\etb))
         (tui:stop tui))))

(defclass layer (tui:standard-window)
  ())

(defmethod tui:present ((window layer))
  (declare (ignore window))
  nil) ; pass, we've already done what we needed to do

(defmethod tui:handle-mouse-event ((window layer) type button y x controlp)
  (tui:put (draw-char *paint-ui*) y x
           (draw-style *paint-ui*)
           (tui::canvas *paint-ui*) window))

(defmethod tui:handle-key-event ((window layer) event)
  (when (and (characterp event) (graphic-char-p event))
    (setf (draw-char *paint-ui*) event)))

(defun tui-main ()
  (let* ((dimensions (tui:terminal-dimensions))
         (lines (car dimensions))
         (columns (cdr dimensions))
         (initial-layer (make-instance 'layer :x 0 :y 0 :lines lines :columns columns))
         (*paint-ui* (make-instance 'paint-ui :focused-window initial-layer
                                              :windows (list initial-layer)
                                              :event-handler #'tui-handle-event)))
    (tui:start *paint-ui*)))

(defun main ()
  #-slynk (tui-main)
  #+slynk (bt:join-thread (bt:make-thread (lambda () (tui-main)))))
