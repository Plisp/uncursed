;;;; DISCLAIMER
;;;; hacky toy for testing colors which is not representative of this library's proper use

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads))

(defpackage #:uncursed-paint
  (:use :cl)
  (:local-nicknames (#:tui :uncursed))
  (:export #:main))
(in-package #:uncursed-paint)

;;; classes/methods

(defvar *paint-ui*)

(defclass paint-ui (tui:tui)
  ((draw-char :initform #\.
              :accessor draw-char)
   (draw-style :initform (tui:make-style :fg #xffffff :bg #x000000)
               :accessor draw-style)))

(defmethod tui:run :before ((tui paint-ui))
  (tui:enable-alternate-screen)
  (tui:set-cursor-shape :invisible))

(defmethod tui:handle-resize ((tui paint-ui))
  tui)

(defun tui-handle-event (tui ev)
  (cond ((equal ev '(#\w :control))
         (tui:stop tui))))

(defclass palette-view (tui:standard-window)
  ((colors :initarg :colors
           :accessor colors)
   (fg :initform 0
       :accessor palette-fg-index)
   (bg :initform 0
       :accessor palette-bg-index)))

(defmethod tui:present ((palette palette-view))
  (with-accessors ((colors colors)
                   (fg-index palette-fg-index)
                   (bg-index palette-bg-index))
      palette
    (loop :for color :in colors
          :for line :from 1
          :do (tui:put #\space line 1 (tui:make-style :bg color))
              (tui:put #\space line 2 (tui:make-style :bg color)))
    (tui:put #\< (1+ fg-index) 3 (tui:make-style :fg (nth fg-index colors)))
    (tui:put #\< (1+ bg-index) 3 (tui:make-style :bg (nth bg-index colors)))))

(defmethod tui:handle-mouse-event ((window palette-view) tui button state y x &key)
  (when (and (eq state :click)
             (<= 1 x 2))
    (let* ((palette-index (1- y))
           (new-color (nth palette-index (colors window))))
      (case button
        (:left (setf (tui:fg (draw-style *paint-ui*)) new-color
                     (palette-fg-index window) palette-index))
        (:right (setf (tui:bg (draw-style *paint-ui*)) new-color
                      (palette-bg-index window) palette-index))))))

(defclass layer-view (tui:standard-window)
  ((layers :initarg :layers
           :accessor layers)))

(defmethod tui:present ((window layer-view))
  (let ((layers (layers window)))
    (loop :for layer :in layers
          :do (loop :for i :from 0 :below (array-total-size layer)
                    :for cell = (row-major-aref layer i)
                    ;; don't do this at home
                    :do (setf (tui:cell-string (row-major-aref tui::*put-buffer* i))
                              (tui:cell-string cell)
                              (tui:cell-style (row-major-aref tui::*put-buffer* i))
                              (tui:cell-style cell))))))

(defmethod tui:handle-mouse-event ((window layer-view) tui button state line col &key)
  (when (and (or (eq state :click) (eq state :drag))
             (eq button :left))
    (block draw
      (handler-bind ((tui:wide-char-overwrite-error
                       (lambda (e)
                         (declare (ignore e))
                         (invoke-restart 'tui:ignore-put)))
                     (tui:window-bounds-error
                       (lambda (e)
                         ;; tried to put wide character at horizontal edge,
                         ;; only way the coordinate could be 'in-bounds'
                         ;; other accesses are an error and the handler will decline
                         (when (= (tui:window-bounds-error-coordinate e)
                                  (tui:rect-cols (tui:dimensions
                                                  (tui:window-bounds-error-window e))))
                           (return-from draw)))))
        ;; usually there is no reason to do this.
        ;; I'm only using tui:put outside PRESENT here since the canvas = data
        (tui:put (draw-char *paint-ui*)
                 line col
                 (draw-style *paint-ui*)
                 (first (layers window))
                 window)))))

(defmethod tui:handle-key-event ((window layer-view) tui event)
  (when (and (characterp event) (graphic-char-p event))
    (setf (draw-char *paint-ui*) event)))

(defun color-scale (length intensity)
  (let ((increment (1+ (truncate intensity (truncate length 5)))))
    (append (loop :for r = intensity
                  :for g :to intensity :by increment
                  :for b = 0
                  :collect (+ (ash r 16) (ash g 8) b))
            (loop :for r :downfrom intensity :to 0 :by increment
                  :for g = intensity
                  :for b = 0
                  :collect (+ (ash r 16) (ash g 8) b))
            (loop :for r = 0
                  :for g = intensity
                  :for b :to intensity :by increment
                  :collect (+ (ash r 16) (ash g 8) b))
            (loop :for r = 0
                  :for g :downfrom intensity :to 0 :by increment
                  :for b = intensity
                  :collect (+ (ash r 16) (ash g 8) b))
            (loop :for r :to intensity :by increment
                  :for g = 0
                  :for b = intensity
                  :collect (+ (ash r 16) (ash g 8) b)))))

(defun tui-main (use-palette)
  (let* ((dimensions (tui:terminal-dimensions))
         (rows (car dimensions))
         (columns (cdr dimensions))
         (palette-window
           (make-instance 'palette-view :dimensions (tui:make-rect :x 0 :y 0
                                                                   :cols 3
                                                                   :rows rows)
                                        :colors (color-scale rows 222)))
         (initial-layer
           (make-array (list rows columns)))
         (layer-view
           (make-instance 'layer-view :dimensions (tui:make-rect :x 3 :y 0
                                                                 :cols (- columns 3)
                                                                 :rows rows)
                                      :layers (list initial-layer)))
         (*paint-ui*
           (make-instance 'paint-ui :focused-window layer-view
                                    :windows (list layer-view palette-window)
                                    :event-handler #'tui-handle-event
                                    :use-palette use-palette)))
    (loop :for idx :below (array-total-size initial-layer)
          :do (setf (row-major-aref initial-layer idx) (make-instance 'tui:cell)))
    (tui:run *paint-ui*)))

(defun main (&optional use-palette)
  (if (member :slynk *features*)
      (bt:make-thread (lambda () (tui-main use-palette)))
      (tui-main use-palette)))
