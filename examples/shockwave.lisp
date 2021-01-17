(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads)
  #+sbcl (require 'sb-concurrency))

(defpackage #:uncursed-shockwave
  (:use :cl)
  (:local-nicknames (#:tui :uncursed))
  (:export #:main))
(in-package #:uncursed-shockwave)

(defclass ui (tui:tui)
  ())

(defmethod tui:run :before ((tui ui))
  (tui:enable-alternate-screen)
  (tui:set-cursor-shape :invisible))

(defclass view (tui:standard-window)
  ())

(defstruct wave
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (radius 0 :type fixnum)
  (color #xfcba03 :type fixnum))

(defstruct (circle (:include wave))
  )

(defstruct (flame (:include wave))
  (width 2 :type fixnum)
  (dying nil :type boolean))

(defstruct (triangle (:include wave))
  (orientation 0.0 :type single-float))

(defvar *waves*)

(defvar *log*)
(defun log* (o)
  #+sbcl (sb-concurrency:send-message *log* o)
  o)

(defgeneric present-wave (wave view))

(defmethod present-wave ((c circle) view)
  (loop :with r = (circle-radius c)
        :with dimensions = (tui:dimensions view)
        :with view-rows = (tui:rect-rows dimensions)
        :with view-cols = (tui:rect-cols dimensions)
        :for y :from (- (circle-y c) r) :to (+ (circle-y c) r)
        :for dy = (- y (circle-y c))
        :for dx = (isqrt (- (* r r) (* dy dy)))
        :for x-left = (- (circle-x c) dx)
        :for x-right = (+ (circle-x c) dx)
        :do (when (and (>= x-left 1) (<= 1 y view-rows))
              (tui:put #\space y x-left (tui:make-style :bg (circle-color c))))
            (when (and (<= x-right view-cols) (<= 1 y view-rows))
              (tui:put #\space y x-right (tui:make-style :bg (circle-color c))))
            ;; reflect with respect to the bounding box - this fills gaps
            (let* ((box-x (- (circle-x c) r 1))
                   (box-y (- (circle-y c) r 1))
                   (x-left-rel (- x-left box-x))
                   (x-right-rel (- x-right box-x))
                   (y-rel (- y box-y))
                   (x (+ box-x y-rel))
                   (y-high (+ box-y x-left-rel))
                   (y-low (+ box-y x-right-rel)))
              (when (and (>= y-high 1) (<= 1 x view-cols))
                (tui:put #\space y-high x (tui:make-style :bg (circle-color c))))
              (when (and (<= y-low view-rows) (<= 1 x view-cols))
                (tui:put #\space y-low x (tui:make-style :bg (circle-color c)))))))
-
(defmethod present-wave ((w triangle) view)
  (let* ((dimensions (tui:dimensions view))
         (view-rows (tui:rect-rows dimensions))
         (view-cols (tui:rect-cols dimensions))
         (theta (mod (* (wave-radius w) (/ pi 16)) (* 2 pi)))
         (v1-x (truncate (+ (wave-x w) (* (wave-radius w) (cos theta)))))
         (v1-y (truncate (+ (wave-y w) (* (wave-radius w) (sin theta)))))
         (v2-x (truncate (+ (wave-x w) (* (wave-radius w) (cos (+ theta (* pi 2/3)))))))
         (v2-y (truncate (+ (wave-y w) (* (wave-radius w) (sin (+ theta (* pi 2/3)))))))
         (v3-x (truncate (+ (wave-x w) (* (wave-radius w) (cos (+ theta (* pi 4/3)))))))
         (v3-y (truncate (+ (wave-y w) (* (wave-radius w) (sin (+ theta (* pi 4/3))))))))
    (when (and (<= 1 v1-y view-rows) (<= 1 v1-x view-cols))
      (tui:put #\space v1-y v1-x (tui:make-style :bg (wave-color w))))
    (when (and (<= 1 v2-y view-rows) (<= 1 v2-x view-cols))
      (tui:put #\space v2-y v2-x (tui:make-style :bg (wave-color w))))
    (when (and (<= 1 v3-y view-rows) (<= 1 v3-x view-cols))
      (tui:put #\space v3-y v3-x (tui:make-style :bg (wave-color w))))))

(defmethod present-wave ((f flame) view)
  (loop :with dimensions = (tui:dimensions view)
        :for x :from (max 1 (- (flame-x f) (flame-radius f)))
          :to (min (+ (flame-x f) (flame-radius f)) (tui:rect-cols dimensions))
        :do (tui:put #\space (max 1 (1+ (- (flame-y f) (random 8))))
                     x (tui:make-style :bg (flame-color f)))))

(defmethod tui:present ((view view))
  (loop :for wave :in *waves*
        :do (present-wave wave view)))

(defmethod tui:handle-resize ((tui ui))
  (let ((window (tui:focused-window tui)))
    (setf (tui:rect-cols (tui:dimensions window)) (tui:columns tui)
          (tui:rect-rows (tui:dimensions window)) (tui:lines tui))
    (setf *waves* (remove-if-not (lambda (w)
                                   (and (<= 1 (wave-x w) (tui:columns tui))
                                        (<= 1 (wave-y w) (tui:lines tui))))
                                 *waves*))))

(defmethod tui:handle-key-event ((window view) tui event)
  nil)

(defparameter *bonfire* nil)

(defparameter *green-mod* 1.08)
(defmethod tui:handle-mouse-event ((window view) tui button state line col &key)
  (case button
    (:left (when (eq state :release) (push (make-circle :y line :x col) *waves*)))
    (:middle (when (eq state :release) (setf *bonfire* (not *bonfire*))))
    (:right (when (eq state :release) (push (make-triangle :y line :x col) *waves*)))
    (:wheel-up (setf *green-mod* (max 1.01 (- *green-mod* 0.03))))
    (:wheel-down (setf *green-mod* (min 1.08 (+ *green-mod* 0.03))))))

(defun tui-handle-event (tui ev)
  (cond ((equal ev '(#\w :control))
         (tui:stop tui))
        ))

(defparameter *tick* 0.05)

(defun %tick (tui)
  (let ((view-bounds (tui:dimensions (tui:focused-window tui))))
    (when *bonfire*
      (loop :repeat 9
            :do (push (make-flame
                       :y (tui:rect-rows view-bounds)
                       :x (+ (- (truncate (tui:rect-cols view-bounds) 2) 2)
                             (random 4)))
                      *waves*)))
    (loop :with new-waves
          :for w :in *waves*
          :do (let ((old (wave-color w)))
                (setf (wave-color w)
                      (+ (ash (truncate (uncursed-sys::red old) 1.04) 16)
                         (ash (truncate (uncursed-sys::green old) *green-mod*) 8)
                         (ash (truncate (uncursed-sys::blue old) 1.00) 0))))
              (etypecase w
                (circle
                 (incf (circle-radius w))
                 (unless (> (circle-radius w)
                            (sqrt (+ (expt (tui:rect-cols view-bounds) 2)
                                     (expt (tui:rect-rows view-bounds) 2))))
                   (push w new-waves)))
                (triangle
                 (setf (triangle-radius w) (max 0 (- (triangle-radius w)
                                                     (- (random 3) 1))))
                 (decf (triangle-y w))
                 (unless (> (triangle-radius w)
                            (sqrt (+ (expt (tui:rect-cols view-bounds) 2)
                                     (expt (tui:rect-rows view-bounds) 2))))
                   (push w new-waves)))
                (flame
                 (incf (flame-x w) (- (random 5) 2))
                 (let ((prev (flame-radius w)))
                   (if (and (not (flame-dying w))
                            (< prev 20))
                       (incf (flame-radius w) (truncate 6 (1+ (flame-radius w))))
                       (progn
                         (setf (flame-dying w) t)
                         (decf (flame-radius w) 1))))
                 (unless (or (<= (flame-radius w) 0)
                             (<= (flame-y w) 1))
                   (decf (flame-y w) 1)
                   (push w new-waves))))
          :finally (setf *waves* new-waves)))
  *tick*)

(defun tick (tui)
  (%tick tui))

(defun tui-main ()
  (let* ((dimensions (tui:terminal-dimensions))
         (view
           (make-instance 'view
                          :dimensions (tui:make-rectangle :x 0
                                                          :y 0
                                                          :rows (car dimensions)
                                                          :cols (cdr dimensions))))
         (ui
           (make-instance 'ui :focused-window view
                              :windows (list view)
                              :event-handler #'tui-handle-event)))
    (setf *waves* (list))
    (tui:schedule-timer ui (tui:make-timer *tick* #'tick))
    (tui:run ui)
    #+sbcl
    (when (boundp '*log*)
      (sb-concurrency:send-message *log* :stop))))

(defun main ()
  (if (member :slynk *features*)
      (progn
        (bt:make-thread (lambda () (tui-main)))
        #+sbcl
        (setf *log* (sb-concurrency:make-mailbox :name "log"))
        (loop :for m = (sb-concurrency:receive-message *log*)
              :until (eq m :stop)
              :do (print m)
                  (force-output)))
      (tui-main)))
