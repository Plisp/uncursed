(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads)
  #+sbcl (require 'sb-concurrency))

(defpackage #:uncursed-shockwave
  (:use :cl)
  (:local-nicknames (#:tui :uncursed))
  (:export #:main))
(in-package #:uncursed-shockwave)

;;; logging from a remote slime session

(defvar *log*)
(defvar *tui*)
(defun log* (o)
  #+(and sbcl slynk) (sb-concurrency:send-message *log* o)
  o)

;;; waves

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

(defgeneric present-wave (wave rect))

(defmethod present-wave ((c circle) dimensions)
  (flet ((shade (y x)
           (tui:put #\space y x dimensions (tui:make-style :bg (circle-color c)))))
    (loop :with r = (circle-radius c)
          :with view-rows = (tui:rect-rows dimensions)
          :with view-cols = (tui:rect-cols dimensions)
          :for y :from (- (circle-y c) r) :to (+ (circle-y c) r)
          :for dy = (- y (circle-y c))
          :for dx = (isqrt (- (* r r) (* dy dy)))
          :for x-left = (- (circle-x c) dx)
          :for x-right = (+ (circle-x c) dx)
          :do (when (and (<= 1 x-left view-cols) (<= 1 y view-rows)) ; can be clipped
                (shade y x-left))
              (when (and (<= x-right view-cols) (<= 1 y view-rows))
                (shade y x-right))
              ;; reflect with respect to the bounding box - this fills gaps
              (let* ((box-x (- (circle-x c) r 1))
                     (box-y (- (circle-y c) r 1))
                     (x-left-rel (- x-left box-x))
                     (x-right-rel (- x-right box-x))
                     (y-rel (- y box-y))
                     (x (+ box-x y-rel))
                     (y-high (+ box-y x-left-rel))
                     (y-low (+ box-y x-right-rel)))
                ;; y-high can be clipped by a window resize
                (when (and (<= 1 y-high view-rows) (<= 1 x view-cols))
                  (shade y-high x))
                (when (and (<= y-low view-rows) (<= 1 x view-cols))
                  (shade y-low x))))))

(defmethod present-wave ((w triangle) dimensions)
  (flet ((shade (y x)
           (tui:put #\space y x dimensions (tui:make-style :bg (wave-color w)))))
    (let* ((view-rows (tui:rect-rows dimensions))
           (view-cols (tui:rect-cols dimensions))
           (theta (mod (* (wave-y w) (/ pi 16)) (* 2 pi)))
           (v1-x (truncate (+ (wave-x w) (* (wave-radius w) (cos theta)))))
           (v1-y (truncate (+ (wave-y w) (* (wave-radius w) (sin theta)))))
           (v2-x (truncate (+ (wave-x w) (* (wave-radius w) (cos (+ theta (* pi 2/3)))))))
           (v2-y (truncate (+ (wave-y w) (* (wave-radius w) (sin (+ theta (* pi 2/3)))))))
           (v3-x (truncate (+ (wave-x w) (* (wave-radius w) (cos (+ theta (* pi 4/3)))))))
           (v3-y (truncate (+ (wave-y w) (* (wave-radius w) (sin (+ theta (* pi 4/3))))))))
      (when (and (<= 1 v1-y view-rows) (<= 1 v1-x view-cols))
        (shade v1-y v1-x))
      (when (and (<= 1 v2-y view-rows) (<= 1 v2-x view-cols))
        (shade v2-y v2-x))
      (when (and (<= 1 v3-y view-rows) (<= 1 v3-x view-cols))
        (shade v3-y v3-x)))))

(defmethod present-wave ((f flame) dimensions)
  (loop :for x :from (max 1 (- (flame-x f) (flame-radius f)))
          :to (min (+ (flame-x f) (flame-radius f)) (tui:rect-cols dimensions))
        :for y = (1+ (- (flame-y f) (random 8)))
        :do (when (and (<= 1 y (tui:rect-rows dimensions)))
              (tui:put #\space y x dimensions (tui:make-style :bg (flame-color f))))))

;;; tui code

(defclass ui (tui:tui)
  ((waves :initform (make-array 0 :adjustable t :fill-pointer t)
          :accessor waves
          :type (vector wave))))

(defmethod tui:redisplay ((ui ui))
  (with-simple-restart (nil "continue from redisplay error")
    (loop :with rows := (tui:rows ui)
          :with cols := (tui:cols ui)
          :for wave :across (waves ui)
          :do (present-wave wave (tui:make-rect :x 0 :y 0 :rows rows :cols cols)))))

(defparameter *bonfire* nil)
(defparameter *green-mod* 1.08)

(defmethod tui:dispatch-event ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (if (tui:mouse-event-p event)
        (let* ((data (tui:event-kind event))
               (button (tui:mouse-data-button data))
               (state (tui:mouse-data-state data))
               (row (tui:mouse-data-row data))
               (col (tui:mouse-data-col data)))
          (case button
            (:left (when (eq state :release)
                     (vector-push-extend (make-circle :y row :x col) (waves ui))))
            (:middle (when (eq state :release)
                       (setf *bonfire* (not *bonfire*))))
            (:right (when (eq state :release)
                      (vector-push-extend (make-triangle :y row :x col) (waves ui))))
            (:wheel-up (setf *green-mod* (max 1.01 (- *green-mod* 0.03))))
            (:wheel-down (setf *green-mod* (min 1.08 (+ *green-mod* 0.03)))))
          #+sbcl (log* (format nil "saw button ~a,~a at row ~d, col ~d"
                               button state row col)))
        (progn
          #+sbcl (log* event)
          (cond ((and (equal (tui:event-kind event) #\c) (tui:event-controlp event))
                 (tui:stop ui)))))))

;;; tick logic

(defparameter *tick* 0.032)

(defgeneric update-wave (wave rows cols))

(defmethod update-wave ((w circle) rows cols)
  (incf (circle-radius w))
  ;; remove if radius exceeds screen diagonal
  (unless (> (circle-radius w)
             (sqrt (+ (expt cols 2) (expt rows 2))))
    w))

(defmethod update-wave ((w triangle) rows cols)
  (decf (triangle-y w))
  (setf (triangle-radius w) (max (- (triangle-radius w) (- (random 3) 1))
                                 0))
  (unless (> (triangle-radius w)
             (sqrt (+ (expt cols 2) (expt rows 2))))
    w))

(defmethod update-wave ((w flame) rows cols)
  (incf (flame-x w) (- (random 5) 2))
  (let ((prev (flame-radius w)))
    (if (and (not (flame-dying w))
             (< prev 20))
        (incf (flame-radius w) (truncate 6 (1+ (flame-radius w))))
        (progn
          (setf (flame-dying w) t)
          (decf (flame-radius w) 1))))
  ;; remove waves that have drifted out of bounds
  (unless (or (<= (flame-radius w) 0)
              (<= (flame-y w) 1))
    (decf (flame-y w) 1)
    w))

(declaim (notinline tick))
(defun tick (tui)
  (let ((rows (tui:rows tui))
        (cols (tui:cols tui))
        (waves (waves tui)))
    ;; create new flames for bonfire
    (when *bonfire*
      (loop :repeat 10
            :do (vector-push-extend
                 (make-flame :y rows
                             :x (+ (- (truncate cols 2) 5)
                                   (random 10)))
                 waves)))
    ;; update wave positions
    (loop :with copy = (copy-seq waves)
          :initially (setf (fill-pointer waves) 0)
          :for w :across copy
          :do (let ((old (wave-color w)))
                (setf (wave-color w)
                      (uncursed-sys:color
                       (truncate (uncursed-sys:red old) 1.06)
                       (truncate (uncursed-sys:green old) *green-mod*)
                       (truncate (uncursed-sys:blue old) 1.0))))
              (alexandria:when-let (w (update-wave w rows cols))
                (vector-push w waves))))
  *tick*)

(defun tui-main ()
  (let ((tui (make-instance 'ui)))
    (setf *tui* tui)
    (tui:schedule-timer tui
                        (tui:make-timer *tick*
                                        (lambda (tui)
                                          (restart-case (tick tui)
                                            (continue ()
                                              :report "Continue from tick error"
                                              *tick*)))))
    (unwind-protect (tui:run tui :redisplay-on-input nil)
      #+sbcl
      (sb-concurrency:send-message *log* :stop))))

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
