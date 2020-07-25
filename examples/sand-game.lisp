(eval-when (:load-toplevel :execute)
  (require 'bordeaux-threads))

(defpackage #:uncursed-sand-game
  (:use :cl)
  (:local-nicknames (#:tui :uncursed))
  (:export #:main))
(in-package #:uncursed-sand-game)

(defvar *sand*)
(defvar *sand-count*)
(defvar *projectiles*)

(defclass game-ui (tui:tui)
  ())

;;; sandboard

(defclass sand-view (tui:standard-window)
  ())

(defmethod tui:present ((window sand-view))
  (loop :for y :below (array-dimension *sand* 0)
        :do (loop :for x :below (array-dimension *sand* 1)
                  :do (let ((sand (aref *sand* y x)))
                        (when sand
                          (destructuring-bind (char style) sand
                            (tui:put char (1+ y) (1+ x) style))))))
  (loop :for y :below (array-dimension *projectiles* 0)
        :do (loop :for x :below (array-dimension *projectiles* 1)
                  :do (let ((style (aref *projectiles* y x)))
                        (when style
                          (tui:put #\@ (1+ y) (1+ x) style))))))

(defmethod tui:handle-key-event ((window sand-view) event)
  nil)

(defmethod tui:handle-mouse-event ((window sand-view) type button line col controlp)
  (setf (aref *projectiles* (1- line) (1- col)) (tui:make-style :fg #xffffff)))

;;; score panel

(defclass panel-view (tui:standard-window)
  ())

(defvar *hp*)
(defvar *score*)

(let ((intensity 255)
      (length 70))
  (defparameter *color-scale*
    (let ((increment (1+ (truncate intensity (truncate length 5)))))
      (apply #'alexandria:circular-list
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
                           :collect (+ (ash r 16) (ash g 8) b)))))))

(defvar *panel-color*)

(defmethod tui:present ((window panel-view))
  (loop :for d :across (concatenate 'string "HP: "(princ-to-string *hp*))
        :for col :from 1
        :do (tui:put d 1 col (tui:make-style :fg (car *panel-color*)
                                             :boldp t))
            (setf *panel-color* (cdr *panel-color*)))
  (loop :for d :across (concatenate 'string "Sc0re:" (princ-to-string *score*))
        :for col :from 1
        :do (tui:put d 2 col (tui:make-style :fg (car *panel-color*)
                                             :boldp t))))

(defmethod tui:handle-key-event ((window panel-view) event)
  nil)

(defmethod tui:handle-mouse-event ((window panel-view) type button line col controlp)
  nil)

;;; game logic

(defun tui-handle-event (tui ev)
  (cond ((and (characterp ev) (char= ev #\etb))
         (tui:stop tui))
        ((and (characterp ev) (char= ev #\esc))
         (incf *hp* 100)
         (break))))

(defparameter *tick* 0.1)

(defparameter *chars* #(#\a #\b #\c #\d #\e #\f #\g #\h #\k #\L #\m #\n
                        #\o #\p #\Q #\r #\s #\T #\u #\v #\w #\x #\y #\z
                        #\@ #\& #\$ #\# #\: #\; #\? #\{ #\} #\[ #\] #\.))

(defvar *sand-color*)

;; livecoding hack
(declaim (notinline %game-tick))
(defun %game-tick (tui)
  ;; spawn new
  (setf (aref *sand* 0 (random 80))
        (list (aref *chars* (random (length *chars*)))
              (tui:make-style :fg (car *sand-color*))))
  (setf *sand-color* (cdr *sand-color*))
  ;; shuffle sand down, starting from bottom to prevent overwriting
  (loop :for y :from (1- (array-dimension *sand* 0)) :downto 0
        :do (loop :for x :below (array-dimension *sand* 1)
                  :do (let ((sand (aref *sand* y x)))
                        (when sand
                          (setf (aref *sand* y x) nil)
                          (cond ((= y (1- (tui:lines tui)))
                                 (decf *sand-count*)
                                 (decf *hp*))
                                ((aref *projectiles* y x)
                                 (setf (aref *projectiles* y x) nil)
                                 (decf *sand-count*)
                                 (incf *score* y))
                                ((aref *projectiles* (1+ y) x)
                                 (setf (aref *projectiles* (1+ y) x) nil)
                                 (decf *sand-count*)
                                 (incf *score* y))
                                (t ; nothing blocking it, move down
                                 (setf (aref *sand* (1+ y) x) sand)))))))
  ;; shuffle projectiles up!
  (loop :for y :from 0 :to (1- (array-dimension *projectiles* 0))
        :do (loop :for x :below (array-dimension *projectiles* 1)
                  :do (let ((projectile (aref *projectiles* y x)))
                        (when projectile
                          (setf (aref *projectiles* y x) nil)
                          (unless (zerop y)
                            (setf (aref *projectiles* (1- y) x) projectile))))))
  (incf *sand-count*)
  ;; end game
  (when (minusp *hp*)
    (princ (make-string 10000 :initial-element #\L))
    (force-output)
    (sleep 1)
    (tui:stop tui))
  (tui:redisplay tui)
  *tick*)

(defun game-tick (tui)
  (%game-tick tui))

(defun tui-main ()
  (let* ((dimensions (tui:terminal-dimensions))
         (*hp* 10)
         (rows (car dimensions))
         ;;(columns (cdr dimensions))
         (*sand* (make-array (list rows 80) :initial-element nil))
         (*sand-count* 0)
         (*score* 0)
         (*projectiles* (make-array (list rows 80) :initial-element nil))
         (*sand-color* *color-scale*)
         (*panel-color* *color-scale*)
         (sand-view
           (make-instance 'sand-view :dimensions (tui:make-rectangle :x 0
                                                                     :y 0
                                                                     :cols 80
                                                                     :rows rows)))
         (panel-view
           (make-instance 'panel-view :dimensions (tui:make-rectangle :x 80
                                                                      :y 0
                                                                      :cols 10
                                                                      :rows 10)))
         (game-ui
           (make-instance 'tui:tui :focused-window sand-view
                                   :windows (list sand-view panel-view)
                                   :event-handler #'tui-handle-event)))
    (tui:schedule-timer game-ui (tui:make-timer *tick* #'game-tick))
    ;;(sb-sprof:start-profiling)
    (tui:start game-ui)
    ;;(sb-sprof:stop-profiling)
    ))

(defun main ()
  (if (member :slynk *features*)
      (bt:make-thread (lambda () (tui-main)))
      (tui-main)))
