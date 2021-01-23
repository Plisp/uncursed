;;;; DISCLAIMER
;;;; this was a weekend job, interleaved with implementing timer support and library fixes
;;;; clearly the work of a madman wholly unacquainted with real game development
;;;; It will be left in this state for sentimental value. Do not use the lib in this manner
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'bordeaux-threads))

(defpackage #:uncursed-sand-game
  (:use :cl)
  (:local-nicknames (#:tui :uncursed))
  (:export #:main))
(in-package #:uncursed-sand-game)

(defvar *sand*)
(defvar *sand-count*)
(defvar *projectiles*)

(defparameter *tui-width* 110)
(defparameter *sand-width* 60)

(defclass game-ui (tui:tui)
  ())

(defmethod tui:run :before ((tui game-ui))
  (tui:enable-alternate-screen)
  (tui:set-cursor-shape :invisible))

(defmethod tui:handle-resize ((tui game-ui))
  (when (or (< (tui:cols tui) *tui-width*) (< (tui:rows tui) 30))
    (dotimes (i 10)
      (print "get a bigger terminal"))
    (sleep 2)
    (tui:stop tui)))

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

(defmethod tui:handle-key-event ((window sand-view) tui event)
  nil)

(defmethod tui:handle-mouse-event ((window sand-view) tui button state line col &key)
  (setf (aref *projectiles* (1- line) (1- col)) (tui:make-style :fg #xffffff)))

;;; score panel

(defclass panel-view (tui:standard-window)
  ())

(defvar *hp*)
(defvar *score*)


(defconstant +intensity+ 255)
(defconstant +colors+ 70)

(defparameter *color-scale*
  (let ((increment (1+ (truncate +intensity+ (truncate +colors+ 5)))))
    (apply #'alexandria:circular-list
           (append (loop :for r = +intensity+
                         :for g :to +intensity+ :by increment
                         :for b = 0
                         :collect (+ (ash r 16) (ash g 8) b))
                   (loop :for r :downfrom +intensity+ :to 0 :by increment
                         :for g = +intensity+
                         :for b = 0
                         :collect (+ (ash r 16) (ash g 8) b))
                   (loop :for r = 0
                         :for g = +intensity+
                         :for b :to +intensity+ :by increment
                         :collect (+ (ash r 16) (ash g 8) b))
                   (loop :for r = 0
                         :for g :downfrom +intensity+ :to 0 :by increment
                         :for b = +intensity+
                         :collect (+ (ash r 16) (ash g 8) b))
                   (loop :for r :to +intensity+ :by increment
                         :for g = 0
                         :for b = +intensity+
                         :collect (+ (ash r 16) (ash g 8) b))))))

(defvar *panel-color*)
(defvar *panel-food*)
(defvar *panel-water*)
(defparameter *panel-bar-max* 47)

(defmethod tui:present ((window panel-view))
  (flet ((maybe-rainbow ()
           (when (> *score* 5000)
             (setf *panel-color* (cdr *panel-color*)))))
    (tui:puts "┌────────────────────────────────────────────────┐"
              1 1
              (tui:make-style :fg (car *panel-color*)))
    (maybe-rainbow)
    (tui:puts "┃ Get to 9999 SCORE!! Click bars to replenish    ┃"
              2 1
              (tui:make-style :fg (car *panel-color*) :boldp t))
    (maybe-rainbow)
    (tui:puts (format nil "┃ HP: ~1d                                          ┃" *hp*)
              3 1
              (tui:make-style :fg (car *panel-color*) :boldp t))
    (maybe-rainbow)
    (tui:puts (format nil "┃ Sc0re͇: ~4d                                    ┃" *score*)
              4 1
              (tui:make-style :fg (car *panel-color*) :boldp t))
    ;; border
    (tui:puts "┠────────────────────────────────────────────────┨"
              5 1
              (tui:make-style :fg (car *panel-color*)))
    ;; food text
    (maybe-rainbow)
    (tui:puts (format nil "┃ Food: ~2d                                       ┃" *panel-food*)
              6 1
              (tui:make-style :fg (car *panel-color*) :boldp t))
    ;; water bar
    (tui:put-style (tui:make-style :fg #x228B22 :bg #xD6A373 :boldp t)
                   (tui:make-rect :x 1 :y 5
                                  :rows 1
                                  :cols *panel-food*))
    (maybe-rainbow)
    (tui:put-style (tui:make-style :bg (car *panel-color*) :boldp t)
                   (tui:make-rect :x (1+ *panel-food*) :y 5
                                  :rows 1
                                  :cols (- *tui-width* *sand-width* *panel-food* 2)))
    ;; border
    (tui:puts "┠────────────────────────────────────────────────┨"
              7 1
              (tui:make-style :fg (car *panel-color*)))
    ;; water text
    (maybe-rainbow)
    (tui:puts (format nil "┃ Water: ~2d                                      ┃" *panel-water*)
              8 1
              (tui:make-style :fg (car *panel-color*) :boldp t))
    ;; water bar
    (tui:put-style (tui:make-style :fg #x228B22 :bg #x40A4DF :boldp t)
                   (tui:make-rect :x 1 :y 7
                                  :rows 1
                                  :cols *panel-water*))
    (maybe-rainbow)
    (tui:put-style (tui:make-style :bg (car *panel-color*) :boldp t)
                   (tui:make-rect :x (1+ *panel-water*) :y 7
                                  :rows 1
                                  :cols (- *tui-width* *sand-width* *panel-water* 2)))
    ;; border
    (tui:puts "└────────────────────────────────────────────────┘"
              9 1
              (tui:make-style :fg (car *panel-color*)))))

(defmethod tui:handle-key-event ((window panel-view) tui event)
  nil)

(defmethod tui:handle-mouse-event ((window panel-view) tui button state line col &key)
  (cond ((and (eq state :click) (= line 6))
         (let ((pizza (make-instance 'pizza-view
                                     :dimensions (tui:make-rect :x (random 60)
                                                                :y (random 30)
                                                                :rows 3
                                                                :cols 13))))
           (push pizza (tui:windows tui))))
        ((and (eq state :click) (= line 8))
         (setf *panel-water* (min (+ *panel-water* 40) *panel-bar-max*)))))

;;; the pizza

(defclass pizza-view (tui:standard-window)
  ())

(defmethod tui:present ((window pizza-view))
  (tui:puts "┌───────────┐"
            1 1
            (tui:make-style :fg (car *panel-color*)))
  (tui:puts "│ eat pizza │"
            2 1
            (tui:make-style :fg (car *panel-color*) :italicp t))
  (tui:puts "└───────────┘"
            3 1
            (tui:make-style :fg (car *panel-color*))))

(defmethod tui:handle-key-event ((window pizza-view) tui event)
  nil)

(defmethod tui:handle-mouse-event ((window pizza-view) tui button state line col &key)
  (alexandria:deletef (tui:windows tui) window)
  (setf *panel-food* (min (+ *panel-food* 5) *panel-bar-max*))
  t)

;;; game logic

(defun tui-handle-event (tui ev)
  (cond ((equal ev '(#\w :control))
         (tui:stop tui))))

(defvar *tick* 0.1)
(defvar *time*)

(defparameter *chars* #(#\a #\b #\c #\d #\e #\f #\g #\h #\k #\L #\m #\n
                        #\o #\p #\Q #\r #\s #\T #\u #\v #\w #\x #\y #\z
                        #\@ #\& #\$ #\# #\: #\; #\? #\{ #\} #\[ #\] #\.))

(defvar *sand-color*)

;; livecoding hack
(declaim (notinline %game-tick))
(defun %game-tick (tui)
  (incf *time*)
  (when (> *score* 5000)
    (setf *tick* 0.06))
  ;; spawn new
  (setf (aref *sand* 0 (random *sand-width*))
        (list (aref *chars* (random (length *chars*)))
              (tui:make-style :fg (car *sand-color*))))
  (when (> *score* 5000)
    (setf *sand-color* (cdr *sand-color*)))
  ;; shuffle sand down, starting from bottom to prevent overwriting
  (loop :for y :from (1- (array-dimension *sand* 0)) :downto 0
        :do (loop :for x :below (array-dimension *sand* 1)
                  :do (let ((sand (aref *sand* y x)))
                        (when sand
                          (setf (aref *sand* y x) nil)
                          (cond ((= y (1- (tui:rows tui)))
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
  ;; hunger
  (when (and (zerop (mod *time* 3)) (plusp *panel-food*))
    (decf *panel-food*))
  (when (and (zerop *panel-food*) (zerop (mod *time* 3)))
    (decf *hp*))
  ;; thirst
  (when (and (oddp *time*) (plusp *panel-water*))
    (decf *panel-water*))
  (when (zerop *panel-water*)
    (setf *hp* -1))
  ;; win
  (when (>= *score* 9999)
    (ti:tputs ti:clear-screen)
    (tui:set-cursor-position 10 10)
    (write-string "                    .-'''-.
                   '   _    \
                 /   /` '.   \                              .--.   _..._
 .-.          .-.   |     \  '                       _     _|__| .'     '.
  \ \        / /|   '      |  '                /\    \\   //.--..   .-.   .
   \ \      / / \    \     / /                 `\\  //\\ // |  ||  '   '  |
    \ \    / /   `.   ` ..' /_    _              \`//  \'/  |  ||  |   |  |
     \ \  / /       '-...-'`| '  / |              \|   |/   |  ||  |   |  |
      \ `  /               .' | .' |               '        |  ||  |   |  |
       \  /                /  | /  |                        |__||  |   |  |
       / /                |   `'.  |                            |  |   |  |
   |`-' /                 '   .'|  '/                           |  |   |  |
    '..'                   `-'  `--'                            '--'   '--' ")
    (finish-output)
    (sleep 3)
    (tui:stop tui))
  ;; end game
  (when (minusp *hp*)
    (write-string (make-string 10000 :initial-element #\L))
    (force-output)
    (sleep 1)
    (tui:stop tui))
  (tui:redisplay tui)
  *tick*)

(defun game-tick (tui context)
  (declare (ignore context))
  (%game-tick tui))

(defun tui-main ()
  (let* ((dimensions (tui:terminal-dimensions))
         (*hp* 9)
         (rows (car dimensions))
         (columns (cdr dimensions))
         (*sand* (make-array (list rows *sand-width*) :initial-element nil))
         (*sand-count* 0)
         (*score* 0)
         (*tick* 0.1)
         (*time* 0)
         (*projectiles* (make-array (list rows *sand-width*) :initial-element nil))
         (*sand-color* *color-scale*)
         (*panel-color* *color-scale*)
         (*panel-food* *panel-bar-max*)
         (*panel-water* *panel-bar-max*)
         (sand-view
           (make-instance 'sand-view
                          :dimensions (tui:make-rect :x 0
                                                     :y 0
                                                     :cols *sand-width*
                                                     :rows rows)))
         (panel-view
           (make-instance 'panel-view
                          :dimensions (tui:make-rect :x *sand-width*
                                                     :y 0
                                                     :cols (- *tui-width* *sand-width*)
                                                     :rows rows)))
         (game-ui
           (make-instance 'game-ui :focused-window sand-view
                                   :windows (list sand-view panel-view)
                                   :event-handler #'tui-handle-event)))
    (if (or (< columns *tui-width*) (< rows 30))
        (write-line "get a bigger terminal")
        (progn
          (tui:schedule-timer game-ui (tui:make-timer *tick* #'game-tick))
          (tui:run game-ui)))))

(defun main ()
  (if (member :slynk *features*)
      (bt:make-thread (lambda () (tui-main)))
      (tui-main)))
