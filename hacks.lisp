(in-package :uncursed-sys)

;;; palette approximation

(defparameter *xterm-256*
  (let ((colors (make-array 256 :element-type 'fixnum)))
    (loop :for r :below 6
          :do (loop :for g :below 6
                    :do (loop :for b :below 6
                              :do (flet ((cube-color (c)
                                           (if (plusp c)
                                               (+ 55 (* c 40))
                                               0)))
                                    (setf (aref colors (+ 16 (* r 36) (* g 6) b))
                                          (+ (ash (cube-color r) 16)
                                             (ash (cube-color g) 8)
                                             (cube-color b)))))))
    (loop :for shade :below 24
          :for c = (+ 8 (* shade 10))
          :do (setf (aref colors (+ 232 shade))
                    (+ (ash c 16) (ash c 8) c)))
    (setf (aref colors 00) #x000000)
    (setf (aref colors 01) #x800000)
    (setf (aref colors 02) #x008000)
    (setf (aref colors 03) #x808000)
    (setf (aref colors 04) #x000080)
    (setf (aref colors 05) #x800080)
    (setf (aref colors 06) #x008080)
    (setf (aref colors 07) #xc0c0c0)
    (setf (aref colors 08) #x808080)
    (setf (aref colors 09) #xff0000)
    (setf (aref colors 10) #x00ff00)
    (setf (aref colors 11) #xffff00)
    (setf (aref colors 12) #x0000ff)
    (setf (aref colors 13) #xff00ff)
    (setf (aref colors 14) #x00ffff)
    (setf (aref colors 15) #xffffff)
    colors))

(defun red (color) (ldb (byte 8 16) color))
(defun green (color) (ldb (byte 8 8) color))
(defun blue (color) (ldb (byte 8 0) color))

;; Based on xterm, src/misc.c/allocateClosestRGB()
(defun color-diff (a b)
  (let ((dr (* 0.30 (- (red a) (red b))))
        (dg (* 0.61 (- (green a) (green b))))
        (db (* 0.11 (- (blue a) (blue b)))))
    (+ (expt dr 2) (expt dg 2) (expt db 2))))

(defun approximate-rgb (rgb)
  (loop :with best-diff = most-positive-fixnum
        :with best-color = 0
        :for i :from 0 :below 256
        :for xc :across *xterm-256*
        :for diff = (color-diff rgb xc)
        :do (when (< diff best-diff)
              (setf best-color i
                    best-diff diff))
        :finally (return best-color)))

;;; palette modification - minimise damage by using 16-231 and hoping that orig_colors works

(defvar *current-index* 16)
(defvar *palette* (make-array 231 :element-type 'fixnum :initial-element most-positive-fixnum)
  "Maps indexes 1-231 to their colors to save some sequences. Yes we only actually use 16-231.")

(defun lookup-color (color)
  (loop :for i :from 0
        :for c :across *palette*
        :do (when (= color c)
              (return i))))

(defun next-free-color ()
  (let ((old *current-index*))
    (if (<= *current-index* 230) ; don't overwrite past 231 where the grayscale ramp starts
        (incf *current-index*)
        (setf *current-index* 16))
    old))

(defun reset-colors ()
  (ti:tputs ti:orig-colors)
  (setf *current-index* 16)
  (fill *palette* most-positive-fixnum))

(defun color-magic (c)
  "very clever."
  (truncate (* c 1000/255)))
