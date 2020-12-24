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

;; TODO please remove this once terminfo is updated
(in-package :terminfo)
(defun tparm (string &rest args)
  "Return the string representing the command and arguments."
  (when (null string) (return-from tparm ""))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (do ((stack '()) (flags 0) (width 0) (precision 0) (number 0)
           (dvars (make-array 26 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
           (svars (load-time-value
                   (make-array 26 :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
           (c (read-char in nil) (read-char in nil)))
          ((null c))
        (cond ((char= c #\%)
               (setq c (read-char in) flags 0 width 0 precision 0)
               (tagbody
                state0
                  (case c
                    (#\% (princ c out) (go terminal))
                    (#\: (setq c (read-char in)) (go state2))
                    (#\+ (go state1))
                    (#\- (go state1))
                    (#\# (go state2))
                    (#\Space (go state2))
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (go state3))
                    (#\d (go state5))
                    (#\o (go state6))
                    ((#\X #\x) (go state7))
                    (#\s (go state8))
                    (#\c (princ (code-char (pop stack)) out) (go terminal))
                    (#\p (go state9))
                    (#\P (go state10))
                    (#\g (go state11))
                    (#\' (go state12))
                    (#\{ (go state13))
                    (#\l (push (length (pop stack)) stack) (go terminal))
                    (#\* (push (* (pop stack) (pop stack)) stack)
                     (go terminal))
                    (#\/ (push (let ((n (pop stack))) (/ (pop stack) n)) stack)
                     (go terminal))
                    (#\m (push (let ((n (pop stack))) (mod (pop stack) n))
                               stack)
                     (go terminal))
                    (#\& (push (logand (pop stack) (pop stack)) stack)
                     (go terminal))
                    (#\| (push (logior (pop stack) (pop stack)) stack)
                     (go terminal))
                    (#\^ (push (logxor (pop stack) (pop stack)) stack)
                     (go terminal))
                    (#\= (push (if (= (pop stack) (pop stack)) 1 0) stack)
                     (go terminal))
                    (#\> (push (if (<= (pop stack) (pop stack)) 1 0) stack)
                     (go terminal))
                    (#\< (push (if (>= (pop stack) (pop stack)) 1 0) stack)
                     (go terminal))
                    (#\A (push (if (or (zerop (pop stack))
                                       (zerop (pop stack)))
                                   0
                                   1)
                               stack)
                     (go terminal))
                    (#\O (push (if (and (zerop (pop stack))
                                        (zerop (pop stack)))
                                   0
                                   1)
                               stack)
                     (go terminal))
                    (#\! (push (if (zerop (pop stack)) 1 0) stack)
                     (go terminal))
                    (#\~ (push (logand #xFF (lognot (pop stack))) stack)
                     (go terminal))
                    (#\i (when args
                           (incf (first args))
                           (when (cdr args)
                             (incf (second args))))
                     (go terminal))
                    (#\? (go state14))
                    (#\t (go state15))
                    (#\e (go state16))
                    (#\; (go state17))
                    (otherwise (error "Unknown %-control character: ~C" c)))
                state1
                  (let ((next (peek-char nil in nil)))
                    (when (position next "0123456789# +-doXxs")
                      (go state2)))
                  (if (char= c #\+)
                      (push (+ (pop stack) (pop stack)) stack)
                      (push (let ((n (pop stack))) (- (pop stack) n)) stack))
                  (go terminal)
                state2
                  (case c
                    (#\# (setf flags (logior flags 1)))
                    (#\+ (setf flags (logior flags 2)))
                    (#\Space (setf flags (logior flags 4)))
                    (#\- (setf flags (logior flags 8)))
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                     (go state3))
                    (t (go blah)))
                  (setf c (read-char in))
                  (go state2)
                state3
                  (setf width (digit-char-p c))
                state3-loop
                  (setf c (read-char in))
                  (case c
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                     (setf width (+ (* width 10) (digit-char-p c)))
                     (go state3-loop))
                    (#\. (setf c (read-char in)) (go state4)))
                  (go blah)
                state4
                  (setf precision (digit-char-p c))
                state4-loop
                  (setf c (read-char in))
                  (case c
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                     (setf precision (+ (* precision 10) (digit-char-p c)))
                     (go state4-loop)))
                  (go blah)
                blah
                  (case c
                    (#\d (go state5))
                    (#\o (go state6))
                    ((#\X #\x) (go state7))
                    (#\s (go state8))
                    (otherwise (error "Unknown %-control character: ~C" c)))
                state5
                state6
                state7
                state8
                  (princ (xform (truncate (pop stack)) ; literally this one line was broken
                                c flags width precision) out)
                  (go terminal)
                state9
                  (let* ((i (digit-char-p (read-char in)))
                         (a (nth (1- i) args)))
                    (etypecase a
                      (character (push (char-code a) stack))
                      (integer (push a stack))
                      (string (push a stack))))
                  (go terminal)
                state10
                  (let ((var (read-char in)))
                    (cond ((char<= #\a var #\z)
                           (setf (aref dvars (- (char-code var)
                                                (char-code #\a)))
                                 (pop stack)))
                          ((char<= #\A var #\Z)
                           (setf (aref svars (- (char-code var)
                                                (char-code #\A)))
                                 (pop stack)))
                          (t (error "Illegal variable name: ~C" var))))
                  (go terminal)
                state11
                  (let ((var (read-char in)))
                    (cond ((char<= #\a var #\z)
                           (push (aref dvars (- (char-code var)
                                                (char-code #\a)))
                                 stack))
                          ((char<= #\A var #\Z)
                           (push (aref svars (- (char-code var)
                                                (char-code #\A)))
                                 stack))
                          (t (error "Illegal variable name: ~C" var))))
                  (go terminal)
                state12
                  (push (char-code (read-char in)) stack)
                  (unless (char= (read-char in) #\')
                    (error "Invalid character constant"))
                  (go terminal)
                state13
                  (setq number 0)
                state13-loop
                  (setq c (read-char in))
                  (let ((n (digit-char-p c)))
                    (cond (n (setq number (+ (* 10 number) n))
                             (go state13-loop))
                          ((char= c #\})
                           (push number stack)
                           (go terminal))))
                  (error "Invalid integer constant")
                state14
                  (go terminal)
                state15
                  (when (/= (pop stack) 0)
                    (go terminal))
                  (skip-forward in t)
                  (go terminal)
                state16
                  (skip-forward in nil)
                state17
                terminal
                  #| that's all, folks |#)) ; thank goodness
              (t (princ c out)))))))
