;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; internals
;;

(in-package :uncursed-sys)

;; EQL is fine for integers
(let ((cache (apply #'make-hash-table
                    #+(or sbcl ecl) (list :synchronized t)
                    #+ccl nil)))
  (defun character-width (character)
    "Returns the displayed width of CHARACTER and its string representation as multiple
values."
    (declare (optimize speed))
    (let ((codepoint (char-code character)))
      (values-list
       (ensure-gethash
        codepoint cache
        (cond ((= codepoint 0) (list 2 "^@")) ; NUL is ^@
              ((= codepoint 9) (list 8 "        ")) ; TODO should be variable
              ((<= #xD800 codepoint #xDFFF) (list 2 "�")) ; surrogate
              (t
               (let ((width (cffi:foreign-funcall "wcwidth" c-wchar codepoint :int)))
                 (if (minusp width)
                     (if (< codepoint 32) ; caret notation
                         (list 2 (format nil "^~C" (code-char (logxor codepoint #x40))))
                         (list 2 "�"))
                     (list width (string character)))))))))))

(defun display-width (string)
  "Good enough"
  (reduce #'+ (map 'vector #'character-width string)))

(defvar *fallback-terminal-dimensions* (cons 24 80))

(defun terminal-dimensions ()
  "Returns a cons (LINES . COLUMNS) containing the dimensions of the terminal device
backing FD. Returns NIL on failure."
  (cffi:with-foreign-object (ws '(:struct c-winsize))
    (when (= 0 (cffi:foreign-funcall "ioctl" :int 1 :int c-get-winsz :pointer ws :int))
      (cffi:with-foreign-slots ((c-ws-rows c-ws-cols) ws (:struct c-winsize))
        (return-from terminal-dimensions (cons c-ws-rows c-ws-cols)))))
  ;; attempt env
  (let ((env-lines (uiop:getenv "LINES"))
        (env-columns (uiop:getenv "COLUMNS")))
    (when (and env-lines env-columns)
      (return-from terminal-dimensions (cons env-lines env-columns))))
  *fallback-terminal-dimensions*)

;;; input

;; utilities

;; (defun fd-stream-p (stream)
;;   #+sbcl (sb-sys:fd-stream-p stream)
;;   )

;; (defun stream-fd (stream)
;;   #+sbcl (sb-sys:fd-stream-fd stream)
;;   )

;; (defun coerce-stream-to-fd (stream)
;;   (when (typep stream 'synonym-stream)
;;     (setf stream (symbol-value (synonym-stream-symbol stream))))
;;   (if (fd-stream-p stream)
;;       (stream-fd stream)
;;       (error-fd-indeterminate "could not determine fd for ~s" stream)))

(defun enable-mouse ()
  (format *terminal-io* "~c[?1006h~c[?1003h" #\esc #\esc))

(defun disable-mouse ()
  (format *terminal-io* "~c[?1006l~c[?1003l" #\esc #\esc))

;; setup

(cffi:defcfun "tcgetattr" :int
  (fd :int)
  (termios-p (:pointer (:struct c-termios))))

(cffi:defcfun "tcsetattr" :int
  (fd :int)
  (optional-actions :int)
  (termios-p (:pointer (:struct c-termios))))

(defun setup-terminal (fd)
  "Disables terminal echoing and buffering. Returns a foreign pointer to the original termios.
Sets process locale from environment so hopefully unicode works better."
  (cffi:with-foreign-string (s "")
    (cffi:foreign-funcall "setlocale" :int c-lc-ctype :string s :pointer))
  (let ((old-termios (cffi:foreign-alloc '(:struct c-termios))))
    (when (minusp (tcgetattr fd old-termios))
      (error-syscall-error "tcgetattr failed"))
    (cffi:with-foreign-object (new-termios '(:struct c-termios))
      (setf (cffi:mem-ref new-termios '(:struct c-termios))
            (cffi:mem-ref old-termios '(:struct c-termios)))
      (cffi:with-foreign-slots ((c-iflag c-oflag c-lflag c-cflag) new-termios (:struct c-termios))
        (setf c-iflag (logandc2 c-iflag (logior c-iexten c-igncr c-icrnl c-inlcr
                                                c-inpck c-istrip
                                                c-ixon c-ixoff)))
        (setf c-oflag (logandc2 c-oflag c-opost))
        (setf c-lflag (logandc2 c-lflag (logior c-icanon c-isig c-echo)))
        (setf c-cflag (logandc2 c-lflag c-parenb))
        (when (minusp (tcsetattr fd c-set-attributes-now new-termios))
          (error-syscall-error "tcsetattr failed"))
        old-termios))))

(defun restore-terminal (old-termios fd)
  "Restores the terminal device backing FD to its original state. ORIG-TERMIOS is a pointer
to the original termios struct returned by a call to SETUP-TERM which is freed."
  (when (minusp (tcsetattr fd c-set-attributes-now old-termios))
    (error-syscall-error "tcsetattr failed"))
  (cffi:foreign-free old-termios)
  (values))

;; taken from acute-terminal-control READ-EVENT TODO rewrite
(symbol-macrolet ((read (read-char *standard-input* nil))) ;As we already know, xterm is very poorly and barely designed.
  (defun read-event (&optional (stream *terminal-io*)
                     &aux (*terminal-io* stream) (first read) second third)
    (block nil
      ;;Xterm likes to make its own standards for things, even when standards already exist.
      ;;This tested the eighth bit for the Meta key.
      ;;In a way, this arguably violated the character-set agnosticism, but not truly, I suppose.
      ;;In any case, I'll instead implement the escape-prefixing nonsense.  It couldn't be simple, no.
      ;;(if (logbitp 7 (char-code first))
      ;;    (return (cons :meta (code-char (ldb (byte 7 0) (char-code first))))))
      ;;This permits Escape being its own key without first needing more input.
      ;;Of course, this violates the principle that input should be waited on, but it seems no one cares about that.
      ;;So, you must send at least the first three characters of a control sequence at once for it to be recognized.
      (or (listen) (return first))
      (setq second read)
      ;;This implements the silly Meta key convention that prefixes with escape.
      ;;I must perform a trick to see if it's part of a control function or not.
      (if (eql #.(code-char #x1B) first)
          (if (or (eql #.(code-char #x5B) second)
                  (eql #.(code-char #x4F) second))
              (or (listen) (return (cons :meta second)))
              (return (cons :meta second))))
      (and (eql #.(code-char #x1B) first) ;;Xterm then likes to implement real standards incorrectly.
           (eql #.(code-char #x4F) second) ;;Why use FUNCTION KEY when you can use SS3?
           (setq third read)
           ;;Here's the pattern xterm uses for function keys one through twelve:
           ;;SS3 P     SS3 Q     SS3 R     SS3 S
           ;;CSI 1 5 ~ CSI 1 7 ~ CSI 1 8 ~ CSI 1 9 ~
           ;;CSI 2 0 ~ CSI 2 1 ~ CSI 2 3 ~ CSI 2 4 ~
           ;;Simplicity at its finest, you see.
           ;;With any luck, this garbage can be removed soon.
           (return (and (<= #x50 (char-code third) #x53)
                        (cons :function (- (char-code third) #x4F)))))
      (and (eql #.(code-char #x1B) first)
           (eql #.(code-char #x5B) second)
           (setq third read)
           (return (cond ((char= third #.(code-char #x41)) :up)
                         ((char= third #.(code-char #x42)) :down)
                         ((char= third #.(code-char #x43)) :right)
                         ((char= third #.(code-char #x44)) :left)
                         ((char= third #.(code-char #x4D)) ;xterm X10 mouse reporting
                          (let ((first (- (char-code (or read (return))) 32))
                                (second (- (char-code (or read (return))) 32)))
                            (ignore-errors ;whoever designed this protocol should be executed ;) agreed
                             (list* :mouse
                                    (mod (1+ (char-code (or read (return)))) 4)
                                    (if (> first 223) (return) first)
                                    (if (> second 223) (return) second)))))
                         ((char= third #.(code-char #x3C)) ;xterm SGR mouse reporting
                          (let* ((first (loop for c = read do (or c (return))
                                              until (char= c #.(code-char #x3B))
                                              do (if (or (char= c #.(code-char #x4D)) (char= c #.(code-char #x6D)))
                                                     (progn (unread-char c) (loop-finish)))
                                              do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return))
                                              collect c))
                                 (second (loop for c = read do (or c (return))
                                               until (char= c #.(code-char #x3B))
                                               do (if (or (char= c #.(code-char #x4D)) (char= c #.(code-char #x6D)))
                                                      (progn (unread-char c) (loop-finish)))
                                               do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return))
                                               collect c))
                                 (release)
                                 (third (loop named loop
                                              for c = read do (or c (return-from loop))
                                              until (char= c #.(code-char #x3B))
                                              do (if (char= c #.(code-char #x4D)) (loop-finish))
                                              do (when (char= c #.(code-char #x6D))
                                                   (setq release :release)
                                                   (loop-finish)) ;ignore mouse releases
                                              do (or (char= c #.(code-char #x00)) (<= #x30 (char-code c) #x39) (return-from loop))
                                              collect c))
                                 (first-default #.(make-string 1 :initial-element #.(code-char #x30)))
                                 (default #.(make-string 1 :initial-element #.(code-char #x31))))
                            (setq first (or (and first (make-array (length first) :element-type 'character :initial-contents first)) first-default)
                                  first (parse-integer first)
                                  second (or (and second (make-array (length second) :element-type 'character :initial-contents second)) default)
                                  third (or (and third (make-array (length third) :element-type 'character :initial-contents third)) default))
                            (list* (or release
                                       (case first
                                         (0 :mouse) (1 :mouse) (2 :mouse)
                                         (32 :drag) (34 :drag)
                                         (35 :hover) (51 :hover)
                                         (64 :scroll-up)
                                         (65 :scroll-down)
                                         (66 :scroll-left)
                                         (67 :scroll-right)))
                                   (ldb (byte 2 0) (1+ (if (> first 32) (- first 32) first)))
                                   (parse-integer second)
                                   (parse-integer third)
                                   (unless (zerop (ldb (byte 1 4) first)) :control))))
                         ;;Now, if the sequences sent were sane, I wouldn't even need this COND.
                         ;;Xterm sends CONTROL SEQUENCE INTRODUCER, but with characters other than parameters immediately following it.
                         ;;Instead, I need a different case for each and then I can do the parsing, which is still followed by other identifying characters.
                         ;;So, this could've been a beautiful function, I think, but the horrible design of xterm prevented that.
                         (t (let ((integer (parse-integer
                                            (coerce (cons third
                                                          (loop for char = read do (or char (return))
                                                                do (if (or (char= char #.(code-char #x20)) (char= char #.(code-char #x7E)))
                                                                       (progn (unread-char char) (loop-finish)))
                                                                do (or (char= char #.(code-char #x00)) (<= #x30 (char-code char) #x39) (return))
                                                                if (char/= char #.(code-char #x00)) collect char))
                                                    'string))))
                              (if (char= read #.(code-char #x7E)) ;This is the stupid convention that doesn't scale to infinity.

                                  (let ((function-key (case integer (11 1) (12 2) (13 3) (14 4) (15 5) (17 6) (18 7) (19 8) (20 9) (21 10)
                                                            (23 11) (24 12) (25 13) (26 14) (28 15) (29 16) (31 17) (32 18) (33 19) (34 20))))
                                    (if function-key ; ugh
                                        (cons :function function-key)
                                        (case integer (5 :page-up) (6 :page-down))))
                                  (if (and (char= read #.(code-char #x20)) ;This is FUNCTION KEY, the proper way to do this.
                                           (char= read #.(code-char #x57))) ;Unfortunately, I must do this in an ugly way.
                                      (cons :function integer))))))))
      (and second (not third) (unread-char second))
      first)))

(defun read-event-timeout (&optional timeout (stream *terminal-io*))
  #+sbcl (if timeout
             (handler-case
                 (sb-sys:with-deadline (:seconds timeout)
                   (read-event stream))
               (sb-sys:deadline-timeout ()))
             (read-event stream))
  #+ccl (if timeout
            (handler-case
                (ccl:with-input-timeout ((s stream) 1)
                  (read-event s))
              (ccl:input-timeout ())))
  ;; TODO best way to do this on ecl?
  ;; #+ecl (let ((serve-event::*descriptor-handlers*
  ;;               (copy-list serve-event::*descriptor-handlers*))
  ;;             (event))
  ;;         (serve-event:add-fd-handler (si:file-stream-fd stream) :input
  ;;                                     #'(lambda (fd)
  ;;                                         (setf event (read-event stream))))
  ;;         (serve-event:serve-event timeout))
  #-(or sbcl ccl ecl)
  (when timeout (error "timeout not supported"))
  )

;;; misc

;; NOTE: scrolling is near useless because most terminals don't support changing
;; left/right margins. thus I will not include support as most uses are probably a mistake

(defun enable-alternate-screen ()
  (ti:tputs ti:enter-ca-mode))

(defun disable-alternate-screen ()
  (ti:tputs ti:enter-ca-mode))

(defun clear-screen ()
  (ti:tputs ti:clear-screen))

(defun clear-to-end-of-line ()
  (ti:tputs ti:clr-eol))

(defun clear-chars (&optional (n 1))
  (ti:tputs ti:erase-chars n))

(defun (setf cursor-position) (new-value)
  "NEW-VALUE is a (LINE . COLUMN) pair"
  (ti:tputs ti:cursor-address (car new-value) (cdr new-value)))

(defun set-mouse-shape (style &key blink-p)
  (let ((arg (case style
               (:block 2)
               (:underline 4)
               (:bar 6))))
    (format *terminal-io* "~c[~d q" #\esc (if blink-p (1- arg) arg))))

(defun set-background (r g b)
  (format *terminal-io* "~c[48;2;~d;~d;~dm" #\esc r g b))
