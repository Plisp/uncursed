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
    (let* ((codepoint (char-code character))
           (result (gethash codepoint cache)))
      (or result
          (setf (gethash codepoint cache)
                (cffi:foreign-funcall "wcwidth" c-wchar codepoint :int))))))

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
  "Disables terminal echoing and buffering and enables mouse mode 1003.
Returns a pointer to the original termios. Sets process locale to environment."
  (cffi:with-foreign-string (s "")
    (cffi:foreign-funcall "setlocale" :int c-lc-ctype :string s :pointer))
  (let ((old-termios (cffi:foreign-alloc '(:struct c-termios))))
    (when (minusp (tcgetattr fd old-termios))
      (error-syscall-error "tcgetattr failed"))
    (cffi:with-foreign-object (new-termios '(:struct c-termios))
      (setf (cffi:mem-ref new-termios '(:struct c-termios))
            (cffi:mem-ref old-termios '(:struct c-termios)))
      (cffi:with-foreign-slots ((c-iflag c-oflag c-lflag c-cflag)
                                new-termios (:struct c-termios))
        (setf c-iflag (logandc2 c-iflag (logior c-iexten ; NO system-specific extensions
                                                c-igncr ; don't ignore return
                                                c-inlcr ; don't convert newline->CR
                                                c-ixon c-ixoff ; handle Ctrl-q/s ourselves
                                                ;; don't break unicode
                                                c-inpck ; nobody does parity checking
                                                c-istrip))) ; don't strip 8th bit
        (setf c-iflag (logior c-iflag c-icrnl)) ; convert CR->newline XXX a hack for vico
        (setf c-oflag (logandc2 c-oflag c-opost)) ; NO system-specific processing
        (setf c-lflag (logandc2 c-lflag (logior c-icanon ; no buffering
                                                c-isig ; we'll handle keys ourselves
                                                c-echo))) ; no input echoing
        (setf c-cflag (logandc2 c-lflag c-parenb)) ; no parity checking
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
                             (list* :click
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
                                         (0 :click) (1 :click) (2 :click)
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

(defun mouse-event-p (event)
  (and (listp event)
       (member (first event) '(:click :release :drag :hover
                               :scroll-up :scroll-down :scroll-left :scroll-right))))

(defun read-event-timeout (&optional timeout (stream *terminal-io*))
  #+sbcl (if timeout
             (handler-case
                 (sb-sys:with-deadline (:seconds timeout)
                   (read-event stream))
               (sb-sys:deadline-timeout ()))
             (read-event stream))
  #+ccl (if timeout
            (handler-case
                (ccl:with-input-timeout ((s stream) timeout)
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

;;; sigwinch

(defvar *got-sigwinch* nil)

(defconstant +sigwinch+ c-sigwinch
  "Signal number of SIGWINCH.")

(let (#+ccl sigwinch-thread)
  (defun catch-sigwinch ()
    "Enables handling SIGWINCH. May fail silently."
    #+ccl (setf sigwinch-thread
                (ccl:process-run-function "sigwinch thread"
                                          (lambda ()
                                            (loop
                                              (ccl:wait-for-signal 28 +sigwinch+)
                                              (setf *got-sigwinch* t)))))
    #+ecl (ext:set-signal-handler +sigwinch+ (lambda () (setf *got-sigwinch* t)))
    #+sbcl (sb-sys:enable-interrupt +sigwinch+
                                    (lambda (signo info ucontext)
                                      (declare (ignore signo info ucontext))
                                      (setf *got-sigwinch* t))))

  (defun reset-sigwinch ()
    #+ccl (ccl:process-kill sigwinch-thread)
    #+ecl (ext:set-signal-handler +sigwinch+ :default)
    #+sbcl (sb-sys:enable-interrupt +sigwinch+ :default)))

;;; misc

;; NOTE: scrolling is near useless because most terminals don't support changing
;; left/right margins. thus I will not include support as most uses are probably a mistake

(defun enable-alternate-screen ()
  (ti:tputs ti:enter-ca-mode))

(defun disable-alternate-screen ()
  (ti:tputs ti:exit-ca-mode))

(defun clear-screen ()
  (ti:tputs ti:clear-screen))

(defun clear-to-end-of-line ()
  (ti:tputs ti:clr-eol))

(defun clear-chars (&optional (n 1))
  (ti:tputs ti:erase-chars n))

(defun set-cursor-position (line column)
  "NEW-VALUE is a (LINE . COLUMN) pair"
  (ti:tputs ti:cursor-address line column))

(defun set-cursor-shape (style &key blink-p)
  (if (eq style :invisible)
      (ti:tputs ti:cursor-invisible)
      (let ((arg (case style
                   (:block 2)
                   (:underline 4)
                   (:bar 6))))
        (ti:tputs ti:cursor-visible)
        (format *terminal-io* "~c[~d q" #\esc (if blink-p (1- arg) arg)))))

;; styling

;; TODO split this out of vico and this library?
(defstruct (style (:conc-name nil))
  (fg nil :type (or null (integer #x000000 #xffffff)))
  (bg nil :type (or null (integer #x000000 #xffffff)))
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (underlinep nil :type boolean))

(defun red (color) (ldb (byte 8 16) color))
(defun green (color) (ldb (byte 8 8) color))
(defun blue (color) (ldb (byte 8 0) color))

(defun style-difference (a b)
  (let ((fga (fg a))
        (fgb (fg b))
        (bga (bg a))
        (bgb (bg b))
        differences)
    (unless (or (and (null fga) (null fgb))
                (and fga fgb
                     (= (red fga) (red fgb))
                     (= (green fga) (green fgb))
                     (= (blue fga) (blue fgb))))
      (setf (getf differences :fg) fgb))
    (unless (or (and (null bga) (null bgb))
                (and bga bgb
                     (= (red bga) (red bgb))
                     (= (green bga) (green bgb))
                     (= (blue bga) (blue bgb))))
      (setf (getf differences :bg) bgb))
    (unless (eq (boldp a) (boldp b))
      (setf (getf differences :bold) (boldp b)))
    (unless (eq (italicp a) (italicp b))
      (setf (getf differences :italic) (italicp b)))
    (unless (eq (underlinep a) (underlinep b))
      (setf (getf differences :underline) (underlinep b)))
    differences))

(defvar *default-style* (make-style))

(defun set-style-from-old (current-style new-style)
  (when-let ((diff (style-difference current-style new-style)))
    (flet ((attr-string (name attr)
             (case name
               (:fg (if attr
                        (format nil "38;2;~d;~d;~d;" (red attr) (green attr) (blue attr))
                        (format nil "39;")))
               (:bg (if attr
                        (format nil "48;2;~d;~d;~d;" (red attr) (green attr) (blue attr))
                        (format nil "49;")))
               (:bold (if attr "1;" "22;"))
               (:italic (if attr "3;" "23;"))
               (:underline (if attr "4;" "24;"))
               (otherwise ""))))
      (let ((s (with-output-to-string (s)
                 (format s "~c[" #\esc)
                 (loop :for (name attr) :on diff :by #'cddr
                       :do (write-string (attr-string name attr) s)))))
        (setf (aref s (1- (length s))) #\m) ; last #\;->#\m
        (write-string s *terminal-io*)))))

(defun set-style (style)
  (format *terminal-io* "~c[0m" #\esc) ; probably not the most portable
  (set-style-from-old *default-style* style))

(defun set-foreground (r g b)
  (format *terminal-io* "~c[38;2;~d;~d;~dm" #\esc r g b))

(defun set-background (r g b)
  (format *terminal-io* "~c[48;2;~d;~d;~dm" #\esc r g b))

;; layouting utilities

(defstruct (rectangle (:conc-name rect-))
  (x (error "rectangle X not provided") :type fixnum)
  (y (error "rectangle Y not provided") :type fixnum)
  (rows (error "rectangle ROWS not provided") :type fixnum)
  (cols (error "rectangle COLS not provided") :type fixnum))

;; '(:v
;;   window 0.3
;;   (:h split1 0.5 split2)
;;   another-window)

;; (defun layout (spec)
;;   (labels ((recur (spec )
;;              ()))))
