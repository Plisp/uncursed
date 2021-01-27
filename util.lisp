;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; internals
;;

(in-package :uncursed-sys)

(defparameter *character-widths*
  (apply #'make-hash-table
         #+(or sbcl ecl) (list :synchronized t)
         #+ccl nil))

(eval-when (:execute)
  ;; add exceptions here
  ()
  ;; only alacritty works correctly, so triplewidth support doesn't seem necessary
  ;;(setf (gethash (char-code #\⸻) *character-widths*) 3)
  )

(defun character-width (character)
  "Returns the displayed width of CHARACTER"
  (declare (optimize speed))
  (let* ((codepoint (char-code character))
         (result (gethash codepoint *character-widths*)))
    (or result
        (setf (gethash codepoint *character-widths*)
              (if (not (zerop codepoint)) ; #\null
                  (cffi:foreign-funcall "wcwidth" c-wchar codepoint :int)
                  -1)))))

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

;;; input setup

;; utilities

(defun enable-mouse (&key hover)
  (if hover
      (format *terminal-io* "~c[?1006h~c[?1003h" #\esc #\esc)
      (format *terminal-io* "~c[?1006h~c[?1002h" #\esc #\esc)))

(defun disable-mouse ()
  (format *terminal-io* "~c[?1006l~c[?1002l~c[?1003l" #\esc #\esc #\esc))

(defun enable-focus-tracking ()
  (format *terminal-io* "~c[?1004h" #\esc))

(defun disable-focus-tracking ()
  (format *terminal-io* "~c[?1004l" #\esc))

;; setup

(cffi:defcfun "tcgetattr" :int
  (fd :int)
  (termios-p (:pointer (:struct c-termios))))

(cffi:defcfun "tcsetattr" :int
  (fd :int)
  (optional-actions :int)
  (termios-p (:pointer (:struct c-termios))))

(defun setup-terminal (fd)
  "Disables terminal echoing and buffering. Returns a pointer to the original termios.
Sets process locale from environment."
  (cl-setlocale:set-all-to-native)
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
        (when (minusp (tcsetattr fd c-set-attributes-drain new-termios))
          (error-syscall-error "tcsetattr failed"))
        old-termios))))

(defun restore-terminal (old-termios fd)
  "Restores the terminal device backing FD to its original state. ORIG-TERMIOS is a pointer
to the original termios struct returned by a call to SETUP-TERM which is freed."
  (when (minusp (tcsetattr fd c-set-attributes-flush old-termios))
    (error-syscall-error "tcsetattr failed"))
  (cffi:foreign-free old-termios)
  (values))

;;; input parsing TODO could be more robust to csi garbage

(defun read-integer ()
  (let ((digit-list (loop :with acc
                          :for char = (read-char)
                          :while (digit-char-p char)
                          :do (push char acc)
                          :finally (unread-char char) ; unread terminator
                                   (return (nreverse acc)))))
    (parse-integer (coerce digit-list 'string) :junk-allowed t)))

(defun modify-key (key mod)
  `(,key ,@(when (plusp (logand (1- mod) #b1000)) (list :meta))
         ,@(when (plusp (logand (1- mod) #b0100)) (list :control))
         ,@(when (plusp (logand (1- mod) #b0010)) (list :alt))
         ,@(when (plusp (logand (1- mod) #b0001)) (list :shift))))

;; the function name reflects the horror
(defun read-modified-special-keys-and-f1-f4 (code)
  "CSI 1 ; [mods] [terminator]"
  (let* ((mod (read-integer))
         (terminator (read-char))
         (key (case terminator ; SPECIAL : keep this in sync with below
                (#\P :f1) (#\Q :f2) (#\R :f3) (#\S :f4)
                (#\A :up-arrow) (#\B :down-arrow) (#\C :right-arrow) (#\D :left-arrow)
                (#\H :home)
                (#\F :end))))
    (if (and key mod (<= mod 16))
        (modify-key key mod)
        (concatenate 'list
                      '(:unknown :csi) (princ-to-string code) ";" (princ-to-string mod)
                       (string terminator)))))

(defun code->fkey (code)
  (case code
    (2 :insert)
    (3 :delete)
    (4 :end) ;st?
    (5 :page-up)
    (6 :page-down)
    (15 :f5)
    (17 :f6) (18 :f7) (19 :f8) (20 :f9) (21 :f10)
    (23 :f11) (24 :f12) (25 :f13) (26 :f14)
    (28 :f15) (29 :f16)
    (31 :f17) (32 :f18) (33 :f19) (34 :f20)))

(defun read-modified-function-keys (code)
  "CSI [code] ; [mods] ~"
  (let ((fn (code->fkey code))
        (mod (read-integer))
        (terminator (read-char)))
    (tagbody
       (if (and fn mod (<= mod 16))
           (if (char= terminator #\~)
               (return-from read-modified-function-keys (modify-key fn mod))
               (go fail))
           (go fail))
     fail
       (return-from read-modified-function-keys
         (concatenate 'list
                       '(:unknown :csi) (princ-to-string code) ";" (princ-to-string mod)
                        (string terminator))))))

(defun read-function-and-special-keys ()
  "CSI [code] ~/h (or modified, see above)"
  (let ((code (read-integer))
        (terminator (read-char)))
    (tagbody
       (return-from read-function-and-special-keys
         (case terminator
           (#\~
            (or (code->fkey code) (go fail)))
           (#\h
            (if (= code 4)
                :insert ; TODO why is st doing this
                (go fail)))
           (#\; ; modified key
            (if (= code 1)
                (read-modified-special-keys-and-f1-f4 code)
                (read-modified-function-keys code)))
           (otherwise
            (go fail))))
     fail
       (return-from read-function-and-special-keys
         (concatenate 'list
                       '(:unknown :csi) (princ-to-string code) (string terminator))))))

(defun read-mouse-sgr ()
  "CSI < [code&mods] ; COL ; ROW ; M/m"
  (let* ((code (read-integer))
         (semicolon1 (read-char))
         (col (read-integer))
         (semicolon2 (read-char))
         (row (read-integer))
         (%state (read-char))
         (state (cond ((plusp (logand code 32)) :drag)
                      ((char= %state #\M) :click)
                      ((char= %state #\m) :release)))
         (mods `(,@(when (plusp (logand code 16)) (list :control))
                 ,@(when (plusp (logand code 8)) (list :alt))
                 ,@(when (plusp (logand code 4)) (list :shift))))
         (type (case (+ (ldb (byte 2 0) code)
                        (ash (ldb (byte 2 6) code) 2))
                 (#b0000 :left)
                 (#b0001 :middle)
                 (#b0010 :right)
                 (#b0011 :hover)
                 (#b0100 :wheel-up)
                 (#b0101 :wheel-down)
                 (#b0110 :wheel-left)
                 (#b0111 :wheel-right))))
    (if (and (char= semicolon1 #\;) (char= semicolon2 #\;)
             (or (char= %state #\m) (char= %state #\M))
             code col row)
        (append (list type state row col) mods)
        (concatenate 'list
                      '(:unknown :csi #\<)
                       (princ-to-string code) (string semicolon1)
                       (princ-to-string col) (string semicolon2)
                       (princ-to-string row) (string state)))))

(defun read-event ()
  "Returns a value of one of the following forms:
* CHARACTER - singular character
* (CHARACTER [modifiers]...) - modifiers include :shift, :alt, :control and :meta
* :f1-20, :home, :end, :insert, :delete, :up/:down/:left/:right-arrow, :page-down, :page-up
* (:function/special [modifiers]...) - above but with modifiers
* (:left/middle/right/wheel-up/down/left/right/hover :click/release/drag ROW COL [mods]...)
* (:unknown [key-sequence]...)
Notably (:unknown :csi #\I/O) may be xterm focus in/out events."
  (flet ((adjusted-c0-p (code)
           (and (< code 32)
                (/= code (char-code #\esc))
                (/= code (char-code #\newline)))))
    (let ((first (read-char)))
      (or (and (listen) (char= #\esc first))
          (return-from read-event
            ;; C0 characters except esc are given the :control modifier
            (let ((code (char-code first)))
              (if (adjusted-c0-p code)
                  (list (code-char (+ code 96)) :control)
                  first)))))
    ;; we have #\esc
    (let ((second (read-char)))
      (or (and (listen) (or (char= #\[ second) ; CSI
                            (char= #\O second))) ; SS3
          (return-from read-event
            (let ((code (char-code second)))
              (if (adjusted-c0-p code)
                  (list (code-char (+ code 96)) :control :alt)
                  (list second :alt)))))
      ;; we have SS3 or CSI
      (let ((third (read-char)))
        (ecase second ; TODO use terminfo if needed. currently seems unreliable
          (#\O ; VT220 convention: function keys begin with SS3 on xterms. but why.
           (case third
             (#\P :f1) (#\Q :f2) (#\R :f3) (#\S :f4)
             (otherwise (list :unknown :ss3 third))))
          (#\[ ; parse csi garbage. no rxvt support for now.
           (case third
             (#\A :up-arrow) (#\B :down-arrow) (#\C :right-arrow) (#\D :left-arrow)
             (#\H :home)
             (#\F :end) ; xterm. not so on st
             ;; SPECIAL below are not modified
             (#\P :delete) ; st TODO check their terminfo, kdch1 meant to be \e[3~
             (#\Z (list #\tab :shift))
             (#\< (read-mouse-sgr))
             (otherwise
              (if (digit-char-p third)
                  (progn
                    (unread-char third)
                    (read-function-and-special-keys))
                  (list :unknown :csi third))))))))))

(defun mouse-event-p (event)
  (and (listp event)
       (>= (length event) 4)
       (member (first event) '(:left :middle :right :hover
                               :wheel-up :wheel-down :wheel-left :wheel-right))
       (member (second event) '(:click :release :drag nil))
       (positive-integer-p (third event))
       (positive-integer-p (fourth event))
       (every (lambda (elt) (member elt '(:shift :alt :control :meta)))
              (nthcdr 4 event))))

;;; select TODO

(cffi:defcfun ("close" c-close) :int
  (fd :int))

(cffi:defcfun ("write" c-write) :long
  (fd :int)
  (buf :pointer)
  (n :long))

(cffi:defcfun ("read" c-read) :long
  (fd :int)
  (buf :pointer)
  (n :long))

(cffi:defcfun "pipe" :int
  (pipefd (:pointer :int)))

(defun read-fd (pipe)
  (cffi:foreign-aref pipe '(:array :int 2) 0))

(defun write-fd (pipe)
  (cffi:foreign-aref pipe '(:array :int 2) 1))

(defun set-nonblock (fd)
  (cffi:foreign-funcall "fcntl" :int fd :int c-fsetfl :int c-ononblock :int))

(defun non-blocking-pipe (pipe)
  (when (minusp (pipe pipe))
    (error-syscall-error "pipe failed"))
  (when (minusp (set-nonblock (read-fd pipe)))
    (error-syscall-error "fcntl failed"))
  (when (minusp (set-nonblock (write-fd pipe)))
    (error-syscall-error "fcntl failed")))

(defun pipe-cleanup (pipe)
  (c-close (read-fd pipe))
  (c-close (write-fd pipe)))

(cffi:defcfun "select" :int
  (nfds :int)
  (readfds (:pointer (:struct c-fd-set)))
  (writefds (:pointer (:struct c-fd-set)))
  (exceptfds (:pointer (:struct c-fd-set)))
  (timeout (:pointer (:struct c-timeval))))

(defun fd-setp (fd set)
  (plusp (fd-isset fd set)))

(defun read-event-timeout (&optional timeout)
  "Wait up to timeout seconds waiting for input, returning NIL on timeout or an event."
  #+sbcl (if timeout
             (handler-case
                 (sb-sys:with-deadline (:seconds timeout)
                   (read-event))
               (sb-sys:deadline-timeout ()))
             (read-event))
  #+ccl (if timeout
            (handler-case
                (ccl:with-input-timeout ((s *terminal-io*) timeout)
                  s
                  (read-event))
              (ccl:input-timeout ())))
  ;; TODO best way to do this on ecl?
  ;; #+ecl (let ((serve-event::*descriptor-handlers*
  ;;               (copy-list serve-event::*descriptor-handlers*))
  ;;             (event))
  ;;         (serve-event:add-fd-handler (si:file-stream-fd stream) :input
  ;;                                     #'(lambda (fd)
  ;;                                         (setf event (read-event))))
  ;;         (serve-event:serve-event timeout))
  #-(or sbcl ccl)
  (if timeout
      (error "timeout only supported on sbcl, ccl")
      (read-event)))

;;; sigwinch

(defconstant +sigwinch+ c-sigwinch
  "Signal number of SIGWINCH.")

#+ccl (defvar *sigwinch-thread* nil)
(defun catch-sigwinch (write-pipe)
  "Enables handling SIGWINCH. May fail silently."
  (cffi:with-foreign-object (buf :char)
    #+ccl (setf *sigwinch-thread*
                (ccl:process-run-function "sigwinch thread"
                                          (lambda ()
                                            (loop
                                              (ccl:wait-for-signal 28 +sigwinch+)
                                              (c-write write-pipe buf 1)))))
    #+ecl (ext:set-signal-handler +sigwinch+ (lambda () (setf *got-sigwinch* t)))
    #+sbcl (sb-sys:enable-interrupt +sigwinch+
                                    (lambda (signo info ucontext)
                                      (declare (ignore signo info ucontext))
                                      (c-write write-pipe buf 1)))))

(defun reset-sigwinch ()
  #+ccl (progn (ccl:process-kill *sigwinch-thread*)
               (setf *sigwinch-thread* nil))
  #+ecl (ext:set-signal-handler +sigwinch+ :default)
  #+sbcl (sb-sys:enable-interrupt +sigwinch+ :default))

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

;;; attributes

(defstruct (style (:conc-name nil))
  (fg nil :type (or null (integer #x000000 #xffffff)))
  (bg nil :type (or null (integer #x000000 #xffffff)))
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (reversep nil :type boolean)
  (underlinep nil :type boolean))

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
    (unless (eq (reversep a) (reversep b))
      (setf (getf differences :reverse) (reversep b)))
    (unless (eq (underlinep a) (underlinep b))
      (setf (getf differences :underline) (underlinep b)))
    differences))

(defvar *default-style* (make-style))

(defun initialize-color-magic (index attr)
  (ti:tputs ti:initialize-color index
            (color-magic (red attr))
            (color-magic (green attr))
            (color-magic (blue attr))))

;; if you don't follow ecma-48, too bad
(defun set-style-from-old (current-style new-style &optional use-palette)
  (when-let ((diff (style-difference current-style new-style)))
    (flet ((attr-string (name attr)
             (case name
               (:fg
                (if (not attr)
                    (format nil "39;")
                    (ecase use-palette
                      ((nil)
                       (format nil "38;2;~d;~d;~d;" (red attr) (green attr) (blue attr)))
                      ((t)
                       (if-let (index (lookup-color attr))
                         (format nil "38;5;~d;" index)
                         (let ((index (next-free-color)))
                           (initialize-color-magic index attr)
                           (setf (aref *palette* index) attr)
                           (format nil "38;5;~d;" index))))
                      (:approximate
                       (format nil "38;5;~d;" (approximate-rgb attr))))))
               (:bg
                (if (not attr)
                    (format nil "49;")
                    (ecase use-palette
                      ((nil)
                       (format nil "48;2;~d;~d;~d;" (red attr) (green attr) (blue attr)))
                      ((t)
                       (if-let (index (lookup-color attr))
                         (format nil "48;5;~d;" index)
                         (let ((index (next-free-color)))
                           (initialize-color-magic index attr)
                           (setf (aref *palette* index) attr)
                           (format nil "48;5;~d;" index))))
                      (:approximate
                       (format nil "48;5;~d;" (approximate-rgb attr))))))
               (:bold (if attr "1;" "22;"))
               (:italic (if attr "3;" "23;"))
               (:reverse (if attr "7;" "27;"))
               (:underline (if attr "4;" "24;"))
               (otherwise ""))))
      (let ((s (with-output-to-string (s)
                 (format s "~c[" #\esc)
                 (loop :for (name attr) :on diff :by #'cddr
                       :do (write-string (attr-string name attr) s)))))
        (setf (aref s (1- (length s))) #\m) ; last #\;->#\m
        (write-string s *terminal-io*)))))

(defun set-style (style &optional use-palette)
  (ti:tputs ti:exit-attribute-mode)
  (set-style-from-old *default-style* style use-palette))

(defun set-foreground (r g b)
  (format *terminal-io* "~c[38;2;~d;~d;~dm" #\esc r g b))

(defun set-background (r g b)
  (format *terminal-io* "~c[48;2;~d;~d;~dm" #\esc r g b))
