;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; internals
;;

(in-package #:uncursed-sys)

(defparameter *character-widths*
  (apply #'make-hash-table
         #+(or sbcl ecl) (list :synchronized t)
         #+ccl nil))

(defun %wcwidth (character)
  "0 for non-printing and combining characters, 2 for wide and fullwidth categories.
1 for ambiguous, TODO treat these properly along with Arabic scripts."
  (declare (optimize speed))
  (cond ;; this seems close enough for now
    (#+sbcl (member (sb-unicode:general-category character) '(:me :mn :mc :cf :cc :zl :zp))
     #+(and (not sbcl) unix) (<= (cffi:foreign-funcall "wcwidth" :int (char-code character) :int) 0)
     0)
    ;; zero width characters already excluded
    (#+sbcl (member (sb-unicode:east-asian-width character) '(:W :F))
     #+(and (not sbcl) unix) (= (cffi:foreign-funcall "wcwidth" :int (char-code character) :int) 2)
     2)
    (t 1)))

(defun character-width (character)
  "Returns the displayed width of `character'. See %codepoint-width.
It is recommended to bind *character-widths* dynamically in calling threads."
  (declare (optimize speed))
  (or (gethash character *character-widths*)
      (setf (gethash character *character-widths*) (%wcwidth character))))

 ;; add exceptions here
(eval-when (:load-toplevel :execute)
  ;; only alacritty and windows have this behavior, where it looks terrible without space
  (setf (gethash #\⸻ *character-widths*) 3)
  )

(defun display-width (string)
  (declare (optimize speed))
  (reduce #'+ string :key 'character-width))

;;; terminal dimensions

(define-condition uncursed-error (simple-error)
  ())

(define-condition syscall-error (uncursed-error)
  ())

(defun error-syscall-error (control &rest args)
  (error 'syscall-error
         :format-control #-sbcl control
                         #+sbcl (format nil "~a: ~a" control (sb-int:strerror))
         :format-arguments args))

(defparameter *fallback-terminal-dimensions* (cons 24 80))

#+unix
(defun terminal-dimensions ()
  "Returns a cons (lines . columns) containing the dimensions of the terminal device.
Returns NIL on failure."
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

#+windows
(progn
  (cffi:defctype Handle :pointer)
  (defconstant +std-output-handle+ -11) ; stable

  (cffi:defcfun "GetStdHandle" Handle
    (n :int32)) ; DWORD

  (cffi:defcstruct coord
    (x :int16) ; SHORT
    (y :int16))

  (cffi:defcstruct small-rect
    (left :int16)
    (top :int16)
    (right :int16)
    (bottom :int16))

  (cffi:defcstruct console-screen-buffer-info
    (size (:struct coord))
    (cursor-position (:struct coord))
    (wattributes :uint16)
    (window (:struct small-rect))
    (max-window-size (:struct coord)))

  (cffi:defcfun "GetConsoleScreenBufferInfo" :boolean
    (console-output-handle Handle)
    (console-screen-buffer-info-ptr (:pointer (:struct console-screen-buffer-info))))

  (defun terminal-dimensions ()
    (let ((Handle (GetStdHandle +std-output-handle+)))
      (when (cffi:null-pointer-p (cffi:inc-pointer Handle 1)) ; (void*)-1
        (return-from terminal-dimensions *fallback-terminal-dimensions*))

      (cffi:with-foreign-object (info '(:struct console-screen-buffer-info))
        (if (GetConsoleScreenBufferInfo Handle info) ; boolean
            (let ((window (cffi:foreign-slot-value info
                                                   '(:struct console-screen-buffer-info)
                                                   'window)))
              (cons (1+ (- (getf window 'bottom) (getf window 'top)))
                    (1+ (- (getf window 'right) (getf window 'left)))))
            *fallback-terminal-dimensions*)))))

;;; raw input setup

#+unix
(progn
  (cffi:defcfun "tcgetattr" :int
    (fd :int)
    (termios-p (:pointer (:struct c-termios))))

  (cffi:defcfun "tcsetattr" :int
    (fd :int)
    (optional-actions :int)
    (termios-p (:pointer (:struct c-termios))))

  (defconstant +stdin+ 0)
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
                                                  c-ixon c-ixoff ; Handle Ctrl-q/s ourselves
                                                  ;; don't break unicode
                                                  c-inpck ; nobody does parity checking
                                                  c-istrip))) ; don't strip 8th bit
          (setf c-iflag (logior c-iflag c-icrnl)) ; convert CR->newline XXX a hack for vico
          (setf c-oflag (logandc2 c-oflag c-opost)) ; NO system-specific processing
          (setf c-lflag (logandc2 c-lflag (logior c-icanon ; no buffering
                                                  c-isig ; we'll handle keys ourselves
                                                  c-echo))) ; no input echoing
          (setf c-cflag (logandc2 c-lflag c-parenb)) ; no parity checking
          (when (minusp (tcsetattr fd c-set-attributes-flush new-termios))
            (error-syscall-error "tcsetattr failed"))
          old-termios))))

  (defun restore-terminal (old-termios fd)
    "Restores the terminal device backing `fd' to its original state. `orig-termios' is a
pointer to the original termios struct returned by a call to `setup-term', to be freed."
    (when (minusp (tcsetattr fd c-set-attributes-flush old-termios))
      (error-syscall-error "tcsetattr failed"))
    (cffi:foreign-free old-termios)
    (values)))

#+windows
(progn
  (defconstant +std-input-handle+ -10)

  (cffi:defcfun "GetConsoleMode" :boolean
    (console-handle Handle)
    (mode-ptr (:pointer :uint32)))

  (cffi:defcfun "SetConsoleMode" :boolean
    (console-handle Handle)
    (mode :uint32))

  (defconstant +enable-processed-input+ #x0001)
  (defconstant +enable-line-input+ #x0002)
  (defconstant +enable-echo-input+ #x0004)
  (defconstant +enable-virtual-terminal-input+ #x0200)

  (defconstant +enable-processed-output+ #x0001)
  (defconstant +enable-virtual-terminal-processing+ #x0004)

  (defun setup-terminal ()
    "Disables terminal echoing and buffering. Enables VTE sequences and resize events.
 Only works on standard io."
    (cl-setlocale:set-all-to-native)
    (let ((input-handle (GetStdHandle +std-input-handle+))
          (output-handle (GetStdHandle +std-output-handle+)))
      (when (or (cffi:null-pointer-p (cffi:inc-pointer input-handle 1))
                (cffi:null-pointer-p (cffi:inc-pointer output-handle 1))) ; (void*)-1
        (error-syscall-error "GetStdHandle failed"))
      ;; allocate an int ptr
      (cffi:with-foreign-object (mode-ptr :uint32)
        (let (old-input-mode old-output-mode)
          (or (GetConsoleMode input-handle mode-ptr)
              (error-syscall-error "GetConsoleMode failed"))
          (setf old-input-mode (cffi:mem-ref mode-ptr :uint32))

          (or (GetConsoleMode output-handle mode-ptr)
              (error-syscall-error "GetConsoleMode failed"))
          (setf old-output-mode (cffi:mem-ref mode-ptr :uint32))

          ;; if this fails, we don't expect to be able to restore anyways
          (or (SetConsoleMode
               input-handle (logior ; turn on window input and vte seqs
                             (logand old-input-mode ; raw mode
                                     (lognot (logior +enable-line-input+
                                                     +enable-echo-input+
                                                     +enable-processed-input+)))
                             +enable-virtual-terminal-input+))
              (error-syscall-error "SetConsoleMode failed"))

          (or (SetConsoleMode output-handle (logior old-output-mode
                                                    +enable-processed-output+
                                                    +enable-virtual-terminal-processing+))
              (error-syscall-error "SetConsoleMode failed"))

          (list input-handle old-input-mode output-handle old-output-mode)))))

  (defun restore-terminal (stored)
    "Attempts to restore standard input to its original state."
    (destructuring-bind (input-handle input-mode output-handle output-mode)
        stored
      (or (SetConsoleMode input-handle input-mode)
          (error-syscall-error "SetConsoleMode input restore failed"))
      (or (SetConsoleMode output-handle output-mode)
          (error-syscall-error "SetConsoleMode output restore failed")))
    (values)))

;;; input TODO parsing could be more robust to malformed csi and slow networks

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

;; parsing

(defstruct mouse-data
  button
  state
  row
  col)

(defstruct (event (:type list))
  kind
  (shiftp nil)
  (altp nil)
  (controlp nil)
  (metap nil))

(defun read-integer ()
  (let ((digit-list (loop :with acc
                          :for char = (read-char)
                          :while (digit-char-p char)
                          :do (push char acc)
                          :finally (unread-char char) ; unread terminator
                                   (return (nreverse acc)))))
    (parse-integer (coerce digit-list 'string) :junk-allowed t)))

(defun modified-key-event (key mod)
  (make-event :kind key
              :shiftp (plusp (logand (1- mod) #b0001))
              :altp (plusp (logand (1- mod) #b0010))
              :controlp (plusp (logand (1- mod) #b0100))
              :metap (plusp (logand (1- mod) #b1000))))

;; application mode, mostly unused
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
        (modified-key-event key mod)
        (make-event :kind (list :csi (princ-to-string code) #\; (princ-to-string mod)
                                (string terminator))))))

;; Just forget modified keys on st, I'm only supporting the normal scheme
(defun code->fkey (code)
  (case code
    (1 :home) ; st VT220 application mode
    (2 :insert)
    (3 :delete)
    (4 :end) ; st VT220 moment
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
               (return-from read-modified-function-keys (modified-key-event fn mod))
               (go fail))
           (go fail))
     fail
       (return-from read-modified-function-keys
         (make-event :kind (list :csi (princ-to-string code) #\; (princ-to-string mod)
                                 (string terminator)))))))

(defun read-function-and-special-keys ()
  "CSI [code] ~/h (or modified, see above)"
  (let ((code (read-integer))
        (terminator (read-char)))
    (tagbody
       (return-from read-function-and-special-keys
         (case terminator
           (#\~
            (if-let (code (code->fkey code))
              (make-event :kind code)
              (go fail)))
           (#\h
            (if (= code 4)
                (make-event :kind :insert) ; st uses DEC convention
                (go fail)))
           (#\; ; modified key
            (if (= code 1)
                (read-modified-special-keys-and-f1-f4 code)
                (read-modified-function-keys code)))
           (otherwise
            (go fail))))
     fail
       (return-from read-function-and-special-keys
         (make-event :kind (list :csi (princ-to-string code) (string terminator)))))))

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
        (make-event :kind (make-mouse-data :button type :state state :row row :col col)
                    :shiftp (plusp (logand code 4))
                    :altp (plusp (logand code 8))
                    :controlp (plusp (logand code 16)))
        (make-event :kind (list :csi #\<
                                (princ-to-string code) #\;
                                (princ-to-string col) #\;
                                (princ-to-string row) (string state))))))

(defun read-event ()
  "Returns an event structure, where the event-kind slot is one of
* a character
* a keyword designating some special key
* a mouse-event structure
* a list of codes, notably (:csi #\I/O ... ) may be xterm focus in/out events"
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
                  (make-event :kind (code-char (+ code 96)) :controlp t)
                  (make-event :kind first))))))
    ;; we have #\esc
    (let ((second (read-char)))
      (or (and (listen) (or (char= #\[ second) ; CSI
                            (char= #\O second))) ; SS3
          (return-from read-event
            (let ((code (char-code second)))
              (if (adjusted-c0-p code)
                  (make-event :kind (code-char (+ code 96)) :controlp t :altp t)
                  (make-event :kind second :altp t)))))
      ;; we have SS3 or CSI
      (let ((third (read-char)))
        (ecase second
          (#\O ; VT220 convention: function keys begin with SS3 on xterms. but why.
           (if-let (special
                    (case third
                      (#\P :f1) (#\Q :f2) (#\R :f3) (#\S :f4) (#\H :home) (#\F :end)
                      (#\A :up-arrow) (#\B :down-arrow)
                      (#\C :right-arrow) (#\D :left-arrow)))
             (make-event :kind special)
             (make-event :kind (list :ss3 third))))
          (#\[ ; parse csi garbage. no rxvt support for now.
           (when-let (special
                      (case third
                        (#\A :up-arrow) (#\B :down-arrow) (#\C :right-arrow) (#\D :left-arrow)
                        (#\H :home)
                        (#\F :end)
                        (#\P :delete) ; legacy, used by st
                        ))
             (return-from read-event (make-event :kind special)))
           (cond ((char= third #\Z) (make-event :kind #\tab :shiftp t))
                 ((char= third #\<) (read-mouse-sgr))
                 ((digit-char-p third)
                  (unread-char third)
                  (read-function-and-special-keys))
                 (t
                  (make-event :kind (list :csi third))))))))))

(defun mouse-event-p (event)
  (mouse-data-p (event-kind event)))

;;; select

#+unix
(progn
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
    (plusp (fd-isset fd set))))

#+windows
(progn
  (cffi:defcfun "CreateEventW" Handle
    (security-attributes :pointer)
    (manual-reset :uint32) ; BOOL
    (initial-state :uint32)
    (name :pointer))

  (cffi:defcfun "SetEvent" :boolean
    (event Handle))

  (cffi:defcfun "CloseHandle" :boolean
    (object Handle))

  (defun create-event ()
    (CreateEventW (cffi:null-pointer) 0 0 (cffi:null-pointer)))

  (defun free-event (event)
    (CloseHandle event))

  (defconstant +wait-timeout+ #x00000102)
  (defconstant +wait-failed+ #xFFFFFFFF)
  (cffi:defcfun "WaitForMultipleObjects" :uint32
    (count :uint32)
    (handles (:pointer Handle))
    (wait-all :uint32) ; BOOL
    (timeout-ms :uint32)))

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
  #-(or sbcl ccl)
  (if timeout
      (error "timeout only supported on sbcl, ccl")
      (read-event)))

;;; sigwinch

#+unix
(progn
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
    #+sbcl (sb-sys:enable-interrupt +sigwinch+ :default)))

#+windows
(progn
  (cffi:defcfun "GetNumberOfConsoleInputEvents" :boolean
    (input-handle Handle)
    (num-events-ptr (:pointer :uint32)))

  (cffi:defcstruct mouse-event-record
    (mouse-position (:struct coord))
    (button-state :uint32)
    (ctrl-state :uint32)
    (event-flags :uint32))

  (cffi:defcunion uchar
    (codepoint :uint16) ; WCHAR
    (ascii :char))

  (cffi:defcstruct key-event-record
    (keydown :uint32) ; BOOL
    (repeat-count :uint16) ; WORD
    (virtual-keycode :uint16)
    (virtual-scancode :uint16)
    (char (:union uchar))
    (ctrl-state :uint32))

  ;; (cffi:defcstruct window-buffer-size-record
  ;;   (size (:struct coord)))
  (cffi:defcunion input-record-union
    (window-buffer-size-event (:struct coord))
    (key-event (:struct key-event-record))
    (mouse-event (:struct mouse-event-record)))

  (cffi:defcstruct input-record
    (event-type :unsigned-short)
    (event (:union input-record-union)))

  (cffi:defcfun "PeekConsoleInputW" :boolean
    (input-handle Handle)
    (buffer (:pointer (:struct input-record)))
    (n :unsigned-long)
    (num-read-ptr (:pointer :uint32)))

  (cffi:defcfun "FlushConsoleInputBuffer" :boolean
    (input-handle Handle))

  (defun win-events-left (Handle)
    "Returns a cons cell containing a boolean, true when resized and
an integer representing device events left in the queue.
This must be called to determine whether there are events left, as (listen)
is unreliable and lisp events read may not correspond perfectly to windows events."
    (cffi:with-foreign-object (count-ptr :uint32)
      (or (GetNumberOfConsoleInputEvents Handle count-ptr)
          (error-syscall-error "GetNumberOfConsoleInputEvents failed"))
      (let ((count (cffi:mem-ref count-ptr :uint32)))
        (cffi:with-foreign-object (record-buf '(:struct input-record) count)
          (or (PeekConsoleInputW Handle record-buf count count-ptr)
              (error-syscall-error "PeekConsoleInputW failed"))
          (loop :with read = (cffi:mem-ref count-ptr :uint32)
                :with resize = nil
                :with events = 0
                :for i below read
                :for record = (cffi:mem-aptr record-buf '(:struct input-record) i)
                :do (case (cffi:foreign-slot-value record
                                                   '(:struct input-record)
                                                   'event-type)
                      (#x0004 (setf resize t)) ; buffer size event
                      ((#x0001 #x0002) ; key or mouse events
                       (incf events)))
                :finally (return (cons resize events))))))))


;;; misc

;; NOTE: scrolling is near useless because most terminals don't support changing
;; left/right margins, so I will not include support as most uses are probably a mistake
;;
;; NOTE: windows terminal hardcodes xterm sequences, and doesn't have terminfo
;;       honestly following defacto xterm is fine

(defun enable-alternate-screen ()
  #+unix (ti:tputs ti:enter-ca-mode)
  #+windows (format *terminal-io* "~c[?1049h" #\esc))

(defun disable-alternate-screen ()
  #+unix (ti:tputs ti:exit-ca-mode)
  #+windows (format *terminal-io* "~c[?1049l" #\esc))

(defun clear-screen ()
  #+unix (ti:tputs ti:clear-screen)
  #+windows (format *terminal-io* "~c[2J" #\esc))

(defun clear-to-end-of-line ()
  #+unix (ti:tputs ti:clr-eol)
  #+windows (format *terminal-io* "~c[K" #\esc))

(defun clear-chars (&optional (n 1))
  #+unix (ti:tputs ti:erase-chars n)
  #+windows (format *terminal-io* "~c[~dP" #\esc n))

(defun set-cursor-position (line column)
  "NEW-VALUE is a (LINE . COLUMN) pair, *zero* indexed unlike the higher
level cell grid."
  #+unix (ti:tputs ti:cursor-address line column)
  #+windows (format *terminal-io* "~c[~d;~dH" #\esc (1+ line) (1+ column)))

(defun set-cursor-shape (style &key blink-p)
  (if (eq style :invisible)
      #+unix (ti:tputs ti:cursor-invisible)
      #+windows (format *terminal-io* "~c[?25l" #\esc)
      (let ((arg (case style
                   (:block 2)
                   (:underline 4)
                   (:bar 6))))
        #+unix (ti:tputs ti:cursor-visible)
        #+windows (format *terminal-io* "~c[?25h" #\esc)
        (format *terminal-io* "~c[~d q" #\esc (if blink-p (1- arg) arg)))))

;;; attributes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (style (:conc-name nil)
                    (:copier nil))
    (fg nil :type (or null (integer #x000000 #xffffff)) :read-only t)
    (bg nil :type (or null (integer #x000000 #xffffff)) :read-only t)
    (boldp nil :type boolean :read-only t)
    (italicp nil :type boolean :read-only t)
    (reversep nil :type boolean :read-only t)
    (underlinep nil :type boolean :read-only t))

  (defmethod make-load-form ((o style) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots o))

  (defvar *default-style* (make-style)))

(defun copy-style (style &key fg bg boldp italicp reversep underlinep)
  (make-style :fg (or fg (fg style))
              :bg (or bg (bg style))
              :boldp (or boldp (boldp style))
              :italicp (or italicp (italicp style))
              :reversep (or reversep (reversep style))
              :underlinep (or underlinep (underlinep style))))

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

#+unix ; just don't do this on windows, use direct-color
(defun initialize-color-magic (index attr)
  (ti:tputs ti:initialize-color index
            (color-magic (red attr))
            (color-magic (green attr))
            (color-magic (blue attr))))

;; xterm is the standard
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

(define-constant +blank-style+ (make-style) :test 'equalp)
(defun set-style (style &optional use-palette)
  #+unix (ti:tputs ti:exit-attribute-mode)
  #+windows (format *terminal-io* "~c[m" #\esc)
  (set-style-from-old +blank-style+ style use-palette))

(defun set-foreground (r g b)
  (format *terminal-io* "~c[38;2;~d;~d;~dm" #\esc r g b))

(defun set-background (r g b)
  (format *terminal-io* "~c[48;2;~d;~d;~dm" #\esc r g b))
