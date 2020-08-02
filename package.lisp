;;;; package.lisp

(defpackage :uncursed-sys
  (:use :cl :alexandria)
  (:export #:uncursed-error
           #:syscall-error

           #:red #:green #:blue

           #:style
           #:make-style #:copy-style
           #:fg #:bg #:boldp #:italicp #:underlinep
           #:style-difference
           #:*default-style*

           #:rectangle
           #:make-rectangle #:copy-rectangle
           #:rect-x #:rect-y #:rect-rows #:rect-cols

           ;; util.lisp
           #:character-width
           #:display-width
           #:setup-terminal
           #:restore-terminal
           #:terminal-dimensions
           #:enable-mouse #:disable-mouse
           #:enable-alternate-screen #:disable-alternate-screen
           #:clear-screen #:clear-to-end-of-line #:clear-chars
           #:set-cursor-position
           #:set-cursor-shape
           #:set-foreground #:set-background
           #:set-style #:set-style-from-old
           #:read-event
           #:read-event-timeout
           #:mouse-event-p

           #:catch-sigwinch
           #:reset-sigwinch

           #:tui-base
           ;; methods
           #:lines #:columns
           #:windows
           #:got-winch
           #:run
           #:stop
           #:handle-event
           #:redisplay

           #:window
           ;; methods
           #:dimensions
           #:win-focused-p
           #:present

           ))

(defpackage :uncursed
  (:use :cl :uncursed-sys)
  (:export #:cell
           ;; methods
           #:cell-style
           #:cell-string

           #:make-timer

           #:tui
           ;; methods
           #:handle-resize
           #:focused-window
           #:event-handler
           #:schedule-timer
           #:unschedule-timer

           #:standard-window
           #:window-bounds-error
           ;; methods
           #:handle-mouse-event
           #:handle-key-event

           #:put
           #:puts
           #:put-style
           ;; errors
           #:window-bounds-error
           #:window-bounds-error-coordinate
           #:window-bounds-error-bounds
           #:window-bounds-error-window
           #:wide-char-overwrite-error
           ;; restarts
           #:overwrite-char
           #:ignore-put

           #:curved-box-border
           ))

(eval-when (:compile-toplevel :load-toplevel)
  (do-external-symbols (s :uncursed-sys)
    (export s :uncursed)))
