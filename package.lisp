;;;; package.lisp

(defpackage :uncursed-sys
  (:use :cl :alexandria)
  (:export #:uncursed-error
           #:syscall-error
           #:window-bounds-error

           #:make-adjustable-string
           #:append-to-adjustable

           #:color
           #:make-color #:copy-color
           #:red #:green #:blue

           #:style
           #:make-style #:copy-style
           #:fg #:bg #:boldp #:italicp #:underlinep
           #:style-difference
           #:*default-style*

           ;; util.lisp
           #:character-width
           #:display-width
           #:setup-terminal
           #:restore-terminal
           #:terminal-dimensions
           #:enable-mouse #:disable-mouse
           #:enable-alternate-screen #:disable-alternate-screen
           #:clear-screen #:clear-to-end-of-line #:clear-chars
           #:cursor-position
           #:set-mouse-shape
           #:set-foreground #:set-background
           #:set-style #:set-style-from-old
           #:read-event
           #:read-event-timeout
           #:mouse-event-p

           #:catch-sigwinch
           #:reset-sigwinch

           #:tui-base
           ;; slots
           #:lines #:columns
           #:windows
           #:focused-window
           #:event-handler
           ;; methods
           #:start
           #:stop
           #:handle-winch
           #:handle-event
           #:redisplay

           #:window
           ;; slots
           #:win-y
           #:win-x
           #:win-lines
           #:win-cols
           #:win-focused-p
           ;; methods
           #:present

           #:curved-box-border
           ))

(defpackage :uncursed
  (:use :cl :uncursed-sys)
  (:export #:tui
           #:handle-mouse-event
           #:handle-key-event
           #:standard-window
           #:put
           ))

(eval-when (:compile-toplevel :load-toplevel)
  (do-external-symbols (s :uncursed-sys)
    (export s :uncursed)))
