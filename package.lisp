;;;; package.lisp

(defpackage :uncursed-sys
  (:use :cl :alexandria)
  (:export #:uncursed-error
           #:syscall-error

           #:red #:green #:blue

           #:style
           #:make-style #:copy-style
           #:fg #:bg #:boldp #:italicp #:reversep #:underlinep
           #:style-difference
           #:*default-style*

           #:rect
           #:make-rect #:copy-rect
           #:rect-x #:rect-y #:rect-rows #:rect-cols

           ;; util.lisp
           #:character-width #:*character-widths*
           #:display-width
           #:setup-terminal
           #:restore-terminal
           #:terminal-dimensions
           #:enable-mouse #:disable-mouse
           #:enable-focus-tracking #:disable-focus-tracking
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
           ))

(defpackage :uncursed
  (:use :cl :uncursed-sys)
  (:local-nicknames (:sys :uncursed-sys))
  (:export #:tui-base
           ;; methods
           #:rows #:cols
           #:windows
           #:got-winch
           #:run
           #:stop
           #:handle-event
           #:redisplay

           #:window
           ;; methods
           #:dimensions
           #:focused-p
           #:present

           #:cell
           ;; methods
           #:cell-style
           #:cell-string

           #:make-timer
           ;; methods
           #:timer-callback
           #:timer-interval

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
