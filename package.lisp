;;;; package.lisp

(defpackage :uncursed-sys
  (:use :cl :alexandria)
  (:export #:uncursed-error
           #:syscall-error
           #:error-syscall-error

           #:red #:green #:blue

           #:style
           #:make-style #:copy-style
           #:fg #:bg #:boldp #:italicp #:reversep #:underlinep
           #:style-difference
           #:*default-style*

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
  (:use :cl)
  (:import-from :uncursed-sys
                #:uncursed-error

                #:character-width #:*character-widths*
                #:display-width
                #:terminal-dimensions
                #:enable-mouse #:disable-mouse
                #:enable-focus-tracking #:disable-focus-tracking
                #:enable-alternate-screen #:disable-alternate-screen
                #:mouse-event-p
                #:set-cursor-shape

                #:red #:green #:blue
                #:style
                #:make-style #:copy-style
                #:fg #:bg #:boldp #:italicp #:reversep #:underlinep
                #:style-difference
                #:*default-style*)
  (:local-nicknames (:sys :uncursed-sys))
  (:export #:uncursed-error

           #:character-width #:*character-widths*
           #:display-width
           #:terminal-dimensions
           #:enable-mouse #:disable-mouse
           #:enable-focus-tracking #:disable-focus-tracking
           #:enable-alternate-screen #:disable-alternate-screen
           #:mouse-event-p
           #:set-cursor-shape

           #:red #:green #:blue
           #:style
           #:make-style #:copy-style
           #:fg #:bg #:boldp #:italicp #:reversep #:underlinep
           #:style-difference
           #:*default-style*
           ;;; uncursed.lisp
           #:rect
           #:make-rect #:copy-rect
           #:rect-x #:rect-y #:rect-rows #:rect-cols

           #:tui-base
           ;; methods
           #:rows #:cols
           #:windows
           #:initialize
           #:run
           #:stop
           #:redisplay
           ;; fns
           #:wakeup

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
           #:use-palette

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
           ))
