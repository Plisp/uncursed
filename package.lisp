;;;; package.lisp

(defpackage #:uncursed-sys
  (:use :cl :alexandria)
  (:export #:uncursed-error
           #:syscall-error
           #:error-syscall-error

           #:color
           #:red #:green #:blue

           #:style
           #:make-style #:copy-style
           #:fg #:bg #:boldp #:italicp #:reversep #:underlinep
           #:style-difference
           #:*default-style*

           #:character-width #:*character-widths*
           #:display-width
           #:+stdin+
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

           #:event
           #:event-kind #:event-shiftp #:event-altp #:event-controlp #:event-metap
           #:mouse-event-p
           #:mouse-data
           #:mouse-data-button #:mouse-data-state #:mouse-data-row #:mouse-data-col

           #:read-event
           #:read-event-timeout

           #:catch-sigwinch
           #:reset-sigwinch
           #:win-events-left
           ))

(defpackage #:uncursed
  (:use :cl :alexandria)
  (:import-from #:uncursed-sys
                #:uncursed-error

                #:character-width #:*character-widths*
                #:display-width
                #:terminal-dimensions
                #:enable-mouse #:disable-mouse
                #:enable-focus-tracking #:disable-focus-tracking
                #:enable-alternate-screen #:disable-alternate-screen
                #:set-cursor-shape
                #:event
                #:event-kind #:event-shiftp #:event-altp #:event-controlp #:event-metap
                #:mouse-event-p
                #:mouse-data
                #:mouse-data-button #:mouse-data-state #:mouse-data-row #:mouse-data-col

                #:color
                #:red #:green #:blue

                #:style
                #:make-style #:copy-style
                #:fg #:bg #:boldp #:italicp #:reversep #:underlinep
                #:style-difference
                #:*default-style*)
  (:local-nicknames (:sys #:uncursed-sys))
  (:export #:uncursed-error

           #:character-width #:*character-widths*
           #:display-width
           #:terminal-dimensions
           #:enable-mouse #:disable-mouse
           #:enable-focus-tracking #:disable-focus-tracking
           #:set-cursor-shape

           #:event
           #:event-kind #:event-shiftp #:event-altp #:event-controlp #:event-metap
           #:mouse-event-p
           #:mouse-data
           #:mouse-data-button #:mouse-data-state #:mouse-data-row #:mouse-data-col

           #:color
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

           #:make-timer
           ;; methods
           #:timer-callback
           #:timer-interval

           #:tui
           ;; methods
           #:run
           #:stop
           #:dispatch-event
           #:rows #:cols
           #:redisplay

           #:schedule-timer
           #:cancel-timer
           #:use-palette
           ;; fn
           #:wakeup

           #:put
           #:puts
           #:fill-rect
           ;; errors
           #:rect-bounds-error
           #:rect-bounds-error
           #:rect-bounds-error-coordinate
           #:rect-bounds-error-bounds
           #:rect-bounds-error-rect
           #:wide-char-overwrite-error
           ;; restarts
           #:overwrite-char
           #:ignore-put
           ))
