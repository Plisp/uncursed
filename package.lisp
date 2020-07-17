;;;; package.lisp

(defpackage :uncursed-sys
  (:use :cl :alexandria)
  (:export #:syscall-error

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
           #:set-background
           #:read-event
           #:read-event-timeout

           #:track-sigwinch
           #:untrack-sigwinch

           #:tui-base
           ;; slots
           #:lines #:columns
           #:windows
           #:focused-window
           #:event-handler
           ;; methods
           #:start #:stop
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
           #:handle-window-event

           #:curved-box-border
           ))

(defpackage :uncursed
  (:use :cl :uncursed-sys)
  (:export #:text-box
           ;; slots
           #:text-box-text
           #:text-box-cursor-offset))
