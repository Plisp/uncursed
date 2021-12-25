# uncursed

A library for writing terminal interfaces without curses.
A higher-level buffered abstraction and low-level utilities are available. Supported implementations will include sbcl, ccl and ecl.

Some basic examples can be found in the `examples` directory.
n.b. `sand-game.lisp` features the color/timer functionality of the library and displays flashing colors.

![demo](https://media.giphy.com/media/F0s3CHtvG8bopDwPDv/giphy.gif)

## capabilities
- [x] direct-color (truecolor) support
  - [x] palette fallback (init_color or approximation)
- [x] comprehensive input handling
  - [x] any-event mouse tracking (SGR 1006 only), including modifiers
  - [x] special keys, including modifiers
- [x] unicode support (combining and fullwidth)
- [x] basic terminal resizing support
- [x] timers
- [x] resizing
  - [x] resize on input
  - [x] resize live
- [x] cross-thread `wakeup`
- [x] documentation
- [ ] Windows support
- [ ] better window management interface

## Getting started
For interactive development, a output terminal device is necessary but the SLIME repl within emacs does not emulate a terminal.
To work around this, start a swank server in a terminal session and connect using `M-x slime-connect` or `sly-connect` in emacs.
```
(ql:quickload :swank) ; for sly users, this is slynk
(swank:create-server :dont-close t)
(loop (sleep 1))
```
Assuming you're not interested in dealing with terminal horrors, the exported `tui` class is an easy way to get started.
You will care about its `cols`, `rows`, `focused-window`, `windows` list and `event-handler` slots.
The global event-handler has signature `(event-handler tui-instance event)` events can be found by playing with `examples/input.lisp`.

Some basic methods of interest for subclassing `tui`, each taking the tui as a specialized single argument:
* `initialize`, which is called to setup the terminal so `enable-mouse` and `set-cursor-shape` may
be useful to call here. The default method uses the *alternate screen* so that the terminal's contents can be restored on exit,
enables mouse drag events and makes the cursor invisible.
* `handle-resize`, which will be called when the terminal is resized, with `cols` and `rows` slots updated to the new size.
This is unimplemented by default.

```lisp
;;; assuming tui is a package-local nickname for uncursed - see examples/
(defun tui-main ()
  (let* ((dimensions (tui:terminal-dimensions))
         (view
           (make-instance 'view
                          :dimensions (tui:make-rect :x 0 :y 0
                                                     :rows (car dimensions)
                                                     :cols (cdr dimensions))))
         (ui
           (make-instance 'ui :focused-window view
                              :windows (list view)
                              :event-handler #'tui-handle-event)))
     ;; the #'tick callback should return (values new-interval new-context), or NIL to cancel.
     ;; It's called with the `tui` and provided `context` as arguments
    (tui:schedule-timer ui (tui:make-timer *tick* #'tick :context (waves view)))
    (tui:run ui)))
```
It's pretty simple as you can see. Get the dimensions, initialize a rect structure with 0-based cell coordinates (top left being origin)
for your `window` subclass's `dimensions` slot and use it to initialize a `tui` instance.
On that topic, implementing three methods is useful:
* `defgeneric handle-key-event (window tui event)` - self-explanatory, events take the following forms 
(may not work on all terminals, play with `examples/input.lisp`)
  * `(CHARACTER [modifiers]...)` - modifiers include :shift, :alt, :control and :meta
  * `:f1-20, :home, :end, :insert, :delete, :up/:down/:left/:right-arrow, :page-down, :page-up`
  * `(:function-or-special-key [modifiers]...)` - above but with modifiers

* `defgeneric handle-mouse-event (window tui button state line col &key &allow-other-keys` provides mouse button, `:click`/`:release`/`:drag` state
and position relative to the window. Keyword arguments may include the modifiers above.

* `defgeneric present (window)`, called each redisplay on every window. Drawing is done with:
  * `(put char line col &optional style)` which writes `char` at 1-based line/col coordinates relative to its position
  * `(puts string line col &optional style)` same thing with a string
Both functions may raise a `window-bounds-error` when attempting to draw outside the window's dimensions and
optionally accept a `style` struct with the following fields, corresponding the terminal attributes of rendered text where supported.
```lisp
(defstruct (style (:conc-name nil))
  (fg nil :type (or null (integer #x000000 #xffffff)))
  (bg nil :type (or null (integer #x000000 #xffffff)))
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (reversep nil :type boolean)
  (underlinep nil :type boolean))####
```

The main function looks like this.
If you are using the remote server trick mentioned previously, be sure to start the terminal thread in a separate thread
to avoid slime redirecting output to the REPL, which cannot process terminal escape sequences.
```lisp
(defun main ()
  (if (member :slynk *features*)
      (bt:make-thread (lambda () (tui-main)))
      (tui-main)))
```

## extra tips
* Call `wakeup` on a tui instance to safely trigger a redisplay from another thread, possibly doing some long running computation or i/o
* Use a mailbox-popping loop on the main/slime thread to log messages from drawing code to slime and not the terminal screen, see `examples/shockwave.lisp`
* Use the `use-palette` initarg to control whether the terminal palette is used. I will have nothing to do with this.
