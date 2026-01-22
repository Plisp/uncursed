# uncursed

A cross-platform library for writing terminal interfaces with minimal dependencies (terminfo on unix, recent conhost with VT support on windows). A higher-level buffered drawing abstraction and low-level utilities are provided. Supported implementations will include sbcl, ccl and ecl.

Some useful features are provided beyond stock curses functionality:
while this library doesn't aim to provide a full async i/o loop, it does support input timeouts and thread-safe wakeups to integrate with other event sources. The buffer abstraction allows stateless color drawing (no leaky color pairs) while internally optimizing output sequences and makes an attempt to account for wide unicode characters, detecting attempts to overwrite them or draw them beyond the window edge. However width calculations are done character-wise which misses some combining rules and ambiguous characters are not yet localised. This will be improved as needed, PRs welcome!

Some basic examples can be found in the `examples` directory.
n.b. `sand-game.lisp` features the color/timer functionality of the library and displays flashing colors.

![demo](https://media.giphy.com/media/F0s3CHtvG8bopDwPDv/giphy.gif)

## capabilities
- [x] direct-color (truecolor) support
  - [x] palette fallback (init_color or approximation)
- [x] comprehensive input handling
  - [x] any-event mouse tracking (SGR 1006 only), including modifiers
  - [x] special keys, including modifiers (nonstandard st sequences unsupported)
- [x] basic unicode support (width calculation, overwrite signals)
- [x] timers
- [x] terminal resize hook
- [x] thread-safe `wakeup`
- [x] basic documentation
- [x] Windows support (new!)
- [ ] layout abstraction
- [ ] account for network character delays
- [ ] high-level widget modules

## Getting started
For interactive development, a output terminal device is necessary but the SLIME repl within emacs does not emulate a terminal.
To work around this, start a swank server in a terminal session and connect using `M-x slime-connect` or `sly-connect` in emacs.
```
(ql:quickload :swank) ; for sly users, this is slynk
(swank:create-server :dont-close t)
(loop (sleep 1))
```
Assuming you're not interested in dealing with terminal horrors, the exported `tui` class is an easy way to get started.
You'll care about its `root-view`, `focused-view` and `event-handler` slots.
The global event-handler has signature `(event-handler tui-instance event)` events can be found by testing with `examples/input.lisp`.

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
           (make-instance 'ui :focused-view view
                              :root-view (list view)
                              :event-handler #'tui-handle-event)))
     ;; the #'tick callback should return (values new-interval new-context), or NIL to cancel.
     ;; It's called with the `tui` and provided `context` as arguments
    (tui:schedule-timer ui (tui:make-timer *tick* #'tick :context (waves view)))
    (tui:run ui)))
```
Pretty simple as you can see. Get the dimensions, initialize a rect structure with 0-based cell coordinates (top left being origin)
for your `view` subclass's `dimensions` slot and use it to initialize a `tui` instance.
On that topic, implementing three methods is useful:
* `defgeneric handle-key-event (view tui event)` - self-explanatory, events take the following forms
(may not work on all terminals, test with `examples/input.lisp`)
  * `(CHARACTER [modifiers]...)` - modifiers include :shift, :alt, :control and :meta
  * `:f1-20, :home, :end, :insert, :delete, :up/:down/:left/:right-arrow, :page-down, :page-up`
  * `(:function-or-special-key [modifiers]...)` - above but with modifiers

* `defgeneric handle-mouse-event (view tui button state line col &key &allow-other-keys` provides mouse button, `:click`/`:release`/`:drag` state
and position relative to the view. Keyword arguments may include the modifiers above.

* `defgeneric present (view)`, called each redisplay on every view. Drawing is done with:
  * `(put char line col &optional style)` which writes `char` at 1-based line/col coordinates relative to its position
  * `(puts string line col &optional style)` same thing with a string

Both functions may raise a `view-bounds-error` when attempting to draw outside the view's dimensions and
optionally accept a `style` struct with the following fields, corresponding the terminal attributes of rendered text where supported.
```lisp
(defstruct (style (:conc-name nil))
  (fg nil :type (or null (integer #x000000 #xffffff)))
  (bg nil :type (or null (integer #x000000 #xffffff)))
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (reversep nil :type boolean)
  (underlinep nil :type boolean))
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
* Use the `use-palette` initarg to control whether the terminal palette is used. The library makes a best effort to avoid messing up user colors on exit, prefer approximation to modifying the palette.
