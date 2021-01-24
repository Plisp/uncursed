# uncursed

A library for writing terminal interfaces without curses.
A higher-level buffered abstraction and low-level utilities are available. Supported implementations will include sbcl, ccl and ecl.

Some basic examples can be found in the `examples` directory.
n.b. `sand-game.lisp` features the color/timer functionality of the library and displays flashing colors.

- [x] direct-color (truecolor) support
  - [x] palette fallback (init_color or approximation)
- [x] comprehensive input handling
  - [x] any-event mouse tracking (SGR 1006 only), including modifiers
  - [x] special keys, including modifiers
- [x] unicode support (combining and fullwidth)
- [x] basic terminal resizing support
- [x] timers
- [ ] resizing
  - [x] resize on input
  - [ ] resize live
- [x] cross-thread `wakeup`
- [ ] better window management interface
- [ ] documentation
