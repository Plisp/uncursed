# uncursed

A library for writing terminal interfaces without curses.
A higher-level buffered abstraction and low-level utilities are available. Supported implementations will include sbcl, ccl and ecl.

Some basic examples can be found in the `examples` directory.
n.b. `sand-game.lisp` features the color/timer functionality of the library and displays flashing colors.

- [x] unicode support
- [x] basic terminal resizing support
- [x] comprehensive input handling
  - [x] any-event mouse tracking (SGR 1006 only)
  - [x] special keys, including modifiers
- [x] timers
- [ ] widgets
