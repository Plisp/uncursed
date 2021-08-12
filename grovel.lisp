(in-package :uncursed-sys)

#+unix
(progn
  (include "errno.h")
  (cvar ("errno" c-errno) :int)
  (constant (c-eintr "EINTR"))

  (include "fcntl.h")
  (constant (c-fsetfl "F_SETFL"))
  (constant (c-ononblock "O_NONBLOCK"))

  (include "signal.h")
  (constant (c-sigwinch "SIGWINCH"))

  (include "wchar.h")
  (ctype c-wchar "wchar_t") ;will break on windows

  (include "sys/ioctl.h")
  (cstruct c-winsize "struct winsize"
           (c-ws-rows "ws_row" :type :unsigned-short)
           (c-ws-cols "ws_col" :type :unsigned-short))
  (constant (c-get-winsz "TIOCGWINSZ"))
  (constant (c-set-attributes-flush "TCSAFLUSH"))

  (include "sys/select.h") ; TODO conditionalize
  (cstruct c-fd-set "fd_set")

  (include "time.h")
  (ctype c-time "time_t")
  (include "sys/time.h")
  (ctype c-usecs "suseconds_t")
  (cstruct c-timeval "struct timeval"
           (c-tv-sec "tv_sec" :type c-time)
           (c-tv-usec "tv_usec" :type c-usecs)) ; microseconds

  (include "termios.h")
  (ctype c-tcflag "tcflag_t")
  (cstruct c-termios "struct termios"
           (c-iflag "c_iflag" :type c-tcflag)
           (c-oflag "c_oflag" :type c-tcflag)
           (c-cflag "c_cflag" :type c-tcflag)
           (c-lflag "c_lflag" :type c-tcflag))
  (constant (c-icrnl "ICRNL"))
  (constant (c-iexten "IEXTEN"))
  (constant (c-igncr "IGNCR"))
  (constant (c-inlcr "INLCR"))
  (constant (c-inpck "INPCK"))
  (constant (c-istrip "ISTRIP"))
  (constant (c-ixon "IXON"))
  (constant (c-ixoff "IXOFF"))
  (constant (c-opost "OPOST"))
  (constant (c-icanon "ICANON"))
  (constant (c-isig "ISIG"))
  (constant (c-echo "ECHO"))
  (constant (c-parenb "PARENB")))
