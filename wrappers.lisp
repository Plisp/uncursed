(in-package :uncursed-sys)

#+unix
(progn
  (include "sys/select.h")
  (defwrapper* ("FD_ZERO" fd-zero) :void
    ((set (:pointer (:struct c-fd-set))))
    "FD_ZERO((fd_set *)set);")

  (defwrapper* ("FD_ISSET" fd-isset) :int
    ((fd :int)
     (set (:pointer (:struct c-fd-set))))
    "return FD_ISSET(fd, (fd_set *)set);")

  (defwrapper* ("FD_SET" fd-set) :void
    ((fd :int)
     (set (:pointer (:struct c-fd-set))))
    "return FD_SET(fd, (fd_set *)set);")
  )
