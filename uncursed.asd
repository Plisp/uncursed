(asdf:defsystem #:uncursed
  :description "Another TUI library, this time without curses."
  :author "tianlin qu"
  :license "BSD 3-Clause"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:alexandria :cffi :cl-setlocale :pileup #+unix #:terminfo)
  :version "0.4.0"
  :serial t
  :components ((:file "package")
               #+unix (:cffi-grovel-file "grovel")
               #+unix (:cffi-wrapper-file "wrappers")
               (:file "palette")
               (:file "util")
               (:file "uncursed")
               (:file "elemental")))
