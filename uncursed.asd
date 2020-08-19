(asdf:defsystem :uncursed
  :description "Another TUI library, this time without curses."
  :author "tianlin qu"
  :license "BSD 3-Clause"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:alexandria :cffi :terminfo)
  :version "0.2.0"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "condition")
               (:file "util")
               (:file "uncursed")))
