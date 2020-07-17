(asdf:defsystem :uncursed
  :description "Another terminal library, this time without curses"
  :author "tianlin qu"
  :license "GPLv3"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:alexandria
               :cffi
               :split-sequence
               :terminfo)
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "condition")
               (:file "util")
               (:file "uncursed-base")
               (:file "uncursed")))
