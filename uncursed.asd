(asdf:defsystem #:uncursed
  :description "Another TUI library, this time without curses."
  :author "tianlin qu"
  :license "BSD 3-Clause"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:alexandria :cffi :cl-setlocale :pileup #+unix #:terminfo)
  :version "0.7.0"
  :serial t
  :components ((:file "package")
               #+unix (:cffi-grovel-file "grovel")
               #+unix (:cffi-wrapper-file "wrappers")
               (:file "palette")
               (:file "util")
               (:file "uncursed")
               (:file "elemental"))

  :in-order-to ((asdf:test-op (asdf:test-op #:uncursed/tests))))

(asdf:defsystem #:uncursed/tests
  :depends-on (#:uncursed)
  :components ((:file "tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :uncursed-tests :run-tests)))
