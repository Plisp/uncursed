(asdf:defsystem :uncursed-examples
  :description "Some example usages of UNCURSED."
  :author "tianlin qu"
  :license "GPLv3"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (:uncursed)
  :components ((:module "examples"
                :serial t
                :components ((:file "input")
                             (:file "sand-game")
                             (:file "paint")
                             (:file "shockwave")))))
