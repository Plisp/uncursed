(asdf:defsystem :uncursed-examples
  :description "Some example usages of UNCURSED."
  :author "tianlin qu"
  :license "GPLv3"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on (#:uncursed :bordeaux-threads)
  :components ((:module "examples"
                :serial t
                :components ((:file "input")
                             (:file "shockwave")
                             (:file "layout")
                             ;; (:module "old"
                             ;;  :components
                             ;;  ((:file "sand-game")
                             ;;   (:file "paint")))
                             ))))
