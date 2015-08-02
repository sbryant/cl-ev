(defpackage ev-system
  (:use #:asdf))

(in-package :ev-system)

(defsystem #:ev
  :description "libev bindings"
  :author "Sean Bryant <sean@hackinggibsons.com"
  :license "BSD"
  :depends-on (#:cffi #:trivial-garbage)
  :components ((:module "src" :components
                        ((:file "package")
                         (:file "cffi" :depends-on ("package"))
                         (:file "ev" :depends-on ("cffi"))))))
