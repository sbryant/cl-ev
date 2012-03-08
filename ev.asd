(defpackage ev-system
  (:use #:asdf))

(in-package :ev-system)

(defsystem #:ev
  :depends-on (#:cffi #:trivial-garbage)
  :components ((:module "src" :components
                        ((:file "package")
                         (:file "cffi" :depends-on ("package"))
                         (:file "ev" :depends-on ("cffi"))))))
