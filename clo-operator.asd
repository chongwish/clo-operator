(defpackage clo-operator-asd
  (:use :cl :asdf))

(in-package clo-operator-asd)

(defsystem clo-operator
  :version "0.1"
  :author "chongwish"
  :licence "BSD"
  :description "Additional Operator Extension for Common Lisp"
  :components ((:file "main"
                :pathname "src/main"
                :depends-on ("lib"))
               (:module "lib"
                :pathname "src"
                :components ((:file "pkg")
                             (:file "macro")
                             (:file "fn")
                             (:file "bit")
                             (:file "endian")))))
