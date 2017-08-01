;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp Addtional Operator Extension
;;; @author: chongwish
;;; @email: chongwish@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clo-operator
  (:use :cl)
  (:nicknames :clo-operator))

(in-package #:clo-operator)


(in-package #:clo-operator.bit)

(clo-operator.bit:template 8)
(clo-operator.bit:template 32)
