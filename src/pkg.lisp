;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp Addtional Package Extension
;;; @author: chongwish
;;; @email: chongwish@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clo-operator.pkg
  (:use :cl)
  (:nicknames :clo-operator-pkg)
  (:export #:find-fm
           #:set-package))

(in-package #:clo-operator-pkg)


(declaim (inline find-fm))
(defun find-fm (name)
  "fetch the symbol of a function/macro by string or cons which the second value is the package of the function/macro"
  (if (consp name)
      (if (consp (cdr name))
          (intern (string-upcase (car name)) (string-upcase (cadr name)))
          (intern (string-upcase (car name)) (string-upcase (cdr name))))
      (intern (string-upcase name))))


(defun set-package (name)
  "generate a list that can switch to the other package in macro"
  (let ((ns name))
    (if (consp ns)
        (setf ns (string-upcase (if (consp (cdr ns)) (cadr ns) (cdr ns)))))
    (if (consp name)
        `(in-package ,ns))))

