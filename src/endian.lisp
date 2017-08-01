;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp Addtional Operator Endian Extension
;;; @author: chongwish
;;; @email: chongwish@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clo-operator.endian
  (:use :cl)
  (:nicknames :clo-operator-endian)
  (:export #:transform))

(in-package #:clo-operator.endian)


;;; the type of the unit of endian

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype bit-type ()
    '(and unsigned-byte (satisfies is-bit))))

(defun is-bit (n)
  (and
   (>= n 16)
   (zerop (mod n 16))))

;; (deftype is-bit ()
;;     (let ((pred-fn (gensym)))
;;       (setf (symbol-function pred-fn) (lambda (n)
;;                                         (and
;;                                          (>= n 16)
;;                                          (zerop (mod n 16)))))
;;       `(and unsigned-byte (satisfies ,pred-fn))))


(defmacro transform (n &key (bit 32))
  "*-bit endian transform a number which is little/big endian to big/little endian"
  (declare (bit-type bit))
  (let* ((sym-n (gensym))
         (result (gensym))
         (min-i 0)
         (max-i (/ bit 8))
         (mid-i (/ (- max-i min-i) 2)))
    `(let ((,sym-n ,n)
           (,result 0))
       (loop for i from ,min-i below ,max-i
          do (setf (ldb (byte 8 (* i 8)) ,result) (ldb (byte 8 (* (- ,max-i 1 i) 8)) ,sym-n)))
       ,result)))
