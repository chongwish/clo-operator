;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp Addtional Operator Bit Extension
;;; @author: chongwish
;;; @email: chongwish@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clo-operator.bit
  (:use :cl)
  (:import-from #:clo-operator.macro
                #:define-map-macro<@rest
                #:define-map-macro<...
                #:define-f/m-map-macro<@rest
                #:define-f/m-map-macros<@rest
                #:define-f/m-map-macro<...)
  (:nicknames :clo-operator-bit)
  (:export #:template))

(in-package #:clo-operator.bit)


(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *this-package-name* (package-name *package*))
  (defparameter *bit-list* nil))


(defmacro limit (bit n &rest rest)
  "limit the bit of n less than *"
  `(ldb (byte ,bit 0) ,n))


(defmacro [*]full (bit)
  "fill 1 to every bit of a *-bit number"
  `(- (expt 2 ,bit) 1))


(defun [*]split (bit n)
  "split n to a *-bit list"
  (do ((i 0 (+ i bit))
       (result nil))
      ((and (zerop (ldb (byte bit i) n)) (zerop n)) result)
    (push (ldb (byte bit i) n) result)
    (setf (ldb (byte bit i) n) 0)))


(defmacro define-limit-macro<a&b (macro-name map-name &key (description ""))
  "define a macro that has two parameters was limited to *-bit"
  `(define-f/m-map-macro<... ("limit" ,*this-package-name*) ,(write-to-string macro-name) ,(write-to-string map-name) (a b) :fm-param (a) :description ,description))


(defmacro define-limit-macro<a&b&c (macro-name map-name &key (description ""))
  "define a macro that has three parameters was limited to *-bit"
  `(define-f/m-map-macro<... ("limit" ,*this-package-name*) ,(write-to-string macro-name) ,(write-to-string map-name) (a b c) :fm-param (a) :description ,description))


(defmacro define-limit-macro<@rest (macro-name map-name &key (description ""))
  "define a macro that has one normal and &rest parameters was limited to *-bit"
  `(define-f/m-map-macro<@rest ("limit" ,*this-package-name*) ,(write-to-string macro-name) ,(write-to-string map-name) :fm-param (a) :description ,description))


(defmacro [*]= (bit n)
  "return a number which the bit of it less than *"
  `(limit ,bit ,n))


(define-limit-macro<a&b&c [*]<< ash :description "logical left shift n bit, return a *-bit number")


(define-limit-macro<a&b [*]not lognot :description "logical not, return a *-bit number")


(defmacro [*]>> (bit n bit-shift)
  "logical right shift n bit, return a *-bit number"
  `(* (if (plusp ,n) 1 -1) (limit ,bit (ash (abs ,n) (- 0 ,bit-shift)))))


(defmacro [*]<<> (bit n bit-shift &key (safe t))
  "circular left shift n bit a *-bit number"
  (let* ((i (if safe bit-shift `(mod ,bit-shift ,bit)))
         (j `(- ,i ,bit))
         (sym (gensym)))
    `(let ((,sym ,n))
       ;; (logior (limit ,bit (ash ,sym ,i)) (ash ,sym ,j)))))
       (logior (ash (ldb (byte (- ,bit ,i) 0) ,sym) ,i) (ash ,sym ,j)))))


(defmacro [*]<>> (bit n bit-shift &key (safe t))
  "circular right shift n bit a *-bit number"
  `([*]<<> ,bit ,n (- ,bit (mod ,bit-shift ,bit)) :safe safe))


(defmacro instance ()
  "generate some function the style like '[*]op'"
  (let ((l nil))
    (setf l (append (define-f/m-map-macros<@rest ("LIMIT" "CLO-OPERATOR.BIT")
                        (("[*]+" "+" "limit the bit of the result of the + operator")
                         ("[*]-" "-" "limit the bit of the result of the - operator")
                         ("[*]*" "*" "limit the bit of the result of the * operator")
                         ("[*]/" "/" "limit the bit of the result of the / operator")
                         ("[*]or" "logior" "limit the bit of the result of the logior operator")
                         ("[*]and" "logand" "limit the bit of the result of the logand operator")
                         ("[*]xor" "logxor" "limit the bit of the result of the logxor operator")) :fm-param (a)) l))
    `(eval-when (:compile-toplevel :load-toplevel :execute) ,@l)))


(defun gen-n-macro-name (n name)
  "generate string like '[n]OP' from string which is 'op'"
  (concatenate 'string "[" (write-to-string n) "]" (string-upcase name)))


(defun gen-*-macro-name (name)
  "generate string like '[*]OP' from string which is 'op'"
  (gen-n-macro-name '* name))


(defmacro define-bit-macro<a (n name &optional (description ""))
  `(define-map-macro<... (,(gen-n-macro-name n name) ,*this-package-name*) (,(gen-*-macro-name name) ,*this-package-name*) (a) :args (,n a) :description ,description))

(defmacro define-bit-macro<a&b (n name &optional (description ""))
  `(define-map-macro<... (,(gen-n-macro-name n name) ,*this-package-name*) (,(gen-*-macro-name name) ,*this-package-name*) (a b) :args (,n a b) :description ,description))


(defmacro define-bit-macro<@rest (n name &optional (description ""))
    `(define-map-macro<@rest (,(gen-n-macro-name n name) ,*this-package-name*) (,(gen-*-macro-name name) ,*this-package-name*) :args (,n) :description ,description))


(defmacro template (n)
  "convert all the [*]op function to [n]op export function"
  (in-package #:clo-operator.bit)
  (unless (getf *bit-list* n)
    (setf (getf *bit-list* n) t)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop for (name description) in '(("full" ""))
            collect `(define-bit-macro<a ,n ,name ,description))
       ,@(loop for (name description) in '(("=" "")
                                           ("not" "")
                                           ("split" ""))
            collect `(define-bit-macro<a ,n ,name ,description))
       ,@(loop for (name description) in '(("<<" "")
                                           (">>" "")
                                           ("<<>" "")
                                           ("<>>" ""))
            collect `(define-bit-macro<a&b ,n ,name ,description))
       ,@(loop for (name description) in '(("+" "")
                                           ("-" "")
                                           ("*" "")
                                           ("/" "")
                                           ("and" "")
                                           ("or" "")
                                           ("xor" ""))
            collect `(define-bit-macro<@rest ,n ,name ,description)))))

(instance)
