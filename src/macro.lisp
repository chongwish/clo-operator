;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp Addtional Macro Extension
;;; @author: chongwish
;;; @email: chongwish@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clo-operator.macro
  (:use :cl)
  (:import-from #:clo-operator.pkg
                #:find-fm
                #:set-package)
  (:nicknames :clo-operator-macro)
  (:export #:define-map-macro<@rest
           #:define-map-macros<@rest
           #:define-f/m-map-macro<@rest
           #:define-f/m-map-macros<@rest
           #:define-map-macro<...
           #:define-map-macros<...
           #:define-f/m-map-macro<...
           #:define-f/m-map-macros<...
           #:define-map-macro<a
           #:define-f/m-map-macro<a
           #:define-map-macro<a&b
           #:define-f/m-map-macro<a&b))

(in-package #:clo-operator.macro)


(defmacro define-map-macro<@rest (macro-name map-name &key (args nil) (export t) (description ""))
  "define a macro alias a exist function/macro which parameter is &rest"
  (let ((args (loop for i in args collect (if (symbolp i) (gensym "G") i))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(set-package macro-name)
       ,(if :export `(export ',(find-fm macro-name)))
       (defmacro ,(find-fm macro-name) (,@(remove-if-not #'symbolp args) &rest rest)
         ,description
         `(,(find-fm ',map-name) ,,@args ,@rest)))))


(defmacro define-map-macros<@rest (map-list &key (args nil) (export t))
  "generate a collection of macro that is a alias of a exist function/macro which parameter is &rest"
  (let ((op (loop for (macro-name map-name description) in map-list
               collect `(define-map-macro<@rest ,macro-name map-name :args ,args :export ,export :description ,(if description description "")))))
    `',op))


(defmacro define-f/m-map-macro<@rest (fm macro-name map-name &key (args nil) (fm-param nil) (export t) (description ""))
  "define a macro that need a expression to do with a exist function/macro which parameter is &rest"
  (let* ((syms (remove nil (loop for i in (append fm-param args) collect (if (symbolp i) (cons i (gensym "G"))))))
         (fm-param (loop for i in fm-param collect (if (symbolp i) (cdr (assoc i syms)) i)))
         (args (loop for i in args collect (if (symbolp i) (cdr (assoc i syms)) i))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(set-package macro-name)
       ,(if :export `(export ',(find-fm macro-name)))
       (defmacro ,(find-fm macro-name) (,@(remove-if-not #'symbolp fm-param) ,@(remove-if-not #'symbolp args) &rest rest)
         ,description
         `(,(find-fm ',fm) ,,@fm-param (,(find-fm ',map-name) ,,@args ,@rest))))))


(defmacro define-f/m-map-macros<@rest (fm map-list &key (args nil) (fm-param nil) (export t))
  "generate a collection of macro that need a expression to do with a exist function/macro which parameter is &rest"
  (let ((op (loop for (macro-name map-name description) in map-list
               collect `(define-f/m-map-macro<@rest ,fm ,macro-name ,map-name :args ,args :fm-param ,fm-param :export ,export :description ,(if description description "")))))
    `',op))


(defmacro define-map-macro<... (macro-name map-name parameter &key (args nil) (export t) (description ""))
  "define a macro alias a exist function/macro"
  (let* ((syms (loop for i in parameter collect (cons i (gensym "G"))))
         (parameter (loop for i in parameter collect (cdr (assoc i syms))))
         (args (remove nil (loop for i in args collect (if (symbolp i) (cdr (assoc i syms)) i)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(set-package macro-name)
       ,(if :export `(export ',(find-fm macro-name)))
       (defmacro ,(find-fm macro-name) ,parameter
         ,description
         `(,(find-fm ',map-name) ,,@(if args args parameter))))))


(defmacro define-map-macros<... (map-list parameter &key (args nil) (export t))
  "generate a collection of macro that is a alias of a exist function/macro"
  (let ((op (loop for (macro-name map-name description) in map-list
               collect `(define-map-macro<... ,macro-name ,map-name ,parameter :args ,args :export ,export :description ,(if description description "")))))
    `',op))


(defmacro define-f/m-map-macro<... (fm macro-name map-name parameter &key (fm-param nil) (args nil) (export t) (description ""))
  "define a macro that need a expression to do with a exist function/macro"
  (let* ((syms (loop for i in parameter collect (cons i (gensym "G"))))
         (parameter (loop for i in parameter collect (cdr (assoc i syms))))
         (args (remove nil (loop for i in args collect (if (symbolp i) (cdr (assoc i syms)) i))))
         (fm-param (remove nil (loop for i in fm-param collect (if (symbolp i) (cdr (assoc i syms)) i)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(set-package macro-name)
       ,(if :export `(export ',(find-fm macro-name)))
       (defmacro ,(find-fm macro-name) ,parameter
         ,description
         `(,(find-fm ',fm) ,,@fm-param (,(find-fm ',map-name) ,,@(if args args (if fm-param (reverse (set-difference parameter fm-param)) parameter))))))))


(defmacro define-f/m-map-macros<... (fm map-list parameter &key (fm-param nil) (args nil) (export t))
  "generate a collection of macro that need a expression to do with a exist function/macro"
  (let ((op (loop for (macro-name map-name description) in map-list
               collect `(define-f/m-map-macro<... ,fm ,macro-name ,map-name ,parameter :fm-param ,fm-param :args ,args :export ,export :description ,(if description description "")))))
    `',op))


(defmacro define-map-macro<a (macro-name map-name &key (export t) (description ""))
  "define a macro alias a exist function/macro which has one parameter"
  `(define-map-macro<... ,macro-name ,map-name (a) :export ,export :description ,description))


(defmacro define-f/m-map-macro<a (fm macro-name map-name &key (fm-param nil) (export t) (description ""))
  "define a macro that need a expression to do with a exist function/macro which has one parameter"
  `(define-f/m-map-macro<... ,fm ,macro-name ,map-name (a) :fm-param ,fm-param :export ,export :description ,description))


(defmacro define-map-macro<a&b (macro-name map-name &key (export t) (description ""))
  "define a macro alias a exist function/macro which has two parameter"
  `(define-map-macro<... ,macro-name ,map-name (a b) :export ,export :description ,description))


(defmacro define-f/m-map-macro<a&b (fm macro-name map-name &key (fm-param nil) (export t) (description ""))
  "define a macro that need a expression to do with a exist function/macro which has two parameter"
  `(define-f/m-map-macro<... ,fm ,macro-name ,map-name (a b) :fm-param ,fm-param :export ,export :description ,description))
