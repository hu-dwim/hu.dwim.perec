;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE THE NUMBER OF DEPENDENCIES

(set-dispatch-macro-character
 #\# #\t
 (lambda (s c n)
   (declare (ignore s c n))
   t))

(set-dispatch-macro-character
 #\# #\f
 (lambda (s c n)
   (declare (ignore s c n))
   nil))

(defmacro debug-only (&body body)
  #+debug`(progn ,@body)
  #-debug(declare (ignore body)))

(defun canonical-symbol-name (symbol)
  "Returns the package name and symbol name concatenated."
  (strcat
   (package-name (symbol-package symbol))
   "::"
   (symbol-name symbol)))

(defun symbol-from-canonical-name (name)
  (read-from-string name))

(defmacro delete! (object place)
  `(setf ,place
    (delete ,object ,place)))

(defun find-slot (class slot-name)
  (find slot-name (class-slots class) :key 'slot-definition-name))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

(defmacro prog1-bind (var ret &body body)
  `(bind ((,var ,ret))
    ,@body
    ,var))

(defun mappend (function &rest lists)
  "Same as mapcar except the results are appended."  
  (apply 'append (apply 'mapcar function lists)))
