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

(defun concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(defun initarg-symbol (symbol)
  "Creates a keyword symbol to be used as slot initarg."
  (intern (symbol-name symbol) (find-package 'keyword)))

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

(defun not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (strcat "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))
