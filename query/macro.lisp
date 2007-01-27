;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;
;;;; This is a macro facility for QUERY forms.
;;;; Expanders can be associated to symbols in the global environment.
;;;; Forms having a query macro symbol as their operators will
;;;; be expanded by the query compiler.
;;;;
;;;; Example:
;;;;
;;;; (define-query-macro topic-title-of (m)
;;;;   `(title-of (topic-of ,m)))
;;;; (select ((m message))
;;;;   (assert (equal (topic-title-of m) "topic"))
;;;;   (collect m))
;;;; =>
;;;; (select ((m message))
;;;;   (assert (equal (title-of (topic-of m) "topic")))
;;;;   (collect m))
;;;;

(defmacro define-query-macro (name (&rest args) &body body)
  "Defines name as a query macro."
  `(progn
    (setf (expander-of ',name) #'(lambda ,args ,@body))
    ',name))

(defun expander-of (name)
  "Returns the expander of the query macro named NAME, or NIL."
  (get name 'query-macro))

(defun (setf expander-of) (value name)
  "Sets the expander of the query macro named NAME."
  (setf (get name 'query-macro)
        value))

(defun query-macroexpand1 (form)
  "Expand the query macro at the top of the FORM."
  (bind ((name (if (consp form) (car form)))
         (args (if (consp form) (cdr form)))
         (expander (expander-of name)))
   (if expander
       (apply expander args)
       form)))

(defun query-macroexpand (form)
  "Expand all query macros in the FORM recursively."
  (cond
    ((atom form) form)
    ((constantp form) form)
    ((expander-of (car form))
     (bind (((values expanded-form expanded-p) (query-macroexpand1 form)))
       (if (or expanded-form expanded-p)
           (query-macroexpand ; TODO: detect infinite loops
            expanded-form)
           form)))
    (t
     (cons
      (car form)
      (mapcar 'query-macroexpand (cdr form))))))
