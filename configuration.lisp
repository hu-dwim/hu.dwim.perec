;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def macro debug-only (&body body)
  (if *load-as-production-p*
      (values)
      `(progn
         ,@body)))

(def macro debug-only* (&body body)
  `(unless *load-as-production-p*
     ,@body))

(def macro production-only (&body body)
  (if *load-as-production-p*
      `(progn
         ,@body)
      (values)))

(def macro production-only* (&body body)
  `(if *load-as-production-p*
       (progn
         ,@body)
       (values)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-sharp-l-syntax))
