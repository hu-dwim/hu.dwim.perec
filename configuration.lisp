;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for t and nil in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil)))

(defmacro debug-only (&body body)
  (if (member :debug *features*)
      `(progn ,@body)
      (values)))

(defun file-header ()
  `(eval-always
    (setup-readtable)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-sharp-l-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(unless (assoc "CL-PEREC" swank:*readtable-alist* :test #'string=)
  (let ((*readtable* (copy-readtable)))
    (setup-readtable)
    (flet ((doit (&rest packages)
             (dolist (package packages)
               (push (cons package *readtable*) swank:*readtable-alist*))))
      (doit "CL-PEREC" "CL-PEREC-TEST"))))
