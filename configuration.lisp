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

;; TODO delete this when the patches sent to sbcl-devel are applied
#+sbcl
(unless (fdefinition '(setf standard-instance-access))
  (sb-ext:without-package-locks
    (defun (setf standard-instance-access) (new-value instance location)
      (setf (sb-pcl::clos-slots-ref (sb-pcl::std-instance-slots instance) location) new-value))
    (defun (setf funcallable-standard-instance-access) (new-value instance location)
      (setf (sb-pcl::clos-slots-ref (sb-pcl::fsc-instance-slots instance) location) new-value))
    (warn "Your sbcl does not seem to have (setf standard-instance-access); we've just defined it.")))


