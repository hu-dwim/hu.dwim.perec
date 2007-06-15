;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;
;;; Meta shift

(defconstant +persistent-class-class-id+ +oid-maximum-class-id+)

(defpclass* persistent-class* ()
  ((persistent-class-id :type integer)
   (persistent-class-name :type symbol)))

(defmethod export-to-rdbms :before ((class persistent-class))
  (oid-mode-ecase
    (:class-name)
    ((:class-id :merge)
     (ensure-class-name-and-class-id-map class))))

(defun ensure-class-name-and-class-id-map (class)
  (or *oid-class-id-sequence-exists* (ensure-class-id-sequence))
  (unless (abstract-p class)
    (bind ((class-name (class-name class))
           (class-id (class-name->class-id class-name)))
      (unless (and class-id
                   (class-id->class-name class-id))
        (setf class-id
              (if (eq 'persistent-class* class-name)
                  +persistent-class-class-id+
                  (persistent-class-id-of (or (select-instance (instance)
                                                (from (instance persistent-class*))
                                                (where (eq class-name (persistent-class-name-of instance))))
                                              (make-instance 'persistent-class*
                                                             :persistent-class-id (next-class-id)
                                                             :persistent-class-name class-name)))))
        (setf (class-name->class-id class-name) class-id)
        (setf (class-id->class-name class-id) class-name)))))
