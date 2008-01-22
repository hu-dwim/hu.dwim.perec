;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(def-transformer-wrapper h-unused-reader
  (lambda (rdbms-values index)
    (if (is-vector-of-constant rdbms-values :null index column-number)
        +h-unused-slot-marker+
        (funcall function rdbms-values index))))

(def-transformer-wrapper h-unused-writer
  (bind ((h-unused-rdbms-value (make-array column-number :initial-element :null)))
    (lambda (slot-value rdbms-values index)
      (declare (type simple-vector rdbms-values))
      (if (h-unused-slot-marker-p slot-value)
          (replace rdbms-values h-unused-rdbms-value :start1 index)
          (funcall function slot-value rdbms-values index)))))
