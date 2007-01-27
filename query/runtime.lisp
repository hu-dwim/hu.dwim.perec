;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;; Functions called from compiled queries.

;;;
;;; Lisp implementation of some SQL funtion
;;;
(defun like (str pattern)
  "Matches STR with PATTERN. In the pattern _ and % wildcards can be used"
  (flet ((like-pattern->regex (pattern)
           (setf pattern (regex-replace-all "([.*+?(){}|^$])" pattern "\\\\\\1"))
           (setf pattern (regex-replace-all "(?<!\\\\)_" pattern "."))
           (setf pattern (regex-replace-all "(?<!\\\\)%" pattern ".*"))))
    (if (scan (like-pattern->regex pattern) str) #t #f)))

(defun sum (seq)
  "Returns the sum of non NIL elements of SEQ."
  (iter (for val in-sequence seq)
        (sum (or val 0))))

(defun avg (seq)
  "Returns the average of non NIL elements of SEQ."
  (iter (for val in-sequence seq)
        (sum (or val 0) into sum)
        (counting val into count)
        (finally (return (if (> count 0) (/ sum count) 0)))))

;;;
;;; Caching
;;;
(defun cache-object-with-prefetched-properties (row start prefetched-slots)
  "Caches the objects whose oid and slots are contained by ROW starting at START."
  (bind ((oid-width (length +oid-column-names+))
         (oid (subseq row start (+ start oid-width)))
         (rdbms-values
          (iter (for slot in prefetched-slots)
                (for width = (column-count-of slot))
                (for index initially (+ start oid-width) then (+ index width))
                (collect (subseq row index (+ index width))))))
    (cache-object* oid prefetched-slots rdbms-values)))

(defun cache-object* (oid slots rdbms-values)
  "Caches the objects whose oid and slots are contained by ROW starting at START."
  (bind ((object (cache-object oid)))
    (mapc (lambda (slot rdbms-value)
            (setf (cached-slot-value object (slot-definition-name slot))
                  (funcall (reader-of slot) rdbms-value)))
          slots rdbms-values)
    object))

(defun column-count-of (slot)
  (length (columns-of slot)))

(defun invalidate-persistent-flag-of-cached-objects (class)
  "Sets the persistent slot to unbound for instances of class in the transaction cache."
  (maphash
   (lambda (oid object)
     (declare (ignore oid))
     (when (typep object class)
       (slot-makunbound object 'persistent)))
   (objects-of (current-object-cache))))

;;;
;;; Conversion between lisp and sql values
;;;
(defgeneric value->sql-literal (value type)
  (:method (value type)
           (sql-literal
            :value (first ; TODO
                    (funcall (compute-writer (or type (type-of value)))
                             value))))

  (:method ((value string) (type null)) ; TODO
           (value->sql-literal value 'string))

  (:method ((value number) (type null)) ; TODO BIT
           (value->sql-literal value 'number))

  (:method ((value list) type)
           (sql-literal :value (mapcar #L(value->sql-literal !1 nil) value))))

  

