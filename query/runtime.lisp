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
(defun cache-object-with-prefetched-slots (row start prefetched-slots)
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
            ;; we use the slot-name here because we can't guarantee that the effective slot will match with the class of the object
            (setf (cached-slot-boundp-or-value object (slot-definition-name slot))
                  (restore-slot-value slot rdbms-value)))
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
(defgeneric value->sql-literal (value type &optional args)

  ;; Runtime cast error
  
  (:method (value type &optional args)
           (declare (ignore args))
           (error "Can not cast ~A to ~A" value type))

  ;; Supported types
  
  (:method (value (type symbol) &optional args)
           (sql-literal :value (value->sql-value value type args)))

  (:method (value (type persistent-class) &optional args)
           (assert (null args))
           (assert (typep value type))
           (sql-literal :value (id-of value)))

  (:method (value (type cons) &optional args)
           (assert (null args))
           (value->sql-literal value (first type) (rest type)))

  ;; Infer type from value

  (:method ((value persistent-object) (type null) &optional args)
           (assert (null args))
           (value->sql-literal value (type-of value)))
 
  (:method ((value string) (type null) &optional args) ; TODO
           (assert (null args))
           (value->sql-literal value 'string))

  (:method ((value number) (type null) &optional args) ; TODO BIT
           (assert (null args))
           (value->sql-literal value 'number))

  ;; Iterate on lists

  (:method ((value list) (type persistent-class) &optional args)
           (assert (null args))
           (assert (every #L(typep !1 type) value))
           (sql-literal :value (mapcar 'id-of value)))

  (:method ((value list) (type null) &optional args) ; FIXME list of unknown type, hopefully not a form
           (assert (null args))
           (sql-literal :value (mapcar #L(value->sql-literal !1 type) value))))

(defun value->sql-value (value type type-args)
  (assert type)
  (first ;; FIXME
   (value->sql-values value type type-args)))

(defun value->sql-values (value type type-args)
  (assert type)
  (funcall
    (compute-writer nil (if type-args (cons type type-args) type))
    value))

  

