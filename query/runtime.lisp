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
(defun like (string pattern &key (start 0) end (case-sensitive-p #t))
  "Matches STR with PATTERN. In the pattern _ and % wildcards can be used"
  (flet ((like-pattern->regex (pattern)
           (setf pattern (regex-replace-all "([.*+?(){}|^$])" pattern "\\\\\\1"))
           (setf pattern (regex-replace-all "(?<!\\\\)_" pattern "."))
           (setf pattern (regex-replace-all "(?<!\\\\)%" pattern ".*"))))
    (re-like string
             (like-pattern->regex pattern)
             :start start
             :end end
             :case-sensitive-p case-sensitive-p)))

(defun re-like (string pattern &key (start 0) end (case-sensitive-p #t))
  (bind ((end (or end (length string))))
    (generalized-boolean->boolean
     (if case-sensitive-p
         (scan pattern string :start start :end end)
         (scan (create-scanner pattern :case-insensitive-mode #t) string :start start :end end)))))

;;;
;;; Markers for partial eval
;;;
(defun volatile (x)
  x)

(defun static (x)
  x)

;;; 
;;; SQL text fragments
;;;
(defmacro sql-text (string)
  `(sql-fragment :sql ,string))

;;;
;;; Caching
;;;

(def (function o) cache-instance-with-prefetched-slots (row start class prefetched-slots column-counts)
  "Caches the instances whose oid and slots are contained by ROW starting at START."
  (declare (type simple-vector row)
           (type fixnum start))
  (bind ((oid (rdbms-values->oid* row start))
         (instance (cache-instance oid))
         (instance-class (class-of instance)))
    (when *cache-slot-values*
      (iter (for slot :in prefetched-slots)
            (for (the fixnum column-count) :in column-counts)
            (for (the fixnum index) :initially (+ start +oid-column-count+) :then (the fixnum (+ index column-count)))
            (for instance-slot = (if (eq instance-class class)
                                     slot
                                     (find-slot instance-class (slot-definition-name slot))))
            (for value = (restore-slot-value instance-slot row index))
            (setf (underlying-slot-boundp-or-value-using-class instance-class instance instance-slot) value)
            ;; TODO: maybe this shoud be a prefetch option?
            (propagate-cache-changes instance-class instance instance-slot value)))
    instance))

(defun column-count-of (slot)
  (length (columns-of slot)))

(defun invalidate-persistent-flag-of-cached-instances (class)
  "Sets the persistent slot to unbound for instances of class in the transaction cache."
  (map-cached-instances
   (lambda (instance)
     (when (typep instance class)
       (slot-makunbound instance 'persistent)))))

;;;
;;;
;;;
(defun execute-protected (init body cleanup)
  (unwind-protect
       (progn
         (when init (execute init))
         (mapc 'execute body))
       (when cleanup (execute cleanup))))

;;;
;;; Conversion between lisp and sql values
;;;
(defgeneric value->sql-literal (value type &optional args)

  ;; Runtime cast error
  
  (:method (value type &optional args)
           (error "Can not cast ~A to ~A" value (compose-type type args)))

  ;; Supported types
  
  (:method (value (type symbol) &optional args)
           (assert (not (eql type +unknown-type+)))
           (bind ((type (compose-type type args))
                  (normalized-type (normalized-type-for type)))
             (sql-literal :value (value->sql-value value type)
                          :type (unless (persistent-class-type-p normalized-type)
                                  (compute-column-type type)))))

  (:method (value (type persistent-class) &optional args)
           (assert (null args))
           (assert (typep value type))
           (value->sql-literal value (class-name type)))

  (:method (value (type cons) &optional args)
           (assert (null args))
           (value->sql-literal value (first type) (rest type)))

  ;; Infer type from value

  (:method (value (type (eql +unknown-type+)) &optional args)
           (declare (ignore args))
           (error "Could not infer SQL type for literal: ~S" value))

  (:method ((value persistent-object) (type (eql +unknown-type+)) &optional args)
           (assert (null args))
           (value->sql-literal value (type-of value)))
 
  (:method ((value string) (type (eql +unknown-type+)) &optional args) ; TODO
           (assert (null args))
           (value->sql-literal value 'string))

  (:method ((value integer) (type (eql +unknown-type+)) &optional args)
           (assert (null args))
           (if (<= (- #.(expt 2 31)) value #.(1- (expt 2 31)))
               (value->sql-literal value 'integer-32)
               (value->sql-literal value 'integer)))

  (:method ((value number) (type (eql +unknown-type+)) &optional args)
           (assert (null args))
           (value->sql-literal value 'number))

  ;; Iterate on lists

  (:method ((value list) (type (eql 'set)) &optional args)
           (assert (not (null args)))
           (assert (every #L(typep !1 (first args)) value))
           (sql-literal :value (mapcar #L(value->sql-literal !1 (first args)) value)))

  (:method ((value list) (type (eql +unknown-type+)) &optional args) ; FIXME hopefully not a form
           (assert (null args))
           (sql-literal :value (mapcar #L(value->sql-literal !1 type) value))))

;; TODO: this was a temporary solution that sped up things quite a bit
(defcfun (yyy :computed-in compute-as) (type)
  (normalized-type-for type))

;; TODO: normalized-type-for is a performance killer (don't have an idea yet)
(defun value->sql-value (value type)
  (assert (not (eq type +unknown-type+)))
  (bind ((sql-values (value->sql-values value type)))
    (case (length sql-values)
      (1 (elt sql-values 0))
      (2 (cond
           ((persistent-class-type-p (normalized-type-for type)) ; only id column used
            (elt sql-values 0))
           ((and (null-subtype-p type) (unbound-subtype-p type))
            (assert (elt sql-values 0)) ; check if BOUND
            (elt sql-values 1))         ; omit BOUND column
           (t
            (error "unsupported multi-column type: ~A" type))))
      (t (error "unsupported multi-column type: ~A" type)))))

;; TODO: this was a temporary solution that sped up things quite a bit
(defcfun (xxx :computed-in compute-as) (type)
  (compute-writer nil type))

;; TODO: compute-writer is a performance killer (it's available on the slot)
(defun value->sql-values (value type)
  (assert (not (eq type +unknown-type+)))
  (bind (((values writer wrapper-1 wrapper-2 column-count) (compute-writer nil type))
         (rdbms-values (make-array column-count)))
    (declare (ignore wrapper-1 wrapper-2))
    (funcall writer value rdbms-values 0)
    rdbms-values))

(defun compose-type (type args)
  (if args (cons type args) type))

  

