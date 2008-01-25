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
    (when (and string pattern) ;; FIXME if string or pattern is NIL, the result is NULL in SQL and not FALSE
      (re-like string
               (like-pattern->regex pattern)
               :start start
               :end end
               :case-sensitive-p case-sensitive-p))))

(defun re-like (string pattern &key (start 0) end (case-sensitive-p #t))
  (when (and string pattern) ;; FIXME if string or pattern is NIL, the result is NULL in SQL and not FALSE
    (bind ((end (or end (length string))))
      (generalized-boolean->boolean
       (if case-sensitive-p
           (scan pattern string :start start :end end)
           (scan (create-scanner pattern :case-insensitive-mode #t) string :start start :end end))))))

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
            (for value = (restore-slot-value instance instance-slot row index))
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
;;; Writer
;;;

(defstruct (type-info (:conc-name ti-))
  writer
  column-count
  column-type
  unbound-subtype-p
  null-subtype-p)

(def function compute-type-info (type)
  (when (and (not (eq type +unknown-type+)) (not (contains-syntax-p type)))
    (bind ((element-type
            (if (set-type-p* type)
                (bind ((set-element (set-type-class-for type)))
                  (if (typep set-element 'standard-class)
                      (class-name set-element)
                      set-element))
                type))
           (unbound-subtype-p (unbound-subtype-p type))
           (null-subtype-p (null-subtype-p type))
           (writer (compute-writer element-type))
           (rdbms-types (compute-rdbms-types element-type))
           (column-count (length rdbms-types))
           (column-type (unless (persistent-class-type-p element-type)
                          (last1 rdbms-types))))
      (make-type-info
       :writer writer
       :column-count column-count
       :column-type column-type
       :unbound-subtype-p unbound-subtype-p
       :null-subtype-p null-subtype-p))))

;;;
;;; Conversion between lisp and sql values
;;;
(defgeneric value->sql-literal (value type type-info &optional args)

  ;; Runtime cast error
  
  (:method (value type type-info &optional args)
    (declare (ignore type-info))
    (error "Can not cast ~A to ~A" value (compose-type type args)))

  ;; Compute type-info
  
  (:method (value (type symbol) (type-info null) &optional args)
    (value->sql-literal value type (compute-type-info (compose-type type args)) args))

  ;; Supported types
  
  (:method (value (type symbol) type-info &optional args)
    (declare (ignore args))
    (assert (not (eql type +unknown-type+)))
    (assert type-info)

    (sql-literal :value (value->sql-value value type-info)
                 :type (ti-column-type type-info)))

  (:method (value (type persistent-class) type-info &optional args)
    (assert (null args))
    (assert (typep value type))
    (value->sql-literal value (class-name type) type-info args))

  (:method (value (type cons) type-info &optional args)
    (assert (null args))
    (value->sql-literal value (first type) type-info (rest type)))

  ;; Infer type from value

  (:method (value (type (eql +unknown-type+)) type-info &optional args)
    (declare (ignore args))
    (error "Could not infer SQL type for literal: ~S" value))

  (:method ((value persistent-object) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (value->sql-literal value (type-of value) type-info))
 
  (:method ((value string) (type (eql +unknown-type+)) type-info &optional args) ; TODO
    (assert (null args))
    (value->sql-literal value 'string type-info))

  (:method ((value integer) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (if (<= (- #.(expt 2 31)) value #.(1- (expt 2 31)))
        (value->sql-literal value 'integer-32 type-info)
        (value->sql-literal value 'integer type-info)))

  (:method ((value number) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (value->sql-literal value 'number type-info))

  ;; Iterate on lists

  (:method ((value list) (type (eql 'set)) type-info &optional args)
    (assert (not (null args)))
    (assert (every #L(typep !1 (first args)) value))
    (sql-literal :value (mapcar
                         #L(value->sql-literal !1 (first args) type-info) ;; FIXME review: type-info should be transformed to element type
                         value)))

  (:method ((value list) (type (eql +unknown-type+)) type-info &optional args) ; FIXME hopefully not a form
    (assert (null args))
    (sql-literal :value (mapcar
                         #L(value->sql-literal !1 type type-info)
                         value))))

(defun value->sql-value (value type-info)
  (declare (type type-info type-info))
  (assert type-info)
  (assert (<= 1 (ti-column-count type-info) 2))

  ;; KLUDGE: oddly enough, the current writer for boolean -> sql-boolean-type mapping
  ;; creates "TRUE" or "FALSE". (should be #t or #f)
  (when (typep (ti-column-type type-info) 'sql-boolean-type)
    (return-from value->sql-value (if value #t #f)))
  
  (bind ((sql-values (make-array 2)))
    (declare (dynamic-extent sql-values))
    (funcall (ti-writer type-info) value sql-values 0)
    (ecase (ti-column-count type-info)
      (1 (elt sql-values 0))
      (2 (cond
           ((persistent-object-p value) ; only id column used
            (elt sql-values 0))
           ((and (ti-null-subtype-p type-info) (ti-unbound-subtype-p type-info))
            (assert (elt sql-values 0)) ; check if BOUND
            (elt sql-values 1))         ; omit BOUND column
           (t
            (error "unsupported multi-column type: ~A" (ti-column-type type-info))))))))

(defun compose-type (type args)
  (if args (cons type args) type))

  

