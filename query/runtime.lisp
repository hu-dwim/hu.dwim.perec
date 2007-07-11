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

;;;
;;; Markers for partial eval
;;;
(defun volatile (x)
  x)

(defun static (x)
  x)

;;;
;;; Caching
;;;

(defun cache-instance-with-prefetched-slots (row start prefetched-slots)
  "Caches the instances whose oid and slots are contained by ROW starting at START."
  (bind ((oid (subseq row start (+ start +oid-column-count+)))
         (instance (cache-instance oid)))
    (when *cache-slot-values*
      (iter (for slot :in prefetched-slots)
            (for width = (column-count-of slot))
            (for index :initially (+ start +oid-column-count+) :then (+ index width))
            (setf (cached-slot-boundp-or-value instance (slot-definition-name slot))
                  (restore-slot-value slot row index))))
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

  (:method ((value persistent-object) (type (eql +unknown-type+)) &optional args)
           (assert (null args))
           (value->sql-literal value (type-of value)))
 
  (:method ((value string) (type (eql +unknown-type+)) &optional args) ; TODO
           (assert (null args))
           (value->sql-literal value 'string))

  (:method ((value number) (type (eql +unknown-type+)) &optional args) ; TODO BIT
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

(defun value->sql-value (value type)
  (assert (not (eq type +unknown-type+)))
  (bind ((sql-values (value->sql-values value type)))
    (case (length sql-values)
      (1 (elt sql-values 0))
      (2 (cond
           ((persistent-class-type-p (normalized-type-for type)) ; only id column used
            (elt sql-values 0))
           ((and (null-subtype-p type) (unbound-subtype-p type))
            (assert (elt sql-values 0))     ; check if BOUND
            (elt sql-values 1))             ; omit BOUND column
           (t
            (error "unsupported multi-column type: ~A" type))))
      (t (error "unsupported multi-column type: ~A" type)))))

(defun value->sql-values (value type)
  (assert (not (eq type +unknown-type+)))
  (bind (((values writer wrapper-1 wrapper-2 column-count) (compute-writer nil type))
         (rdbms-values (make-array column-count)))
    (declare (ignore wrapper-1 wrapper-2))
    (funcall writer value rdbms-values 0)
    rdbms-values))

(defun compose-type (type args)
  (if args (cons type args) type))

  

