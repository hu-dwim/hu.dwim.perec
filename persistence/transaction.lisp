;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; with-database, open-database, close-database, with-transaction, with-transaction*, begin, commit, rollback are all inherited from cl-rdbms

(def (class* e) database-mixin ()
  ((oid-instance-id-sequence-exists #f :type boolean)))

(def method oid-instance-id-sequence-exists-p ((self standard-object))
  (error "The required cl-perec:database-mixin is missing from *database* (~A) which is of class ~S" *database* (class-of *database*)))

(def method transaction-mixin-class list ((self database-mixin))
  'transaction-mixin)

(def (class* e) transaction-mixin (transaction-instance-cache-mixin)
  ())

(defmethod rdbms::cleanup-transaction ((transaction transaction-mixin))
  (unwind-protect
       (call-next-method)
    (map-cached-instances
     #L(setf (transaction-of !1) nil))))

(defun instance-in-transaction-p (instance)
  "Returns true iff the instance is attached to a transaction which is in progress."
  (not (null (aprog1 (transaction-of instance)
               (debug-only
                 (assert (or (not it) (transaction-in-progress-p it))))))))

(defun instance-in-current-transaction-p (instance)
  "Returns true iff the instance is attached to the current transaction which is in progress."
  (and (in-transaction-p)
       (eq (transaction-of instance) *transaction*)))

(defgeneric before-committing-instance (instance transaction-event)
  (:method (instance transaction-event)
           (bind ((class (class-of instance)))
             (dolist (slot (persistent-effective-slots-of class))
               (when (eq :on-commit (type-check-of slot))
                 (bind (((:values cached-p slot-value) (slot-value-cached-p instance slot)))
                   (when cached-p
                     (check-slot-value-type instance slot slot-value #t))))))))

(defgeneric after-instance-committed (instance transaction-event)
  (:method (instance transaction-event)
           (values)))

(defmethod commit-transaction :around (database (transaction transaction-mixin))
  (map-created-instances (rcurry #'before-committing-instance :created))
  (map-modified-instances (rcurry #'before-committing-instance :modified))
  (map-deleted-instances (rcurry #'before-committing-instance :deleted))
  (call-next-method)
  (map-created-instances (rcurry #'after-instance-committed :created))
  (map-modified-instances (rcurry #'after-instance-committed :modified))
  (map-deleted-instances (rcurry #'after-instance-committed :deleted)))
