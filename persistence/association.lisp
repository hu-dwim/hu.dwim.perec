(in-package :cl-perec)

(defcclass* persistent-association ()
  ((primary-association-end
    (compute-as nil)
    :type persistent-association-end-direct-slot-definition)
   (secondary-association-end
    (compute-as nil)
    :type persistent-association-end-direct-slot-definition)
   (association-ends
    (compute-as (list (primary-association-end-of -self-) (secondary-association-end-of -self-)))
    :type (list persistent-association-end-direct-slot-definition))
   (association-kind
    (compute-as (let ((cardinality-kinds (mapcar 'cardinality-kind-of (association-ends-of -self-))))
                  (cond ((equal cardinality-kinds '(:1 :1)) :1-1)
                        ((equal cardinality-kinds '(:n :n)) :m-n)
                        (t :1-n))))
    :type symbol
    :documentation "Valid values are :1-1, :1-n or :m-n according to association end cardinalities.")))

(defcclass* persistent-association-end-slot-definition (persistent-slot-definition)
  ((association
    (compute-as nil)
    :type persistent-association)
   (min-cardinality
    0
    :type integer
    :documentation "The minimum number of objects present in an association for this end.")
   (max-cardinality
    :type integer
    :documentation "The maximum number of objects present in an association for this end. Unbound means the maximum number is not defined.")
   (cardinality-kind
    (compute-as (if (and (slot-boundp -self- 'max-cardinality)
                         (eq (max-cardinality-of -self-) 1))
                    :1
                    :n))
    :type symbol
    :documentation "Valid values are :1, :n according to min a max cardinality.")
   (primary-association-end
    (compute-as (eq (name-of -self-) (name-of (primary-association-end-of (association-of -self-)))))
    :type boolean
    :documentation "True iff this end is the primary association end of its association.")
   (secondary-association-end
    (compute-as (eq (name-of -self-) (name-of (primary-association-end-of (association-of -self-)))))
    :type boolean
    :documentation "True iff this end is the secondary association end of its association.")))

(defcclass* persistent-association-end-direct-slot-definition
    (persistent-association-end-slot-definition persistent-direct-slot-definition)
  ())

(defcclass* persistent-association-end-effective-slot-definition
    (persistent-association-end-slot-definition persistent-effective-slot-definition)
  ((id-column
    (compute-as nil)
    :type sql-column)))

;;;;;;;;;;;;;;;;;;
;;; defassociation

(defmacro defassociation (name &body body)
  (declare (ignore name body))
  nil)

(defmacro defassociation* (name &body body)
  (declare (ignore name body))
  nil)

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((association persistent-association))
  (awhen (primary-table-of association)
    (export-to-rdbms it)))

;;;;;;;;;;;
;;; Compute

#+nil
(defmethod compute-table ((association-end persistent-association-end-direct-slot-definition))
  (bind ((association (association-of association-end)))
    (ecase (association-kind-of association)
      (:1-1 (when (secondary-association-end-p association-end)
              (primary-table-of (slot-definition-class association-end))))
      (:1-n (when (eq (cardinality-kind-of association-end) :1)
              (primary-table-of (slot-definition-class association-end))))
      (:m-n (primary-table-of association)))))

#+nil
(defmethod compute-table ((slot persistent-association-end-effective-slot-definition))
  (bind ((association (association-of slot)))
    (primary-table-of
     (ecase (association-kind-of association)
       (:1-1 (slot-definition-class (secondary-association-end-of association)))
       (:1-n (slot-definition-class (find :1 (association-ends-of association) :key #'cardinality-kind-of)))
       (:m-n association)))))

#+nil
(defmethod compute-columns ((slot persistent-association-end-effective-slot-definition))
  (bind ((association (association-of slot)))
    (ecase (association-kind-of association)
      (:1-1 (if (primary-1-1-binary-effective-association-end-p slot)
                (oid-columns-for (primary-table-of (association-element-of slot)))
                (columns-of (secondary-association-end-of association))))
      (:1-n (columns-of (find :1 (association-ends-of association) :key #'cardinality-kind-of)))
      (:m-n (columns-of (most-generic-direct-slot-for slot))))))

#+nil
(defmethod make-columns-for-slot ((association-end direct-association-end))
  (bind ((association-kind (association-kind-of (association-of association-end))))
    (when (or (and (eq association-kind :1-1)
                   (secondary-association-end-p association-end))
              (and (eq association-kind :1-n)
                   (eq (cardinality-kind-of association-end) :1)))
      (make-columns-for-reference-slot association-end))))

#+nil
(defmethod make-columns-for-slot ((association-end direct-association-end))
  (when (eq (association-kind-of (association-of association-end)) :m-n)
    (make-columns-for-reference-slot association-end)))

#+nil
(defmethod compute-data-table-slot-p ((slot effective-association-end))
  (let ((association-kind (association-kind-of (association-of slot)))
        (cardinality-kind (cardinality-kind-of slot)))
    (or (and (eq association-kind :1-n)
             (eq cardinality-kind :1))
        (and (eq association-kind :1-1)
             (secondary-association-end-p slot)))))

#+nil
(defgeneric compute-slot-accessor (effective-slot accessor-type)
  (:method ((effective-association-end effective-association-end) accessor-type)
           (log.dribble "Calculating RDBMS meta data for effective binary association end ~A in ~A"
                        effective-association-end (owner-class-of effective-association-end))
           (ecase (association-kind-of (association-of effective-association-end))
             (:1-1 (compute-1-1-binary-association-end-accessor effective-association-end accessor-type))
             (:1-n (compute-1-n-binary-association-end-accessor effective-association-end accessor-type))
             (:m-n (compute-m-n-binary-association-end-accessor effective-association-end accessor-type))))

  (:method ((effective-slot effective-slot) accessor-type)
           (log.dribble "Calculating RDBMS meta data for effective slot ~A in ~A"
                        effective-slot (owner-class-of effective-slot))
           (cond ((class-or-unspecified-type-p (slot-type-of effective-slot))
                  (compute-class-slot-accessor effective-slot accessor-type))
                 (t
                  (error "Unsupported slot type")))))

;; foreign key will be in the table of the primary-effective-association-end's association element (class) called brother
#+nil
(defun compute-1-1-binary-association-end-accessor (effective-association-end accessor-type)
  (if (primary-1-1-binary-effective-association-end-p effective-association-end)
      (compute-primary-1-1-binary-association-end-accessor effective-association-end accessor-type)
      (compute-secondary-1-1-binary-association-end-accessor effective-association-end accessor-type)))

;; sister's brother slot
#+nil
(defun compute-primary-1-1-binary-association-end-accessor (primary-effective-association-end accessor-type)
  (bind ((primary-direct-association-end (most-generic-direct-slot-for primary-effective-association-end))
	 (secondary-direct-association-end (other-association-end-of primary-direct-association-end)))
    (ecase accessor-type
      ;; select id from brother where sister-id = (id-of sister)
      (reader
       (make-instance 'rdbms-accessor
                      :effective-slot primary-effective-association-end
                      :where-clause (make-association-end-matcher-reader-where-clause
                                     (id-column-name-for secondary-direct-association-end))
                      :transformer 'object-reader))
      ;; update brother set sister-id = (id-of sister) where id = (id-of brother)
      (writer
       (make-instance 'rdbms-accessor
                      :effective-slot primary-effective-association-end
                      :where-clause (make-value-oid-matcher-writer-where-clause
                                     (id-column-name-for secondary-direct-association-end))
                      :transformer 'self-or-nil-writer)))))

;; brother's sister slot
#+nil
(defun compute-secondary-1-1-binary-association-end-accessor (secondary-effective-association-end accessor-type)
  (ecase accessor-type
    ;; select sister-id from brother where id = (id-of brother)
    (reader
     (make-instance 'rdbms-accessor
                    :effective-slot secondary-effective-association-end
                    :where-clause 'oid-matcher-reader-where-clause
                    :transformer 'object-reader))
    ;; update brother set sister-id = (id-of sister) where id = (id-of brother)
    (writer
     (make-instance 'rdbms-accessor
                    :effective-slot secondary-effective-association-end
                    :where-clause 'oid-matcher-writer-where-clause
                    :transformer 'object-writer))))

;; the foreign key will be in the child table 
#+nil
(defun compute-1-n-binary-association-end-accessor (effective-association-end accessor-type)
  (if (eq (cardinality-kind-of effective-association-end) :1)
      (compute-parent-1-n-binary-association-end-accessor effective-association-end accessor-type)
      (compute-children-1-n-binary-association-end-accessor effective-association-end accessor-type)))

#+nil
(defun compute-parent-1-n-binary-association-end-accessor (parent-effective-association-end accessor-type)
  (ecase accessor-type
    (reader
     (make-instance 'rdbms-accessor
                    :effective-slot parent-effective-association-end
                    :where-clause 'oid-matcher-reader-where-clause
                    :transformer 'object-reader))
    (writer
     (make-instance 'rdbms-accessor
                    :effective-slot parent-effective-association-end
                    :where-clause 'oid-matcher-writer-where-clause
                    :transformer 'object-writer))))

#+nil
(defun compute-children-1-n-binary-association-end-accessor (children-effective-association-end accessor-type)
  (bind ((children-direct-association-end (most-generic-direct-slot-for children-effective-association-end))
	 (parent-direct-association-end (other-association-end-of children-direct-association-end)))
    (ecase accessor-type
      (reader
       (make-instance 'rdbms-accessor
                      :effective-slot children-effective-association-end
                      :where-clause (make-association-end-matcher-reader-where-clause
                                     (id-column-name-for parent-direct-association-end))
                      :transformer 'object-reader))
      (writer
       (make-instance 'rdbms-accessor
                      :effective-slot children-effective-association-end
                      :where-clause 'value-matcher-writer-where-clause
                      :transformer 'self-writer)))))

#+nil
(defun compute-m-n-binary-association-end-accessor (effective-association-end accessor-type)
  (bind ((direct-association-end (most-generic-direct-slot-for effective-association-end))
         (other-direct-association-end (other-association-end-of direct-association-end)))
    (ecase accessor-type
      (reader
       (make-instance 'rdbms-accessor
                      :effective-slot effective-association-end
                      :where-clause (make-association-end-matcher-reader-where-clause
                                     (id-column-name-for other-direct-association-end))
                      :transformer 'object-reader))
      (writer
       (make-instance 'rdbms-accessor
                      :effective-slot effective-association-end
                      :where-clause (make-association-end-matcher-writer-where-clause
                                     (id-column-name-for other-direct-association-end))
                      :transformer 'self-writer)))))
