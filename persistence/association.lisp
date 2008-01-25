(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent association and slot meta objects

;; TODO: make sure that 1-1 and 1-n associations both store the foreign key in the primary-association-end's table 

(defcclass* persistent-association (exportable)
  ((name
    :type symbol
    :documentation "Unique name of the association. This name can be used to find the association using find-association.")
   (association-end-definitions
    (compute-as nil)
    :type list
    :documentation "Canonical form of the persistent association end direct slot definitions.")
   (primary-association-end
    (compute-as nil)
    :type persistent-association-end-direct-slot-definition)
   (secondary-association-end
    (compute-as nil)
    :type persistent-association-end-direct-slot-definition)
   (association-ends
    (compute-as (list (primary-association-end-of -self-) (secondary-association-end-of -self-)))
    :type (list persistent-association-end-direct-slot-definition))
   (associated-classes
    (compute-as (list (find-class (getf (first (association-end-definitions-of -self-)) :class))
                      (find-class (getf (second (association-end-definitions-of -self-)) :class))))
    :type (list persistent-class))
   (association-kind
    (compute-as (let ((cardinality-kinds (mapcar 'cardinality-kind-of (association-ends-of -self-))))
                  (cond ((equal cardinality-kinds '(:1 :1)) :1-1)
                        ((equal cardinality-kinds '(:n :n)) :m-n)
                        (t :1-n))))
    :type symbol
    :documentation "Valid values are :1-1, :1-n or :m-n according to association end cardinalities.")
   (primary-table
    (compute-as (compute-primary-table -self- -current-value-))
    :type table
    :documentation "The table which holds the oids of the associated instances.")))

(defcclass* persistent-association-end-slot-definition (persistent-slot-definition)
  ((association
    (compute-as nil)
    :type persistent-association)
   (associated-class
    (compute-as (bind ((type (canonical-type-of -self-)))
                  (find-class (if (set-type-p* type)
                                  (set-type-class-for type)
                                  (persistent-class-type-for type)))))
    :type persistent-class)
   (association-end-query
    (compute-as (compute-association-end-query -self-))
    :type t)
   (min-cardinality
    :type integer
    :documentation "The minimum number of instances present in an association for this end.")
   (max-cardinality
    :type integer
    :documentation "The maximum number of instances present in an association for this end. Unbound means the maximum number is not defined.")
   (cardinality-kind
    (compute-as (if (and (slot-boundp -self- 'max-cardinality)
                         (eq (max-cardinality-of -self-) 1))
                    :1
                    :n))
    :type symbol
    :documentation "Valid values are :1, :n according to min a max cardinality.")
   (primary-association-end
    (compute-as (eq (slot-definition-name -self-)
                    (slot-definition-name (primary-association-end-of (association-of -self-)))))
    :type boolean
    :documentation "True iff this end is the primary association end of its association.")
   (secondary-association-end
    (compute-as (eq (slot-definition-name -self-)
                    (slot-definition-name (secondary-association-end-of (association-of -self-)))))
    :type boolean
    :documentation "True iff this end is the secondary association end of its association.")))

(defcclass* persistent-association-end-direct-slot-definition
    (persistent-association-end-slot-definition persistent-direct-slot-definition)
  ((min-cardinality
    (compute-as (bind ((type (canonical-type-of -self-)))
                  (if (and (not (null-subtype-p type))
                           (not (unbound-subtype-p type)))
                      1
                      0))))
   (max-cardinality
    (compute-as (if (set-type-p* (canonical-type-of -self-))
                    :n
                    1)))
   (other-association-end
    (compute-as (if (primary-association-end-p -self-)
                    (secondary-association-end-of (association-of -self-))
                    (primary-association-end-of (association-of -self-))))
    :type persistent-association-end-direct-slot-definition))
  (:metaclass identity-preserving-class))

(defcclass* persistent-association-end-effective-slot-definition
    (persistent-association-end-slot-definition persistent-effective-slot-definition)
  ((min-cardinality
    (compute-as (apply #'max (mapcar #'min-cardinality-of (direct-slots-of -self-)))))
   (max-cardinality
    (compute-as (apply #'min* (mapcar #'max-cardinality-of (direct-slots-of -self-)))))
   (other-association-end
    (compute-as (bind ((class (associated-class-of (first (direct-slots-of -self-)))))
                  (ensure-finalized class)
                  (other-effective-association-end-for class -self-)))
    :type persistent-association-end-direct-slot-definition)))

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((association persistent-association))
  (mapc #'ensure-exported (remove-if #'null (mapcar #'primary-table-of (associated-classes-of association))))
  (awhen (primary-table-of association)
    (ensure-exported it)))

;;;;;;;;;;;
;;; Compute

(defmethod compute-slot-mapping ((slot persistent-association-end-effective-slot-definition))
  (when (eq (cardinality-kind-of slot) :1)
    (call-next-method)))

(defmethod compute-slot-reader ((slot persistent-association-end-effective-slot-definition))
  (when (eq (cardinality-kind-of slot) :1)
    (call-next-method)))

(defmethod compute-slot-writer ((slot persistent-association-end-effective-slot-definition))
  (when (eq (cardinality-kind-of slot) :1)
    (call-next-method)))

(defmethod compute-primary-table ((association persistent-association) current-table)
  (when (eq (association-kind-of association) :m-n)
    (make-instance 'association-primary-table
                   :name (rdbms-name-for (name-of association) :table)
                   :columns (compute-as
                              (mappend #'columns-of
                                       (mapcar #'effective-association-end-for (association-ends-of association)))))))

(defmethod compute-primary-class ((slot persistent-association-end-effective-slot-definition))
  (bind ((association (association-of slot)))
    (ecase (association-kind-of association)
      (:1-1 (if (primary-association-end-p slot)
                (call-next-method)
                (primary-class-of (other-association-end-of slot))))
      (:1-n (if (eq :1 (cardinality-kind-of slot))
                (call-next-method)
                (primary-class-of (other-association-end-of slot))))
      (:m-n nil))))

(defmethod compute-table ((slot persistent-association-end-effective-slot-definition))
  (bind ((association (association-of slot)))
    (if (eq :m-n (association-kind-of association))
        (primary-table-of association)
        (call-next-method))))

(defmethod compute-columns ((slot persistent-association-end-effective-slot-definition))
  (bind ((association (association-of slot)))
    (ecase (association-kind-of association)
      (:1-1 (if (primary-association-end-p slot)
                (call-next-method)
                (columns-of (other-association-end-of slot))))
      (:1-n (if (eq :1 (cardinality-kind-of slot))
                (call-next-method)
                (columns-of (other-association-end-of slot))))
      (:m-n (make-columns-for-reference-slot (class-name (slot-definition-class slot))
                                             (strcat (slot-definition-name slot)
                                                     "-for-"
                                                     (set-type-class-for (canonical-type-of slot))))))))

(defmethod compute-data-table-slot-p ((slot persistent-association-end-effective-slot-definition))
  (bind ((association (association-of slot)))
    (ecase (association-kind-of association)
      (:1-1 (if (primary-association-end-p slot)
                (call-next-method)
                #f))
      (:1-n (if (eq :1 (cardinality-kind-of slot))
                (call-next-method)
                #f))
      (:m-n #f))))

(defgeneric compute-association-end-query (association-end))

(defcclass* association-primary-table (table)
  ()
  (:documentation "This is a special table related to a persistent association."))

;;;;;;;;;;;
;;; Utility

(defparameter *persistent-associations* (make-hash-table)
  "A mapping from association names to association instances.")

(def function find-association (name)
  (gethash name *persistent-associations*))

(def function (setf find-association) (new-value name)
  (setf (gethash name *persistent-associations*) new-value))

(def function finalize-persistent-associations ()
  (iter (for (association-name association) :in-hashtable *persistent-associations*)
        (ensure-all-computed-slots-are-valid association)))

(def (function io) to-one-association-end-p (association-end)
  (eq (cardinality-kind-of association-end) :1))

(def (function io) to-many-association-end-p (association-end)
  (eq (cardinality-kind-of association-end) :n))

(def (function io) effective-association-end-for (direct-association-end)
  (find-slot (slot-definition-class direct-association-end) (slot-definition-name direct-association-end)))

(def (function io) other-effective-association-end-for (class effective-slot)
  (find-slot class (slot-definition-name (some #'other-association-end-of (direct-slots-of effective-slot)))))

(defun association-end-accessor-p (name)
  (and (symbolp name)
       (effective-association-ends-for-accessor name)))

(defun effective-association-ends-for-accessor (name)
  (collect-if #L(typep !1 'persistent-association-end-effective-slot-definition)
              (effective-slots-for-accessor name)))

(defun min* (&rest args)
  (if (find-if #'numberp args)
      (apply #'min (delete-if-not #'numberp args))
      :n))
