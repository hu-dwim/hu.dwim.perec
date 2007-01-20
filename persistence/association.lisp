(in-package :cl-perec)

(defclass* persistent-association ()
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
                        ((equal cardinality-kinds '(:n :n)) :n-n)
                        (t :1-n))))
    :type symbol
    :documentation "Valid values are :1-1, :1-n or :n-n according to association end cardinalities."))
  (:metaclass computed-class))

(defclass* persistent-association-end-slot-definition (persistent-slot-definition)
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
    :documentation "True iff this end is the secondary association end of its association."))
  (:metaclass computed-class))

(defclass* persistent-association-end-direct-slot-definition
    (persistent-association-end-slot-definition persistent-direct-slot-definition)
  ()
  (:metaclass computed-class))

(defclass* persistent-association-end-effective-slot-definition
    (persistent-association-end-slot-definition persistent-effective-slot-definition)
  ((id-column
    (compute-as nil)
    :type sql-column))
  (:metaclass computed-class))
