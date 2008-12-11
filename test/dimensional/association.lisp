;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(def test test/dimensional/association/cache (association &key (size 10))
  (bind ((primary-class (prc::slot-definition-class (prc::primary-association-end-of association)))
         (secondary-class (prc::slot-definition-class (prc::secondary-association-end-of association)))
         (primary-instances)
         (secondary-instances))

    (generate-random-graph association :size size)
    (with-transaction
      (setf primary-instances (select (instance) (from instance) (where (typep instance primary-class)))
            secondary-instances (select (instance) (from instance) (where (typep instance secondary-class))))
      (cache-dimensional-association association))  ; FIXME non-dimensional
    (check-graphs-are-equal primary-instances secondary-instances association)))

(def function generate-random-graph (association &key (size 10))
  (bind ((primary-association-end (prc::primary-association-end-of association))
         (secondary-association-end (prc::secondary-association-end-of association))
         (primary-slot-name (slot-definition-name primary-association-end))
         (primary-class (prc::slot-definition-class primary-association-end))
         (secondary-class (prc::slot-definition-class secondary-association-end))
         (primary-instances)
         (secondary-instances))
    (with-transaction
      (purge-instances primary-class)
      (purge-instances secondary-class)
      (iter (repeat size)
            (push (make-instance primary-class) primary-instances)
            (push (make-instance secondary-class) secondary-instances))
      (iter (repeat (/ (* size size) 2))
            (for primary-instance = (random-elt primary-instances))
            (for secondary-instance = (random-elt secondary-instances))
            (ecase (prc::cardinality-kind-of primary-association-end)
              (:1 (setf (slot-value primary-instance primary-slot-name) secondary-instance))
              (:n (bind ((prc::*lazy-slot-value-collections* #t))
                    (ensure-item (slot-value primary-instance primary-slot-name) secondary-instance))))))))

(def function check-graphs-are-equal (primary-instances secondary-instances association)
  (bind ((primary-slot-name (slot-definition-name (prc::primary-association-end-of association)))
         (secondary-slot-name (slot-definition-name (prc::secondary-association-end-of association))))
    (labels ((check (instances slot-name)
               (iter (for instance :in instances)
                     (for cached-value = (prc::underlying-slot-value instance slot-name))
                     (for loaded-instance = (load-instance instance))
                     (for loaded-value = (slot-value loaded-instance slot-name))
                     (is (equal-sets cached-value loaded-value)))))
      (with-transaction
        (check primary-instances primary-slot-name)
        (check secondary-instances secondary-slot-name)))))

(def function equal-sets (value-1 value-2)
  (or (and (listp value-1) (listp value-2)
           (null (set-exclusive-or value-1 value-2 :test #'p-eq)))
      (and (prc::persistent-object-p value-1) (prc::persistent-object-p value-2)
           (p-eq value-1 value-2))
      (and (d-value-p value-1) (d-value-p value-2)
           (d-value-equal value-1 value-2 :test #'equal-sets))))