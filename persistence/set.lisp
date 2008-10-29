;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;
;;; Set

;; TODO: enforce disjunct-set in defassociation for 1-N associations mapped as foreign keys in the RDBMS?

;; TODO: set types need some refactoring, to have a common base type and to be less fragile
(defptype set (&optional element-type)
  (declare (ignore element-type))
  '(or list persistent-slot-set-container))

(defmethod shared-initialize :around ((type set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-from-plist args :element-type)))

;; TODO: distinguish between set type and disjunct set type (the latter used in 1-n associations for example)
;; TODO: assert 1-n associations use disjunct-set type
(defptype disjunct-set (&optional element-type)
  (declare (ignore element-type))
  '(or list persistent-slot-set-container))

(defmethod shared-initialize :around ((type disjunct-set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-from-plist args :element-type)))

(defun ordered-set-p (instance)
  (declare (ignore instance))
  #t)

(defptype ordered-set (&optional element-type by)
  (declare (ignore element-type by))
  '(and (satisfies ordered-set-p)
        (or list persistent-slot-set-container)))

(defmethod shared-initialize :around ((type ordered-set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-from-plist args :element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy slot set container

(defclass* persistent-slot-set-container (set-container)
  ((instance
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition)))

(def generic check-insert-item (set item)
  (:method ((set persistent-slot-set-container) item)
    (when (find-item set item)
      (error "The item ~A is already in the association end set ~A" item set))))

(defmethod insert-item :before ((set persistent-slot-set-container) item)
  (check-insert-item set item))

(defmethod insert-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set))
         (instance (instance-of set)))
    ;; TODO: this is incorrect, add test?
    (check-slot-value-type instance slot item)
    (let ((rdbms-values (make-array +oid-column-count+)))
      (object-writer instance rdbms-values 0)
      (update-records (name-of (table-of slot))
                      (columns-of slot)
                      rdbms-values
                      (make-oid-matcher-where-clause item)))))

(defmethod ensure-item ((set persistent-slot-set-container) (item persistent-object))
  (unless (find-item set item)
    (insert-item set item)))

(def generic check-delete-item (set item)
  (:method ((set persistent-slot-set-container) item)
    (unless (find-item set item)
      (error "The item ~A is not in the association end set ~A" item set))))

(defmethod delete-item :before ((set persistent-slot-set-container) item)
  (check-delete-item set item))

(defmethod delete-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (check-slot-value-type (instance-of set) slot item)
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    '(nil nil)
                    (make-oid-matcher-where-clause item))))

(defmethod find-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (not (zerop (select-count-* (list (name-of (table-of slot)))
                                (sql-and (make-oid-matcher-where-clause (instance-of set) (oid-column-of slot))
                                         (make-oid-matcher-where-clause item)))))))

(defmethod size ((set persistent-slot-set-container))
  (bind ((slot (slot-of set)))
    (select-count-* (list (name-of (table-of slot)))
                    (make-oid-matcher-where-clause (instance-of set) (oid-column-of slot)))))

(defmethod empty-p ((set persistent-slot-set-container))
  (= 0 (size set)))

(defmethod empty! ((set persistent-slot-set-container))
  (delete-slot-set (instance-of set) (slot-of set)))

(defmethod list-of ((set persistent-slot-set-container))
  (restore-slot-set (instance-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-slot-set-container))
  (store-slot-set (instance-of set) (slot-of set) new-value))

(defmethod iterate-items ((set persistent-slot-set-container) function)
  (mapc function (list-of set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy persistent set with identity

(defpclass persistent-set-element ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defpclass persistent-set ()
  ())

(defassociation*
  ((:class persistent-set :slot items :type (set persistent-set-element))
   (:class persistent-set-element :slot sets :type (set persistent-set))))

(defmethod insert-item ((set persistent-set) (item persistent-set-element))
  (insert-item (items-of* set) item))

(defmethod delete-item ((set persistent-set) (item persistent-set-element))
  (delete-item (items-of* set) item))

(defmethod find-item ((set persistent-set) (item persistent-set-element))
  (find-item (items-of* set) item))

(defmethod size ((set persistent-set))
  (with-lazy-slot-value-collections
    (size (items-of set))))

(defmethod empty-p ((set persistent-set))
  (= 0 (size set)))

(defmethod empty! ((set persistent-set))
  (with-lazy-slot-value-collections
    (empty! (items-of set))))

(defmethod list-of ((set persistent-set))
  (items-of set))

(defmethod (setf list-of) (items (set persistent-set))
  (setf (items-of set) items))

(defmethod iterate-items ((set persistent-set) function)
  (mapc function (items-of set)))
