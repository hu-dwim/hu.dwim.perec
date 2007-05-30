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
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-keywords args :element-type)))

;; TODO: distinguish between set type and disjunct set type (the latter used in 1-n associations for example)
;; TODO: assert 1-n associations use disjunct-set type
(defptype disjunct-set (&optional element-type)
  (declare (ignore element-type))
  '(or list persistent-slot-set-container))

(defmethod shared-initialize :around ((type disjunct-set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-keywords args :element-type)))

(defun ordered-set-p (instance)
  (declare (ignore instance))
  #t)

(defptype ordered-set (&optional element-type by)
  (declare (ignore element-type by))
  '(and (satisfies ordered-set-p)
        (or list persistent-slot-set-container)))

(defmethod shared-initialize :around ((type ordered-set-type) slot-names &rest args &key element-type &allow-other-keys)
  (apply #'call-next-method type slot-names :element-type (parse-type element-type) (remove-keywords args :element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy slot set container

(defclass* persistent-slot-set-container (set-container)
  ((instance
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition)))

(defmethod insert-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    (object-writer (instance-of set))
                    (id-column-matcher-where-clause item))))

(defmethod delete-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    '(nil nil)
                    (id-column-matcher-where-clause item))))

(defmethod search-for-item ((set persistent-slot-set-container) (item persistent-object) &key &allow-other-keys)
  (not-yet-implemented))

(defmethod size ((set persistent-slot-set-container))
  (bind ((slot (slot-of set)))
    (caar (execute (sql `(select (count *)
                          ,(name-of (table-of slot))
                          ,(id-column-matcher-where-clause (instance-of set) (id-column-of slot))))))))

(defmethod empty-p ((set persistent-slot-set-container))
  (= 0 (size set)))

(defmethod empty! ((set persistent-slot-set-container))
  (delete-slot-set (instance-of set) (slot-of set)))

(defmethod list-of ((set persistent-slot-set-container))
  (restore-slot-set (instance-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-slot-set-container))
  (store-slot-set (instance-of set) (slot-of set) new-value))

(defmethod iterate-items ((set persistent-slot-set-container) fn)
  (mapc fn (list-of set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy persistent set with identity

(defpclass persistent-set ()
  ())

;; TODO: implement persistent set with identity (needs a separate table just like m-n associations to store references)

(defmethod insert-item ((set persistent-set) (item persistent-object))
  (not-yet-implemented))

(defmethod delete-item ((set persistent-set) (item persistent-object))
  (not-yet-implemented))

(defmethod search-for-item ((set persistent-set) (item persistent-object) &key &allow-other-keys)
  (not-yet-implemented))

(defmethod size ((set persistent-set))
  (not-yet-implemented))

(defmethod empty-p ((set persistent-set))
  (= 0 (size set)))

(defmethod empty! ((set persistent-set))
  (not-yet-implemented))

(defmethod list-of ((set persistent-set))
  (not-yet-implemented))

(defmethod (setf list-of) (new-value (set persistent-set))
  (not-yet-implemented))

(defmethod iterate-items ((set persistent-set) fn)
  (not-yet-implemented))
