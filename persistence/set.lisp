;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;
;;; Set

(deftype set (&optional type)
  (declare (ignore type))
  '(and (not unbound)
        (not null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy slot set container

(defclass* persistent-slot-set-container (set-container)
  ((object)
   (slot)))

(defmethod insert-item ((set persistent-slot-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    (object-writer (object-of set))
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
                          ,(id-column-matcher-where-clause (object-of set) (id-column-of slot))))))))

(defmethod empty-p ((set persistent-slot-set-container))
  (= 0 (size set)))

(defmethod empty! ((set persistent-slot-set-container))
  (delete-slot-set (object-of set) (slot-of set)))

(defmethod list-of ((set persistent-slot-set-container))
  (restore-slot-set (object-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-slot-set-container))
  (store-slot-set (object-of set) (slot-of set) new-value))

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
