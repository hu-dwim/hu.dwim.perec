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
  t)

(defmethod compute-reader ((type (eql 'set)) &optional type-specification)
  (declare (ignorable type-specification))
  'object-reader)

(defmethod compute-writer ((type (eql 'set)) &optional type-specification)
  (declare (ignorable type-specification))
  'self-writer)

;;;;;;;;;;;;;;;;;;
;;; Lazy container

(defclass* persistent-set-container (set-container)
  ((object)
   (slot)))

(defmethod insert-item ((set persistent-set-container) (item persistent-object))
  (insert-into-slot-set (object-of set) (slot-of set) item))

(defmethod delete-item ((set persistent-set-container) (item persistent-object))
  (delete-from-slot-set (slot-of set) item))

(defmethod search-for-item ((set persistent-set-container) (item persistent-object) &key &allow-other-keys)
  (not-yet-implemented))

(defmethod size ((set persistent-set-container))
  (size-of-slot-set (object-of set) (slot-of set)))

(defmethod empty-p ((set persistent-set-container))
  (= 0 (size set)))

(defmethod empty! ((set persistent-set-container))
  (delete-slot-set (object-of set) (slot-of set)))

(defmethod list-of ((set persistent-set-container))
  (restore-slot-set (object-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-set-container))
  (store-slot-set (object-of set) (slot-of set) new-value))

(defmethod iterate-items ((set persistent-set-container) fn)
  (mapc fn (list-of set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent set with identity

(defpclass persistent-set ()
  ())

;; TODO:
