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

(defmethod compute-reader-transformer ((type (eql 'set)) &optional type-specification)
  (declare (ignorable type-specification))
  'object-reader)

(defmethod compute-writer-transformer ((type (eql 'set)) &optional type-specification)
  (declare (ignorable type-specification))
  'self-writer)

;;;;;;;;;;;;;;;;;;
;;; Lazy container

(defclass* persistent-set-container (set-container)
  ((object)
   (slot)))

(defmethod insert-item ((set persistent-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (insert-into-set (object-of set) slot item)))

(defmethod delete-item ((set persistent-set-container) (item persistent-object))
  (bind ((slot (slot-of set)))
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    '(nil nil)
                    (funcall (where-clause-of (writer-of slot)) (object-of set) item))))

(defmethod search-for-item ((set persistent-set-container) (item persistent-object) &key &allow-other-keys)
  (not-yet-implemented))

(defmethod size ((set persistent-set-container))
  (caar (execute (sql `(select (count *)
                        ,(name-of (table-of (slot-of set)))
                        ,(funcall (where-clause-of (reader-of (slot-of set))) (object-of set)))))))

(defmethod empty-p ((set persistent-set-container))
  (= 0 (size set)))

(defmethod empty! ((set persistent-set-container))
  (bind ((slot (slot-of set)))
    (delete-set (object-of set) slot)))

(defmethod list-of ((set persistent-set-container))
  (restore-set (object-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-set-container))
  (store-set (object-of set) (slot-of set) new-value))

(defmethod iterate-items ((set persistent-set-container) fn)
  (mapc fn (list-of set)))
