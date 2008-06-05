(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;
;; Store interface

(defgeneric insert-into-association-end-set-t (instance t-association-end item)
  (:documentation "TODO"))

(defgeneric delete-from-association-end-set-t (instance t-association-end item)
  (:documentation "TODO"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; t-s lazy association end set containers


(defclass* persistent-association-end-set-container-t (persistent-association-end-set-container)
  ())

;;; empty-p, ensure-item, iterate-items inherited

(defmethod insert-item ((set persistent-association-end-set-container-t) item)
  (insert-into-association-end-set-t (instance-of set) (slot-of set) item))

(defmethod delete-item ((set persistent-association-end-set-container-t) item)
  (delete-from-association-end-set-t (instance-of set) (slot-of set) item))

(defmethod find-item ((set persistent-association-end-set-container-t) (item persistent-object))
  (not-yet-implemented))

(defmethod size ((set persistent-association-end-set-container-t))
  (not-yet-implemented))

(defmethod empty! ((set persistent-association-end-set-container-t))
  (not-yet-implemented))

(defmethod list-of ((set persistent-association-end-set-container-t))
  (not-yet-implemented))

(defmethod (setf list-of) (new-value (set persistent-association-end-set-container-t))
  (not-yet-implemented))

