(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;
;; Store interface

(def generic insert-into-association-end-set-d (instance d-association-end item &key coordinates)
  (:documentation "TODO"))

(def generic delete-from-association-end-set-d (instance d-association-end item &key coordinates)
  (:documentation "TODO"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; d-s lazy association end set containers


(def class* persistent-association-end-set-container-d (persistent-association-end-set-container)
  ())

(def method check-insert-item ((set persistent-association-end-set-container-d) item)
  nil)

(def method insert-item ((set persistent-association-end-set-container-d) item)
  (insert-into-association-end-set-d (instance-of set) (slot-of set) item
                                     :coordinates (collect-coordinates-from-variables
                                                   (dimensions-of (slot-of set)))))

(def method check-delete-item ((set persistent-association-end-set-container-d) item)
  nil)

(def method delete-item ((set persistent-association-end-set-container-d) item)
  (delete-from-association-end-set-d (instance-of set) (slot-of set) item
                                     :coordinates (collect-coordinates-from-variables
                                                   (dimensions-of (slot-of set)))))

(def method find-item ((set persistent-association-end-set-container-d) (item persistent-object))
  (not-yet-implemented))

(def method ensure-item ((set persistent-association-end-set-container-d) (item persistent-object))
  (not-yet-implemented))

(def method size ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method empty-p ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method empty! ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method list-of ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method (setf list-of) (new-value (set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method iterate-items ((set persistent-association-end-set-container-d) fn)
  (not-yet-implemented))
