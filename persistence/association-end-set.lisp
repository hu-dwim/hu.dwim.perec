(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP extension for association ends

(defmethod propagate-cache-changes ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-association-end-effective-slot-definition) new-value)
  (bind ((other-slot (other-association-end-of slot)))
    (cond ((eq (association-kind-of (association-of slot)) :1-1)
           ;; BEFORE
           ;; instance <-> old-other-instance
           ;; new-value <-> old-other-new-value
           ;; AFTER
           ;; old-other-instance -> nil
           ;; instance <-> new-value
           ;; old-other-new-value -> nil
           (when (slot-value-cached-p instance slot)
             (when-bind old-other-instance (and (underlying-slot-boundp-using-class class instance slot)
                                                (underlying-slot-value-using-class class instance slot))
               (when (slot-value-cached-p old-other-instance other-slot)
                 (setf (underlying-slot-value-using-class (class-of old-other-instance) old-other-instance other-slot)
                       nil))))
           (when (and new-value
                      (not (unbound-slot-value-p new-value))
                      (slot-value-cached-p new-value other-slot))
             (when-bind old-other-new-value
                 (and (underlying-slot-boundp-using-class (class-of new-value) new-value other-slot)
                      (underlying-slot-value-using-class (class-of new-value) new-value other-slot))
               (when old-other-new-value
                 (setf (underlying-slot-value-using-class (class-of old-other-new-value) old-other-new-value slot) nil)))
             (setf (underlying-slot-value-using-class (class-of new-value) new-value other-slot) instance)))
          ((eq (association-kind-of (association-of slot)) :1-n)
           ;; invalidate all cached back references 
           (if (eq (cardinality-kind-of slot) :n)
               (invalidate-cached-1-n-association-end-set-slot other-slot))))))

;; TODO: this is hell slow for huge transactions, what if this kind of caching is turned off after some limit?
(defun invalidate-cached-1-n-association-end-set-slot (slot)
  (bind ((class (slot-definition-class slot)))
    (map-cached-instances
     #L(when (typep !1 class)
         (invalidate-cached-slot !1 (find-slot (class-of !1) (slot-definition-name slot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lazy association end set containers

(defclass* persistent-association-end-set-container (persistent-slot-set-container)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1-n association end set

(defclass* persistent-1-n-association-end-set-container (persistent-association-end-set-container)
  ())

(defmethod insert-item :after ((set persistent-1-n-association-end-set-container) item)
  (bind ((slot (slot-of set))
         (class (class-of item))
         (other-slot (other-effective-association-end-for class slot)))
    (setf (underlying-slot-value-using-class class item other-slot) (instance-of set))))

(defmethod delete-item :after ((set persistent-1-n-association-end-set-container) item)
  (bind ((class (class-of item))
         (other-slot (other-effective-association-end-for class (slot-of set))))
    (setf (underlying-slot-value-using-class class item other-slot) nil)))

(defmethod empty! :after ((set persistent-1-n-association-end-set-container))
  (invalidate-cached-1-n-association-end-set-slot (other-association-end-of (slot-of set))))

(defmethod list-of ((set persistent-1-n-association-end-set-container))
  (restore-1-n-association-end-set (instance-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-1-n-association-end-set-container))
  (store-1-n-association-end-set (instance-of set) (slot-of set) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; m-n association end set

(defclass* persistent-m-n-association-end-set-container (persistent-association-end-set-container)
  ())

(defmethod insert-item ((set persistent-m-n-association-end-set-container) item)
  (insert-into-m-n-association-end-set (instance-of set) (slot-of set) item))

(defmethod delete-item ((set persistent-m-n-association-end-set-container) item)
  (bind ((slot (slot-of set))
         (other-slot (other-association-end-of slot))
         (instance (instance-of set)))
    (check-slot-type instance slot item)
    (delete-records (name-of (table-of (slot-of set)))
                    (sql-and (id-column-matcher-where-clause item (id-column-of slot))
                             (id-column-matcher-where-clause instance (id-column-of other-slot))))))

(defmethod size ((set persistent-m-n-association-end-set-container))
  (bind ((slot (slot-of set))
         (other-slot (other-association-end-of slot)))
    (elt-0-0 (execute (sql `(select (count *)
                             ,(name-of (table-of (slot-of set)))
                             ,(id-column-matcher-where-clause (instance-of set) (id-column-of other-slot))))))))

(defmethod empty! ((set persistent-m-n-association-end-set-container))
  (delete-m-n-association-end-set (instance-of set) (slot-of set)))

(defmethod list-of ((set persistent-m-n-association-end-set-container))
  (restore-m-n-association-end-set (instance-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-m-n-association-end-set-container))
  (store-m-n-association-end-set (instance-of set) (slot-of set) new-value))
