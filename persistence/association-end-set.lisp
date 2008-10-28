(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP extension for association ends

(defmethod propagate-cache-changes ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-association-end-effective-slot-definition) new-value)
  (bind ((association-kind (association-kind-of (association-of slot))))
    (case association-kind
      (:1-1
       ;; BEFORE
       ;; instance <-> old-other-instance
       ;; new-value <-> old-other-new-value
       (flet ((update-cached-slot-value-in-other-instance (instance slot)
                (bind (((:values cache-p other-instance) (slot-value-cached-p instance slot)))
                  (when (and cache-p
                             other-instance
                             (not (unbound-slot-marker-p other-instance)))
                    (bind ((other-class (class-of other-instance))
                           (other-slot (other-effective-association-end-for other-class slot)))
                      (setf (underlying-slot-value-using-class other-class other-instance other-slot) nil))))))
         ;; old-other-instance -> nil
         (update-cached-slot-value-in-other-instance instance slot)
         (when (and new-value
                    (not (unbound-slot-marker-p new-value)))
           (bind ((new-value-class (class-of new-value))
                  (new-other-slot (other-effective-association-end-for new-value-class slot)))
             ;; old-other-new-value -> nil
             (update-cached-slot-value-in-other-instance new-value new-other-slot)
             ;; new-value -> instance
             (setf (underlying-slot-value-using-class new-value-class new-value new-other-slot) instance)))))
      (:1-n
       ;; invalidate all cached back references 
       (if (eq (cardinality-kind-of slot) :n)
           (bind ((other-slot (other-association-end-of slot))
                  ((:values cache-p old-slot-value) (slot-value-cached-p instance slot)))
             (if cache-p
                 (unless (unbound-slot-marker-p old-slot-value)
                   (dolist (child old-slot-value)
                     ;; FIXME: other-slot must be from the type (class-of child) use slot-name instead?
                     (invalidate-cached-slot child other-slot)))
                 (invalidate-cached-1-n-association-end-set-slot other-slot))))))))

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
    (check-slot-value-type instance slot item)
    (delete-records (name-of (table-of (slot-of set)))
                    (sql-and (make-oid-matcher-where-clause item (oid-column-of slot))
                             (make-oid-matcher-where-clause instance (oid-column-of other-slot))))))

(defmethod find-item ((set persistent-m-n-association-end-set-container) (item persistent-object))
  (bind ((association-end (slot-of set))
         (other-association-end (other-association-end-of association-end)))
    (not (zerop (select-count-* (list (name-of (table-of association-end)))
                                (sql-and (make-oid-matcher-where-clause (instance-of set)
                                                                        (oid-column-of other-association-end))
                                         (make-oid-matcher-where-clause item
                                                                        (oid-column-of association-end))))))))

(defmethod size ((set persistent-m-n-association-end-set-container))
  (bind ((slot (slot-of set))
         (other-slot (other-association-end-of slot)))
    (select-count-* (list (name-of (table-of (slot-of set))))
                    (make-oid-matcher-where-clause (instance-of set) (oid-column-of other-slot)))))

(defmethod empty! ((set persistent-m-n-association-end-set-container))
  (delete-m-n-association-end-set (instance-of set) (slot-of set)))

(defmethod list-of ((set persistent-m-n-association-end-set-container))
  (restore-m-n-association-end-set (instance-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-m-n-association-end-set-container))
  (store-m-n-association-end-set (instance-of set) (slot-of set) new-value))
