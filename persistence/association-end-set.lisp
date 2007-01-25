(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP extension for association ends

(defmethod propagate-cache-changes ((class persistent-class)
                                    (object persistent-object)
                                    (slot persistent-association-end-effective-slot-definition) new-value)
  (debug-only (assert (debug-persistent-p object)))
  (bind ((other-slot (other-association-end-of slot)))
    (cond ((eq (association-kind-of (association-of slot)) :1-1)
           ;; BEFORE
           ;; object <-> old-other-object
           ;; new-value <-> old-other-new-value
           ;; AFTER
           ;; old-other-object -> nil
           ;; object <-> new-value
           ;; old-other-new-value -> nil
           (when (slot-value-cached-p object slot)
             (when-bind old-other-object (cached-slot-value-using-class class object slot)
               (when (slot-value-cached-p old-other-object other-slot)
                 (setf (cached-slot-value-using-class (class-of old-other-object) old-other-object other-slot) nil))))
           (when (and new-value
                      (slot-value-cached-p new-value other-slot))
             (when-bind old-other-new-value
                 (cached-slot-value-using-class (class-of new-value) new-value other-slot)
               (when old-other-new-value
                 (setf (cached-slot-value-using-class (class-of old-other-new-value) old-other-new-value slot) nil)))
             (setf (cached-slot-value-using-class (class-of new-value) new-value other-slot) object)))
          ((eq (association-kind-of (association-of slot)) :1-n)
           ;; invalidate all cached back references 
           (if (eq (cardinality-kind-of slot) :n)
               (invalidate-cached-1-n-association-end-set-slot other-slot))))))

(defun invalidate-cached-1-n-association-end-set-slot (slot)
  (bind ((class (slot-definition-class slot)))
    (iter (for (id object) in-hashtable (current-objects))
          (when (typep object class)
            (invalidate-cached-slot object (find-slot (class-of object) (slot-definition-name slot)))))))

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
         (other-slot (other-association-end-for class slot)))
    (setf (cached-slot-value-using-class class item other-slot) (object-of set))))

(defmethod delete-item :after ((set persistent-1-n-association-end-set-container) item)
  (bind ((class (class-of item))
         (other-slot (other-association-end-for class (slot-of set))))
    (setf (cached-slot-value-using-class class item other-slot) nil)))

(defmethod empty! :after ((set persistent-1-n-association-end-set-container))
  (invalidate-cached-1-n-association-end-set-slot (other-association-end-of (slot-of set))))

(defmethod list-of ((set persistent-1-n-association-end-set-container))
  (restore-1-n-association-end-set (object-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-1-n-association-end-set-container))
  (store-1-n-association-end-set (object-of set) (slot-of set) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; m-n association end set

(defclass* persistent-m-n-association-end-set-container (persistent-association-end-set-container)
  ())

(defmethod insert-item ((set persistent-m-n-association-end-set-container) item)
  (insert-into-m-n-association-end-set (object-of set) (slot-of set) item))

(defmethod delete-item ((set persistent-m-n-association-end-set-container) item)
  (bind ((slot (slot-of set))
         (other-slot (other-association-end-of slot)))
    (delete-records (name-of (table-of (slot-of set)))
                    (sql-and (id-column-matcher-where-clause item (id-column-of slot))
                             (id-column-matcher-where-clause (object-of set) (id-column-of other-slot))))))

(defmethod size ((set persistent-m-n-association-end-set-container))
  (caar (execute (sql `(select (count *)
                        ,(name-of (table-of (slot-of set)))
                        ,(id-column-matcher-where-clause (object-of set) (id-column-of (slot-of set))))))))

(defmethod empty! ((set persistent-m-n-association-end-set-container))
  (delete-m-n-association-end-set (object-of set) (slot-of set)))

(defmethod list-of ((set persistent-m-n-association-end-set-container))
  (restore-m-n-association-end-set (object-of set) (slot-of set)))

(defmethod (setf list-of) (new-value (set persistent-m-n-association-end-set-container))
  (store-m-n-association-end-set (object-of set) (slot-of set) new-value))


