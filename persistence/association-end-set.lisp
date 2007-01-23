(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP extension for association ends

#+nil
(defgeneric association-end-value (object association-end-name)
  (:method ((object persistent-object)
            (association-end-name symbol))
           (bind ((class (class-of object))
                  (association-end (find-slot class association-end-name)))
             (association-end-value-using-class class object association-end))))

#+nil
(defgeneric association-end-value-using-class (class object association-end) 
  (:method ((class persistent-class)
            (object persistent-object)
            (association-end effective-binary-association-end))
           (cond ((and (eq (association-kind-of (association-of association-end)) :1-n)
                       (eq (cardinality-kind-of association-end) :n))
                  (make-instance 'rdbms-1-n-association-end-set-container
                                 :object object
                                 :effective-association-end association-end))
                 ((eq (association-kind-of (association-of association-end)) :m-n)
                  (make-instance 'rdbms-m-n-association-end-set-container
                                 :object object
                                 :effective-association-end association-end))
                 (t
                  (error "Unknown association end type")))))

#+nil
(defmethod propagate-cache-changes ((class persistent-class)
                                    (object persistent-object)
                                    (slot effective-association-end) new-value)
  (debug-only (assert (debug-persistent-p object)))
  (bind ((other-slot (most-generic-other-effective-association-end-for slot)))
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
;;;;;;;;;;;;;;;;;;;
;;; Lazy containers

#+nil
(defclass* rdbms-association-end-set-container (set-container)
  ((object)
   (effective-association-end)))

#+nil
(defclass* rdbms-1-n-association-end-set-container (rdbms-association-end-set-container)
  ())

#+nil
(defclass* rdbms-m-n-association-end-set-container (rdbms-association-end-set-container)
  ())

#+nil
(defmethod insert-item ((set rdbms-1-n-association-end-set-container) item)
  (bind ((slot (effective-association-end-of set))
         (other-slot (most-generic-other-effective-association-end-for slot)))
    (insert-into-1-n-association-end-set (object-of set) slot item)
    (setf (cached-slot-value-using-class (class-of item) item other-slot) (object-of set))))

#+nil
(defmethod insert-item ((set rdbms-m-n-association-end-set-container) item)
  (insert-into-m-n-association-end-set (object-of set) (effective-association-end-of set) item))

#+nil
(defmethod size ((set rdbms-association-end-set-container))
  (caar (execute (sql `(select (count *)
                        ,(name-of (table-of (effective-association-end-of set)))
                        ,(funcall (where-clause-of (reader-of (effective-association-end-of set))) (object-of set)))))))

#+nil
(defmethod empty-p ((set rdbms-1-n-association-end-set-container))
  (= 0 (size set)))

#+nil
(defun invalidate-cached-1-n-association-end-set-slot (slot)
  (bind ((class (owner-class-of slot)))
    (iter (for (id object) in-hashtable (current-objects))
          (when (typep object class)
            (invalidate-cached-slot object (find-effective-slot (class-of object) (name-of slot)))))))

#+nil
(defmethod empty! ((set rdbms-1-n-association-end-set-container))
  (bind ((slot (effective-association-end-of set))
         (other-slot (most-generic-other-effective-association-end-for slot)))
    (delete-1-n-association-end-set (object-of set) slot)
    (invalidate-cached-1-n-association-end-set-slot other-slot)))

#+nil
(defmethod empty! ((set rdbms-m-n-association-end-set-container))
  (delete-m-n-association-end-set (object-of set) (effective-association-end-of set)))

#+nil
(defmethod list-of ((set rdbms-1-n-association-end-set-container))
  (load-1-n-association-end-set (object-of set) (effective-association-end-of set)))

#+nil
(defmethod list-of ((set rdbms-m-n-association-end-set-container))
  (load-m-n-association-end-set (object-of set) (effective-association-end-of set)))

#+nil
(defmethod (setf list-of) (new-value (set rdbms-1-n-association-end-set-container))
  (store-1-n-association-end-set (object-of set) (effective-association-end-of set) new-value))

#+nil
(defmethod (setf list-of) (new-value (set rdbms-m-n-association-end-set-container))
  (store-m-n-association-end-set (object-of set) (effective-association-end-of set) new-value))

#+nil
(defmethod iterate-nodes ((set rdbms-association-end-set-container) fn)
  (mapc fn (list-of set)))

#+nil
(defmethod delete-item ((set rdbms-1-n-association-end-set-container) item)
  (bind ((slot (effective-association-end-of set))
         (other-slot (most-generic-other-effective-association-end-for slot)))
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    '(nil nil)
                    (funcall (where-clause-of (writer-of slot)) (object-of set) item))
    (setf (cached-slot-value-using-class (class-of item) item other-slot) nil)))

#+nil
(defmethod delete-item ((set rdbms-m-n-association-end-set-container) item)
  (bind ((slot (effective-association-end-of set))
         (other-slot (most-generic-other-effective-association-end-for slot)))
    (delete-records (name-of (table-of (effective-association-end-of set)))
                    (sql-and (funcall (where-clause-of (writer-of slot)) (object-of set) item)
                             (funcall (where-clause-of (writer-of other-slot)) item (object-of set))))))
