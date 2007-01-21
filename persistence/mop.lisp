;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;
;;; MOP methods 

;; allows persistent keyword argument for persistent-direct-slot-definitions according to CLOS mop
(defmethod shared-initialize :around ((slot persistent-direct-slot-definition) slot-names
                                      &rest args &key persistent &allow-other-keys)
  (declare (ignore persistent))
  (apply #'call-next-method slot slot-names args))

;; TODO: inject association ends
(defmethod initialize-instance :around ((class persistent-class)
                                        &rest args
                                        &key name direct-slots direct-superclasses
                                        &allow-other-keys)
  (let ((result (apply #'call-next-method
                       class
                       :direct-slots (process-direct-slot-definitions direct-slots)
                       :direct-superclasses (ensure-persistent-object-class name direct-superclasses)
                       #+lispworks :optimize-slot-access #+lispworks nil 
                       (remove-keywords args :direct-slots))))
    (invalidate-computed-slot class 'persistent-direct-slots)
    result))

;; TODO: inject association ends
(defmethod reinitialize-instance :around ((class persistent-class)
                                          &rest args
                                          &key direct-slots direct-superclasses
                                          &allow-other-keys)
  (let ((result (apply #'call-next-method
                       class
                       :direct-slots (process-direct-slot-definitions direct-slots)
                       :direct-superclasses (ensure-persistent-object-class (class-name class) direct-superclasses)
                       #+lispworks :optimize-slot-access #+lispworks nil
                       (remove-keywords args :direct-slots))))
    (invalidate-computed-slot class 'persistent-direct-slots)
    result))

(defmethod validate-superclass ((class standard-class)
                                (superclass persistent-class))
  t)

(defmethod validate-superclass ((class persistent-class)
                                (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class)
                                         &key persistent &allow-other-keys)
  (if persistent
      (find-class 'persistent-direct-slot-definition)
      (call-next-method)))

(defmethod effective-slot-definition-class ((class persistent-class)
                                            &key &allow-other-keys)
  (declare (special %persistent%))
  (if %persistent%
      (find-class 'persistent-effective-slot-definition)
      (call-next-method)))

(defmethod compute-effective-slot-definition ((class persistent-class)
                                              slot-name
                                              direct-slot-definitions)
  ;; the special %persistent% will be passed down to effective-slot-definition-class
  ;; there's no standard way to call the default slot option merge algorithm, so we
  ;; cannot extend it with our own in a nice way
  ;; the persistent flag is t for an effective slot if any of its direct slots is persistent
  (let* ((%persistent% (some (lambda (slot)
                               (typep slot 'persistent-direct-slot-definition))
                             direct-slot-definitions))
         (effective-slot-definition (call-next-method)))
    (declare (special %persistent%))
    (when %persistent%
      (setf (direct-slots-of effective-slot-definition) direct-slot-definitions))
    effective-slot-definition))

(defmethod compute-slots :after ((class persistent-class))
  "Invalidates the cached slot value of persistent-effective-slots whenever the effective slots are recomputed, so that all dependent computed state will be invalidated and recomputed when requested."
  (invalidate-computed-slot class 'persistent-effective-slots))

;;;;;;;;;;;;;;;;;;
;;; Helper methods

(defun ensure-persistent-object-class (name direct-superclasses)
  (unless (eq 'persistent-object name)
    (let ((persistent-object (find-class 'persistent-object))
          (persistent-class (find-class 'persistent-class)))
      (if (find-if (lambda (direct-superclass)
                     (member persistent-class
                             (compute-class-precedence-list
                              (class-of direct-superclass))))
                   direct-superclasses)
          direct-superclasses
          (cons persistent-object direct-superclasses)))))

(defun process-direct-slot-definitions (direct-slots)
  (loop for direct-slot :in direct-slots
        collect (if (getf direct-slot :persistent)
                    direct-slot
                    (if (get-properties direct-slot '(:persistent))
                        ;; remove :persistent nil
                        (remove-keywords direct-slot :persistent)
                        ;; add default :persistent t
                        (append direct-slot '(:persistent t))))))










#+nil(defmethod compute-effective-slot-definition ((class persistent-class) name direct-slot-definitions)
  (bind (((direct-slots non-direct-slots) (partitionx direct-slot-definitions #L(typep !1 'direct-slot) #t))
         (all-direct-slots-are-direct-slots-p (null non-direct-slots)))
    ;; we do not support mixing persistent and non persistent slots with inheritance
    (assert (or (null non-direct-slots) (null direct-slots)))
    (if all-direct-slots-are-direct-slots-p
        (bind ((standard-initargs (sb-pcl::compute-effective-slot-definition-initargs class direct-slot-definitions))
               (slot-initargs (compute-effective-slot-initargs class direct-slot-definitions))
               (initargs (append slot-initargs standard-initargs))
               (class (apply #'effective-slot-definition-class class :slot all-direct-slots-are-direct-slots-p initargs)))
          (prog1-bind effective-slot-definition (apply #'make-instance class initargs)
            (setf (direct-slots-of effective-slot-definition) direct-slot-definitions)
            (setf (owner-class-of effective-slot-definition) class)))
        (call-next-method))))

#+nil(defun compute-effective-slot-initargs (class direct-slot-definitions)
  (iter (for slot-option-name in (delete-duplicates
                                  (collect-if #L(eq (symbol-package !1) (find-package :cl-perec))
                                              (mapcan #L(mapcar #'slot-definition-name
                                                                (class-slots (class-of !1)))
                                                      direct-slot-definitions))))
        (bind ((specific-direct-slot-definitions
                (collect-if #L(find slot-option-name (class-slots (class-of !1)) :key 'slot-definition-name)
                            direct-slot-definitions)))
          (appending
           (compute-effective-slot-slot-option class
                                               (first (sort (copy-list specific-direct-slot-definitions)
                                                            #L(subtypep (class-of !1) (class-of !2))))
                                               slot-option-name
                                               specific-direct-slot-definitions)))))

#+nil(defun slot-initarg-and-value (object slot-name)
  (when (and (slot-boundp object slot-name)
             (slot-value object slot-name))
    (list (initarg-symbol slot-name)
          (slot-value object slot-name))))

#+nil(defmethod compute-effective--slot-option (class
                                           (direct-slot persistent-direct-slot-definition)
                                           slot-option-name
                                           direct-slot-definitions)
  (if (member slot-option-name '(cached prefetched))
      (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions)
      (call-next-method)))
