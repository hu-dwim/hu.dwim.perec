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
(defmethod initialize-instance :around ((class persistent-class) &rest args)
  (apply #'shared-ininitialize-persistent-class class #'call-next-method args))

;; TODO: inject association ends
(defmethod reinitialize-instance :around ((class persistent-class) &rest args)
  (apply #'shared-ininitialize-persistent-class class #'call-next-method :name (class-name class) args))

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
                                            &key persistent &allow-other-keys)
  (if persistent
      (find-class 'persistent-effective-slot-definition)
      (call-next-method)))

(defmethod compute-effective-slot-definition ((class persistent-class)
                                              slot-name
                                              direct-slot-definitions)
  (bind ((persistent (some (lambda (slot)
                             (typep slot 'persistent-direct-slot-definition))
                           direct-slot-definitions)))
    (if persistent
        (bind ((standard-initargs (compute-standard-effective-slot-definition-initargs class direct-slot-definitions))
               (slot-initargs (compute-persistent-effective-slot-definition-initargs class direct-slot-definitions))
               (initargs (append slot-initargs standard-initargs))
               (class (apply #'effective-slot-definition-class class :persistent #t initargs)))
          (apply #'make-instance class :direct-slots direct-slot-definitions initargs))
        (call-next-method))))

(defun compute-standard-effective-slot-definition-initargs (class direct-slot-definitions)
  #+sbcl(sb-pcl::compute-effective-slot-definition-initargs class direct-slot-definitions)
  #-sbcl(not-yet-implemented))

(defun compute-persistent-effective-slot-definition-initargs (class direct-slot-definitions)
  (iter (for slot-option-name in (delete-duplicates
                                  (remove-if-not #L(eq (symbol-package !1) (find-package :cl-perec))
                                                 (mapcan #L(mapcar #'slot-definition-name
                                                                   (class-slots (class-of !1)))
                                                         direct-slot-definitions))))
        (bind ((specific-direct-slot-definitions
                (remove-if-not #L(find slot-option-name (class-slots (class-of !1)) :key 'slot-definition-name)
                               direct-slot-definitions)))
          (appending
           (compute-persistent-effective-slot-definition-option class
                                                                (first (sort (copy-list specific-direct-slot-definitions)
                                                                             #L(subtypep (class-of !1) (class-of !2))))
                                                                slot-option-name
                                                                specific-direct-slot-definitions)))))

(defgeneric compute-persistent-effective-slot-definition-option (class direct-slot slot-option-name direct-slot-definitions)
  (:method (class
            (direct-slot persistent-direct-slot-definition)
            slot-option-name
            direct-slot-definitions)
           (when (member slot-option-name '(cached prefetched))
             (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions))))

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

;; this is not the real shared-initialize because portable programs are not allowed to override that
(defun shared-ininitialize-persistent-class (class call-next-method &rest args
                                                   &key name direct-slots direct-superclasses &allow-other-keys)
  (mapc #L(delete! class (depends-on-of !1))
        (when (slot-boundp class 'depends-on-me)
          (depends-on-me-of class)))
  (setf (depends-on-me-of class) nil)
  ;; call initialize-instance or reinitialize-instance next method
  (let ((result (apply call-next-method
                       class
                       :direct-slots (process-direct-slot-definitions direct-slots)
                       :direct-superclasses (ensure-persistent-object-class name direct-superclasses)
                       #+lispworks :optimize-slot-access #+lispworks nil 
                       (remove-keywords args :direct-slots))))
    (invalidate-computed-slot class 'persistent-direct-slots)
    result)
  ;; update dependencies
  (mapc #L(let ((type (remove-null-and-unbound-if-or-type (slot-definition-type !1))))
            (when (set-type-p type)
              (pushnew class (depends-on-of (find-class (second type))))
              (pushnew (find-class (second type)) (depends-on-me-of class))))
        (class-direct-slots class)))

(defun slot-initarg-and-value (object slot-name)
  (when (and (slot-boundp object slot-name)
             (slot-value object slot-name))
    (list (initarg-symbol slot-name)
          (slot-value object slot-name))))
