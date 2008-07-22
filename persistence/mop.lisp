;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;
;;; MOP methods 

;; allows persistent keyword argument for persistent-direct-slot-definitions according to CLOS mop
;; even though there is no such slot in the class
(defmethod shared-initialize :around ((slot persistent-direct-slot-definition) slot-names
                                      &rest args &key persistent &allow-other-keys)
  (declare (ignore persistent))
  (apply #'call-next-method slot slot-names args))

(defmethod make-instance ((instance identity-preserving-class) &key instance &allow-other-keys)
  ;; used in class finalization protocol when instantiating direct slot definitions
  ;; this allows associations to be defined independently of direct slot definitions
  ;; and ensure-class to be called without loosing the old non association direct slot definitions
  (aif instance
       it
       (call-next-method)))

;; NOTE: allows to have standard slots within a persistent-class and prevents defassociation to kill these slots 
(defmethod make-instance :around ((class (eql (find-class 'standard-direct-slot-definition))) &key instance &allow-other-keys)
  (aif instance
       it
       (call-next-method)))

(defmethod initialize-instance :around ((class persistent-class) &rest args)
  (apply #'shared-ininitialize-around-persistent-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class persistent-class) &rest args)
  ;; update type dependencies first
  (mapc #L(deletef (depends-on-of !1) class)
        (depends-on-me-of class))
  (setf (depends-on-me-of class) nil)
  ;; emulate shared initialize which is not allowed to be overridden
  (apply #'shared-ininitialize-around-persistent-class class #'call-next-method :name (class-name class) args))

(defmethod reinitialize-instance :before ((association persistent-association) &key &allow-other-keys)
  (mapc #L(deletef (depends-on-of !1) association)
        (associated-classes-of association)))

(defmethod shared-initialize :after ((association persistent-association) slot-names &key &allow-other-keys)
  (mapc #L(pushnew association (depends-on-of !1))
        (associated-classes-of association)))

(defmethod validate-superclass ((class standard-class) (superclass persistent-class))
  t)

(defmethod validate-superclass ((class persistent-class) (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class)
                                         &rest args &key instance persistent association &allow-other-keys)
  (cond (instance
         (class-of instance))
        (association
         (apply #'direct-slot-definition-class association args))
        (persistent
         (find-class 'persistent-direct-slot-definition))
        (t
         (call-next-method))))

(defmethod direct-slot-definition-class ((class persistent-association) &key &allow-other-keys)
  (find-class 'persistent-association-end-direct-slot-definition))

(defmethod effective-slot-definition-class ((class persistent-class)
                                            &rest args &key instance persistent association &allow-other-keys)
  (cond (instance
         (class-of instance))
        (association
         (apply #'effective-slot-definition-class association args))
        (persistent
         (find-class 'persistent-effective-slot-definition))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-association) &key &allow-other-keys)
  (find-class 'persistent-association-end-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class persistent-class) slot-name direct-slot-definitions)
  (if (some (lambda (slot)
              (typep slot 'persistent-direct-slot-definition))
            direct-slot-definitions)
      (bind ((standard-initargs (compute-standard-effective-slot-definition-initargs class direct-slot-definitions))
             (slot-initargs (compute-persistent-effective-slot-definition-initargs class direct-slot-definitions))
             (initargs (append slot-initargs standard-initargs))
             (effective-slot-class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (prog1-bind effective-slot-definition
            (if (subtypep effective-slot-class 'persistent-effective-slot-definition)
                (apply #'make-instance effective-slot-class :direct-slots direct-slot-definitions initargs)
                (apply #'make-instance effective-slot-class initargs))
          (bind ((type (canonical-type-of effective-slot-definition))
                 (normalized-type (normalized-type-for type))
                 (mapped-type (mapped-type-for normalized-type))
                 (unbound-subtype-p (unbound-subtype-p type))
                 (null-subtype-p (and (not (null-subtype-p mapped-type))
                                      (null-subtype-p type)))
                 (initfunction (slot-definition-initfunction effective-slot-definition)))
            (when (and (or null-subtype-p
                           (set-type-p* type))
                       (not unbound-subtype-p)
                       (not initfunction))
              (setf (slot-definition-initfunction effective-slot-definition)
                    (constantly nil))))))
      (call-next-method)))

(def function compute-standard-effective-slot-definition-initargs (class direct-slot-definitions)
  #+sbcl(sb-pcl::compute-effective-slot-definition-initargs class direct-slot-definitions)
  #-sbcl(not-yet-implemented))

(def function compute-persistent-effective-slot-definition-initargs (class direct-slot-definitions)
  (iter (for slot-option-name in (delete-duplicates
                                  (collect-if #L(not (eq (symbol-package !1) (find-package :common-lisp)))
                                              (mapcan #L(mapcar #'slot-definition-name
                                                                (class-slots (class-of !1)))
                                                      direct-slot-definitions))))
        (bind ((specific-direct-slot-definitions
                (collect-if #L(find slot-option-name (class-slots (class-of !1)) :key 'slot-definition-name)
                            direct-slot-definitions)))
          (appending
           (compute-persistent-effective-slot-definition-option class
                                                                (first (sort (copy-list specific-direct-slot-definitions)
                                                                             #L(subtypep (class-of !1) (class-of !2))))
                                                                slot-option-name
                                                                specific-direct-slot-definitions)))))

(defgeneric compute-persistent-effective-slot-definition-option (class direct-slot slot-option-name direct-slot-definitions)
  (:method ((class persistent-class)
            direct-slot-definition
            slot-option-name
            direct-slot-definitions)
    nil)

  (:method ((class persistent-class)
            (direct-slot persistent-direct-slot-definition)
            slot-option-name
            direct-slot-definitions)
    (if (member slot-option-name '(cache prefetch index unique type-check))
        (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions)
        (call-next-method)))

  (:method ((class persistent-class)
            (direct-slot persistent-association-end-direct-slot-definition)
            slot-option-name
            direct-slot-definitions)
    (if (member slot-option-name '(association))
        (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions)
        (call-next-method))))

(defmethod finalize-inheritance :after ((class persistent-class))
  (invalidate-inheritance class)
  (mapc #L(ensure-slot-reader* class !1)
        (collect-if #L(set-type-p* (canonical-type-of !1))
                    (persistent-effective-slots-of class)))
  (setf (standard-instance-access (class-prototype class) (slot-definition-location (find-slot class 'persistent))) #f))

(defmethod compute-slots :after ((class persistent-class))
  "Invalidates the cached slot values whenever the effective slots are recomputed, so that all dependent computed state will be invalidated and recomputed when requested."
  (invalidate-computed-slot class 'standard-direct-slots)
  (invalidate-computed-slot class 'standard-effective-slots))

(def function invalidate-inheritance (class)
  (invalidate-computed-slot class 'persistent-direct-super-classes)
  (invalidate-computed-slot class 'persistent-direct-sub-classes)
  (mapc #L(invalidate-computed-slot !1 'persistent-direct-super-classes)
        (persistent-direct-sub-classes-of class))
  (mapc #L(invalidate-computed-slot !1 'persistent-direct-sub-classes)
        (persistent-direct-super-classes-of class)))

;;;;;;;;;;;
;;; Utility

(defgeneric persistent-class-default-superclass (class)
  (:method ((class persistent-class))
    (find-class 'persistent-object nil)))

(def function ensure-persistent-class-default-superclass (class name direct-superclasses)
  (bind ((default-superclass (persistent-class-default-superclass class)))
    (if (or (null default-superclass)
            (eq (class-name default-superclass) name))
        direct-superclasses
        (if (or (member default-superclass direct-superclasses)
                (find-if (lambda (direct-superclass)
                           (member (class-of class)
                                   (compute-class-precedence-list
                                    (class-of direct-superclass))))
                         direct-superclasses))
            direct-superclasses
            (append direct-superclasses (list default-superclass))))))

(def function process-direct-slot-definitions (direct-slots)
  (loop for direct-slot :in direct-slots
     collect (if (or (getf direct-slot :instance)
                     (getf direct-slot :persistent))
                 direct-slot
                 (if (hasf direct-slot :persistent)
                     ;; remove :persistent nil
                     (remove-from-plist direct-slot :persistent)
                     ;; add default :persistent t
                     (append direct-slot '(:persistent t))))))

(def function association-direct-slot-definitions (class)
  (when (slot-boundp class 'depends-on)
    (let ((depends-on-associations
           (collect-if #L(typep !1 'persistent-association)
                       (depends-on-of class))))
      (mappend (lambda (association)
                 (let ((association-end-definitions
                       (collect-if #L(eq (class-name class) (getf !1 :class))
                                   (association-end-definitions-of association))))
                  (mapcar #L(append (list :name (getf !1 :slot)
                                          :association association
                                          :persistent #t)
                                    (remove-from-plist !1 :slot :class :accessor))
                          association-end-definitions)))
               depends-on-associations))))

;; this is not the real shared-initialize because portable programs are not allowed to override that
;; so we are somewhat emulating it by calling this function from both initialize-instance and reinitialize-instance
(def function shared-ininitialize-around-persistent-class (class call-next-method &rest args
                                                    &key name direct-slots direct-superclasses &allow-other-keys)
  ;; call initialize-instance or reinitialize-instance next method
  (prog1
      (apply call-next-method
             class
             :direct-slots (append (process-direct-slot-definitions direct-slots)
                                   (association-direct-slot-definitions class))
             :direct-superclasses (ensure-persistent-class-default-superclass class name direct-superclasses)
             :abstract (first (getf args :abstract))
             (remove-from-plist args :direct-slots :direct-superclasses :abstract))
    (setf (find-persistent-class name) class)
    (invalidate-inheritance class)
    (invalidate-computed-slot class 'standard-direct-slots)
    (invalidate-computed-slot class 'standard-effective-slots)
    ;; update type specific class dependencies
    (mapc #L(bind ((type (canonical-type-of !1)))
              (when (set-type-p* type)
                (bind ((associated-class (find-class (set-type-class-for type))))
                  (pushnew class (depends-on-of associated-class))
                  (pushnew associated-class (depends-on-me-of class)))))
          (persistent-direct-slots-of class))
    (mapc #L(bind ((association (association-of !1))
                   (association-end-position
                    (position (slot-definition-name !1) (association-end-definitions-of association)
                              :key #L(getf !1 :slot))))
              (if (= 0 association-end-position)
                  (setf (primary-association-end-of association) !1)
                  (setf (secondary-association-end-of association) !1)))
          (collect-if #L (typep !1 'persistent-association-end-direct-slot-definition)
                      (class-direct-slots class)))))

(def function ensure-slot-reader* (class slot)
  (when-bind reader-name (reader-name-of slot)
    (bind ((reader (concatenate-symbol reader-name "*"))
           (reader-gf (ensure-generic-function reader :lambda-list '(instance))))
      (ensure-method reader-gf
                     `(lambda (instance)
                        (with-lazy-slot-value-collections
                          (slot-value-using-class ,class instance ,slot)))
                     :specializers (list class)))))

(def function slot-initarg-and-value (instance slot-name)
  (when (slot-boundp instance slot-name)
    (list (first (slot-definition-initargs (find-slot (class-of instance) slot-name)))
          (slot-value instance slot-name))))

(def function reader-name-of (effective-slot)
  (first (some #'slot-definition-readers (direct-slots-of effective-slot))))

(def function writer-name-of (effective-slot)
  (first (some #'slot-definition-writers (direct-slots-of effective-slot))))

(def function class-slots (class)
  (closer-mop:class-slots (ensure-finalized class)))
