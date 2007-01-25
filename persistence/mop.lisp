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

(defmethod make-instance ((slot-definition-class-meta-object persistent-slot-definition-class)
                          &key instance &allow-other-keys)
  ;; used in class finalization protocol when instantiating direct slot definitions
  ;; this allows associations to be defined independently of direct slot definitions
  ;; and ensure-class to be called without loosing the old non association direct slot definitions
  (aif instance
       it
       (call-next-method)))

(defmethod initialize-instance :around ((class persistent-class) &rest args)
  (apply #'shared-ininitialize-around-persistent-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class persistent-class) &rest args)
  ;; update type dependencies first
  (mapc #L(delete! class (depends-on-of !1))
        (depends-on-me-of class))
  (setf (depends-on-me-of class) nil)
  ;; emulate shared initialize which is not allowed to be overridden
  (apply #'shared-ininitialize-around-persistent-class class #'call-next-method :name (class-name class) args))

(defmethod reinitialize-instance :before ((association persistent-association) &key &allow-other-keys)
  (mapc #L(delete! association (depends-on-of !1))
        (associated-classes-of association)))

(defmethod shared-initialize :after ((association persistent-association) slot-names &key &allow-other-keys)
  (mapc #L(pushnew association (depends-on-of !1))
        (associated-classes-of association)))

(defmethod validate-superclass ((class standard-class)
                                (superclass persistent-class))
  t)

(defmethod validate-superclass ((class persistent-class)
                                (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class)
                                         &key instance persistent association &allow-other-keys)
  (cond (instance
         (class-of instance))
        (association
         (find-class 'persistent-association-end-direct-slot-definition))
        (persistent
         (find-class 'persistent-direct-slot-definition))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-class)
                                            &key instance persistent association &allow-other-keys)
  (cond (instance
         (class-of instance))
        (association
         (find-class 'persistent-association-end-effective-slot-definition))
        (persistent
         (find-class 'persistent-effective-slot-definition))
        (t
         (call-next-method))))

(defmethod compute-effective-slot-definition ((class persistent-class)
                                              slot-name
                                              direct-slot-definitions)
  (if (some (lambda (slot)
              (typep slot 'persistent-direct-slot-definition))
            direct-slot-definitions)
      (bind ((standard-initargs (compute-standard-effective-slot-definition-initargs class direct-slot-definitions))
             (slot-initargs (compute-persistent-effective-slot-definition-initargs class direct-slot-definitions))
             (initargs (append slot-initargs standard-initargs))
             (class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (apply #'make-instance class :direct-slots direct-slot-definitions initargs))
      (call-next-method)))

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
             (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions)))

  (:method (class
            (direct-slot persistent-association-end-direct-slot-definition)
            slot-option-name
            direct-slot-definitions)
           (if (member slot-option-name '(min-cardinality max-cardinality association))
               (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions)
               (call-next-method))))

(defmethod finalize-inheritance :after ((class persistent-class))
  (mapc #L(ensure-slot-reader* class !1)
        (remove-if-not #L(set-type-p (remove-null-and-unbound-if-or-type (slot-definition-type !1)))
                       (persistent-effective-slots-of class))))

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
        collect (if (or (getf direct-slot :instance)
                        (getf direct-slot :persistent))
                    direct-slot
                    (if (get-properties direct-slot '(:persistent))
                        ;; remove :persistent nil
                        (remove-keywords direct-slot :persistent)
                        ;; add default :persistent t
                        (append direct-slot '(:persistent t))))))

(defun association-direct-slot-definitions (class)
  (when (slot-boundp class 'depends-on)
    (let ((depends-on-associations
           (remove-if-not #L(typep !1 'persistent-association)
                          (depends-on-of class))))
      (mapcar #L(let ((association-end-definition
                       (find (class-name class) (association-end-definitions-of !1)
                             :key #L(getf !1 :class))))
                  (append (list :name (getf association-end-definition :slot)
                                :association !1
                                :persistent #t
                                :initfunction (eval `(lambda () ,(getf association-end-definition :initform))))
                          (remove-keywords association-end-definition :slot :class :accessor)))
              depends-on-associations))))

;; this is not the real shared-initialize because portable programs are not allowed to override that
;; so we are somewhat emulating it by calling this function from both initialize-instance and reinitialize-instance
(defun shared-ininitialize-around-persistent-class (class call-next-method &rest args
                                                    &key name direct-slots direct-superclasses &allow-other-keys)
  ;; call initialize-instance or reinitialize-instance next method
  (prog1
      (apply call-next-method
             class
             :direct-slots (append (process-direct-slot-definitions direct-slots)
                                   (association-direct-slot-definitions class))
             :direct-superclasses (ensure-persistent-object-class name direct-superclasses)
             :abstract (first (getf args :abstract))
             #+lispworks :optimize-slot-access #+lispworks nil 
             (remove-keywords args :direct-slots))
    (invalidate-computed-slot class 'persistent-direct-slots)
    ;; update type specific class dependencies
    (mapc #L(let ((type (remove-null-and-unbound-if-or-type (slot-definition-type !1))))
              (when (set-type-p type)
                (pushnew class (depends-on-of (find-class (second type))))
                (pushnew (find-class (second type)) (depends-on-me-of class))))
          (class-direct-slots class))
    (mapc #L(let ((association (association-of !1)))
              (if (= 0 (position (slot-definition-name !1) (association-end-definitions-of association)
                                 :key #L(getf !1 :slot)))
                  (setf (primary-association-end-of association) !1)
                  (setf (secondary-association-end-of association) !1)))
          (remove-if-not #L (typep !1 'persistent-association-end-direct-slot-definition)
                         (class-direct-slots class)))))

(defun ensure-slot-reader* (class slot)
  (bind ((reader (concatenate-symbol (first (some #'slot-definition-readers (direct-slots-of slot))) "*"))
         (reader-gf (ensure-generic-function reader :lambda-list '(object))))
    (ensure-method reader-gf
                   `(lambda (object)
                     (with-lazy-slot-values
                       (slot-value-using-class ,class object ,slot)))
                   :specializers (list class))))

(defun slot-initarg-and-value (object slot-name)
  (when (and (slot-boundp object slot-name)
             (slot-value object slot-name))
    (list (initarg-symbol slot-name)
          (slot-value object slot-name))))
