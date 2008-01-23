;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;
;;; Defining types

(defparameter *persistent-types* (make-hash-table))

(defclass* persistent-type ()
  ((name
    :type symbol)
   (documentation
    :type string)
   (args
    :type list)
   (body
    :type list)
   (substituter
    :type function)))

(defmacro defptype (name args &body body)
  (bind ((common-lisp-type-p (eq (symbol-package name) (find-package :common-lisp)))
         (allow-nil-args-p (or (null args)
                               (member (first args) '(&rest &optional))))
         (documentation (when (stringp (first body))
                          (pop body)))
         (type-class-name (type-class-name-for name)))
    `(progn
      (eval-when (:compile-toplevel :load-toplevel :execute)
        ;; this import is needed because the name of the type class for types named by cl symbols are interned into
        ;; the cl-perec package and therefore cannot be exported from *package*. make sure it's imported.
        (import ',type-class-name ,*package*)
        (export '(,name ,type-class-name) ,*package*))
      (defclass* ,type-class-name (persistent-type)
        ,(append
          `((name ',name)
            (args ',args)
            (body ',body)
            (substituter
             (lambda ,args ,@body)
             :allocation :class)
            (parser
             (lambda ,args
               (make-instance ',type-class-name
                              ,@(mappend #L(list (intern (symbol-name !1) (find-package :keyword)) !1)
                                         (lambda-list-to-variable-list args :include-&rest #t))))
             :allocation :class))
          (mapcar #L(list !1 nil) (lambda-list-to-variable-list args :include-&rest #t)))
        (:export-accessor-names-p #t))
      (eval-when (:load-toplevel :execute)
        (bind ((class (ensure-finalized (find-class ',type-class-name))))
          ,(when allow-nil-args-p
                 `(bind ((prototype (class-prototype class))
                         (substituter (substituter-of prototype))
                         (parser (parser-of prototype))
                         (substituted-type (funcall substituter)))
                   (ensure-class ',type-class-name :direct-superclasses
                    (list (ensure-finalized (find-class (type-super-class-name-for ',name substituted-type)))))
                   (bind ((type (if (symbolp substituted-type)
                                    (make-instance ',type-class-name)
                                    (change-class (parse-type substituted-type) ',type-class-name))))
                     (setf (name-of type) ',name)
                     (setf (args-of type) ',args)
                     (setf (body-of type) ',body)
                     (setf (documentation-of type) ',documentation)
                     (setf (find-type ',name) type))))))
      ,(if common-lisp-type-p
           `',name
           `(deftype ,name ,args ,@body)))))

(def (function io) find-type (type &key (otherwise :error))
  (or (gethash (first (ensure-list type)) *persistent-types*)
      (case otherwise
        (:error (error "Unknown type specifier ~S" type))
        (:warn (warn "Unknown type specifier ~S" type)
               nil)
        (t otherwise))))

(def (function io) (setf find-type) (new-value type)
  (setf (gethash (first (ensure-list type)) *persistent-types*) new-value))

(defun type-class-name-for (type)
  (concatenate-symbol (if (eq (symbol-package type)
                              (find-package :common-lisp))
                          (find-package :cl-perec)
                          (symbol-package type))
                      type "-type"))

(defun type-super-class-name-for (name type)
  (cond ((and (symbolp type)
              (not (eq name type)))
         (if (find-class (type-class-name-for type) nil)
             (type-class-name-for type)
             'persistent-type))
        ((and (consp type)
              (not (eq name (first type))))
         (let ((el (first type)))
           (cond ((eq 'and el)
                  (let ((super-class-name (type-class-name-for (find-if #'symbolp (cdr type)))))
                    (if (find-class super-class-name nil)
                        super-class-name
                        (type-class-name-for el))))
                 ((member el '(or not))
                  'persistent-type)
                 (t
                  (type-class-name-for el)))))
        (t
         'persistent-type)))

(defun type-specifier-p (type)
  (find-type type :otherwise #f))

(defun substitute-type-arguments (type args)
  (apply (substituter-of (find-type type)) args))

;;;;;;;;;;;;;;;;
;;; Type checker

(defparameter *type-check-slot-values* #t)

(defmacro with-type-checking-slot-values (&body body)
  `(bind ((*type-check-slot-values* #t))
    ,@body))

(defmacro without-type-checking-slot-values (&body body)
  `(bind ((*type-check-slot-values* #f))
    ,@body))

(defcondition* slot-type-error (type-error)
  ((instance
    nil
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The value ~2I~:_~S ~I~_in slot ~A of instance ~A is not of type ~2I~_~S.~:>"
             (type-error-datum condition)
             (slot-definition-name (slot-of condition))
             (instance-of condition)
             (type-error-expected-type condition)))))

;; TODO: take care about performance
(def (function io) check-slot-value-type (instance slot slot-value &optional (on-commit #f))
  (when *type-check-slot-values*
    (bind ((type (if on-commit
                     (slot-definition-type slot)
                     (always-checked-type-of slot)))
           (normalized-type (normalized-type-of slot))
           (check-type (if (set-type-p normalized-type)
                           (set-type-class-for normalized-type)
                           type)))
      (unless (typep slot-value check-type)
        (cerror "Ignore type error" 'slot-type-error :instance instance :slot slot :datum slot-value :expected-type type)))))

;;;;;;;;;;;;;;;;;;
;;; Canonical type

(defun canonical-type-for (type)
  (iter (for simplified-type :initially type :then (simplify-boolean-form (canonical-type-for* (->dnf simplified-type))))
        (for previous-type :previous simplified-type)
        (until (equal simplified-type previous-type))
        (finally (return simplified-type))))

(defvar *canonical-types* nil
  "A list of type names to be treated as canonical types when a type is converted into canonical form.")

(defun find-class* (class-or-name)
  (if (typep class-or-name 'standard-class)
      class-or-name
      (find-class class-or-name)))

(defun canonical-type-p (type)
  (member (first (ensure-list type)) *canonical-types*))

(defun class-type-p (type)
  (and (symbolp type)
       (find-class type nil)))

(defun disjunct-type-p (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
          (subtypep `(and ,type-1 ,type-2) nil))))

(defun disjunct-type-p* (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
          (subtypep (canonical-type-for `(and ,type-1 ,type-2)) nil))))

(defun canonical-type-for* (type)
  (pattern-case type
    ;; universal type
    (t t)
    ;; empty type
    (nil nil)
    ;; some primitive types
    (boolean 'boolean)
    (double 'double)
    (keyword 'keyword)
    ;; known to be canonical
    ((?is ?type canonical-type-p)
     type)
    ;; simple class
    ((?is ?type class-type-p)
     type)
    ;; (and a (not a)) -> nil
    ((?or (and (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (and (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     nil)
    ;; (and a a) -> a
    ((and (?* ?x) ?a (?* ?y) ?a (?* ?z))
     (canonical-type-for (list* 'and (append ?x (list ?a) ?y ?z))))
    ;; (or a (not a)) -> t
    ((?or (or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     t)
    ;; (or a a) -> a
    ((or (?* ?x) ?a (?* ?y) ?a (?* ?z))
     (canonical-type-for (list* 'or (append ?x (list ?a) ?y ?z))))
    ;; persistent classes without intersecion
    ((and (?* ?x) ?a (?* ?x) ?b (?* ?z)
          (?if (and (persistent-class-type-p ?a)
                    (persistent-class-type-p ?b)
                    (not (intersection (list* (find-class* ?a) (persistent-effective-sub-classes-of (find-class* ?a)))
                                       (list* (find-class* ?b) (persistent-effective-sub-classes-of (find-class* ?b))))))))
     nil)
    ;; (disjunct-type-p a b) -> nil
    ((?or (and (?* ?x) ?a (?* ?y) ?b (?* ?z)
               (?if (disjunct-type-p ?a ?b)))
          (and (?* ?x) ?b (?* ?y) ?a (?* ?z)
               (?if (disjunct-type-p ?a ?b))))
     nil)
    ;; (disjunct-type-p a (not b)) -> a
    ((?or (and (?* ?x) ?a (?* ?y) (not ?b) (?* ?z)
               (?if (or (disjunct-type-p ?a ?b)
                        (disjunct-type-p* ?a ?b))))
          (and (?* ?x) (not ?b) (?* ?y) ?a (?* ?z)
               (?if (or (disjunct-type-p ?a ?b)
                        (disjunct-type-p* ?a ?b))))
          ;; (subtypep a b) -> a
          (and (?* ?x) ?a (?* ?y) ?b (?* ?z)
               (?if (subtypep ?a ?b)))
          (and (?* ?x) ?b (?* ?y) ?a (?* ?z)
               (?if (subtypep ?a ?b))))
     (canonical-type-for `(and ,@?x ,@?y ,?a ,@?z)))
    ;; recursion for and/or arguments
    (((?or or and not) . ?args)
     (list* (first type) (mapcar #'canonical-type-for (cdr type))))
    ;; expand types
    ((?is ?type symbolp)
     (canonical-type-for (substitute-type-arguments type nil)))
    ;; known system type
    ((?is ?type type-specifier-p)
     (let ((substituted-type (substitute-type-arguments (first type) (cdr type))))
       (if (not (equal substituted-type type))
           (canonical-type-for substituted-type)
           type)))
    ;; no more options
    (?type
     type)))

(defparameter *mapped-type-precedence-list*
  '(nil
    unbound
    null
    boolean
    integer-16
    integer-32
    integer-64
    integer
    float-32
    float-64
    float
    double
    number
    text
    duration
    string
    timestamp
    date
    time
    unsigned-byte-vector
    member
    symbol*
    symbol
    form
    serialized
    set
    disjunct-set
    ordered-set
    t)
  "An ordered list of types which are mapped to RDBMS.")

;;;;;;;;;;;;;;;;
;;; Type mapping

(defmacro defmapping (name sql-type reader writer)
  (flet ((function-designator-p (transformer)
           (and (consp transformer)
                (eq (first transformer) 'quote)
                (symbolp (second transformer))
                (second transformer))))
    `(progn
       (defmethod compute-column-type* ((type (eql ',name)) type-specification)
         (declare (ignorable type-specification))
         ,sql-type)

       (defmethod compute-reader* ((type (eql ',name)) type-specification)
         (declare (ignorable type-specification))
         ,(aif (function-designator-p reader)
               (if cl-perec-system:*load-as-production-p*
                   `(fdefinition ',it)
                   `',it)
               reader))

       (defmethod compute-writer* ((type (eql ',name)) type-specification)
         (declare (ignorable type-specification))
         ,(aif (function-designator-p writer)
               (if cl-perec-system:*load-as-production-p*
                   `(fdefinition ',it)
                   `',it)
               writer)))))

;;;;;;;;;;;;;;;
;;; Type parser

(defun parse-type (type-specifier)
  (etypecase type-specifier
    (symbol (or (find-type type-specifier :otherwise nil)
                (find-persistent-class type-specifier)))
    (list
     (apply (parser-of (class-prototype (find-class (type-class-name-for (first type-specifier)))))
            (rest type-specifier)))
    (persistent-type type-specifier)))

;;;;;;;;;;;;;;;;;
;;; Type unparser

;; TODO: unparse it into a list
(defun unparse-type (type)
  (declare (ignore type))
  )

;;;;;;;;;;;;;;;;;;;;
;;; Destructure type

(defun destructure-type (type)
  "Returns (values normalized-type null-subtype-p unbound-subtype-p) corresponding to the given type."
  (bind ((normalized-type (normalized-type-for type))
         (mapped-type (mapped-type-for normalized-type))
         (unbound-subtype-p (unbound-subtype-p type))
         (null-subtype-p (and (not (null-subtype-p mapped-type))
                              (null-subtype-p type))))
    (values normalized-type null-subtype-p unbound-subtype-p)))

;;;;;;;;;;;;;;;;
;;; Type matcher

(defvar *matches-type-cut-function*)

(defun default-matches-type-cut (instance slot type)
  (declare (ignore instance slot))
  (or (persistent-object-p type)
      (set-type-p type)))

(defun matches-type (value type &key (cut-function #'default-matches-type-cut) (signal-type-violations #f))
  (bind ((*matches-type-cut-function* cut-function))
    (flet ((body ()
             (aprog1 (matches-type* value type)
               (unless it
                 (error (make-condition 'value-type-violation :value value :value-type type))))))
      (if (not signal-type-violations)
          (handler-case (body)
            (type-violation () #f))
          (body)))))

(defcondition* type-violation ()
  ())

(defcondition* value-type-violation (type-violation)
  ((value
    :type t)
   (value-type
    :type the-type)))

(defcondition* instance-slot-type-violation (type-violation)
  ((instance
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition)))

(defgeneric matches-type* (value type)
  (:documentation "Checks if the given value matches the type.")

  (:method (value type)
           (error "Value ~A could not be matched against type ~A" value type))

  (:method (value (type list))
           (typep value type)))

