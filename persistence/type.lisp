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
        (:export-class-name-p #t)
        (:export-accessor-names-p #t))
      (export ',name)
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
                          *package*
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

(def (function io) check-slot-type (instance slot slot-value &optional (on-commit #f))
  (bind ((type (if on-commit
                   (slot-definition-type slot)
                   (always-checked-type-of slot)))
         (normalized-type (normalized-type-of slot))
         (check-type (if (set-type-p normalized-type)
                         (set-type-class-for normalized-type)
                         type)))
    (unless (typep slot-value check-type)
      (error 'slot-type-error :instance instance :slot slot :datum slot-value :expected-type type))))

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
             (subtypep (canonical-type-for `(and ,type-1 ,type-2)) nil))))

(defun canonical-type-for* (type)
  (pattern-case type
    (t t)
    (nil nil)
    (boolean 'boolean)
    (double 'double)
    ((?is ?type canonical-type-p)
     type)
    ((?is ?type class-type-p)
     type)
    ((?or (and (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (and (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     nil)
    ((and (?* ?x) ?a (?* ?y) ?a (?* ?z))
     (canonical-type-for (list* 'and (append ?x (list ?a) ?y ?z))))
    ((?or (or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     t)
    ((or (?* ?x) ?a (?* ?y) ?a (?* ?z))
     (canonical-type-for (list* 'or (append ?x (list ?a) ?y ?z))))
    ((and (?* ?x) ?a (?* ?x) ?b (?* ?z)
          (?if (and (persistent-class-type-p ?a)
                    (persistent-class-type-p ?b)
                    (not (intersection (list* (find-class* ?a) (persistent-effective-sub-classes-of (find-class* ?a)))
                                       (list* (find-class* ?b) (persistent-effective-sub-classes-of (find-class* ?b))))))))
     nil)
    ((?or (and (?* ?x) ?a (?* ?y) (not ?b) (?* ?z)
               (?if (disjunct-type-p ?a ?b)))
          (and (?* ?x) (not ?b) (?* ?y) ?a (?* ?z)
               (?if (disjunct-type-p ?a ?b))))
     (canonical-type-for `(and ,@?x ,@?y ,?a ,@?z)))
    (((?or or and not) . ?args)
     (list* (first type) (mapcar #'canonical-type-for (cdr type))))
    ((?is ?type symbolp)
     (canonical-type-for (substitute-type-arguments type nil)))
    ((?is ?type type-specifier-p)
     (let ((substituted-type (substitute-type-arguments (first type) (cdr type))))
       (if (not (equal substituted-type type))
           (canonical-type-for substituted-type)
           type)))
    (?type
     type)))

(defvar *mapped-type-precedence-list*
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
    form
    unsigned-byte-vector
    member
    symbol*
    symbol
    serialized
    set
    disjunct-set
    ordered-set
    t)
  "An ordered list of types which are mapped to RDBMS.")

;;;;;;;;;;;;;;;;
;;; Type mapping

(defmacro defmapping (name sql-type reader writer)
  `(progn
    (defmethod compute-column-type* ((type (eql ',name)) type-specification)
      (declare (ignorable type-specification))
      ,sql-type)

    (defmethod compute-reader* ((type (eql ',name)) type-specification)
      (declare (ignorable type-specification))
      ,reader)

    (defmethod compute-writer* ((type (eql ',name)) type-specification)
      (declare (ignorable type-specification))
      ,writer)))

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

