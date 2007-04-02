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
                               (eq '&optional (first args))
                               (eq '&rest (first args))))
         (documentation (when (stringp (first body))
                          (pop body))))
    `(progn
      ,(when args
             `(defclass* ,(type-class-name-for name) (persistent-type)
               ,(append
                 `((name ',name)
                   (args ',args)
                   (body ',body))
                 (mapcar #L(list !1 nil) (argument-names-for args)))
               (:export-class-name-p #t)
               (:export-accessor-names-p #t)))
      ,(when (or allow-nil-args-p
                 (not common-lisp-type-p))
             `(eval-when (:load-toplevel :execute)
               (bind ((substituter (lambda ,args ,@body))
                      (type ,(when allow-nil-args-p
                                   `(parse-type (apply substituter nil)))))
                 (unless type
                   (setf type (make-instance 'persistent-type)))
                 (setf (name-of type) ',name)
                 (setf (args-of type) ',args)
                 (setf (body-of type) ',body)
                 (setf (documentation-of type) ',documentation)
                 (setf (substituter-of type) substituter)
                 (setf (find-type ',name) type)
                 type)))
      ,(if common-lisp-type-p
           `',name
           `(deftype ,name ,args ,@body)))))

(defun find-type (type)
  (gethash (first (ensure-list type)) *persistent-types*))

(defun (setf find-type) (new-value type)
  (setf (gethash (first (ensure-list type)) *persistent-types*) new-value))

(defun type-class-name-for (type)
  (concatenate-symbol (if (eq (symbol-package type)
                              (find-package :common-lisp))
                          (find-package :cl-perec)
                          (symbol-package type))
                      type "-type"))

(defun argument-names-for (args)
  (remove-if #L(or (eq !1 '&optional)
                   (eq !1 '&key)
                   (eq !1 '&rest)
                   (eq !1 '&allow-other-keys))
             args))

(defun type-specifier-p (type)
  (find-type type))

(defun substitute-type-arguments (type args)
  (let ((persistent-type (find-type type)))
    (if persistent-type
        (apply (substituter-of (find-type type)) args)
        (error "Unknown type specifier: ~A" type))))

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
    member
    symbol*
    symbol
    serialized
    set
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

(defgeneric parse-keyword-type-parameters (type type-parameters)
  (:method (type type-parameters)
           type-parameters))

(defgeneric parse-positional-type-parameters (type type-parameters)
  (:method (type (type-parameters null))
           nil)
  
  (:method (type type-parameters)
           (let ((args (argument-names-for (args-of type))))
             ;; TODO: eliminate this eval by storing the lambde in the defptype
             (eval `(apply (lambda ,(args-of type)
                             (list ,@(mappend #L(list (intern (symbol-name !1) (find-package :keyword)) !1) args)))
                     ',type-parameters)))))

(defun parse-type (type-specifier)
  (etypecase type-specifier
    (symbol (or (find-type type-specifier)
                (find-persistent-class type-specifier)))
    (list
     (let ((type (make-instance (type-class-name-for (first type-specifier)))))
       (apply #'reinitialize-instance type
              (cond ((= 0 (length type-specifier))
                     nil)
                    ((find-if #'keywordp (cdr type-specifier))
                     (parse-keyword-type-parameters type (rest type-specifier)))
                    (t (parse-positional-type-parameters type (rest type-specifier)))))))
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

(defun default-matches-type-cut (object slot type)
  (declare (ignore object slot))
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

(defcondition* object-slot-type-violation (type-violation)
  ((object
    :type persistent-object)
   (slot
    :type persistent-effective-slot-definition)))

(defgeneric matches-type* (value type)
  (:documentation "Checks if the given value matches the type.")

  (:method (value type)
           (error "Value ~A could not be matched against type ~A" value type))

  (:method (value (type list))
           (typep value type)))

