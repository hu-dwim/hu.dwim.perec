;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;
;;; Defining types

(def constant +type-error-marker+ '+type-error-marker+)

(def special-variable *persistent-types* (make-hash-table))

(def class* persistent-type ()
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

(def macro defptype (name args &body body)
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

(def function type-class-name-for (type)
  (concatenate-symbol (if (eq (symbol-package type)
                              (find-package :common-lisp))
                          (find-package :cl-perec)
                          (symbol-package type))
                      type "-type"))

(def function type-super-class-name-for (name type)
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

(def function type-specifier-p (type)
  (find-type type :otherwise #f))

(def function substitute-type-arguments (type args)
  (apply (substituter-of (find-type type)) args))

;;;;;;;;;;;;;;;;
;;; Type checker

(def special-variable *type-check-slot-values* #t)

(def (macro e) with-type-checking-slot-values (&body body)
  `(bind ((*type-check-slot-values* #t))
    ,@body))

(def (macro e) without-type-checking-slot-values (&body body)
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
;; TODO: when type-check is :on-commit checks during the transaction should be debug-only and at the end do the real type-check
(def (function io) check-slot-value-type (instance slot slot-value &optional (on-commit #f))
  (when *type-check-slot-values*
    (bind ((canonical-type (canonical-type-of slot))
           (type (if on-commit
                     canonical-type
                     (always-checked-type-of slot)))
           (check-type (if (set-type-p* canonical-type)
                           (set-type-class-for canonical-type)
                           type)))
      (unless (typep slot-value check-type)
        (cerror "Ignore type error" 'slot-type-error :instance instance :slot slot :datum slot-value :expected-type type)))))

;;;;;;;;;;;;;;;;;;
;;; Canonical type

(def function canonical-type-for (type)
  (iter (for simplified-type :initially type :then (simplify-boolean-form (canonical-type-for* (->dnf simplified-type))))
        (for previous-type :previous simplified-type)
        (until (equal simplified-type previous-type))
        (finally (return simplified-type))))

(def (special-variable :documentation "A list of type names to be treated as canonical types when a type is converted into canonical form.")
    *canonical-types*
    '(nil
      unbound
      null
      boolean
      integer
      float
      float-32
      float-64
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
      t))

(def function find-class* (class-or-name)
  (if (typep class-or-name 'standard-class)
      class-or-name
      (find-class class-or-name)))

(def function canonical-type-p (type)
  (member (first (ensure-list type)) *canonical-types*))

(def function class-type-p (type)
  (and (symbolp type)
       (find-class type nil)))

(def function disjunct-type-p (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
          (subtypep `(and ,type-1 ,type-2) nil))))

(def function disjunct-type-p* (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
          (subtypep (canonical-type-for `(and ,type-1 ,type-2)) nil))))

(def function canonical-type-for* (type)
  (pattern-case type
    ((?is ?type canonical-type-p)
     type)
    ;; simple class
    ((?is ?type class-type-p)
     type)
    ;; set types
    ((?is ?type set-type-p*)
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

;;;;;;;;;;;;;;;;
;;; Type mapping

(def (special-variable :documentation "An ordered list of types which are mapped to RDBMS.") *mapped-type-precedence-list*
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
      t))

(defclass* mapping ()
  ((reader
    :type (or symbol function))
   (writer
    :type (or symbol function))
   (tagged
    :type boolean)
   (rdbms-types
    :type list)))

(def macro defmapping (name sql-type reader writer)
  "A mapping specifies how a type is mapped to RDBMS. It defines the transformers to convert between the rdbms values and the slot value."
  (flet ((function-designator-p (transformer)
           (and (consp transformer)
                (eq (first transformer) 'quote)
                (symbolp (second transformer))
                (second transformer))))
    `(progn
       (defmethod compute-rdbms-types* ((mapped-type (eql ',name)) normalized-type)
         (declare (ignorable normalized-type))
         (ensure-list ,sql-type))

       (defmethod compute-reader* ((mapped-type (eql ',name)) normalized-type)
         (declare (ignorable normalized-type))
         ,(aif (function-designator-p reader)
               (if cl-perec-system:*load-as-production-p*
                   `(fdefinition ',it)
                   `',it)
               reader))

       (defmethod compute-writer* ((mapped-type (eql ',name)) normalized-type)
         (declare (ignorable normalized-type))
         ,(aif (function-designator-p writer)
               (if cl-perec-system:*load-as-production-p*
                   `(fdefinition ',it)
                   `',it)
               writer)))))

(defgeneric compute-type-tag (mapped-type)
  (:documentation "Returns a type tag which will be stored in the tag column when needed.")

  (:method (mapped-type)
    #t))

(defun compute-rdbms-types (type)
  (rdbms-types-of (compute-mapping type)))

(defgeneric compute-rdbms-types* (mapped-type normalized-type)
  (:method (mapped-type normalized-type)
    (error "Cannot map type ~A to RDBMS types" mapped-type))

  (:method ((mapped-type symbol) normalized-type)
    (if (persistent-class-type-p normalized-type)
        (append
         (list +oid-id-sql-type+)
         (oid-mode-ecase
           (:class-name
            (list +oid-class-name-sql-type+))
           (:class-id
            (list +oid-class-id-sql-type+))
           (:merge)))
        (call-next-method))))

(defun compute-reader (type)
  (reader-of (compute-mapping type)))

(defgeneric compute-reader* (mapped-type normalized-type)
  (:method (mapped-type normalized-type)
    (error "Cannot map type ~A to a reader" mapped-type))

  (:method ((mapped-type symbol) normalized-type)
    (if (persistent-class-type-p mapped-type)
        (if cl-perec-system:*load-as-production-p*
            #'object-reader
            'object-reader)
        (call-next-method))))

(defun compute-writer (type)
  (writer-of (compute-mapping type)))

(defgeneric compute-writer* (mapped-type normalized-type)
  (:method (mapped-type normalized-type)
    (error "Cannot map type ~A to a writer" mapped-type))

  (:method ((mapped-type symbol) normalized-type)
    (if (persistent-class-type-p mapped-type)
        (if cl-perec-system:*load-as-production-p*
            #'object-writer
            'object-writer)
        (call-next-method))))

(defun compute-mapping (type)
  (labels ((compute-or-type-mapping (type)
             (iter (with primary-rdbms-types = nil)
                   (with null-value-count = 0)
                   (for subtype :in (cdr type))
                   (for normalized-type = (normalized-type-for subtype))
                   (for mapped-type = (or (mapped-type-for normalized-type) subtype))
                   (for rdbms-types = (compute-rdbms-types* mapped-type normalized-type))
                   (if (eq :null (first rdbms-types))
                       (incf null-value-count)
                       (if primary-rdbms-types
                           (error "Unsupported multiple primary types using the 'or' type combinator: ~A" type)
                           (setf primary-rdbms-types rdbms-types)))
                   (collect (compute-type-tag subtype) :into type-tags)
                   (collect (compute-reader* mapped-type normalized-type) :into readers)
                   (collect (compute-writer* mapped-type normalized-type) :into writers)
                   (finally
                    (return
                      (cond ((and primary-rdbms-types
                                  (= null-value-count 1))
                             (make-instance 'mapping
                                            :tagged #f
                                            :rdbms-types primary-rdbms-types
                                            :reader (combined-reader readers)
                                            :writer (combined-writer writers)))
                            ((and primary-rdbms-types
                                  (> null-value-count 1))
                             (make-instance 'mapping
                                            :tagged #t
                                            :rdbms-types (list* (sql-boolean-type) primary-rdbms-types)
                                            :reader (tagged-reader type-tags readers)
                                            :writer (tagged-writer type-tags writers)))
                            (t
                             (error "Unsupported type: ~A" type))))))))
    (bind ((normalized-type (normalized-type-for type))
           (mapped-type (or (mapped-type-for normalized-type) type)))
      (cond ((set-type-p type)
             nil)
            ((or-type-p type)
             (compute-or-type-mapping type))
            ((unbound-subtype-p type)
             (compute-or-type-mapping `(or unbound ,type)))
            ((null-subtype-p type)
             (compute-or-type-mapping `(or null ,type)))
            (t
             (make-instance 'mapping
                            :tagged #f
                            :rdbms-types (compute-rdbms-types* mapped-type normalized-type)
                            :reader (compute-reader* mapped-type normalized-type)
                            :writer (compute-writer* mapped-type normalized-type)))))))

(def function lisp-value->rdbms-values (type lisp-value)
  (bind ((mapping (compute-mapping type))
         (column-count (length (rdbms-types-of mapping)))
         (result (make-array column-count)))
    (values result (funcall (writer-of mapping) lisp-value result 0))))

(def function rdbms-values->lisp-value (type rdbms-values)
  (funcall (reader-of (compute-mapping type)) rdbms-values 0))

;;;;;;;;
;;; Type

(def function mapped-type-for (type)
  "Returns the smalleset supertype which is directly mapped to RDBMS based on *MAPPED-TYPE-PRECEDENCE-LIST*."
  (if (persistent-class-type-p type)
      type
      (find-if #L(cond ((or (eq type !1)
                            (and (listp type)
                                 (eq (first type) !1)))
                        #t)
                       ((eq 'member !1)
                        (and (listp type)
                             (eq 'member (first type))))
                       (t
                        (subtypep type !1)))
               *mapped-type-precedence-list*)))

(def function normalized-type-for (type)
  "Returns a type which does not include subtypes mapped to :null column values."
  (if (or-type-p type)
      (bind ((subtypes
              (remove-if 'type-mapped-to-null-p
                         (cdr type))))
        (if (<= (length subtypes) 1)
            (first subtypes)
            (cons 'or subtypes)))
      (unless (type-mapped-to-null-p type)
        type)))

(def function type-mapped-to-null-p (type)
  (and (symbolp type)
       (equal '(:null) (compute-rdbms-types* type type))))

(def function primitive-type-p (type)
  "Accepts types such as boolean, integer, string, double, etc. which are directly mapped to RDBMS."
  (or (eq type t)
      (and (not (or-type-p type))
           (not (persistent-class-type-p type))
           (not (set-type-p* type))
           (not (unbound-subtype-p type))
           (not (null-subtype-p type)))))

(def function primitive-type-p* (type)
  "Same as primitive-type-p but also accepts values such as (or unbound integer), (or null string), (or unbound null boolean), etc."
  (primitive-type-p (normalized-type-for type)))

(def function persistent-class-type-p (type)
  "Returns true for persistent class types and false otherwise."
  (and (symbolp type)
       (find-persistent-class type)))

(def function persistent-class-type-p* (type)
  "Same as persistent-class-type-p but also accepts values such as (or unbound persistent-object), (or null persistent-object), (or unbound null persistent-object) etc."
  (persistent-class-type-p (persistent-class-type-for type)))

(def function persistent-class-type-for (type)
  (unless (eq type t)
    (canonical-type-for `(and persistent-object ,type))))

(def function or-type-p (type)
  "Returns true for type using the compound type specifier 'or'."
  (and (consp type)
       (eq 'or (first type))))

(def function set-type-p (type)
  (and (consp type)
       (eq (first type) 'set)))

(def function disjunct-set-type-p (type)
  (and (consp type)
       (eq (first type) 'disjunct-set)))

(def function ordered-set-type-p (type)
  (and (consp type)
       (eq (first type) 'ordered-set)))

(def function set-type-p* (type)
  "Returns true for all kind of persistent set types."
  (and (consp type)
       (member (first type) '(set disjunct-set ordered-set))))

(def function set-type-class-for (type)
  (second type))

(def function unbound-subtype-p (type)
  (and (not (eq 'member type))
       (subtypep 'unbound type)))

;; NOTE: boolean and symbol should not be treated as a subtype of null here
(def function null-subtype-p (type)
  (and (not (eq 'member type))
       (not (subtypep 'boolean type))
       (not (subtypep 'symbol type))
       (subtypep 'null type)))

(def function tagged-type-p (type)
  (tagged-p (compute-mapping type)))

;;;;;;;;;;;;;;;
;;; Type parser

(def function parse-type (type-specifier)
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
(def function unparse-type (type)
  (declare (ignore type))
  )

;;;;;;;;;;;;;;;;;;;;
;;; Destructure type

(def function destructure-type (type)
  "Returns (values canonical-type null-subtype-p unbound-subtype-p) corresponding to the given type."
  (bind ((normalized-type (normalized-type-for type))
         (mapped-type (mapped-type-for normalized-type))
         (unbound-subtype-p (unbound-subtype-p type))
         (null-subtype-p (and (not (null-subtype-p mapped-type))
                              (null-subtype-p type))))
    (values normalized-type null-subtype-p unbound-subtype-p)))

;;;;;;;;;;;;;;;;
;;; Type matcher

(def special-variable *matches-type-cut-function*)

(def function default-matches-type-cut (instance slot type)
  (declare (ignore instance slot))
  (or (persistent-object-p type)
      (set-type-p* type)))

(def function matches-type (value type &key (cut-function #'default-matches-type-cut) (signal-type-violations #f))
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
