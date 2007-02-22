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
   (args
    :type list)
   (body
    :type list)))

;; TODO: use defclass* only from defptype*
(defmacro defptype (name args slots &body body)
  (bind ((common-lisp-type-p (eq (symbol-package name) (find-package :common-lisp)))
         (allow-nil-args-p (or (null args)
                               (eq '&optional (first args))
                               (eq '&rest (first args)))))
    `(progn
      ,(when args
             `(defclass* ,(type-class-name-for name) (persistent-type)
               ,(append
                 `((name ',name)
                   (args ',args)
                   (body ',body)) slots)))
      ,(when (or allow-nil-args-p
                 (not common-lisp-type-p))
             `(eval-when (:load-toplevel :execute)
               (let ((parsed-type (when ,allow-nil-args-p
                                    (parse-type (substitute-type-arguments* ',args ',body nil)))))
                 (setf (find-type ',name)
                       (or (and ,allow-nil-args-p
                                parsed-type
                                (aprog1 parsed-type
                                  (setf (name-of it) ',name)
                                  (setf (args-of it) ',args)
                                  (setf (body-of it) ',body)))
                           (make-instance 'persistent-type :name ',name :args ',args :body ',body))))))
      ,(if common-lisp-type-p
           `',name
           `(deftype ,name ,args ,@body)))))

(defmacro defptype* (name args slots &body body)
  `(defptype ,name ,args ,slots ,@body))

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

(defun type-specifier-p (type)
  (find-type type))

(defun substitute-type-arguments (type args)
  (let ((persistent-type (find-type type)))
    (if persistent-type
        (substitute-type-arguments* (args-of persistent-type) (body-of persistent-type) args)
        (error "Unknown type specifier: ~A" type))))

(defun substitute-type-arguments* (args body actual-args)
  (eval `(apply (lambda ,args ,@body) ',actual-args)))

;;;;;;;;;;;;;;;;;;
;;; Canonical type

(defun canonical-type-for (type)
  (iter (for simplified-type :initially type :then (simplify-boolean-form (canonical-type-for* (->dnf simplified-type))))
        (for previous-type :previous simplified-type)
        (until (equal simplified-type previous-type))
        (finally (return simplified-type))))

(defvar *canonical-types* nil
  "A list of type names to be treated as canonical types when a type is converted into canonical form.")

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
    ((?or (or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     t)
    ((and (?* ?x) ?a (?* ?x) ?b (?* ?z)
          (?if (and (persistent-class-type-p ?a)
                    (persistent-class-type-p ?b)
                    (not (intersection (list* (find-class ?a) (persistent-effective-sub-classes-of (find-class ?a)))
                                       (list* (find-class ?b) (persistent-effective-sub-classes-of (find-class ?b))))))))
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
    t)
  "An ordered list of types which are mapped to RDBMS.")

;;;;;;;;;;;;;;;;;
;;; RDBMS mapping

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
           type-parameters)

  ;; TODO: ?
  #+nil
  (:method ((type collection-type) type-parameters)
           (append (list :element-type (parse-type (getf type-parameters :element-type)))
                   (call-next-method type (remove-keywords type-parameters :element-type)))))

(defgeneric parse-positional-type-parameters (type type-parameters)
  (:method (type type-parameters)
           (let ((args (remove-if #L(or (eq !1 '&optional)
                                        (eq !1 '&key)
                                        (eq !1 '&rest)
                                        (eq !1 '&allow-other-keys))
                                  (args-of type))))
             (eval `(apply (lambda ,(args-of type)
                             (list ,@(mappend #L(list (intern (symbol-name !1) (find-package :keyword)) !1) args)))
                     ',type-parameters))))

  ;; TODO: ?
  #+nil
  (:method ((type collection-type) type-parameters)
           (append (list :element-type (parse-type (car type-parameters)))
                   (call-next-method type (cdr type-parameters)))))

(defun parse-type (type-specifier)
  (etypecase type-specifier
    (symbol (find-type type-specifier))
    (list
     (let ((type (make-instance (type-class-name-for (first type-specifier)))))
       (apply #'reinitialize-instance type
              (cond ((= 0 (length type-specifier))
                     nil)
                    ((find-if #'keywordp (cdr type-specifier))
                     (parse-keyword-type-parameters type (rest type-specifier)))
                    (t (parse-positional-type-parameters type (rest type-specifier)))))))
    (persistent-type type-specifier)))
