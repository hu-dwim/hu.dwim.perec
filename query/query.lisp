;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;(declaim-debug)

;;;
;;; Query
;;;
(defclass* query (copyable-mixin)
  ((lexical-variables
    nil
    :type (list lexical-variable))
   (query-variables
    nil
    :type (list query-variable))
   (body
    nil
    :type list)
   (flatp
    :writer (setf flatp)
    :type boolean)
   (uniquep
    #f
    :accessor uniquep
    :type boolean)
   (prefetchp
    #t
    :accessor prefetchp
    :type boolean)
   (result-type
    'list
    :type (member 'list 'scroll))))

(define-copy-method copy-inner-class progn ((self query) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots lexical-variables query-variables body flatp uniquep prefetchp result-type)))

(defmethod print-object ((query query) stream)
  (print-unreadable-object (query stream :type t)
    (prin1 (query-hash-key-for query) stream)))

(defun query-hash-key-for (query)
  (list (mapcar 'name-of (lexical-variables-of query)) (select-form-of query)))

(defgeneric select-form-of (query)
  (:method ((query query))
           `(select ,(options-of query) ,(get-query-variable-names query)
             ,@(body-of query))))

(defgeneric options-of (query)
  (:method ((query query))
           (nconc
            (when (slot-boundp query 'flatp)
              (list :flatp (flatp query)))
            (when (uniquep query)
              (list :uniquep #t))
            (when (not (prefetchp query))
              (list :prefetchp #f))
            (when (not (eq (result-type-of query) 'list))
              (list :result-type (result-type-of query))))))

(defgeneric flatp (query)
  (:method ((query query))
    (and (slot-boundp query 'flatp)
         (slot-value query 'flatp))))

(defmethod add-lexical-variable ((query query) variable-name)
  (aprog1 (make-lexical-variable :name variable-name)
    (push it (lexical-variables-of query))))

(defmethod add-query-variable ((query query) variable-name)
  (aprog1 (make-query-variable :name variable-name)
    (push it (query-variables-of query))))

(defun find-query-variable (query variable-name)
  (find variable-name (query-variables-of query) :key 'name-of))

(defun find-lexical-variable (query variable-name)
  (find variable-name (lexical-variables-of query) :key 'name-of))

(defun find-variable (query variable-name)
  (or (find-query-variable query variable-name)
      (find-lexical-variable query variable-name)))

(defun get-query-variable-names (query)
  (mapcar 'name-of (query-variables-of query)))

(defun get-query-variable-types (query)
  (mapcar 'xtype-of (query-variables-of query)))

(defun add-joined-variable (query variable)
  (push variable (query-variables-of query)))

(defun get-variables (query)
  (append (lexical-variables-of query) (query-variables-of query)))

;;;
;;; Simple query
;;;
(defclass* simple-query (query)
  ((asserts
    nil
    :type list
    :documentation "List of conditions of assert forms.")
   (action
    :collect
    :type (member :collect :purge))
   (action-args
    nil
    :type list
    :documentation "List of expressions of the action form.")
   (order-by
    nil
    :type list
    :documentation "Format: (:asc <expr1> :desc <expr2> ...)")
   (where-clause
    nil))
  (:documentation "SIMPLE-QUERY only contains (assert ...) forms and one (collect ...) and
 optionally an ORDER-BY clause form at top-level."))

(define-copy-method copy-inner-class progn ((self simple-query) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots asserts action action-args order-by where-clause)))

(defgeneric collects-of (query)
  (:method ((query simple-query))
           (debug-only (assert (eq (action-of query) :collect)))
           (action-args-of query)))

(defgeneric (setf collects-of) (value query)
  (:method (value (query simple-query))
           (debug-only (assert (eq (action-of query) :collect)))
           (setf (action-args-of query) value)))

(defmethod add-assert ((query simple-query) condition)
  (appendf (asserts-of query) (list condition)))

(defmethod add-collect ((query simple-query) expression)
  (appendf (collects-of query) (list expression)))

(defmethod add-order-by ((query simple-query) expression &optional (direction :asc))
  (assert (member direction '(:asc :desc)))
  (nconcf (order-by-of query) (list direction expression)))

(defmethod set-order-by ((query simple-query) expression &optional (direction :asc))
  (assert (member direction '(:asc :desc)))
  (setf (order-by-of query) (list direction expression)))

(defgeneric add-where-clause (query where-clause)
  (:method ((query simple-query) where-clause)
           (cond
             ((not (where-clause-of query))
              (setf (where-clause-of query) where-clause))
             ((and (listp (where-clause-of query)) (eq (car (where-clause-of query)) 'sql-and))
              (appendf (where-clause-of query) (list where-clause)))
             (t
              (setf (where-clause-of query)
                    `(sql-and ,(where-clause-of query) ,where-clause))))))

(defmethod body-of ((query simple-query))
  `(,@(mapcar #L(`(assert ,!1)) (asserts-of query))
    ,(if (eq (action-of query) :collect)
         `(collect ,@(collects-of query))
         `(purge ,@(action-args-of query)))
    ,@(when (order-by-of query)
            `(order-by ,@(order-by-of query)))))

(defmethod flatp ((query simple-query))
  (if (slot-boundp query 'flatp)
      (call-next-method)
      (<= (length (collects-of query)) 1)))

;;;
;;; Query builder
;;;
(defclass* query-builder (copyable-mixin)
  ((current-query-variable nil)))

(define-copy-method copy-inner-class progn ((self query-builder) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots current-query-variable)))

(defclass* simple-query-builder (query-builder simple-query)
  ())

(defun preprocess-query-expression (query expression)
  (setf expression (tree-substitute (name-of (current-query-variable-of query))
                                    '*current-query-variable* expression))
  expression)

(defmethod add-query-variable ((query query-builder) variable-name)
  (setf (current-query-variable-of query) (call-next-method)))

(defmethod add-assert ((query simple-query-builder) condition)
  (call-next-method query (preprocess-query-expression query condition)))

(defmethod add-collect ((query simple-query-builder) expression)
  (call-next-method query (preprocess-query-expression query expression)))

(defmethod add-order-by ((query simple-query-builder) expression &optional (direction :asc))
  (call-next-method query (preprocess-query-expression query expression) direction))

;;;
;;; Parse select forms
;;;
(defmethod make-query ((select-form null) &optional lexical-variables)
  (let ((query (make-instance 'simple-query-builder)))
    (iter (for variable in lexical-variables)
          (add-lexical-variable query variable))
    query))

(defmethod make-query ((select-form cons) &optional lexical-variables)

  (labels ((select-macro-expand (select-form)
             (if (eq (first select-form) 'select)
                 select-form
                 (bind (((values select-form expanded-p) (macroexpand-1 select-form)))
                   (if expanded-p
                       (select-macro-expand select-form)
                       select-form))))
           (make-lexical-variables (variable-names)
             (iter (for variable-name in variable-names)
                   (collect (make-lexical-variable :name variable-name))))
           (make-query-variables (variable-specs)
             (iter (for variable-spec in variable-specs)
                   (typecase variable-spec
                     (symbol (collect (make-query-variable :name variable-spec)))
                     (cons (collect (make-query-variable :name (car variable-spec))))
                     (otherwise (error "Symbol or symbol/type pair expected, found ~S in select: ~:W"
                                       variable-spec select-form)))))
           (add-variable-type-asserts (variable-specs body)
             (dolist (variable-spec variable-specs body)
               (when (and (listp variable-spec) (>= (length variable-spec) 2))
                 (push `(assert (typep ,(first variable-spec) ',(second variable-spec))) body))))
           (make-query (options vars body)
             (bind ((lexical-variables (make-lexical-variables lexical-variables))
                    (query-variables (make-query-variables vars))
                    (body (add-variable-type-asserts vars body))
                    (collect-form (find 'collect body :key 'first :test 'eq))
                    (purge-form (find 'purge body :key 'first :test 'eq))
                    (order-by-form (find 'order-by body :key 'first :test 'eq))
                    (other-forms (remove order-by-form (remove purge-form (remove collect-form body))))
                    (simple-query-p (and (or collect-form purge-form)
                                         (every #L(eql (car !1) 'assert) other-forms))))

               (if simple-query-p
                   (apply 'make-instance 'simple-query
                          :lexical-variables lexical-variables
                          :query-variables query-variables
                          :body body
                          :asserts (mapcar 'second other-forms)
                          :action (if collect-form :collect :purge)
                          :action-args (if collect-form (cdr collect-form) (cdr purge-form))
                          :order-by (cdr order-by-form)
                          options)
                   (apply 'make-instance 'query
                          :lexical-variables lexical-variables
                          :query-variables query-variables
                          :body body
                          options)))))
    
    (pattern-case (select-macro-expand select-form)
      ((select (?and ((?is ?k keywordp) . ?rest) ?options) (?is ?variable-specs listp) . ?body)
       (make-query ?options ?variable-specs ?body))
      ((select (?is ?variable-specs listp) . ?body)
       (make-query nil ?variable-specs ?body))
      (?otherwise
       (error "Malformed select statement: ~:W" select-form)))))
