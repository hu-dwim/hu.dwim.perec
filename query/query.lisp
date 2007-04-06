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
    :type (member 'list 'scroll))
   (asserts
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
   (sql-order-by
    nil
    :type list
    :documentation "Format: (:asc <sql-expr-1> :desc <sql-expr-2> ...)")
   (sql-where
    nil)))

(define-copy-method copy-inner-class progn ((self query) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots lexical-variables query-variables body flatp uniquep prefetchp result-type
                asserts action action-args order-by sql-order-by sql-where)))

(defmethod print-object ((query query) stream)
  (print-unreadable-object (query stream :type t)
    (prin1 (query-hash-key-for query) stream)))

(defun query-hash-key-for (query)
  (list (mapcar 'name-of (lexical-variables-of query)) (select-form-of query)))

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
           (if (slot-boundp query 'flatp)
               (slot-value query 'flatp)
               (<= (length (collects-of query)) 1))))

(defmethod add-lexical-variable ((query query) variable-name)
  (aprog1 (make-lexical-variable :name variable-name)
    (push it (lexical-variables-of query))))

(defmethod add-query-variable ((query query) variable-name)
  (aprog1 (make-query-variable :name variable-name)
    (push it (query-variables-of query))))

(defun get-query-variable-names (query)
  (mapcar 'name-of (query-variables-of query)))

(defun get-query-variable-types (query)
  (mapcar 'xtype-of (query-variables-of query)))

(defun add-joined-variable (query variable)
  (push variable (query-variables-of query)))

(defun get-variables (query)
  (append (lexical-variables-of query) (query-variables-of query)))

(defgeneric select-form-of (query)
  (:method ((query query))
           (flet ((optional (clause)
                    (when clause (list clause))))
             (bind ((action (ecase (action-of query) (:collect 'select) (:purge 'purge)))
                    (action-list (action-args-of query))
                    (options (options-of query))
                    (variables (get-query-variable-names query))
                    (asserts (asserts-of query))
                    (where-clause (case (length asserts)
                                    (0 nil)
                                    (1 `(where ,(first asserts)))
                                    (t `(where (and ,@asserts)))))
                    (order-by-clause (when (order-by-of query) `(order-by ,@(order-by-of query)))))
             `(,action ,options ,action-list
                 (from ,@variables)
                 ,@(optional where-clause)
                 ,@(optional order-by-clause))))))


(defgeneric collects-of (query)
  (:method ((query query))
           (assert (eq (action-of query) :collect))
           (action-args-of query)))

(defgeneric (setf collects-of) (value query)
  (:method (value (query query))
           (assert (eq (action-of query) :collect))
           (setf (action-args-of query) value)))

(defmethod add-assert ((query query) condition)
  (appendf (asserts-of query) (list condition)))

(defmethod add-collect ((query query) expression)
  (appendf (collects-of query) (list expression)))

(defmethod add-order-by ((query query) expression &optional (direction :asc))
  (assert (member direction '(:asc :desc)))
  (nconcf (order-by-of query) (list direction expression)))

(defmethod set-order-by ((query query) expression &optional (direction :asc))
  (assert (member direction '(:asc :desc)))
  (setf (order-by-of query) (list direction expression)))

(defgeneric add-where-clause (query where-clause)
  (:method ((query query) where-clause)
           (setf (sql-where-of query)
                 (combine-with 'sql-and (sql-where-of query) where-clause))))

;;;
;;; Query builder
;;;
(defclass* query-builder (copyable-mixin)
  ((current-query-variable nil)))

(define-copy-method copy-inner-class progn ((self query-builder) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots current-query-variable)))

(defclass* simple-query-builder (query-builder query)
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

(defmethod make-query ((form cons) &optional lexical-variables)

  (labels ((query-macro-expand (form)
             (if (member (first form) '(select purge))
                 form
                 (bind (((values form expanded-p) (macroexpand-1 form)))
                   (if expanded-p
                       (query-macro-expand form)
                       form))))
           (make-lexical-variables (variable-names)
             (iter (for variable-name in variable-names)
                   (collect (make-lexical-variable :name variable-name))))
           (make-query-variables (variable-specs)
             (iter (for variable-spec in variable-specs)
                   (typecase variable-spec
                     (symbol (collect (make-query-variable :name variable-spec)))
                     (cons (collect (make-query-variable :name (car variable-spec))))
                     (otherwise (error "Symbol or symbol/type pair expected, found ~S in select: ~:W"
                                       variable-spec form)))))
           (make-asserts (where-clause variable-specs)
             (append
              (iter (for variable-spec in variable-specs)
                    (when (and (listp variable-spec) (>= (length variable-spec) 2))
                      (collect `(typep ,(first variable-spec) ',(second variable-spec)))))
              (when where-clause
                (bind ((where-cond (second where-clause)))
                  (if (and (listp where-cond) (eq 'and (car where-cond)))
                      (rest where-cond)
                      (list where-cond))))))
           (make-select (options select-list clauses)
             (bind ((lexical-variables (make-lexical-variables lexical-variables))
                    (from-clause (find 'from clauses :key #'first))
                    (where-clause (find 'where clauses :key #'first))
                    (order-by-clause (find 'order-by clauses :key #'first))
                    (query-variables (make-query-variables (rest from-clause)))
                    (asserts (make-asserts where-clause (rest from-clause)))
                    (other-clauses (remove from-clause (remove where-clause (remove order-by-clause clauses)))))
               (unless from-clause
                 (error "Missing FROM clause in: ~:W" form))
               (when other-clauses
                 (error "Unrecognized or duplicated clauses ~:W in query ~:W"
                        other-clauses form))

               (apply 'make-instance 'query
                      :lexical-variables lexical-variables
                      :query-variables query-variables
                      :asserts asserts
                      :action :collect
                      :action-args select-list
                      :order-by (rest order-by-clause)
                      options)))
           (make-purge (options purge-list clauses)
             (bind ((lexical-variables (make-lexical-variables lexical-variables))
                    (from-clause (find 'from clauses :key 'first))
                    (where-clause (find 'where clauses :key 'first))
                    (query-variables (make-query-variables (rest from-clause)))
                    (asserts (make-asserts where-clause (rest from-clause)))
                    (other-clauses (remove from-clause (remove where-clause clauses))))
               (unless from-clause
                 (error "Missing FROM clause in: ~:W" form))
               (when other-clauses
                 (error "Unrecognized or duplicated clauses ~:W in query ~:W"
                        other-clauses form))

               (apply 'make-instance 'query
                      :lexical-variables lexical-variables
                      :query-variables query-variables
                      :asserts asserts
                      :action :purge
                      :action-args purge-list
                      options))))
    
    (pattern-case (query-macro-expand form)
      ((select (?and (?or nil ((?is ?k keywordp) . ?rest)) ?options) (?is ?select-list listp) .
               ?clauses)
       (make-select ?options ?select-list ?clauses))
      ((select (?is ?select-list listp) . ?clauses)
       (make-select nil ?select-list ?clauses))
      ((purge (?and (?or nil ((?is ?k keywordp) . ?rest)) ?options) (?is ?purge-list listp) .
              ?clauses)
       (make-purge ?options ?purge-list ?clauses))
      ((purge  (?is ?purge-list listp) . ?clauses)
       (make-purge nil ?purge-list ?clauses))
      (?otherwise
       (error "Malformed query: ~:W" form)))))
