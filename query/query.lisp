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
    :type boolean)
   (uniquep
    #f
    :accessor uniquep
    :type boolean)
   (prefetch-mode
    :all
    :type (member :none :accessed :all))
   (result-type
    'list
    :type (member 'list 'scroll))
   (asserts
    nil
    :type list
    :documentation "List of conditions of assert forms.")
   (action
    :collect
    :type (member :collect :purge :update))
   (action-args
    nil
    :type list
    :documentation "List of expressions of the action form.")
   (group-by
    nil
    :type list
    :documentation "List of slot values.")
   (having
    nil
    :type list)
   (order-by
    nil
    :type list
    :documentation "Format: (:ascending <expr1> :descending <expr2> ...)")
   (offset
    nil
    :type (or null integer)
    :documentation "Offset of the first returned element, default is 0")
   (limit
    nil
    :type (or null integer)
    :documentation "Number of max. returned elements, default is all.")
   (sql-select-list
    nil)
   (sql-where
    nil)
   (sql-order-by
    nil
    :type list
    :documentation "Format: (:ascending <sql-expr-1> :descending <sql-expr-2> ...)")))

(define-copy-method copy-inner-class progn ((self query) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots lexical-variables query-variables flatp uniquep prefetch-mode result-type
                asserts action action-args group-by having order-by offset limit sql-select-list
                sql-where sql-order-by)))

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
            (unless (eq (prefetch-mode-of query) :all)
              (list :prefetch-mode (prefetch-mode-of query)))
            (unless (eq (result-type-of query) 'list)
              (list :result-type (result-type-of query))))))

(defun mapc-query (fn query)
  (mapc fn (asserts-of query))
  (mapc fn (action-args-of query))
  (mapc fn (group-by-of query))
  (mapc fn (having-of query))
  (mapc #L(when (syntax-object-p !1) (funcall fn !1)) (order-by-of query)))

(defmethod flatp :around ((query query))
  (if (slot-boundp query 'flatp)
      (call-next-method)
      (<= (length (collects-of query)) 1)))

(defmethod add-lexical-variable ((query query) variable-name)
  (aprog1 (make-lexical-variable :name variable-name)
    (push it (lexical-variables-of query))))

(defmethod add-query-variable ((query query) variable-name)
  (aprog1 (make-query-variable :name variable-name)
    (push it (query-variables-of query))))

(defun get-query-variable-names (query)
  (mapcar 'name-of (query-variables-of query)))

(defun get-query-variable-types (query)
  (mapcar 'persistent-type-of (query-variables-of query)))

(defun add-joined-variable (query variable)
  (push variable (query-variables-of query)))

(defun get-variables (query)
  (append (lexical-variables-of query) (query-variables-of query)))

(defgeneric select-form-of (query)
  (:method ((query query))
    (flet ((optional (clause)
             (when clause (list clause))))
      (ecase (action-of query)
        (:collect
            (bind ((action-list (action-args-of query))
                   (options (options-of query))
                   (variables (get-query-variable-names query))
                   (asserts (asserts-of query))
                   (where-clause (case (length asserts)
                                   (0 nil)
                                   (1 `(where ,(first asserts)))
                                   (t `(where (and ,@asserts)))))
                   (group-by-clause (when (group-by-of query) `(group-by ,@(group-by-of query))))
                   (having-clause (case (length (having-of query))
                                    (0 nil)
                                    (1 `(having ,(first (having-of query))))
                                    (t `(having (and ,@(having-of query))))))
                   (order-by-clause (when (order-by-of query) `(order-by ,@(order-by-of query))))
                   (offset-clause (when (offset-of query) `(offset ,(offset-of query))))
                   (limit-clause (when (limit-of query) `(limit ,(limit-of query)))))
              `(select ,@(optional options) ,action-list
                       (from ,@variables)
                       ,@(optional where-clause)
                       ,@(optional group-by-clause)
                       ,@(optional having-clause)
                       ,@(optional order-by-clause)
                       ,@(optional offset-clause)
                       ,@(optional limit-clause))))
        (:purge
            (bind ((action-list (action-args-of query))
                   (options (options-of query))
                   (variables (get-query-variable-names query))
                   (asserts (asserts-of query))
                   (where-clause (case (length asserts)
                                   (0 nil)
                                   (1 `(where ,(first asserts)))
                                   (t `(where (and ,@asserts))))))
              `(purge ,@(optional options) ,action-list
                      (from ,@variables)
                      ,@(optional where-clause))))
        (:update
            (bind ((action-list (action-args-of query))
                   (options (options-of query))
                   (variables (get-query-variable-names query))
                   (asserts (asserts-of query))
                   (from-clause (when (rest variables) `(from ,@(rest variables))))
                   (where-clause (case (length asserts)
                                   (0 nil)
                                   (1 `(where ,(first asserts)))
                                   (t `(where (and ,@asserts))))))
              `(update ,@(optional options) (,(first variables))
                       (set ,@action-list)
                       ,@(optional from-clause)
                       ,@(optional where-clause))))))))


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

(defmethod add-group-by ((query query) expression)
  (appendf (group-by-of query) (list expression)))

(defmethod add-having ((query query) expression)
  (appendf (having-of query) (list expression)))

(defmethod add-order-by ((query query) expression &optional (direction :ascending))
  (assert (member direction '(:asc :ascending :desc :descending)))
  (nconcf (order-by-of query) (list direction expression)))

(defmethod set-order-by ((query query) expression &optional (direction :ascending))
  (assert (member direction '(:asc :ascending :desc :descending)))
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

(defmethod add-order-by ((query simple-query-builder) expression &optional (direction :ascending))
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
             (if (member (first form) '(select purge update))
                 form
                 (bind (((:values form expanded-p) (macroexpand-1 form)))
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
           (find-clause (clause-name clauses &optional (optionalp #t))
             (bind ((found-clauses (remove clause-name clauses :key #'first :test-not #'eql)))
               (case (length found-clauses)
                 (0 (unless optionalp
                      (error 'missing-query-clause-error :form form :clause clause-name)))
                 (1 (first found-clauses))
                 (t (error 'duplicated-query-clause-error :form form :clause clause-name)))))
           (check-where-clause (clause)
             (when (and clause (not (length= 1 (rest clause))))
               (error 'malformed-query-clause-error
                      :form form
                      :clause-form clause
                      :detail "One condition expected in WHERE clause."))
             clause)
           (check-group-by-clause (clause)
             clause)
           (check-having-clause (clause)
             (when (and clause (not (length= 1 (rest clause))))
               (error 'malformed-query-clause-error
                      :form form
                      :clause clause
                      :detail "One condition expected in HAVING clause."))
             clause)
           (check-order-by-clause (clause)
             (when clause
               (unless (evenp (length (rest clause)))
                 (error 'malformed-query-clause-error
                        :form form
                        :clause clause
                        :detail "Plist expected in the body of the ORDER-BY clause."))
               (iter (for (dir expr) on (rest clause) by #'cddr)
                     (unless (member dir '(:asc :ascending :desc :descending))
                       (error 'malformed-query-clause-error
                              :form form
                              :clause clause
                              :detail ":ASCENDING or :DESCENDING expected as sorting directions."))))
             clause)
           (check-offset-limit-clause (clause)
             (when clause
               (unless (length= 1 (rest clause))
                 (error 'malformed-query-clause-error
                        :form form
                        :clause clause
                        :detail "OFFSET/LIMIT expect one integer argument.")))
             clause)
           (make-select (options select-list clauses)
             (remf options :compile-at-macroexpand)
             (bind ((lexical-variables (make-lexical-variables lexical-variables))
                    (from-clause (find-clause 'from clauses #f))
                    (where-clause (check-where-clause (find-clause 'where clauses)))
                    (group-by-clause (check-group-by-clause (find-clause 'group-by clauses)))
                    (having-clause (check-having-clause (find-clause 'having clauses)))
                    (order-by-clause (check-order-by-clause (find-clause 'order-by clauses)))
                    (offset-clause (check-offset-limit-clause (find-clause 'offset clauses)))
                    (limit-clause (check-offset-limit-clause (find-clause 'limit clauses)))
                    (query-variables (make-query-variables (rest from-clause)))
                    (asserts (make-asserts where-clause (rest from-clause)))
                    (extra-clauses (set-difference clauses
                                                   (list from-clause where-clause group-by-clause
                                                         having-clause order-by-clause offset-clause
                                                         limit-clause))))
               (when extra-clauses
                 (error 'unrecognized-query-clause-error
                        :form form
                        :clause (first (first extra-clauses))))

               (apply 'make-instance 'query
                      :lexical-variables lexical-variables
                      :query-variables query-variables
                      :asserts asserts
                      :action :collect
                      :action-args select-list
                      :group-by (rest group-by-clause)
                      :having (rest having-clause)
                      :order-by (rest order-by-clause)
                      :offset (second offset-clause)
                      :limit (second limit-clause)
                      options)))
           (make-purge (options purge-list clauses)
             (remf options :compile-at-macroexpand)
             (bind ((lexical-variables (make-lexical-variables lexical-variables))
                    (from-clause (find-clause 'from clauses))
                    (where-clause (check-where-clause (find-clause 'where clauses)))
                    (query-variables (make-query-variables (rest from-clause)))
                    (asserts (make-asserts where-clause (rest from-clause)))
                    (extra-clauses (remove from-clause (remove where-clause clauses))))

               (when extra-clauses
                 (error 'unrecognized-query-clause-error
                        :form form
                        :clause (first (first extra-clauses))))

               (apply 'make-instance 'query
                      :lexical-variables lexical-variables
                      :query-variables query-variables
                      :asserts asserts
                      :action :purge
                      :action-args purge-list
                      options)))
           (make-update (options update-list clauses)
             (remf options :compile-at-macroexpand)
             (bind ((lexical-variables (make-lexical-variables lexical-variables))
                    (set-clause (find-clause 'set clauses))
                    (from-clause (find-clause 'from clauses))
                    (query-variables (make-query-variables (list* update-list (rest from-clause))))
                    (where-clause (check-where-clause (find-clause 'where clauses)))
                    (asserts (make-asserts where-clause (list* update-list (rest from-clause))))
                    (extra-clauses (set-difference clauses (list set-clause from-clause where-clause))))
               (when extra-clauses
                 (error 'unrecognized-query-clause-error
                        :form form
                        :clause (first (first extra-clauses))))

               (apply 'make-instance 'query
                      :lexical-variables lexical-variables
                      :query-variables query-variables
                      :asserts asserts
                      :action :update
                      :action-args (rest set-clause)
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
      ((update (?and (?or nil ((?is ?k keywordp) . ?rest)) ?options) (?is ?update-list listp) .
               ?clauses)
       (make-update ?options ?update-list ?clauses))
      ((update  (?is ?update-list listp) . ?clauses)
       (make-update nil ?update-list ?clauses))
      (?otherwise
       (error 'query-syntax-error
              :form form)))))
