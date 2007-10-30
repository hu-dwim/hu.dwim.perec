(in-package :cl-perec)

;;;----------------------------------------------------------------------------
;;; SQL mapping of expressions
;;;

;;; TODO: inherited associations mapped to column in the derived entity (missing)
;;;

(defun transform-to-sql (condition)
  "Transforms the CONDITION of an assert to an SQL expression."
    (bind ((sql nil)
          (success #f))
     (catch 'sql-map-failed
       (setf sql (syntax-to-sql condition))
       (setf success #t))
     (values sql success)))

(defun sql-map-failed ()
  (throw 'sql-map-failed nil))


(defgeneric syntax-to-sql (syntax)
  (:documentation "Maps a lisp form to SQL.")
  
  (:method (syntax)
           (if (free-of-query-variables-p syntax)
               `(value->sql-literal ,syntax ,(backquote-type-syntax (xtype-of syntax)))
               (sql-map-failed)))

  (:method ((literal literal-value))
           (literal-to-sql (value-of literal) (xtype-of literal) literal))

  (:method ((variable lexical-variable))
           `(value->sql-literal ,(name-of variable) ,(backquote-type-syntax (xtype-of variable))))

  (:method ((variable query-variable))
           (sql-id-column-reference-for variable))

  (:method ((access slot-access))
           (slot-access-to-sql (accessor-of access) (arg-of access) access))

  (:method ((call function-call))
           (bind ((fn (fn-of call))
                  (args (args-of call)))
             (function-call-to-sql fn (length args) (first args) (second args) call)))

  (:method ((call macro-call))
           (bind ((macro (macro-of call))
                  (args (args-of call)))
             (macro-call-to-sql macro (length args) (first args) (second args) call))))

(defgeneric literal-to-sql (value type literal)
  (:documentation "Maps a literal value to SQL.")

  (:method (value type literal)
           (cond
             ((and (keywordp value) (eq type +unknown-type+)) value)
             ((syntax-object-p type) `(value->sql-literal ,literal ,(backquote-type-syntax type)))
             (t (value->sql-literal value type)))))

(defgeneric slot-access-to-sql (accessor arg access)
  (:method (accessor arg access)
           (if (free-of-query-variables-p access)
               `(value->sql-literal ,access ,(backquote-type-syntax (xtype-of access)))
               (sql-map-failed)))

  (:method (accessor (variable query-variable) (access slot-access))
           ;; slot accessor
           (bind ((slot (slot-of access)))
             (if (and slot (persistent-slot-p slot))
                 (sql-column-reference-for slot variable)
                 (sql-map-failed))))

  (:method (accessor (variable query-variable) (access association-end-access))
           ;; association-end accessor
           (if (association-end-of access)
               (bind ((association-end (association-end-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (if (primary-association-end-p association-end)
                        (sql-column-reference-for association-end variable)
                        (sql-subselect-for-secondary-association-end association-end variable)))
                   (:1-n
                    (if (to-one-association-end-p association-end)
                        (sql-column-reference-for association-end variable)
                        (sql-subselect-for-secondary-association-end association-end variable)))
                   (:m-n
                    (sql-subselect-for-m-n-association association-end variable))))
               (sql-map-failed))))


(defgeneric function-call-to-sql (fn n-args arg1 arg2 call)

  (:method (fn n-args arg1 arg2 call)
           (cond
             ;; eq,eql and friends
             ;;   special cases: one or both of the args is NIL
             ;;                  one or both of the args is unbound
             ((member fn '(eq eql equal))
              (sql-equal
               (syntax-to-sql arg1)
               (syntax-to-sql arg2)
               :unbound-check-1 (unbound-check-for arg1)
               :unbound-check-2 (unbound-check-for arg2)
               :null-check-1 (null-check-for arg1)
               :null-check-2 (null-check-for arg2)))
             ;; (<fn> <arg> ...), where <fn> has SQL counterpart
             ;; e.g. (+ 1 2) --> (1 + 2)
             ((sql-operator-p fn)
              `(funcall ',(sql-operator-for fn) ,@(mapcar 'syntax-to-sql (args-of call))))
             ;; When the function call does not depend on query variables
             ;; evaluate it at runtime and insert its value into the SQL query.
             ;; The persistent-objects in the value are converted to object ids.
             ((every 'free-of-query-variables-p (args-of call))
              `(value->sql-literal ,call ,(backquote-type-syntax (xtype-of call))))
             ;; Otherwise the assert containing the function call cannot be mapped to SQL.
             (t
              (sql-map-failed))))

  ;; member form -> in (ignore keyword args, TODO)
  (:method ((fn (eql 'member)) n-args arg1 arg2 call)
           (cond
             ((literal-value-p arg2)
              (if (null (value-of arg2))
                  (sql-false-literal)
                  `(sql-in ,(syntax-to-sql arg1) ,(syntax-to-sql arg2))))
             ((free-of-query-variables-p arg2)
              `(if (null ,(unparse-query-syntax arg2))
                (sql-false-literal)
                ,(unparse-query-syntax `(sql-in ,(syntax-to-sql arg1) ,(syntax-to-sql arg2)))))
             (t `(sql-in ,(syntax-to-sql arg1) ,(syntax-to-sql arg2)))))

  ;; (member <object> (<association-end-accessor> <query-variable>))
  ;; e.g. (member m1 (messages-of topic)) --> (_m1.topic_id = _topic.id)
  (:method ((fn (eql 'member)) (n-args (eql 2)) (object query-variable) (access association-end-access) call)
           ;; member form -> join
           ;;   example:
           ;;   (member m (messages-of t)) -> m.topic_id = t.id
           (if (or (not (query-variable-p (arg-of access)))
                   (not (association-end-of access)))
               (call-next-method)
               (bind ((association-end (association-end-of access))
                      (variable (arg-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (sql-map-failed))
                   (:1-n
                    (sql-join-condition-for object variable association-end))
                   (:m-n
                    (sql-join-condition-for-m-n-association object variable association-end))))))

  ;; eq form -> join
  ;;   examples:
  ;;   (eq (topic-of message) topic) -> message.topic_id = topic.id
  ;;   (eq (wife-of man) woman)      -> man.wife_id = woman.id
  ;;   (eq (husband-of woman) man)   -> man.wife_id = woman.id
  (:method ((fn (eql 'eq)) (n-args (eql 2)) (access association-end-access) object call)

           (if (not (association-end-of access))
               (sql-map-failed)
               (bind ((association-end (association-end-of access))
                      (other-end (other-association-end-of association-end))
                      (variable (arg-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (if (primary-association-end-p association-end)
                        (call-next-method)
                        (syntax-to-sql
                         (make-function-call ;; reverse
                          :fn fn
                          :args (list (make-association-end-access :association-end other-end
                                                                   :accessor (reader-name-of other-end)
                                                                   :args (list object))
                                      variable)))))
                   (:1-n
                    (call-next-method))
                   (:m-n
                    (sql-map-failed))))))

  (:method ((fn (eql 'eq)) (n-args (eql 2)) object (access association-end-access) call)
           (if (association-end-access-p object)
               (call-next-method)
               (function-call-to-sql fn 2 access object call)))

  ;; (typep variable type)
  ;;   example:
  ;;   (typep o #<class user>) -> exists(select 1 from user u where u.id = o.id)
  (:method ((fn (eql 'typep)) (n-args (eql 2)) (variable query-variable) (type literal-value) call)
           (if (persistent-class-p (value-of type))
               (sql-exists-subselect-for-variable variable (value-of type))
               (call-next-method)))

  ;; (null (accessor variable))

  ;; TODO NOT EXIST (subselect)
  ;;   e.g ((and (association-end-access-p expr) (not (contains-syntax-p expr)))
  ;;         `(cache-instance-with-prefetched-slots ,row ,i ,(normalized-type-for (xtype-of expr)) nil '(1)))
  (:method ((fn (eql 'null)) (n-args (eql 1)) (access slot-access) arg2 call)
           (bind ((slot (slot-of access)))
             (if (and slot
                      (not (typep slot 'persistent-association-end-effective-slot-definition))
                      (query-variable-p (arg-of access)))
                 (sql-slot-is-null (arg-of access) slot)
                 (call-next-method))))

  ;; (slot-boundp variable slot-name)
  (:method ((fn (eql 'slot-boundp)) (n-args (eql 2)) (variable query-variable) (slot-name literal-value) call)
           ;; TODO: accept non-literal slot-name
           (bind ((slot-name (when (symbolp (value-of slot-name)) (value-of slot-name)))
                  (slot (when slot-name (find-slot-definition variable slot-name))))
             (if (and slot (typep slot 'persistent-effective-slot-definition))
                 (sql-slot-boundp variable slot)
                 (sql-map-failed))))

  ;; (length (<n-ary-association-end-accessor> <query-var>))
  ;; e.g. (length (messages-of topic)) -->
  ;;         (select count(_m.id) from _message _m where _m.topic_id = _topic.id)
  (:method ((fn (eql 'length)) (n-args (eql 1)) (access association-end-access) arg2 call)
           (if (and (association-end-of access)
                    (query-variable-p (arg-of access)))
               (ecase (association-kind-of (association-of (association-end-of access)))
                 (:1-1
                  (sql-map-failed))
                 (:1-n
                  (sql-aggregate-subselect-for-variable
                   'sql-count
                   (association-end-of access)
                   (arg-of access)))
                 (:m-n
                  (sql-aggregate-subselect-for-m-n-association-end
                   'sql-count
                   (association-end-of access)
                   (arg-of access))))
               (call-next-method))))

(defgeneric macro-call-to-sql (macro n-args arg1 arg2 call)
  (:method (macro n-args arg1 arg2 call)
           (cond
             ((sql-operator-p macro)
              `(funcall ',(sql-operator-for macro) ,@(mapcar 'syntax-to-sql (args-of call))))
             ((every 'free-of-query-variables-p (args-of call))
              `(value->sql-literal ,call ,(backquote-type-syntax (xtype-of call))))
             (t
              (sql-map-failed)))))

(defun free-of-query-variables-p (syntax)
  (typecase syntax
    (query-variable #f)
    (unparsed-form (free-of-query-variables-p (form-of syntax)))
    (compound-form (every 'free-of-query-variables-p (operands-of syntax)))
    (cons (and (free-of-query-variables-p (car syntax))
               (free-of-query-variables-p (cdr syntax))))
    (otherwise #t)))

(defun find-slot-definition (owner slot-name)
  (find-slot-by-owner-type owner (effective-slots-for-slot-name slot-name)))

(defgeneric unbound-check-for (syntax)
  (:method (syntax)
           nil)

  (:method ((access slot-access))
           (bind ((type (xtype-of access))
                  (slot (slot-of access))
                  (variable (arg-of access)))
             (debug-only (assert (not (contains-syntax-p type))))
             (when (and slot (query-variable-p variable))
               (cond
                 ((eq type +unknown-type+) (sql-map-failed))
                 ((complex-type-p type) `(sql-not ,(sql-bound-column-reference-for slot variable)))
                 ((unbound-subtype-p type) `(sql-is-null ,(sql-column-reference-for slot variable)))
                 (t nil)))))

  (:method ((access association-end-access))
           nil))

;; TODO needs review
(defgeneric null-check-for (syntax)
  (:method (syntax)
           nil)

  (:method ((variable lexical-variable))
           (bind ((type (xtype-of variable)))
             (if (maybe-null-subtype-p type)
                 `(sql-is-null ,(syntax-to-sql variable))
                 nil)))

  (:method ((access slot-access))
           (bind ((type (xtype-of access))
                  (slot (slot-of access))
                  (variable (arg-of access)))
             (debug-only (assert (not (contains-syntax-p type))))
             (if (and slot (query-variable-p variable) (maybe-null-subtype-p type))
                 `(sql-is-null ,(sql-column-reference-for slot variable))
                 nil)))

  (:method ((access association-end-access))
           nil))


