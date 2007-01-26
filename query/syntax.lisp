(in-package :cl-perec)

;;;; Syntax nodes:
;;;;
;;;; Syntax node
;;;;   type
;;;;
;;;; Literal
;;;;   value
;;;;
;;;; Lexical variable
;;;;   name
;;;;
;;;; Query variable
;;;;   name
;;;;
;;;; Attribute access form
;;;;   attribute
;;;;   arg
;;;;
;;;; Association-end access form
;;;;   association-end
;;;;   arg
;;;;
;;;; Function call
;;;;   function
;;;;   args
;;;;
;;;; Macro call
;;;;   macro-name
;;;;   args
;;;;
;;;; Special form
;;;;   operator
;;;;   args

(defmacro define-syntax-node (name (&rest supers) slots)
  `(progn
    ;; syntax-node class
    ,(bind ((supers (append supers (list 'copyable-mixin))))
           `(defclass* ,name ,supers
             ,slots))
    ;; make
    ,(bind ((make-fn-name (concatenate-symbol "make-" name)))
           `(defun ,make-fn-name (&rest init-args)
             (apply 'make-instance ',name init-args)))
    ;; copy
    ,(bind ((slot-names (mapcar #L(if (consp !1) (first !1) !1) slots)))
           `(define-copy-method copy-inner-class progn ((self ,name) copy copy-htable)
             (with-slot-copying (copy copy-htable self)
               (copy-slots ,@slot-names))))
    ;; predicate
    ,(bind ((predicate-name (if (position #\- (symbol-name name))
                                (concatenate-symbol name "-p")
                                (concatenate-symbol name "p"))))
           `(defun ,predicate-name (object)
             (typep object ',name)))))

;;;
;;; Reader
;;;
(defun pattern-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((spec (read stream t nil t)))
    (if (and (consp spec) (symbolp (first spec)))
        (bind ((instance (allocate-instance (find-class (first spec)))))
          (apply #'shared-initialize instance nil (rest spec)))
        (error "wrong pattern syntax: ~A~%" spec))))

(defmacro enable-pattern-reader (&optional (dispatch-character #\M))
  "Enable the pattern reader for the rest of the file (being loaded or compiled).
Be careful when using in different situations, because it modifies *readtable*."
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (set-dispatch-macro-character #\# ,dispatch-character #'pattern-reader)))

;;;
;;; AST nodes
;;;

(define-syntax-node syntax-object ()
  ((xtype nil)))

(define-syntax-node unparsed-form (syntax-object)
  ((form)))

(define-syntax-node atomic-form (syntax-object)
  ())

(define-syntax-node literal-value (atomic-form)
  ((value)))

(define-syntax-node variable (atomic-form)
  ((name)))

(define-syntax-node lexical-variable (variable)
  ())

(define-syntax-node dynamic-variable (variable)
  ())

(define-syntax-node query-variable (variable)
  ((xtype +the-persistent-object-class+)
   (prefetched-properties)))

(define-syntax-node joined-variable (query-variable)
  ((object :documentation "Object which owns the association-end.")
   (association-end :documentation "The association-end of the object or NIL (means id).")))

(define-syntax-node compound-form (syntax-object)
  ((operator)
   (operands)))

(define-syntax-node function-call (compound-form)
  ((operator :initarg :fn :accessor fn-of)
   (operands :initarg :args :accessor args-of)))

(define-syntax-node property-access (function-call)
  ((operator :initarg :accessor :accessor accessor-of)
   (property)))

(define-syntax-node attribute-access (property-access)
  ((property :initarg :attribute :accessor attribute-of)))

(define-syntax-node association-end-access (property-access)
  ((property :initarg :association-end :accessor association-end-of)))

(define-syntax-node macro-call (compound-form)
  ((operator :initarg :macro :accessor macro-of)
   (operands :initarg :args :accessor args-of)))

(define-syntax-node special-form (compound-form)
  ())

(defmethod make-load-form ((object syntax-object) &optional env)
  (make-load-form-saving-slots
   object
   :slot-names (mapcar 'slot-definition-name (class-slots (class-of object)))
   :environment env))

(defgeneric arg-of (property-access)
  (:method ((access property-access))
           (first (args-of access))))

(defgeneric (setf arg-of) (value property-access)
  (:method (value (access property-access))
           (setf (args-of access) (list value))))

(defmethod print-object ((variable variable) stream)
  (print-unreadable-object (variable stream :type t)
    (when (slot-boundp variable 'name)
      (princ (name-of variable) stream))))

(defmethod print-object ((literal literal-value) stream)
  (print-unreadable-object (literal stream :type t :identity t)
    (when (slot-boundp literal 'value)
      (princ (value-of literal) stream))))


(defmethod print-object ((form compound-form) stream)
  (print-unreadable-object (form stream :type t)
    (princ (if (slot-boundp form 'operator) (operator-of form) "?") stream)
    (princ " " stream)
    (princ (if (slot-boundp form 'operands) (operands-of form) "?") stream)))


;;;;
;;;; Parse/unparse
;;;;

;; PORT: dependency problem
(defgeneric lexical-variables-of (object))

;; PORT: dependency problem
(defgeneric query-variables-of (object))

(defun parse-query-form (form query)
  (bind ((variables (append (lexical-variables-of query) (query-variables-of query))))
    (acond
     ((syntax-object-p form) form)
     ((and (symbolp form) (find form variables :key 'name-of)) it)
     ((and (atom form) (constantp form))
      (make-literal-value :value form))
     ((and (consp form) (eq (first form) 'quote))
      (make-literal-value :value (second form)))
     ((symbolp form)
      (make-dynamic-variable :name form))
     ((and (symbolp (first form)) (slot-accessor-p (first form)))
      (make-attribute-access :accessor (first form)
                             :args (list (parse-query-form (second form) query))))
     ((and (symbolp (first form)) (association-end-accessor-p (first form)))
      (make-association-end-access :accessor (first form)
                                   :args (list (parse-query-form (second form) query))))
     ((and (symbolp (first form)) (macro-function (first form)))
      (make-macro-call :macro (first form)
                       :args (if (parse-args-p (first form))
                                 (mapcar #L(parse-query-form !1 query) (rest form))
                                 (mapcar #L(make-unparsed-form :form !1) (rest form)))))
     ((and (symbolp (first form)) (special-operator-p (first form)))
      (make-special-form :operator (first form)
                         :operands (mapcar #L(make-unparsed-form :form !1) (rest form))))
     ((and (symbolp (first form)) (fboundp (first form)))
      (make-function-call :fn (first form)
                          :args (mapcar #L(parse-query-form !1 query) (rest form))))
     (t (error "Syntax error: ~S~%" form)))))

(defgeneric unparse-query-syntax (syntax)
  (:method ((unparsed unparsed-form))
           (form-of unparsed))
  (:method ((variable variable))
           (name-of variable))
  (:method ((literal literal-value))
           (if (self-evaluating-p (value-of literal))
               (value-of literal)
               `(quote ,(value-of literal))))
  (:method ((form compound-form))
           (cons (operator-of form) (mapcar 'unparse-query-syntax (operands-of form))))
  (:method ((pair cons)) ;; legacy
            (rcons (unparse-query-syntax (car pair))
                   (unparse-query-syntax (cdr pair))
                   pair))
  (:method (object) ;; legacy
           object))

(defun self-evaluating-p (val)
  (and (atom val)
       (or (not (symbolp val))
           (keywordp val)
           (eq val t)
           (eq val nil))))

(defun parse-args-p (macro-name)
  (member macro-name '(and or)))


;;;;
;;;; Pattern matching
;;;;

;;;
;;; Binding environment
;;;

(defconstant failed-match nil)
(defconstant no-bindings (if (boundp 'no-bindings)
			     (symbol-value 'no-bindings)
			     '((t . t))))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t failed-match))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (equal bindings no-bindings)
            nil
            bindings)))

(defun pattern-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

;;;
;;; Matcher (PAIPROLOG matcher + objects)
;;;
(defun pattern-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings failed-match) failed-match)
        ((pattern-variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((object-pattern-p pattern)
         (object-matcher pattern input bindings))
        ((and (consp pattern) (consp input)) 
         (pattern-match (rest pattern) (rest input)
                        (pattern-match (first pattern) (first input) 
                                       bindings)))
        (t failed-match)))

(defmacro pattern-case (expr &body clauses)
  (with-unique-names (expr-var)
    `(bind ((,expr-var ,expr))
      (acond
       ,@(mapcar
          (lambda (clause)
            (bind ((pattern-vars (collect-pattern-variables (car clause))))
              `((pattern-match ',(car clause) ,expr-var)
                (let ,(mapcar #L(`(,!1 (binding-val (get-binding ',!1 it)))) pattern-vars)
                  (declare (ignorable ,@pattern-vars))
                  ,@(cdr clause)))))
          clauses)))))

(defun collect-pattern-variables (syntax &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (labels ((recurse ()
             (typecase syntax
               (syntax-object
                (collect-slots (mapcar 'slot-definition-name (class-slots (class-of syntax)))))
               (cons
                (collect-pattern-variables
                 (car syntax)
                 (collect-pattern-variables (cdr syntax) found-so-far)))
               (otherwise
                found-so-far)))
           (collect-slots (slots)
             (cond
               ((null slots) found-so-far)
               ((slot-boundp syntax (first slots))
                (collect-pattern-variables (slot-value syntax (first slots))
                                           (collect-slots (rest slots))))
               (t (collect-slots (rest slots))))))
    (if (pattern-variable-p syntax)
        (adjoin syntax found-so-far)
        (recurse))))


(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun object-pattern-p (pattern)
  (typep pattern 'syntax-object))

(defun object-matcher (pattern input bindings)
  (labels ((slot-matcher (slots bindings)
             (cond
               ((eq bindings failed-match) failed-match)
               ((null slots) bindings)
               ((and (slot-boundp pattern (first slots))
                     (slot-boundp input (first slots)))
                (slot-matcher (rest slots)
                              (pattern-match (slot-value pattern (first slots))
                                             (slot-value input (first slots))
                                             bindings)))
               ((slot-boundp pattern (first slots)) failed-match)
               (t (slot-matcher (rest slots) bindings)))))
    (if (or (eq bindings failed-match) (not (typep input (class-of pattern))))
        failed-match
        (bind ((slots (mapcar 'slot-definition-name (class-slots (class-of pattern)))))
          (slot-matcher slots bindings)))))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pattern-match var input bindings)))
    (if (or (eq new-bindings failed-match)
            (not (funcall pred input)))
        failed-match
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings failed-match) failed-match)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pattern-match (first patterns) input
                                     bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      failed-match
      (let ((new-bindings (pattern-match (first patterns) 
                                         input bindings)))
        (if (eq new-bindings failed-match)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      failed-match
      bindings))




