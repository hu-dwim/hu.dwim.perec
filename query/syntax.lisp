;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

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
;;;; Slot access form
;;;;   slot
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
             ;; TODO: make this a macro because applying make-instance is a hell slow thing to do just for some syntax
             (apply #'make-instance ',name init-args)))
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

(defconstant +unknown-type+ :unknown)

(define-syntax-node syntax-object ()
  ((xtype +unknown-type+)
   (volatilep :accessor volatilep)))

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
  ((xtype +persistent-object-class+)))

(define-syntax-node joined-variable (query-variable)
  ((object :documentation "Object which owns the association-end.")
   (association-end :documentation "The association-end of the object or NIL (means id).")))

(define-syntax-node compound-form (syntax-object)
  ((operator)
   (operands)))

(define-syntax-node function-call (compound-form)
  ((operator :initarg :fn :accessor fn-of)
   (operands :initarg :args :accessor args-of)))

(define-syntax-node slot-access (function-call)
  ((operator :initarg :accessor :accessor accessor-of)
   (slot nil)))

(define-syntax-node association-end-access (slot-access)
  ((slot :initarg :association-end :accessor association-end-of)))

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

(defgeneric arg-of (slot-access)
  (:method ((access slot-access))
           (first (args-of access))))

(defgeneric (setf arg-of) (value slot-access)
  (:method (value (access slot-access))
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

(defun null-literal-p (syntax)
  (and (literal-value-p syntax)
       (null (value-of syntax))))

;;;;
;;;; Parse/unparse
;;;;
(defun parse-query-form (form variables)
  (acond
   ((syntax-object-p form) form)
   ((and (symbolp form) (find form variables :key 'name-of)) it)
   ((and (atom form) (constantp form))
    (make-literal-value :value form))
   ((and (consp form) (eq (first form) 'quote))
    (make-literal-value :value (second form)))
   ((symbolp form)
    (make-dynamic-variable :name form))
   ((and (symbolp (first form)) (association-end-accessor-p (first form)))
    (make-association-end-access :accessor (first form)
                                 :args (list (parse-query-form (second form) variables))))
   ((and (symbolp (first form)) (slot-accessor-p (first form)))
    (make-slot-access :accessor (first form)
                      :args (list (parse-query-form (second form) variables))))
   ((and (symbolp (first form)) (macro-function (first form)))
    (make-macro-call :macro (first form)
                     :args (if (parse-args-p (first form))
                               (mapcar #L(parse-query-form !1 variables) (rest form))
                               (mapcar #L(make-unparsed-form :form !1) (rest form)))))
   ((and (symbolp (first form)) (special-operator-p (first form)))
    (make-special-form :operator (first form)
                       :operands (mapcar #L(make-unparsed-form :form !1) (rest form))))
   ((and (member (first form) '(static volatile)) (length=1 (rest form)))
    (bind ((syntax (parse-query-form (second form) variables)))
      (setf (volatilep syntax) (eq (first form) 'volatile))
      syntax))
   ((member (first form) '(sum avg))
    (make-function-call :fn (first form)
                        :args (mapcar #L(parse-query-form !1 variables) (rest form))))
   ((and (symbolp (first form)) (fboundp (first form)))
    (make-function-call :fn (first form)
                        :args (mapcar #L(parse-query-form !1 variables) (rest form))))
   (t (error "Syntax error: ~S~%" form))))

(defgeneric unparse-query-syntax (syntax)
  (:method :around ((syntax syntax-object))
           (if (slot-boundp syntax 'volatilep)
               `(,(if (volatilep syntax) 'volatile 'static) ,(call-next-method))
               (call-next-method)))
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
;;;; Substitute
;;;;
(defgeneric substitute-syntax (syntax subs)

  (:method :around (syntax subs)
           (aif (assoc syntax subs)
                (cdr it)
                (call-next-method)))
  
  (:method ((syntax t) (subs null))
           syntax)

  (:method ((syntax t) (subs cons))
           syntax)

  (:method ((literal literal-value) (subs cons)) ; FIXME
           (bind ((value (substitute-syntax (value-of literal) subs)))
             (if (eq value (value-of literal))
                 literal
                 value)))

  (:method ((cons cons) (subs cons))
           (rcons (substitute-syntax (car cons) subs)
                  (substitute-syntax (cdr cons) subs)
                  cons))

  (:method ((unparsed unparsed-form) (subs cons))
           (setf (form-of unparsed) (substitute-syntax (form-of unparsed) subs))
           unparsed)

  (:method ((compound compound-form) (subs cons))
           (setf (operands-of compound) (substitute-syntax (operands-of compound) subs))
           compound)

  )

(defgeneric syntax-fold (syntax f g)

  (:method (syntax f g)
           (funcall f syntax))

  (:method ((compound compound-form) f g)
           (funcall g (funcall f compound) (syntax-fold (operands-of compound) f g)))

  (:method ((unparsed unparsed-form) f g)
           (funcall g (funcall f unparsed) (syntax-fold (form-of unparsed))))

  (:method ((cons cons) f g)
           (funcall g (funcall f cons) (syntax-fold (car cons) f g) (syntax-fold (cdr cons) f g))))

(defun find-if-syntax (predicate syntax)
  (syntax-fold
   syntax
   (lambda (node) (when (funcall predicate node) node))
   (lambda (parent &rest children) (or parent (some #'identity children)))))

