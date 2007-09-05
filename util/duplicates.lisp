;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE THE NUMBER OF DEPENDENCIES

(defun canonical-symbol-name (symbol)
  "Returns the package name and symbol name concatenated."
  (strcat
   (package-name (symbol-package symbol))
   "::"
   (symbol-name symbol)))

(defun symbol-from-canonical-name (name)
  (read-from-string name))

(defun concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(defmacro delete! (object place)
  `(setf ,place
    (delete ,object ,place)))

(def (function io) find-slot (class-or-name slot-name)
  (find slot-name
        (the list
          (class-slots (if (symbolp class-or-name)
                           (find-class class-or-name)
                           class-or-name)))
        :key 'slot-definition-name
        :test 'eq))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

(defmacro prog1-bind (var ret &body body)
  `(bind ((,var ,ret))
    ,@body
    ,var))

(defun hasf (plist indicator)
  (not (eq (getf plist indicator :unbound) :unbound)))

(defun collect-if (predicate sequence)
  "Collect elements from SEQUENCE for which the PREDICATE is true."
  (remove-if-not predicate sequence))

(defun length=1 (list)
  "Returns t if the length of the LIST is 1. (Faster than (= (length list) 1))"
  (and (consp list)
       (null (rest list))))

(defun mappend (function &rest lists)
  "Same as mapcar except the results are appended."  
  (apply 'append (apply 'mapcar function lists)))

(defmacro appendf (place &rest lists)
  "Like append, but setfs back the result"
  `(setf ,place (append ,place ,@lists)))

(defmacro nconcf (place &rest lists)
  `(setf ,place (nconc ,place ,@lists)))

(defun rcons (car cdr cons)
  "Returns a cons having CAR as car and CDR as cdr reusing CONS if possible."
  (if (and (eq car (car cons)) (eq cdr (cdr cons)))
      cons
      (cons car cdr)))

(defun tree-substitute (new old list
                            &key from-end (test #'eql) (test-not nil)
                            (end nil) (count nil) (key nil) (start 0))
  "Starting from LIST non-destructively replaces OLD with NEW."
  (if (consp list)
      (prog1-bind result
        (iter (for newitem in (ensure-list new))
              (for olditem in (ensure-list old))
              (setf list (substitute newitem olditem list :from-end from-end :test test :test-not test-not
                                     :end end :count count :key key :start start))
              (finally (return list)))
        (iter (for node first result then (cdr node))
              (until (null node))
              (for el = (car node))
              (setf (car node) (tree-substitute new old el :from-end from-end :test test :test-not test-not
                                                :end end :count count :key key :start start))))
      (if (funcall test list old)
          new
          list)))

(defun find-tree-root (node parent-function)
  (find-on-parent-chain node parent-function
                        (lambda (node)
                          (if (funcall parent-function node)
                              nil
                              node))))

(defun find-on-parent-chain (node parent-function map-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (awhen (funcall map-function current-node)
          (return it))))

(defun not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (strcat "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(defmacro bind-cartesian-product (((&rest variables) lst) &body body)
  (labels ((generate (variables l)
             (if (cdr variables) 
                 `(dolist (,(car variables) ,l)
                   ,(generate (cdr variables) l))
                 `(dolist (,(car variables) ,l)
                   ,@body))))
    (if variables
        (with-unique-names (l)
          `(let ((,l ,lst))
            ,(generate variables l)))
        nil)))

(defmacro bind-cartesian-product* (names-values-pairs &body forms)
  (if names-values-pairs
      (bind ((names-and-values (first names-values-pairs))
             (names (first names-and-values))
             (values (rest names-and-values)))
        (cons 'progn
              (iter (for value :in values)
                    (collect `(bind ((,names ,value))
                               (bind-cartesian-product* ,(rest names-values-pairs)
                                ,@forms))))))
      `(progn
        ,@forms)) )

(defun lessp (obj1 obj2)
  (typecase obj1
    (real (< obj1 obj2))
    (string (string< obj1 obj2))
    (character (char< obj1 obj2))
    (local-time (local-time< obj1 obj2))))

(defun less-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (<= obj1 obj2))
    (string (string<= obj1 obj2))
    (character (char<= obj1 obj2))
    (local-time (local-time<= obj1 obj2))))

(defun greaterp (obj1 obj2)
  (typecase obj1
    (real (> obj1 obj2))
    (string (string> obj1 obj2))
    (character (char> obj1 obj2))
    (local-time (local-time> obj1 obj2))))

(defun greater-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (>= obj1 obj2))
    (string (string>= obj1 obj2))
    (character (char>= obj1 obj2))
    (local-time (local-time>= obj1 obj2))))

(defun combine-with (op list-or-item item)
  (cond
    ((null list-or-item) item)
    ((and (listp list-or-item) (eq (car list-or-item) op))
     (append list-or-item (list item)))
    (t (list op list-or-item item))))


;; Lambda list stuff from Stefil
(defmacro with-lambda-parsing ((lambda-form &key finally) &body body)
  (with-unique-names (cell)
    `(iter
      (with -in-keywords- = #f)
      (with -in-optionals- = #f)
      (with -rest-variable-name- = nil)
      (for ,cell :first ,lambda-form :then (cdr ,cell))
      (while ,cell)
      (for -variable-name- = (if (or -in-optionals-
                                     -in-keywords-)
                                 (first (ensure-list (car ,cell)))
                                 (car ,cell)))
      (for -default-value- = (if (or -in-optionals-
                                     -in-keywords-)
                                 (second (ensure-list (car ,cell)))
                                 (car ,cell)))
      (case -variable-name-
        (&optional (setf -in-optionals- #t))
        (&key (setf -in-keywords- #t)
              (setf -in-optionals- #f))
        (&allow-other-keys)
        (&rest (setf -rest-variable-name- (car (cdr ,cell)))
               (setf ,cell (cdr ,cell)))
        (t ,@body))
      (finally ,@finally))))

(defun lambda-list-to-funcall-list (args)
  (with-lambda-parsing (args :finally ((return (values result -rest-variable-name-))))
    (if -in-keywords-
        (progn
          (collect (intern (symbol-name (first (ensure-list -variable-name-)))
                           #.(find-package "KEYWORD")) :into result)
          (collect -variable-name- :into result))
        (collect -variable-name- :into result))))

(defun lambda-list-to-funcall-expression (function args)
  (bind (((values arg-list rest-variable) (lambda-list-to-funcall-list args)))
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(defun lambda-list-to-variable-list (args &key (include-defaults #f) (include-&rest #f))
  (with-lambda-parsing (args :finally ((return (if (and include-&rest
                                                        -rest-variable-name-)
                                                   (cons -rest-variable-name- result)
                                                   result))))
    (collect (if include-defaults
                 (list -variable-name- -default-value-)
                 -variable-name-)
      :into result)))

;; copied from alexandria
(defmacro define-constant (name initial-value &key (test 'eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST
defaults to EQL, and if given it must be a symbol naming a function. If
DOCUMENTATION is given, it becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  `(defconstant ,name
    (let ((new ,initial-value))
      (if (boundp ',name)
          (let ((old (symbol-value ',name)))
            (cond
              ((constantp ',name)
               (cond
                 ((,test old new)
                  old)
                 (t
                  (cerror "Try to redefine the constant."
                          "~@<~S is an already defined constant whose value ~
                            ~S is not equal to the provided initial value ~S ~
                            under ~S.~:@>" ',name old new ',test)
                  new)))
              (t
               (cerror "Try to redefine the variable as a constant."
                       "~@<~S is an already bound non-constant variable ~
                         whose value is ~S.~:@>" ',name old)
               new)))
          new))
    ,@(when documentation `(,documentation))))

(defun elt-0 (vector)
  (elt vector 0))

(defun elt-1 (vector)
  (elt vector 1))

(defun elt-0-0 (vector)
  (elt (elt vector 0) 0))
