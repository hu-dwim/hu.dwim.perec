;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE THE NUMBER OF DEPENDENCIES

(def (function o) canonical-symbol-name (symbol)
  "Returns the package name and symbol name concatenated."
  (declare (type symbol symbol))
  ;; TODO: check for valid symbol names and ':'
  (concatenate 'string
               (package-name (symbol-package symbol))
               "::"
               (symbol-name symbol)))

(def (function o) symbol-from-canonical-name (name)
  (declare (type string name))
  (bind ((pos (position #\: name))
         (package-name (subseq name 0 pos))
         (symbol-name (subseq name (+ 2 pos)))
         (package (find-package package-name)))
    (intern symbol-name package)))

(def function concatenate-symbol (&rest args)
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

(def (function eo) concatenate-string (&rest args)
  ;; don't inline, otherwise the compiler macro is kicked
  (apply #'concatenate 'string args))

(def compiler-macro concatenate-string (&rest args)
  `(concatenate 'string ,@args))

(def (function io) find-slot (class-or-name slot-name)
  (find slot-name
        (the list
          (class-slots (if (symbolp class-or-name)
                           (find-class class-or-name)
                           class-or-name)))
        :key 'slot-definition-name
        :test 'eq))

(def macro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(def macro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(def macro prog1-bind (var ret &body body)
  `(bind ((,var ,ret))
     ,@body
     ,var))

(def macro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(def macro rebind (bindings &body body)
  `(let ,(loop
            for symbol-name in bindings
            collect (list symbol-name symbol-name))
     ,@body))

(def (constant :test 'equalp) +ascii-alphabet+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def function random-string (&optional (length 32) (alphabet +ascii-alphabet+))
  (loop with id = (make-string length)
     with alphabet-length = (length alphabet)
     for i below length
     do (setf (cl:aref id i)
              (cl:aref alphabet (random alphabet-length)))
     finally (return id)))

(def function hasf (plist indicator)
  (not (eq (getf plist indicator :unbound) :unbound)))

(def function collect-if (predicate sequence)
  "Collect elements from SEQUENCE for which the PREDICATE is true."
  (remove-if-not predicate sequence))

(def function rcons (car cdr cons)
  "Returns a cons having CAR as car and CDR as cdr reusing CONS if possible."
  (if (and (eq car (car cons)) (eq cdr (cdr cons)))
      cons
      (cons car cdr)))

(def function tree-substitute (new old list
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

(def function find-tree-root (node parent-function)
  (find-on-parent-chain node parent-function
                        (lambda (node)
                          (if (funcall parent-function node)
                              nil
                              node))))

(def function find-on-parent-chain (node parent-function map-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (awhen (funcall map-function current-node)
          (return it))))

(def function not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate-string "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(def macro bind-cartesian-product (((&rest variables) lst) &body body)
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

(def macro bind-cartesian-product* (names-values-pairs &body forms)
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

(def function lessp (obj1 obj2)
  (typecase obj1
    (real (< obj1 obj2))
    (string (string< obj1 obj2))
    (character (char< obj1 obj2))
    (local-time (local-time< obj1 obj2))))

(def function less-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (<= obj1 obj2))
    (string (string<= obj1 obj2))
    (character (char<= obj1 obj2))
    (local-time (local-time<= obj1 obj2))))

(def function greaterp (obj1 obj2)
  (typecase obj1
    (real (> obj1 obj2))
    (string (string> obj1 obj2))
    (character (char> obj1 obj2))
    (local-time (local-time> obj1 obj2))))

(def function greater-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (>= obj1 obj2))
    (string (string>= obj1 obj2))
    (character (char>= obj1 obj2))
    (local-time (local-time>= obj1 obj2))))

(def function combine-with (op list-or-item item)
  (cond
    ((null list-or-item) item)
    ((and (listp list-or-item) (eq (car list-or-item) op))
     (append list-or-item (list item)))
    (t (list op list-or-item item))))

(def (function io) generalized-boolean->boolean (value)
  (if value #t #f))

(def function permute (vector indices)
  (let ((vector-copy (make-array (length vector))))
    (declare (dynamic-extent vector-copy))
    (iter (for i :from 0 :below (length vector))
          (setf (aref vector-copy i)
                (aref vector (aref indices i))))
    (replace vector vector-copy)))

;; Lambda list stuff from Stefil
(def macro with-lambda-parsing ((lambda-form &key finally) &body body)
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

(def function lambda-list-to-funcall-list (args)
  (with-lambda-parsing (args :finally ((return (values result -rest-variable-name-))))
    (if -in-keywords-
        (progn
          (collect (intern (symbol-name (first (ensure-list -variable-name-)))
                           #.(find-package "KEYWORD")) :into result)
          (collect -variable-name- :into result))
        (collect -variable-name- :into result))))

(def function lambda-list-to-funcall-expression (function args)
  (bind (((:values arg-list rest-variable) (lambda-list-to-funcall-list args)))
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(def function lambda-list-to-variable-list (args &key (include-defaults #f) (include-&rest #f))
  (with-lambda-parsing (args :finally ((return (if (and include-&rest
                                                   -rest-variable-name-)
                                              (cons -rest-variable-name- result)
                                              result))))
    (collect (if include-defaults
                 (list -variable-name- -default-value-)
                 -variable-name-)
      :into result)))

(defun binary-search (value vector &key (key #'identity) (sort-fn #'<))
  (declare (vector vector))
  (labels ((recurse (start end)
             (if (< start end)
                 (let* ((i (+ start (truncate (- end start) 2)))
                        (elt (aref vector i))
                        (key-value (funcall key elt)))
                   (cond ((funcall sort-fn value key-value)
                          (recurse start i))
                         ((funcall sort-fn key-value value)
                          (recurse (1+ i) end))
                         (t
                          i)))
                 (- (1+ start)))))
    (recurse 0 (length vector))))

(defun lower-bound (value vector &key (key #'identity) (sort-fn #'<))
  "Returns the lowest index where the VALUE can be inserted into the sorted VECTOR without messing up the ordring."
  (declare (vector vector))
  (bind ((index (binary-search value vector :key key :sort-fn sort-fn)))
    (if (< index 0)
        (1- (- index))
        (iter (for i from (1- index) downto 0)
              (when (funcall sort-fn (funcall key (aref vector i)) value)
                (return (1+ i)))
              (finally (return 0))))))

(defun upper-bound (value vector &key (key #'identity) (sort-fn #'<))
  "Returns the greatest index where the VALUE can be inserted into the sorted VECTOR without messing up the ordering."
  (declare (vector vector))
  (bind ((index (binary-search value vector :key key :sort-fn sort-fn))
         (size (length vector)))
    (if (< index 0)
        (1- (- index))
        (iter (for i from (1+ index) below size)
              (when (funcall sort-fn value (funcall key (aref vector i)))
                (return i))
              (finally (return size))))))

