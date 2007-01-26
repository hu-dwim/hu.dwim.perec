;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE THE NUMBER OF DEPENDENCIES

(set-dispatch-macro-character
 #\# #\t
 (lambda (s c n)
   (declare (ignore s c n))
   t))

(set-dispatch-macro-character
 #\# #\f
 (lambda (s c n)
   (declare (ignore s c n))
   nil))

(defmacro debug-only (&body body)
  #+debug`(progn ,@body)
  #-debug(declare (ignore body)))

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

(defun find-slot (class slot-name)
  (find slot-name (class-slots class) :key 'slot-definition-name))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

(defmacro prog1-bind (var ret &body body)
  `(bind ((,var ,ret))
    ,@body
    ,var))

(defun collect-if (predicate sequence)
  "Collect elements from SEQUENCE for which the PREDICATE is true."
  (remove-if-not predicate sequence))

(defun length=1 (list)
  "Returns t if the length of the LIST is 1. (Faster than (eq (length list) 1))"
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

(defmacro surround-body-when (test surround-with &body body)
  `(flet ((body () (progn ,@body)))
    (declare (inline body))
    (if ,test
        (,@surround-with)
        (body))))

(defun lessp (obj1 obj2)
  (typecase obj1
    (real (< obj1 obj2))
    (string (string< obj1 obj2))
    (character (char< obj1 obj2))))

(defun less-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (<= obj1 obj2))
    (string (string<= obj1 obj2))
    (character (char<= obj1 obj2))))

(defun greaterp (obj1 obj2)
  (typecase obj1
    (real (> obj1 obj2))
    (string (string> obj1 obj2))
    (character (char> obj1 obj2))))

(defun greater-or-equal-p (obj1 obj2)
  (typecase obj1
    (real (>= obj1 obj2))
    (string (string>= obj1 obj2))
    (character (char>= obj1 obj2))))

