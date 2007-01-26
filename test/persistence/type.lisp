;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* test/types :in test)

(defgeneric object-equal-p (object-1 object-2)
  (:method (object-1 object-2)
           (equalp object-1 object-2))

  (:method ((object-1 symbol) (object-2 symbol))
           (or (call-next-method)
               (and (not (symbol-package object-1))
                    (not (symbol-package object-2))
                    (equal (symbol-name object-1)
                           (symbol-name object-2)))))

  (:method ((object-1 list) (object-2 list))
           (every #'object-equal-p object-1 object-2))

  (:method ((object-1 structure-object) (object-2 structure-object))
           (let ((class-1 (class-of object-1))
                 (class-2 (class-of object-2)))
             (and (eq class-1 class-2)
                  (every (lambda (slot)
                           (object-equal-p
                            (slot-value-using-class class-1 object-1 slot)
                            (slot-value-using-class class-2 object-2 slot)))
                         (class-slots class-1)))))

  (:method ((object-1 standard-object) (object-2 standard-object))
           (let ((class-1 (class-of object-1))
                 (class-2 (class-of object-2)))
             (and (eq class-1 class-2)
                  (every (lambda (slot)
                           (or (and (not (slot-boundp-using-class class-1 object-1 slot))
                                    (not (slot-boundp-using-class class-2 object-2 slot)))
                               (object-equal-p
                                (slot-value-using-class class-1 object-1 slot)
                                (slot-value-using-class class-2 object-2 slot))))
                         (class-slots class-1)))))

  (:method ((object-1 persistent-object) (object-2 persistent-object))
           (or (eq object-1 object-2)
               (object-equal-p (cl-perec::oid-of object-1)
                               (cl-perec::oid-of object-2)))))

(defmacro deftypetest (name type test-value &key (test 'object-equal-p))
  (with-unique-names (value)
    `(deftest ,(cl-perec::concatenate-symbol "test/type/" name) ()
      (let ((,value (with-transaction ,test-value))
            object)
        (with-transaction
          (with-confirmed-descructive-changes
            (cl-perec::ensure-exported
             (defpclass* type-test ()
               ((,name :type ,type))))))
        (flet ((make ()
                 (setf object
                       (apply #'make-instance
                              'type-test
                              (first (slot-definition-initargs (prc::find-slot (find-class 'type-test) ',name)))
                              ,value
                              nil)))
               (test ()
                 (is (,test ,value (slot-value object ',name)))))
          (with-caching-slot-values
            (with-transaction
              (make)
              (test)))
          (without-caching-slot-values
            (with-transaction
              (make)
              (test)))
          (with-transaction
            (make))
          (with-transaction
            (revive-object object)
            (test)))))))

(defclass* standard-class-type-test ()
  ((slot 0)))

(defpclass* persistent-class-type-test ()
  ((slot 0 :type integer)))

(defstruct structure-type-test
  (slot 0 :type integer))

(deftypetest t/1 t nil)
(deftypetest t/2 t t)
(deftypetest t/3 t #f)
(deftypetest t/4 t #t)
(deftypetest t/5 t 0)
(deftypetest t/6 t 0.1)
(deftypetest t/7 t "something")
(deftypetest t/8 t 'something)
(deftypetest t/9 t (make-structure-type-test))
(deftypetest t/10 t (make-instance 'standard-class-type-test))
(deftypetest t/11 t (make-instance 'persistent-class-type-test))
(deftypetest t/12 t (list nil #f #t 0 0.1 "something" 'something
                          (make-structure-type-test)
                          (make-instance 'standard-class-type-test)
                          (make-instance 'persistent-class-type-test)))

(deftypetest serialized/1 serialized (list nil #f #t 0 0.1 "something" 'something
                                           (make-structure-type-test)
                                           (make-instance 'standard-class-type-test)
                                           (make-instance 'persistent-class-type-test)))
(deftypetest serialized/2 (serialized 32) t)

(deftypetest boolean/1 boolean #t)
(deftypetest boolean/2 boolean #f)

(deftypetest integer-16/1 integer-16 0)
(deftypetest integer-16/2 integer-16 +1)
(deftypetest integer-16/3 integer-16 -1)
(deftypetest integer-16/4 integer-16 (1- (expt 2 15)))
(deftypetest integer-16/5 integer-16 (- (expt 2 15)))

(deftypetest integer-32/1 integer-32 0)
(deftypetest integer-32/2 integer-32 +1)
(deftypetest integer-32/3 integer-32 -1)
(deftypetest integer-32/4 integer-32 (1- (expt 2 31)))
(deftypetest integer-32/5 integer-32 (- (expt 2 31)))

(deftypetest integer-64/1 integer-64 0)
(deftypetest integer-64/2 integer-64 +1)
(deftypetest integer-64/3 integer-64 -1)
(deftypetest integer-64/4 integer-64 (1- (expt 2 63)))
(deftypetest integer-64/5 integer-64 (- (expt 2 63)))

(deftypetest integer/1 integer 0)
(deftypetest integer/2 integer +1)
(deftypetest integer/3 integer -1)
(deftypetest integer/4 integer (expt 2 128))
(deftypetest integer/5 integer (- (expt 2 128)))

(deftypetest float-32/1 float-32 0)
(deftypetest float-32/2 float-32 +1.5)
(deftypetest float-32/3 float-32 -1.5)

(deftypetest float-64/1 float-64 0)
(deftypetest float-64/2 float-64 +1.5)
(deftypetest float-64/3 float-64 -1.5)

(deftypetest number/1 number 0)
(deftypetest number/2 number +1.5)
(deftypetest number/3 number -1.5)
(deftypetest number/4 number 1/10)
(deftypetest number/5 number -1/10)
(deftypetest number/6 number (expt 2 128))
(deftypetest number/7 number (- (expt 2 128)))

(deftypetest string/1 string "")
(deftypetest string/2 string (random-string 10000))
(deftypetest string/3 (string 20) "")
(deftypetest string/4 (string 20) "something")

(deftypetest symbol/1 symbol 'something)
(deftypetest symbol/2 symbol 'cl-perec-test::something)
(deftypetest symbol/3 (symbol* 30) 'cl-user::something)

(deftypetest date/1 date (let ((*default-timezone* +utc-zone+)) (parse-timestring "2006-06-06")) :test local-time=)

(deftypetest time/1 time (let ((*default-timezone* +utc-zone+)) (parse-timestring "06:06:06")) :test local-time=)

(deftypetest timestamp/1 timestamp (parse-timestring "2006-06-06T06:06:06Z") :test local-time=)

(deftypetest duration/1 duration "06:06:06")
(deftypetest duration/2 duration "1-01-01 06:06:06")

(deftypetest member/1 (member one two three) 'one)
(deftypetest member/2 (member one two three) 'two)
(deftypetest member/3 (member one two three) 'three)

(deftypetest form/1 form '(progn 1 (print "Hello") 2))
(deftypetest form/2 (form 100) '(progn 1 (print "Hello") 2))
