;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* (test/persistence/type :in test/persistence))

;; TODO: factor this out into: equal*, <*, <=*, >*, >=*, =*, /=*
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

  (:method ((object-1 local-time) (object-2 local-time))
           (local-time= object-1 object-2))

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
           (p-eq object-1 object-2)))

(defmacro def-type-test (name type &optional (test-value nil test-value-p))
  (with-unique-names (value)
    `(deftest ,(cl-perec::concatenate-symbol "test/persistence/type/" name) ()
      (let ((,value
             ,(when test-value-p
                    `(with-transaction ,test-value)))
            object)
        (declare (ignorable ,value))
        (with-transaction
          (with-confirmed-descructive-changes
            (cl-perec::ensure-exported
             (defpclass* type-test ()
               ((,name :type ,type))))))
        (flet ((make-object ()
                 (execute "delete from _type_test")
                 (setf object
                       (apply #'make-instance
                              'type-test
                              ,(when test-value-p
                                     `(list
                                       (first (slot-definition-initargs (prc::find-slot (find-class 'type-test) ',name)))
                                       ,value)))))
               (test-object (object)
                 ,(when (and test-value-p test-value)
                        `(is (= 0 (prc::elt-0-0
                                   (execute (strcat "select count(*) from _type_test where "
                                                    (rdbms::name-of
                                                     (last1
                                                      (prc::columns-of (prc::find-slot (find-class 'type-test) ',name))))
                                                    " is null"))))))
                 (is ,(if test-value-p
                          `(object-equal-p ,value (slot-value object ',name))
                          `(not (slot-boundp object ',name))))))
          (with-and-without-caching-slot-values
            (with-one-and-two-transactions (make-object)
              (test-object -instance-))))))))

(defclass* standard-class-type-test ()
  ((slot 0)))

(define-copy-method (copy-one copy-query) ((thing standard-class-type-test) htable)
  thing)

(defpclass* persistent-class-type-test ()
  ((slot 0 :type integer)))

(defstruct structure-type-test
  (slot 0 :type integer))

(def-type-test t/1 t)
(def-type-test t/2 t nil)
(def-type-test t/3 t t)
(def-type-test t/4 t #f)
(def-type-test t/5 t #t)
(def-type-test t/6 t 0)
(def-type-test t/7 t 0.1)
(def-type-test t/8 t "something")
(def-type-test t/9 t 'something)
(def-type-test t/10 t (make-structure-type-test))
(def-type-test t/11 t (make-instance 'standard-class-type-test))
(def-type-test t/12 t (make-instance 'persistent-class-type-test))
(def-type-test t/13 t (list nil #f #t 0 0.1 "something" 'something
                          (make-structure-type-test)
                          (make-instance 'standard-class-type-test)
                          (make-instance 'persistent-class-type-test)))

(def-type-test serialized/1 serialized (list nil #f #t 0 0.1 "something" 'something
                                           (make-structure-type-test)
                                           (make-instance 'standard-class-type-test)
                                           (make-instance 'persistent-class-type-test)))
(def-type-test serialized/2 (serialized 32) t)

(def-type-test boolean/1 boolean #t)
(def-type-test boolean/2 boolean #f)

(def-type-test integer-16/1 integer-16 0)
(def-type-test integer-16/2 integer-16 +1)
(def-type-test integer-16/3 integer-16 -1)
(def-type-test integer-16/4 integer-16 (1- (expt 2 15)))
(def-type-test integer-16/5 integer-16 (- (expt 2 15)))

(def-type-test integer-32/1 integer-32 0)
(def-type-test integer-32/2 integer-32 +1)
(def-type-test integer-32/3 integer-32 -1)
(def-type-test integer-32/4 integer-32 (1- (expt 2 31)))
(def-type-test integer-32/5 integer-32 (- (expt 2 31)))

(def-type-test integer-64/1 integer-64 0)
(def-type-test integer-64/2 integer-64 +1)
(def-type-test integer-64/3 integer-64 -1)
(def-type-test integer-64/4 integer-64 (1- (expt 2 63)))
(def-type-test integer-64/5 integer-64 (- (expt 2 63)))

(def-type-test integer/1 integer 0)
(def-type-test integer/2 integer +1)
(def-type-test integer/3 integer -1)
(def-type-test integer/4 integer (expt 2 128))
(def-type-test integer/5 integer (- (expt 2 128)))

(def-type-test float-32/1 float-32 0)
(def-type-test float-32/2 float-32 +1.5)
(def-type-test float-32/3 float-32 -1.5)

(def-type-test float-64/1 float-64 0)
(def-type-test float-64/2 float-64 +1.5)
(def-type-test float-64/3 float-64 -1.5)

(def-type-test number/1 number 0)
(def-type-test number/2 number +1.5)
(def-type-test number/3 number -1.5)
(def-type-test number/4 number 1/10)
(def-type-test number/5 number -1/10)
(def-type-test number/6 number (expt 2 128))
(def-type-test number/7 number (- (expt 2 128)))

(def-type-test string/1 string "")
(def-type-test string/2 string (random-string 10000))
(def-type-test string/3 (string 20) "                    ")
(def-type-test string/4 (string 20) "something           ")

(def-type-test text/1 text "")
(def-type-test text/2 text (random-string 10000))
(def-type-test text/3 (text 20) "")
(def-type-test text/4 (text 20) "something")

(def-type-test symbol/1 symbol 'something)
(def-type-test symbol/2 symbol 'cl-perec-test::something)
(def-type-test symbol/3 (symbol* 30) 'cl-user::something)

(def-type-test date/1 date (parse-datestring "2006-06-06"))

(def-type-test time/1 time (parse-datestring "06:06:06"))

(def-type-test timestamp/1 timestamp (parse-timestring "2006-06-06T06:06:06Z"))
(def-type-test timestamp/2 timestamp (parse-timestring "2006-06-06T06:06:06+06:00"))
(def-type-test timestamp/3 timestamp (parse-timestring "2006-06-06T06:06:06.123456+06:00"))

(def-type-test duration/1 duration "06:06:06")
(def-type-test duration/2 duration "1-01-01 06:06:06")

(def-type-test member/1 (member one two three) 'one)
(def-type-test member/2 (member one two three) 'two)
(def-type-test member/3 (member one two three) 'three)

(def-type-test form/1 form '(progn 1 (print "Hello") 2))
(def-type-test form/2 (form 100) '(progn 1 (print "Hello") 2))

(def-type-test unsigned-byte-vector/1 unsigned-byte-vector (make-array 8 :element-type '(unsigned-byte 8) :initial-element 42))

(def-type-test or-null-string/1 (or null string) nil)
(def-type-test or-null-string/2 (or null string) "something")

(def-type-test or-unbound-string/1 (or unbound string))
(def-type-test or-unbound-string/2 (or unbound string) "something")

(def-type-test or-unbound-null-string/1 (or unbound null string))
(def-type-test or-unbound-null-string/2 (or unbound null string) nil)
(def-type-test or-unbound-null-string/3 (or unbound null string) "something")

(defpclass* slot-initial-value-test ()
  ((slot :type (or null string))))

(deftest test/persistence/type/initial-value ()
  (flet ((test-object ()
           (with-transaction
             (let ((object (make-instance 'slot-initial-value-test)))
               (is (slot-boundp object 'slot))
               (is (not (slot-of object)))))))
    (without-caching-slot-values
      (test-object))
    (with-caching-slot-values
      (test-object))))
