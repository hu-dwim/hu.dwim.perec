(in-package :cl-perec-test)

(defsuite test/types)

(in-suite test/types)

(defmacro deftypetest (name type test-value &key (test 'equalp))
  (with-unique-names (value)
    `(deftest ,(cl-perec::concatenate-symbol "test/type/" name) ()
      (let ((,value ,test-value))
        (with-transaction
          (defpclass* type-test ()
            ((,name :type ,type))))
        (with-transaction
          (let ((object (make-instance 'type-test ,(cl-perec::initarg-symbol name) ,value)))
            (is (,test ,value (slot-value object ',name)))))
        (let ((object
               (with-transaction
                 (make-instance 'type-test ,(cl-perec::initarg-symbol name) ,value))))
          (with-transaction
            (revive-object object)
            (is (,test ,value (slot-value object ',name)))))))))

(defclass* class-type-test ()
  ((slot 0)))

(defstruct structure-type-test
  (slot 0 :type integer))

(deftypetest t/1 t nil)
(deftypetest t/2 t #f)
(deftypetest t/3 t #t)
(deftypetest t/4 t 0)
(deftypetest t/5 t 0.1)
(deftypetest t/6 t "something")
(deftypetest t/7 t 'something)
(deftypetest t/8 t (make-instance 'class-type-test))
(deftypetest t/9 t (list nil #f #t 0 0.1 "something" 'something
                         (make-instance 'class-type-test) (make-structure-type-test) (make-instance 'type-test)))

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

(deftypetest float-16/1 float-16 0)
(deftypetest float-16/2 float-16 +1.5)
(deftypetest float-16/3 float-16 -1.5)

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

(deftypetest standard-string/1 standard-string "")
(deftypetest standard-string/2 standard-string "something")

(deftypetest symbol/1 standard-symbol 'something)
(deftypetest symbol/2 standard-symbol 'cl-perec-test::something)
(deftypetest symbol/3 standard-symbol 'cl-user::something)

(deftypetest date/1 date (let ((*default-timezone* +utc-zone+)) (parse-timestring "2006-06-06")) :test local-time=)

(deftypetest time/1 time (let ((*default-timezone* +utc-zone+)) (parse-timestring "06:06:06")) :test local-time=)

(deftypetest timestamp/1 timestamp (parse-timestring "2006-06-06T06:06:06Z") :test local-time=)

(deftypetest duration/1 duration "06:06:06")
(deftypetest duration/2 duration "1-01-01 06:06:06")

(deftypetest enumerated/1 (enumerated one two three) 'one)
(deftypetest enumerated/2 (enumerated one two three) 'two)
(deftypetest enumerated/3 (enumerated one two three) 'three)

(deftypetest form/1 standard-form '(progn 1 (print "Hello") 2))
