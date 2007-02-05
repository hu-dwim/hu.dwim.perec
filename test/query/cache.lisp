(in-package :cl-perec-test)

(defsuite* test/query/cache :in test/query)

(defmacro run-cache-test (&body body)
  `(with-fixture fill-data-3
    (export-all-classes)
    (with-transaction
      (prc::clear-compiled-query-cache)
      (prc::reset-compile-query-counter)
      (symbol-macrolet ((counter prc::*compile-query-counter*))
        ,@body))))

(defpclass* query-cache-test ()
  ((attr-1 :type integer-32)))

(defpclass* query-cache-2-test ()
  ((attr-1 :type integer-32)))

;; PORT:
(defixture fill-data-3
  (with-transaction
    (purge-objects 'query-cache-test)
    (make-instance 'query-cache-test :attr-1 1)
    (make-instance 'query-cache-test :attr-1 2)))

(deftest test/query/cache-1 ()
  (run-cache-test
    (is (= counter 0))
    (select ((o query-cache-test)) (collect o))
    (is (= counter 1))
    (select ((o query-cache-test)) (collect o))
    (is (= counter 1))))

(deftest test/query/cache-2 ()
  (run-cache-test
    (bind ((query (make-query `(select ((o query-cache-test)) (collect o)))))
      (is (= counter 0))
      (execute-query query)
      (is (= counter 1))
      (add-assert query `(equal (attr-1-of o) 1))
      (execute-query query)
      (is (= counter 2)))))
   
(deftest test/query/cache-3 ()
  (run-cache-test
    (bind ((query (make-query `(select ((o query-cache-test)) (collect o))))
           (result (execute-query query)))
      (add-assert query `(equal (attr-1-of o) 1))
      (is (not (equal result (execute-query query)))))))

(deftest test/query/cache-4 ()
  (run-cache-test
    (with-confirmed-descructive-changes
      (bind ((class (find-class 'query-cache-2-test))
             (query (make-query '(select ((o query-cache-2-test)) (collect o)))))
        (is (= counter 0))
        (execute-query query)
        (is (= counter 1))
        (ensure-class-using-class class
                                  (class-name class)
                                  :metaclass (class-of class)
                                  :direct-superclasses (class-direct-superclasses class)
                                  :direct-slots nil)
        (prc::ensure-exported class)
        (execute-query query)
        (is (= counter 2))))))


