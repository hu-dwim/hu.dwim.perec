(in-package :cl-perec-test)

(defsuite* (test/query/polymorphic :in test/query) )

(defpclass* pt-0-test ()
  ())

(defpclass* pt-1-test ()
  ((attr1 :type integer)))

(defpclass* pt-2-test ()
  ((attr1 :type string)))


(defixture polymorph-test-data
  (with-transaction
    (purge-objects 'pt-1-test)
    (purge-objects 'pt-2-test)
    (make-instance 'pt-1-test :attr1 1)
    (make-instance 'pt-2-test :attr1 "1")))


(deftest test/query/select/polymorph-1 ()
  (test-query (:select-count 0 :record-count 0 :fixture polymorph-test-data)
    (select ((o pt-0-test))
      (assert (or (and (typep o 'pt-1-test)
                       (= (attr1-of o) 1))
                  (and (typep o 'pt-2-test)
                       (string= (attr1-of o) "1"))))
      (collect o))))

(deftest test/query/select/polymorph-2 ()
  (test-query (:select-count nil :record-count 2 :fixture polymorph-test-data) ;; TODO select-count
    (select (o)
      (assert (or (and (typep o 'pt-1-test)
                       (= (attr1-of o) 1))
                  (and (typep o 'pt-2-test)
                       (string= (attr1-of o) "1"))))
      (collect o))))