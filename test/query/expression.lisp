(in-package :cl-perec-test)

(defsuite* (test/query/expression :in test/query))

(defpclass* expression-test ()
  ((string-attr :type (text 50))
   (or-null-string-attr :type (or null (text 50)))
   (date-attr :type date)))

(defixture expression-data
  (with-transaction
    (purge-instances 'expression-test)
    
    (make-instance 'expression-test
                   :string-attr "string1"
                   :date-attr (parse-datestring "2007-07-11"))
    (make-instance 'expression-test
                   :string-attr "String2"
                   :date-attr (parse-datestring "2007-07-15"))))

(deftest test/query/expression/local-time<= ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (local-time<= (parse-datestring "2007-07-10")
                           (date-attr-of o)
                           (parse-datestring "2007-07-12"))))))

(deftest test/query/expression/like-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (string-attr-of o) "s%ng_")))))

(deftest test/query/expression/like-2 ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (string-attr-of o) "r%g" :start 2 :end 6)))))

(deftest test/query/expression/like-3 ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (or-null-string-attr-of o) "s%ng_")))))

(deftest test/query/expression/like-ci ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (string-attr-of o) "s%ng_" :case-sensitive-p #f)))))

(deftest test/query/expression/re-like-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (re-like (string-attr-of o) "s.+ngx?y*.")))))

(deftest test/query/expression/re-like-2 ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (re-like (string-attr-of o) "r.+gx?y*" :start 2 :end 6)))))

(deftest test/query/expression/re-like-ci ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (re-like (string-attr-of o) "s.+ngx?y*." :case-sensitive-p #f)))))
