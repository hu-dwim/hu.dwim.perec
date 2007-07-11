(in-package :cl-perec-test)

(defsuite* (test/query/expression :in test/query))

(defpclass* expression-test ()
  ((string-attr :type (text 50))
   (date-attr :type date)))

(defixture expression-data
  (with-transaction
    (purge-instances 'expression-test)
    
    (make-instance 'expression-test
                   :string-attr "string1"
                   :date-attr (parse-datestring "2007-07-11"))
    (make-instance 'expression-test
                   :string-attr "string2"
                   :date-attr (parse-datestring "2007-07-15"))))

(deftest test/query/expression/local-time<= ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (local-time<= (parse-datestring "2007-07-10")
                           (date-attr-of o)
                           (parse-datestring "2007-07-12"))))))
