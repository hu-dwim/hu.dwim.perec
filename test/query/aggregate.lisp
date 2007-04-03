(in-package :cl-perec-test)

(defsuite* (test/query/aggregate :in test/query))

#|
(defpclass* aggregate-test ()
  ((int-attr :type integer-32)
   (str-attr :type (text 50))
   (date-attr :type date)))

(defixture aggregate-data
  (with-transaction
    (export-all-classes)
    (purge-instances 'aggregate-test)
    (make-instance 'aggregate-test
                   :int-attr 1
                   :str-attr "1"
                   :date-attr (encode-local-time 0 0 0 0 1 1 2001 :timezone +utc-zone+))
    (make-instance 'aggregate-test
                   :int-attr 2
                   :str-attr "2"
                   :date-attr (encode-local-time 0 0 0 0 2 1 2001 :timezone +utc-zone+))
    (make-instance 'aggregate-test
                   :int-attr 3
                   :str-attr "3"
                   :date-attr (encode-local-time 0 0 0 0 3 1 2001 :timezone +utc-zone+))))

(deftest test/query/aggregate/int ()
  (with-setup aggregate-data
    (with-transaction
      (is
       (equal
        (select ((o aggregate-test))
          (collect (min (int-attr-of o)) (max (int-attr-of o)) (avg (int-attr-of o))))
        '(1 3 2))))))
|#
