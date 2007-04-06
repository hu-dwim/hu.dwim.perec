(in-package :cl-perec-test)

(defsuite* (test/query/aggregate :in test/query))


(defpclass* aggregate-test ()
  ((int-attr :type (or null integer-32))
   (str-attr :type (or null (text 50)))
   (date-attr :type (or null date))))

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
                   :date-attr (encode-local-time 0 0 0 0 3 1 2001 :timezone +utc-zone+))
    (make-instance 'aggregate-test
                   :int-attr nil
                   :str-attr nil
                   :date-attr nil)))

(deftest test/query/aggregate/int ()
  (with-setup aggregate-data
    (with-transaction
      (is
       (equal
        (select ((count (int-attr-of o)) (sum (int-attr-of o))
                 (min (int-attr-of o)) (max (int-attr-of o)) (avg (int-attr-of o)))
          (from (o aggregate-test)))
        '((3 6 1 3 2)))))))

