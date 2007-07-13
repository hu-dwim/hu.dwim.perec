(in-package :cl-perec-test)

(defsuite* (test/query/group-by :in test/query))


(defpclass* group-by-test ()
  ((int-attr :type (or null integer-32))
   (str-attr :type (or null (text 50)))
   (date-attr :type (or null date))))

(defixture group-by-data
  (with-transaction
    (purge-instances 'group-by-test)
    (make-instance 'group-by-test
                   :int-attr 1
                   :str-attr "1"
                   :date-attr (parse-datestring "2001-01-01"))
    (make-instance 'group-by-test
                   :int-attr 2
                   :str-attr "2"
                   :date-attr (parse-datestring "2001-01-02"))
    (make-instance 'group-by-test
                   :int-attr 3
                   :str-attr "3"
                   :date-attr (parse-datestring "2001-01-03"))
    (make-instance 'group-by-test
                   :int-attr nil
                   :str-attr nil
                   :date-attr nil)))

(defmacro def-group-by-test (name (&rest args) &body body)
  `(deftest ,name ,args
    (with-setup group-by-data
      (with-transaction
        ,@body))))

(def-group-by-test test/query/group-by/string ()
  (is
   (null
    (set-exclusive-or
     (select ((count (int-attr-of o)))
       (from (o group-by-test))
       (group-by (str-attr-of o)))
     '(0 1 1 1)
     :test 'equalp))))

(def-group-by-test test/query/group-by/string/2 ()
  (is
   (null
    (set-exclusive-or
     (select ((str-attr-of o) (count (int-attr-of o)))
       (from (o group-by-test))
       (group-by (str-attr-of o)))
     '((:null 0) ("1" 1) ("2" 1) ("3" 1))
     :test 'equalp))))
