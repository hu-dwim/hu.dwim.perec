(in-package :cl-perec-test)

(defsuite* (test/query/partial-eval :in test/query))

(defpclass* partial-eval-test ()
  ((int-attr :type integer-32)))

(defixture partial-eval-data
  (with-transaction
    (purge-instances 'partial-eval-test)
    (make-instance 'partial-eval-test :int-attr 1)
    (make-instance 'partial-eval-test :int-attr 2)))

(defvar *counter* 0)

(defun count-one ()
  (incf *counter*))

(deftest test/query/partial-eval/static ()
  (with-setup partial-eval-data
    (bind ((*enable-partial-eval* #t) 
           (query (make-query '(select ((int-attr-of o))
                                (from (o partial-eval-test))
                                (where (= (count-one) (int-attr-of o))))))
           (*counter* 0))
      (with-transaction
        (compile-query query)
        (is (= *counter* 1))
        (is (equal (first (execute-query query)) 1))
        (is (equal (first (execute-query query)) 1))))))

(deftest test/query/partial-eval/volatile ()
  (with-setup partial-eval-data
    (bind ((*enable-partial-eval* #t)
           (query (make-query '(select ((int-attr-of o))
                                (from (o partial-eval-test))
                                (where (= (volatile (count-one)) (int-attr-of o))))))
           (*counter* 0))
      (with-transaction
        (compile-query query)
        (is (= *counter* 0))
        (is (equal (first (execute-query query)) 1))
        (is (equal (first (execute-query query)) 2))))))
