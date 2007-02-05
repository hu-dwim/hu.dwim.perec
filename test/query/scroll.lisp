(in-package :cl-perec-test)

(defsuite* test/query/select/scroll :in test/query/select)

(defun check-page (page start end)
  "Checks if PAGE contains the integers from START (inclusive) to END (exclusive)."
  (is
   (and (= (length page) (- end start))
        (iter (for i index-of-vector page)
              (for v from start below end)
              (always (= (first (aref page i)) v))))))

(defpclass* scroll-test ()
  ((attr-1 :type integer-32)))

(defixture fill-data-5
  (with-transaction
    (export-all-classes)
    (purge-objects 'scroll-test)
    (iter (for i from 0 below 10)
          (make-instance 'scroll-test :attr-1 i))))

(defmacro run-scroll-test (&body body)
  `(with-fixture fill-data-5
    (run-queries
      ,@body)))

(deftest test/query/select/scroll/counts ()
  (run-scroll-test
    (bind ((scroll (select (:result-type scroll) ((o scroll-test)) (collect (attr-1-of o)))))
      (is (= (element-count scroll) 10))
      (setf (page-size scroll) 3)
      (is (= (page-count scroll) 4))
      (setf (page-size scroll) 10)
      (is (= (page-count scroll) 1))
      (setf (page-size scroll) 11)
      (is (= (page-count scroll) 1)))))
   
(deftest test/query/select/scroll/forward ()
  (run-scroll-test
    (bind ((scroll (select (:result-type scroll) ((o scroll-test)) (collect (attr-1-of o)))))
      (setf (page-size scroll) 3)
      (first-page! scroll)
      (iter (for i from 0 below 10 by 3)
            (check-page (elements scroll) i (min (+ i 3) 10))
            (next-page! scroll)))))

(deftest test/query/select/scroll/backward ()
  (run-scroll-test
    (bind ((scroll (select (:result-type scroll) ((o scroll-test)) (collect (attr-1-of o)))))
      (setf (page-size scroll) 3)
      (last-page! scroll)
      (iter (for i from 9 downto 0 by 3)
            (check-page (elements scroll) i (min (+ i 3) 10))
            (previous-page! scroll)))))

(deftest test/query/select/scroll/random ()
  (run-scroll-test
    (bind ((scroll (select (:result-type scroll) ((o scroll-test)) (collect (attr-1-of o)))))
      (setf (page-size scroll) 3)
      (setf (page scroll) 1)
      (check-page (elements scroll) 3 6)
      (setf (page scroll) 3)
      (check-page (elements scroll) 9 10)
      (setf (page scroll) 2)
      (check-page (elements scroll) 6 9)
      (setf (page scroll) 0)
      (check-page (elements scroll) 0 3))))

(deftest test/query/select/scroll/transactions ()
  (with-fixture fill-data-5
      (bind ((scroll (with-transaction
                       (select (:result-type scroll) ((o scroll-test)) (collect (attr-1-of o))))))
        (setf (page-size scroll) 3)
        (run-queries
          (first-page! scroll)
          (check-page (elements scroll) 0 3)
          (last-page! scroll)
          (check-page (elements scroll) 9 10)))))

(deftest test/query/select/scroll/modify ()
  (run-scroll-test
    (bind ((scroll (select (:result-type scroll) ((o scroll-test)) (collect (attr-1-of o)))))
      (setf (page-size scroll) 3)
      (check-page (elements scroll) 0 3)
      (purge-object (select-first-matching scroll-test))
      (check-page (elements scroll) 1 4))))
