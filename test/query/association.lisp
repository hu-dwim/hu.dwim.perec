(in-package :cl-perec-test)

(defsuite* (test/query/select/association :in test/query/select))

(defpclass* person-test ()
  ((name :type (text 50))))

(defpclass* movie-test ()
  ((title :type (text 50))))

(defpclass* man-test (person-test)
  ())

(defpclass* woman-test (person-test)
  ())

(defpclass* performer-test (person-test)
  ())

(defpclass* actor-test (performer-test man-test)
  ())

(defpclass* actress-test (performer-test woman-test)
  ())

(defassociation*
  ((:class man-test :slot wife :type (or null woman-test))
   (:class woman-test :slot husband :type (or null man-test))))

(defassociation*
  ((:class movie-test :slot performers :type (set performer-test))
   (:class performer-test :slot movies :type (set movie-test))))

(defixture association-test-fixture
  (with-transaction
    (export-all-classes)
    (purge-objects 'movie-test)
    (purge-objects 'person-test)
    (let ((oceans-twelwe (make-instance 'movie-test :title "Ocean's Twelwe"))
          (mr&mrs-smith (make-instance 'movie-test :title "Mr. & Mrs. Smith"))
          (george-clooney (make-instance 'actor-test :name "George Clooney"))
          (brad-pitt (make-instance 'actor-test :name "Brad Pitt"))
          (angelina-jolie (make-instance 'actress-test :name "Angelina Jolie")))
      (setf (performers-of oceans-twelwe) (list george-clooney brad-pitt)
            (performers-of mr&mrs-smith) (list brad-pitt angelina-jolie)
            (wife-of brad-pitt) angelina-jolie))))

(deftest test/query/select/association/1-1 ()
  (test-query (:select count 1 :record-count 1 :fixture association-test-fixture)
    (select ((m man))
      (assert (equal (name-of (wife-of))))
      (collect m))))

(deftest test/query/select/association-n-m ()
  (test-query (:select-count 1 :record-count 4 :fixture association-test-fixture)
    (select ((m movie-test) (p performer-test))
      (assert (member p (performers-of m)))
      (collect (title-of m) (name-of p)))))

(deftest test/query/select/association-1-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture association-test-fixture)
    (select ((m man-test) (w woman-test))
      (assert (eq (wife-of m) w))
      (collect m w)))
  (test-query (:select-count 1 :record-count 1 :fixture association-test-fixture)
    (select ((m man-test) (w woman-test))
      (assert (eq (husband-of w) m))
      (collect m w))))

(deftest test/query/select/association-chain ()
  (test-query (:select-count nil :record-count 1 :fixture association-test-fixture) ;; TODO select-count
    (select ((m man-test))
      (assert (not (null (wife-of m))))
      (assert (typep (husband-of (wife-of m)) 'performer-test))
      (assert (> (length (movies-of (husband-of (wife-of m)))) 0))
      (collect m))))

(deftest test/query/select/association-m-n/lexical-1 ()
  (test-query (:select-count nil :record-count 2 :fixture association-test-fixture)
    (let ((s (select-first-matching movie-test)))
      (select ((p performer-test))
        (assert (member p (performers-of s)))
        (collect p)))))

(deftest test/query/select/association-m-n/lexical-2 ()
  (test-query (:select-count nil :record-count 2 :fixture association-test-fixture)
    (let ((s (select-first-matching movie-test)))
      (select ((p performer-test))
        (assert (member s (movies-of p)))
        (collect p)))))

