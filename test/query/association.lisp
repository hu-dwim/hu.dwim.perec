(in-package :cl-perec-test)

(defpclass* person ()
  ((first-name :type (string 50))
   (last-name :type (string 50))))

(defpclass* movie ()
  ((title :type (string 50))))

(defpclass* man (person)
  ())

(defpclass* woman (person)
  ())

(defpclass* performer (person)
  ())

(defpclass* actor (performer man)
  ())

(defpclass* actress (performer woman)
  ())

(defassociation*
  ((:class man :slot wife :type (or null woman))
   (:class woman :slot husband :type (or null man))))

(defassociation*
  ((:class movie :slot performers :type (set performer))
   (:class performer :slot movies :type (set movie))))

(defixture fill-data-2
  (with-transaction
    (purge-objects 'movie)
    (purge-objects 'person)
    (let ((oceans-twelwe (make-instance 'movie :title "Ocean's Twelwe"))
          (mr&mrs-smith (make-instance 'movie :title "Mr. & Mrs. Smith"))
          (george-clooney (make-instance 'actor :first-name "George" :last-name "Clooney"))
          (brad-pitt (make-instance 'actor :first-name "Brad" :last-name "Pitt"))
          (angelina-jolie (make-instance 'actress :first-name "Angelina" :last-name "Jolie")))
      (setf (performers-of oceans-twelwe) (list george-clooney brad-pitt)
            (performers-of mr&mrs-smith) (list brad-pitt angelina-jolie)
            (wife-of brad-pitt) angelina-jolie))))

(deftest test/query/select/association-n-m ()
  (test-query (:select-count 1 :record-count 4 :fixture fill-data-2)
    (select ((m movie) (p performer))
      (assert (member p (performers-of m)))
      (collect (title-of m) (first-name-of p) (last-name-of p)))))

(deftest test/query/select/association-1-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-2)
    (select ((m man) (w woman))
      (assert (eq (wife-of m) w))
      (collect m w)))
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-2)
    (select ((m man) (w woman))
      (assert (eq (husband-of w) m))
      (collect m w))))

(deftest test/query/select/association-chain ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-2)
    (select ((m man))
      (assert (not (null (wife-of m))))
      (assert (typep (husband-of (wife-of m)) 'performer))
      (assert (> (length (movies-of (husband-of (wife-of m)))) 0))
      (collect m))))
