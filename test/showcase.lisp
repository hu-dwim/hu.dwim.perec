(in-package :cl-perec-test)

(mapc #L(handler-case (drop-table !1)
          (error (e) (declare (ignore e))))
      '(_c1 _c2 _c3 _c4))

(start-sql-recording)

(defpclass c1 ()
  ((s1 :type string :initform "no" :initarg :s1 :accessor s1-of)))

(with-transaction
  (make-instance 'c1 :s1 "hello"))

(defpclass c2 (c1)
  ((s2 :type (or null integer-16) :initarg :s2 :accessor s2-of)))

(with-transaction
  (s1-of (make-instance 'c2 :s1 "hello" :s2 12)))

(with-transaction
  (select ((o c1))
    (collect o)))

(defpclass c3 ()
  ((s3 :type c1 :initarg :s3 :accessor s3-of)))

(let ((o
       (with-transaction
         (make-instance 'c3 :s3 (make-instance 'c2 :s2 nil)))))
  (with-transaction
    (revive-instance o)
    (s2-of (s3-of o))))

(defpclass c4 ()
  ())

(defassociation
  ((:class c4 :slot c1s :type (set c1) :accessor c1s-of :initarg :c1s)
   (:class c1 :slot c4 :type c4 :accessor c4-of :initarg :c4)))

(with-transaction
  (make-instance 'c1 :c4 (make-instance 'c4)))

(with-transaction
  (let ((c4 (select-first-matching-instance c4)))
    (select ((c1 c1))
      (assert (eq c4 (c4-of c1)))
      (collect c4 (s1-of c1)))))
