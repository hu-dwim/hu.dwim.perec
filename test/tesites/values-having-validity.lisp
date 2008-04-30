;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Values having validity

(defsuite* (test/tesites/values-having-validity :in test/tesites))

(deftest test/tesites/values-having-validity/collect/single-value ()
  (is (values-having-validity=
       (make-single-values-having-validity 1000 (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ"))
       (collect-values-having-validity
        `((1000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-02TZ")))
        'first 'second 'third
        (lambda (validity-start validity-end)
          (error "No value for validity range: ~A - ~A" validity-start validity-end))
        (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(deftest test/tesites/values-having-validity/collect/no-value ()
  (is (values-having-validity=
       (make-single-values-having-validity 1 (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ"))
       (collect-values-having-validity
        nil
        'first 'second 'third
        (lambda (validity-start validity-end)
          (declare (ignore validity-start validity-end))
          1)
        (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(deftest test/tesites/values-having-validity/collect/multiple-value ()
  (is (values-having-validity=
       (make-values-having-validity (list 2000 1000 3000 nil)
                                    (list (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-10TZ") (parse-timestring "2007-01-20TZ") (parse-timestring "2007-01-30TZ"))
                                    (list (parse-timestring "2007-01-10TZ") (parse-timestring "2007-01-20TZ") (parse-timestring "2007-01-30TZ") (parse-timestring "2008-01-01TZ")))
       (collect-values-having-validity
        `((1000 ,(parse-timestring "2007-01-10TZ") ,(parse-timestring "2007-01-20TZ"))
          (2000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-15TZ"))
          (3000 ,(parse-timestring "2007-01-05TZ") ,(parse-timestring "2007-01-30TZ")))
        'first 'second 'third
        (lambda (validity-start validity-end)
          (declare (ignore validity-start validity-end))
          nil)
        (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))))

(deftest test/tesites/values-having-validity/extract ()
  (iter (for (validity-start validity-end value) :in-values-having-validity
             (values-having-validity-value
              (make-instance 'values-having-validity
                             :values (make-array 3 :initial-contents '(1000 2000 3000))
                             :validity-starts (make-array 3 :initial-contents (list (parse-timestring "2006-01-01TZ")
                                                                                    (parse-timestring "2007-01-01TZ")
                                                                                    (parse-timestring "2008-01-01TZ")))
                             :validity-ends (make-array 3 :initial-contents (list (parse-timestring "2007-01-01TZ")
                                                                                  (parse-timestring "2008-01-01TZ")
                                                                                  (parse-timestring "2009-01-01TZ"))))
              (parse-timestring "2006-06-06TZ")
              (parse-timestring "2008-07-07TZ")))
        (for index from 0)
        (ecase index
          (0 (is (= value 1000))
             (is (local-time= validity-start (parse-timestring "2006-06-06TZ")))
             (is (local-time= validity-end (parse-timestring "2007-01-01TZ"))))
          (1 (is (= value 2000))
             (is (local-time= validity-start (parse-timestring "2007-01-01TZ")))
             (is (local-time= validity-end (parse-timestring "2008-01-01TZ"))))
          (2 (is (= value 3000))
             (is (local-time= validity-start (parse-timestring "2008-01-01TZ")))
             (is (local-time= validity-end (parse-timestring "2008-07-07TZ")))))))

(defun make-values-having-validity* (values-and-validities)
  (make-values-having-validity
   (mapcar #'first values-and-validities)
   (mapcar #L(parse-timestring (second !1)) values-and-validities)
   (mapcar #L(parse-timestring (third !1)) values-and-validities)))

(deftest test/tesites/values-having-validity/setf ()
  (labels ((test-value () (make-values-having-validity*
                           '((1000 "2006-01-01TZ" "2007-01-01TZ")
                             (2000 "2007-01-01TZ" "2008-01-01TZ")
                             (3000 "2008-01-01TZ" "2009-01-01TZ"))))
           (set-on-test-value (value validity-start validity-end)
             (aprog1 (test-value)
               (setf (values-having-validity-value it
                                                   (parse-timestring validity-start)
                                                   (parse-timestring validity-end))
                     value))))
    (is (values-having-validity=
         (set-on-test-value 4000 "2005-01-01TZ" "2010-01-01TZ")
         (make-values-having-validity* '((4000 "2005-01-01TZ" "2010-01-01TZ")))))

    (is (values-having-validity=
         (set-on-test-value 4000 "2005-01-01TZ" "2006-01-01TZ")
         (make-values-having-validity* '((4000 "2005-01-01TZ" "2006-01-01TZ")
                                         (1000 "2006-01-01TZ" "2007-01-01TZ")
                                         (2000 "2007-01-01TZ" "2008-01-01TZ")
                                         (3000 "2008-01-01TZ" "2009-01-01TZ")))))

    (is (values-having-validity=
         (set-on-test-value 4000 "2009-01-01TZ" "2010-01-01TZ")
         (make-values-having-validity* '((1000 "2006-01-01TZ" "2007-01-01TZ")
                                         (2000 "2007-01-01TZ" "2008-01-01TZ")
                                         (3000 "2008-01-01TZ" "2009-01-01TZ")
                                         (4000 "2009-01-01TZ" "2010-01-01TZ")))))

    (is (values-having-validity=
         (set-on-test-value 4000 "2006-06-01TZ" "2006-07-01TZ")
         (make-values-having-validity* '((1000 "2006-01-01TZ" "2006-06-01TZ")
                                         (4000 "2006-06-01TZ" "2006-07-01TZ")
                                         (1000 "2006-07-01TZ" "2007-01-01TZ")
                                         (2000 "2007-01-01TZ" "2008-01-01TZ")
                                         (3000 "2008-01-01TZ" "2009-01-01TZ")))))))

(deftest test/tesites/values-having-validity/collect ()
  (is (values-having-validity=
       (iter (for value in '(1 2 3))
             (for start in '("2005-01-01TZ" "2006-01-01TZ" "2007-01-01TZ"))
             (for end in '("2006-01-01TZ" "2007-01-01TZ" "2008-01-01TZ"))
             (collect-value-with-validity value :from start :to end))
       (make-values-having-validity* '((1 "2005-01-01TZ" "2006-01-01TZ")
                                       (2 "2006-01-01TZ" "2007-01-01TZ")
                                       (3 "2007-01-01TZ" "2008-01-01TZ")))))

  (is (values-having-validity=
       (iter (for value in '(1 2 3))
             (for start in '("2005-01-01TZ" "2006-01-01TZ" "2007-01-01TZ"))
             (for end in '("2006-01-01TZ" "2007-01-01TZ" "2008-01-01TZ"))
             (collect-value-with-validity (value start end)))
       (make-values-having-validity* '((1 "2005-01-01TZ" "2006-01-01TZ")
                                       (2 "2006-01-01TZ" "2007-01-01TZ")
                                       (3 "2007-01-01TZ" "2008-01-01TZ")))))

  (is (values-having-validity=
       (iter (for value in '(1 2 3))
             (for start in '("2005-01-01TZ" "2006-01-01TZ" "2007-01-01TZ"))
             (for end in '("2006-01-01TZ" "2007-01-01TZ" "2008-01-01TZ"))
             (collect-value-with-validity (value start end) into a)
             (finally (return a)))
       (make-values-having-validity* '((1 "2005-01-01TZ" "2006-01-01TZ")
                                       (2 "2006-01-01TZ" "2007-01-01TZ")
                                       (3 "2007-01-01TZ" "2008-01-01TZ")))))

  (is (values-having-validity=
       (iter (for value in '(1 2 3))
             (for start in '("2005-01-01TZ" "2006-01-01TZ" "2007-01-01TZ"))
             (for end in '("2006-01-01TZ" "2007-01-01TZ" "2008-01-01TZ"))
             (when (oddp value)
               (collect-value-with-validity (value start end))))
       (make-values-having-validity* '((1 "2005-01-01TZ" "2006-01-01TZ")
                                       (3 "2007-01-01TZ" "2008-01-01TZ"))))))

(deftest test/tesites/values-having-validity/iterate ()
  (is (values-having-validity=
       (iter (for (s e v1 v2) :in-values-having-validity ((make-values-having-validity*
                                                           '((2 "2005-01-01TZ" "2006-01-01TZ")
                                                             (3 "2006-01-01TZ" "2007-01-01TZ")
                                                             (5 "2007-01-01TZ" "2008-01-01TZ")))
                                                          (make-values-having-validity*
                                                           '((7 "2005-01-01TZ" "2006-06-01TZ")
                                                             (11 "2006-06-01TZ" "2007-06-01TZ")
                                                             (13 "2007-06-01TZ" "2008-01-01TZ")))))
             (collect-value-with-validity (* v1 v2) :from s :to e))
       (make-values-having-validity* '((1 "2005-01-01TZ" "2006-01-01TZ")
                                       (2 "2006-01-01TZ" "2007-01-01TZ")
                                       (3 "2007-01-01TZ" "2008-01-01TZ"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Children having validity

(defsuite* (test/tesites/children-having-validity :in test/tesites))

(deftest test/tesites/children-having-validity/collect/single-value ()
  (is (values-having-validity=
       (make-single-values-having-validity '(1000) (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ"))
       (collect-children-having-validity
        `((1000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2008-01-01TZ") 1))
        (lambda (e)
          (elt e 0))
        (lambda (e)
          (elt e 1))
        (lambda (e)
          (elt e 2))
        (lambda (e)
          (elt e 3))
        (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))))

(deftest test/tesites/children-having-validity/collect/no-value ()
  (is (values-having-validity=
       (make-single-values-having-validity nil (parse-timestring "2006-01-01TZ") (parse-timestring "2008-01-01TZ"))
       (collect-children-having-validity
        ()
        (lambda (e)
          (elt e 0))
        (lambda (e)
          (elt e 1))
        (lambda (e)
          (elt e 2))
        (lambda (e)
          (elt e 3))
        (parse-timestring "2006-01-01TZ") (parse-timestring "2008-01-01TZ")))))

(deftest test/tesites/children-having-validity/collect/multiple-value ()
  (is (values-having-validity=
       (make-values-having-validity (list nil '(2000) '(3000 2000) '(3000 2000 1000) '(3000 1000) '(3000) nil)
                                    (list (parse-timestring "2006-01-01TZ") (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-05TZ") (parse-timestring "2007-01-10TZ")
                                          (parse-timestring "2007-01-15TZ") (parse-timestring "2007-01-20TZ") (parse-timestring "2007-01-30TZ"))
                                    (list (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-05TZ") (parse-timestring "2007-01-10TZ") (parse-timestring "2007-01-15TZ")
                                          (parse-timestring "2007-01-20TZ") (parse-timestring "2007-01-30TZ") (parse-timestring "2008-01-01TZ")))
       (collect-children-having-validity
        `((1000 ,(parse-timestring "2007-01-10TZ") ,(parse-timestring "2007-01-20TZ") 1)
          (2000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-15TZ") 1)
          (3000 ,(parse-timestring "2007-01-05TZ") ,(parse-timestring "2007-01-30TZ") 1))
        (lambda (e)
          (elt e 0))
        (lambda (e)
          (elt e 1))
        (lambda (e)
          (elt e 2))
        (lambda (e)
          (elt e 3))
        (parse-timestring "2006-01-01TZ") (parse-timestring "2008-01-01TZ")))))
