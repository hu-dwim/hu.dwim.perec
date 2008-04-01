;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Values having validity

(defsuite* (test/tesites/values-having-validty :in test/tesites))

(deftest test/tesites/values-having-validty/collect/single-value ()
  (is (values-having-validity=
       (make-single-values-having-validity 1000 (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ"))
       (collect-values-having-validity
        `((1000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-02TZ")))
        'first 'second 'third
        (lambda (validity-start validity-end)
          (error "No value for validity range: ~A - ~A" validity-start validity-end))
        (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(deftest test/tesites/values-having-validty/collect/no-value ()
  (is (values-having-validity=
       (make-single-values-having-validity 1 (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ"))
       (collect-values-having-validity
        nil
        'first 'second 'third
        (lambda (validity-start validity-end)
          (declare (ignore validity-start validity-end))
          1)
        (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(deftest test/tesites/values-having-validty/collect/multiple-value ()
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
  (iter (for (value validity-start validity-end index) :in-values-having-validity
             (extract-values-having-validity
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Children having validity

(defsuite* (test/tesites/children-having-validty :in test/tesites))

(deftest test/tesites/children-having-validty/collect/single-value ()
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

(deftest test/tesites/children-having-validty/collect/no-value ()
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

(deftest test/tesites/children-having-validty/collect/multiple-value ()
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
