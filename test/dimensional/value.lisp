;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Values having validity

(defsuite* (test/dimensional/values-having-validity :in test/dimensional))

(def function make-values-having-validity* (values-and-validities)
  (make-values-having-validity
   (mapcar #'first values-and-validities)
   (mapcar #L(parse-timestring (second !1)) values-and-validities)
   (mapcar #L(parse-timestring (third !1)) values-and-validities)))

(def test test/dimensional/values-having-validity/collect/single-value ()
  (is (values-having-validity=
       (make-values-having-validity* '((1000 "2007-01-01TZ" "2007-01-02TZ")))
       (collect-values-having-validity
        `((1000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-02TZ")))
        'first 'second 'third
        (lambda (validity-begin validity-end)
          (error "No value for validity range: ~A - ~A" validity-begin validity-end))
        (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(def test test/dimensional/values-having-validity/collect/no-value ()
  (is (values-having-validity=
       (make-single-d-value 1 (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ"))
       (collect-values-having-validity
        nil
        'first 'second 'third
        (lambda (validity-begin validity-end)
          (declare (ignore validity-begin validity-end))
          1)
        (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(def test test/dimensional/values-having-validity/collect/multiple-value ()
  (is (values-having-validity=
       (make-values-having-validity*
        '((2000 "2007-01-01TZ" "2007-01-10TZ")
          (1000 "2007-01-10TZ" "2007-01-20TZ")
          (3000 "2007-01-20TZ" "2007-01-30TZ")
          (nil "2007-01-30TZ" "2008-01-01TZ")))
       (collect-values-having-validity
        `((1000 ,(parse-timestring "2007-01-10TZ") ,(parse-timestring "2007-01-20TZ"))
          (2000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-15TZ"))
          (3000 ,(parse-timestring "2007-01-05TZ") ,(parse-timestring "2007-01-30TZ")))
        'first 'second 'third
        (lambda (validity-begin validity-end)
          (declare (ignore validity-begin validity-end))
          nil)
        (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))))

(def test test/dimensional/values-having-validity/extract ()
  (bind ((values-having-validity (make-values-having-validity*
                                  `((1000 "2006-01-01TZ" "2007-01-01TZ")
                                    (2000 "2007-01-01TZ" "2008-01-01TZ")
                                    (3000 "2008-01-01TZ" "2009-01-01TZ")))))
    
    (iter (for (validity-begin validity-end value) :in-values-having-validity
               (value-at-coordinates values-having-validity
                                     (parse-timestring "2006-06-06TZ")
                                     (parse-timestring "2008-07-07TZ")))
          (for index from 0)
          (ecase index
            (0 (is (= value 1000))
               (is (timestamp= validity-begin (parse-timestring "2006-06-06TZ")))
               (is (timestamp= validity-end (parse-timestring "2007-01-01TZ"))))
            (1 (is (= value 2000))
               (is (timestamp= validity-begin (parse-timestring "2007-01-01TZ")))
               (is (timestamp= validity-end (parse-timestring "2008-01-01TZ"))))
            (2 (is (= value 3000))
               (is (timestamp= validity-begin (parse-timestring "2008-01-01TZ")))
               (is (timestamp= validity-end (parse-timestring "2008-07-07TZ"))))))

    (iter (for (validity-begin validity-end value) :in-values-having-validity
               (value-at-coordinates values-having-validity
                                     (parse-timestring "2006-06-06TZ")
                                     (parse-timestring "2008-01-01TZ")))
          (for index from 0)
          (ecase index
            (0 (is (= value 1000))
               (is (timestamp= validity-begin (parse-timestring "2006-06-06TZ")))
               (is (timestamp= validity-end (parse-timestring "2007-01-01TZ"))))
            (1 (is (= value 2000))
               (is (timestamp= validity-begin (parse-timestring "2007-01-01TZ")))
               (is (timestamp= validity-end (parse-timestring "2008-01-01TZ"))))))))



(def test test/dimensional/values-having-validity/setf ()
  (labels ((test-value () (make-values-having-validity*
                           '((1000 "2006-01-01TZ" "2007-01-01TZ")
                             (2000 "2007-01-01TZ" "2008-01-01TZ")
                             (3000 "2008-01-01TZ" "2009-01-01TZ"))))
           (set-on-test-value (value validity-begin validity-end)
             (aprog1 (test-value)
               (setf (value-at-coordinates it
                                           (parse-timestring validity-begin)
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

(def test test/dimensional/values-having-validity/collect ()
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

(def test test/dimensional/values-having-validity/iterate-1 ()
  (is (values-having-validity=
       (iter (for (s e (v1 :skip-if-missing) (v2 :skip-if-missing))
                  :in-values-having-validity ((make-values-having-validity*
                                               '((2 "2005-01-01TZ" "2006-01-01TZ")
                                                 (3 "2006-01-01TZ" "2007-01-01TZ")
                                                 (5 "2007-01-01TZ" "2008-01-01TZ")))
                                              (make-values-having-validity*
                                               '((7 "2005-06-01TZ" "2006-06-01TZ")
                                                 (11 "2006-06-01TZ" "2007-06-01TZ")
                                                 (13 "2007-06-01TZ" "2008-06-01TZ")))))
             (collect-value-with-validity (* v1 v2) :from s :to e))
       (make-values-having-validity* '((14 "2005-06-01TZ" "2006-01-01TZ")
                                       (21 "2006-01-01TZ" "2006-06-01TZ")
                                       (33 "2006-06-01TZ" "2007-01-01TZ")
                                       (55 "2007-01-01TZ" "2007-06-01TZ")
                                       (65 "2007-06-01TZ" "2008-01-01TZ"))))))

(def test test/dimensional/values-having-validity/iterate-2 ()
  (is (values-having-validity=
       (iter (for (s e (v1 :default 1) (v2 :default 1))
                  :in-values-having-validity ((make-values-having-validity*
                                               '((2 "2005-01-01TZ" "2006-01-01TZ")
                                                 (3 "2006-01-01TZ" "2007-01-01TZ")
                                                 (5 "2007-01-01TZ" "2008-01-01TZ")))
                                              (make-values-having-validity*
                                               '((7 "2005-06-01TZ" "2006-06-01TZ")
                                                 (11 "2006-06-01TZ" "2007-06-01TZ")
                                                 (13 "2007-06-01TZ" "2008-06-01TZ")))))
             (collect-value-with-validity (* v1 v2) :from s :to e))
       (make-values-having-validity* '((2 "2005-01-01TZ" "2005-06-01TZ")
                                       (14 "2005-06-01TZ" "2006-01-01TZ")
                                       (21 "2006-01-01TZ" "2006-06-01TZ")
                                       (33 "2006-06-01TZ" "2007-01-01TZ")
                                       (55 "2007-01-01TZ" "2007-06-01TZ")
                                       (65 "2007-06-01TZ" "2008-01-01TZ")
                                       (13 "2008-01-01TZ" "2008-06-01TZ"))))))

(def test test/dimensional/values-having-validity/iterate-3 ()
  (signals error
    (iter (for (s e v1 v2)
               :in-values-having-validity ((make-values-having-validity*
                                            '((2 "2005-01-01TZ" "2006-01-01TZ")
                                              (3 "2006-01-01TZ" "2007-01-01TZ")
                                              (5 "2007-01-01TZ" "2008-01-01TZ")))
                                           (make-values-having-validity*
                                            '((7 "2005-06-01TZ" "2006-06-01TZ")
                                              (11 "2006-06-01TZ" "2007-06-01TZ")
                                              (13 "2007-06-01TZ" "2008-06-01TZ")))))
          (collect-value-with-validity (* v1 v2) :from s :to e))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Children having validity

(defsuite* (test/dimensional/children-having-validity :in test/dimensional))

(def test test/dimensional/children-having-validity/collect/single-value ()
  (is (values-having-validity=
       (make-values-having-validity*
        '(((1000) "2007-01-01TZ" "2008-01-01TZ")))
       (collect-children-having-validity
        `((1000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2008-01-01TZ") ,prc::+t-insert+))
        (lambda (e)
          (elt e 0))
        (lambda (e)
          (elt e 1))
        (lambda (e)
          (elt e 2))
        (lambda (e)
          (elt e 3))
        (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))))

(def test test/dimensional/children-having-validity/collect/no-value ()
  (is (values-having-validity=
       (make-values-having-validity*
        '((nil "2006-01-01TZ" "2008-01-01TZ")))
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

(def test test/dimensional/children-having-validity/collect/multiple-value ()
  (is (values-having-validity=
       (make-values-having-validity*
        `((() "2006-01-01TZ" "2007-01-01TZ")
          ((2000) "2007-01-01TZ" "2007-01-05TZ")
          ((2000 3000) "2007-01-05TZ" "2007-01-10TZ")
          ((1000 2000 3000) "2007-01-10TZ" "2007-01-15TZ")
          ((1000 3000) "2007-01-15TZ" "2007-01-20TZ")
          ((3000)  "2007-01-20TZ" "2007-01-30TZ")
          (() "2007-01-30TZ" "2008-01-01TZ")))
       (collect-children-having-validity
        `((1000 ,(parse-timestring "2007-01-10TZ") ,(parse-timestring "2007-01-20TZ") ,prc::+t-insert+)
          (2000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-15TZ") ,prc::+t-insert+)
          (3000 ,(parse-timestring "2007-01-05TZ") ,(parse-timestring "2007-01-30TZ") ,prc::+t-insert+))
        (lambda (e)
          (elt e 0))
        (lambda (e)
          (elt e 1))
        (lambda (e)
          (elt e 2))
        (lambda (e)
          (elt e 3))
        (parse-timestring "2006-01-01TZ") (parse-timestring "2008-01-01TZ"))
       :test (lambda (set1 set2)
               (null (set-difference set1 set2))))))
