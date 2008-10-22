;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;
;;; D value (multi dimensional value)

(defsuite* (test/dimensional/value :in test/dimensional))

(def special-variable *test-dimensions* (mapcar #'lookup-dimension '(time validity enumerated)))

(def function is-not-null (value)
  (is (not (null value)) "The value should not be nil here")
  value)

;;;;;;
;;; Range

(def test test/dimensional/value/range-intersection ()
  (bind ((dimension (find-dimension 'validity)))
    (is (null (coordinate-intersection dimension '(0 . 10) '(10 . 20))))
    (is (null (coordinate-intersection dimension '(0 . 10) '(10 . 10))))
    (is (equal '(0 . 0) (coordinate-intersection dimension '(0 . 10) '(0 . 0))))
    (is (equal '(5 . 10) (coordinate-intersection dimension '(0 . 10) '(5 . 15))))
    (is (equal '(0 . 5) (coordinate-intersection dimension '(0 . 10) '(-5 . 5))))
    (is (equal '(0 . 10) (coordinate-intersection dimension '(0 . 10) '(-5 . 15))))))

(def test test/dimensional/value/range-union ()
  (bind ((dimension (find-dimension 'validity)))
    (is (equal '(0 . 20) (coordinate-union dimension '(0 . 10) '(10 . 20))))
    (is (equal '(0 . 10) (coordinate-union dimension '(0 . 10) '(10 . 10))))
    (is (equal '(0 . 10) (coordinate-union dimension '(0 . 10) '(0 . 0))))
    (is (equal '(0 . 15) (coordinate-union dimension '(0 . 10) '(5 . 15))))
    (is (equal '(-5 . 10) (coordinate-union dimension '(0 . 10) '(-5 . 5))))
    (is (equal '(-5 . 15) (coordinate-union dimension '(0 . 10) '(-5 . 15))))))

(def test test/dimensional/value/range-difference ()
  (bind ((dimension (find-dimension 'validity)))
    (is (equal '((0 . 10)) (coordinate-difference dimension '(0 . 10) '(10 . 20))))
    (is (equal '((0 . 10)) (coordinate-difference dimension '(0 . 10) '(10 . 10))))
    (is (equal '((0 . 10)) (coordinate-difference dimension '(0 . 10) '(0 . 0))))
    (is (equal '((0 . 5)) (coordinate-difference dimension '(0 . 10) '(5 . 15))))
    (is (equal '((5 . 10)) (coordinate-difference dimension '(0 . 10) '(-5 . 5))))
    (is (equal '((0 . 1) (9 . 10)) (coordinate-difference dimension '(0 . 10) '(1 . 9))))
    (is (null (coordinate-difference dimension '(0 . 10) '(-5 . 15))))))

;;;;;;
;;; Coordinates union

(def test test/dimensional/value/coordinates-a-union-empty-is-a (dimensions coordinates-a)
  (is (coordinates-equal (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         (make-empty-coordinates dimensions)))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-union-a-is-a (dimensions coordinates-a)
  (is (coordinates-equal (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         coordinates-a))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-union-b-is-b-union-a (dimensions coordinates-a coordinates-b)
  (is (coordinates-equal (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         coordinates-b))
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-b
                                                         coordinates-a)))))

(def test test/dimensional/value/coordinates-a-union-b-union-c-is-a-union-b-union-c (dimensions coordinates-a coordinates-b coordinates-c)
  (is (coordinates-equal (is-not-null (coordinates-union dimensions
                                                         (is-not-null (coordinates-union dimensions
                                                                                         coordinates-a
                                                                                         coordinates-b))
                                                         coordinates-c))
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         (is-not-null (coordinates-union dimensions
                                                                                         coordinates-b
                                                                                         coordinates-c)))))))

(def test test/dimensional/value/coordinates-union-invariants ()
  (test/dimensional/value/coordinates-a-union-empty-is-a *test-dimensions* '((10 . 20) (0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-union-a-is-a *test-dimensions* '((10 . 20) (0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-union-b-is-b-union-a *test-dimensions*
                                                             '((10 . 20) (0 . 100) (a))
                                                             '((20 . 40) (0 . 100) (a)))
  (test/dimensional/value/coordinates-a-union-b-union-c-is-a-union-b-union-c *test-dimensions*
                                                                             '((10 . 20) (0 . 100) (a))
                                                                             '((20 . 40) (0 . 100) (a))
                                                                             '((40 . 80) (0 . 100) (a))))

;;;;;;
;;; Coordinates intersecion

(def test test/dimensional/value/coordinates-a-intersection-empty-is-empty (dimensions coordinates-a)
  (is (null (coordinates-intersection dimensions
                                      coordinates-a
                                      (make-empty-coordinates dimensions)))))

(def test test/dimensional/value/coordinates-a-intersection-a-is-a (dimensions coordinates-a)
  (is (coordinates-equal (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                coordinates-a))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-intersection-b-is-b-intersection-a (dimensions coordinates-a coordinates-b)
  (is (coordinates-equal (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                coordinates-b))
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-b
                                                                coordinates-a)))))

(def test test/dimensional/value/coordinates-a-intersection-b-intersection-c-is-a-intersection-b-intersection-c (dimensions coordinates-a coordinates-b coordinates-c)
  (is (coordinates-equal (is-not-null (coordinates-intersection dimensions
                                                                (is-not-null (coordinates-intersection dimensions
                                                                                                       coordinates-a
                                                                                                       coordinates-b))
                                                                coordinates-c))
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                (is-not-null (coordinates-intersection dimensions
                                                                                                       coordinates-b
                                                                                                       coordinates-c)))))))

(def test test/dimensional/value/coordinates-intersection-invariants ()
  (test/dimensional/value/coordinates-a-intersection-empty-is-empty *test-dimensions* '((10 . 20) (0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-intersection-a-is-a *test-dimensions* '((10 . 20) (0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-intersection-b-is-b-intersection-a *test-dimensions*
                                                                           '((10 . 40) (0 . 100) (a b))
                                                                           '((20 . 60) (50 . 200) (a c)))
  (test/dimensional/value/coordinates-a-intersection-b-intersection-c-is-a-intersection-b-intersection-c *test-dimensions*
                                                                                                         '((10 . 40) (0 . 100) (a b c))
                                                                                                         '((20 . 60) (50 . 200) (b c d))
                                                                                                         '((0 . 100) (20 . 60) (c))))

;;;;;;
;;; Coordinates difference

(def test test/dimensional/value/coordinates-a-difference-empty-is-a (dimensions coordinates-a)
  (is (prc::every* #'coordinates-equal
                   (is-not-null (coordinates-difference dimensions
                                                        coordinates-a
                                                        (make-empty-coordinates dimensions)))
                   (list coordinates-a))))

(def test test/dimensional/value/coordinates-a-difference-a-is-empty (dimensions coordinates-a)
  (is (null (coordinates-difference dimensions
                                    coordinates-a
                                    coordinates-a))))

(def test test/dimensional/value/coordinates-difference-invariants ()
  (test/dimensional/value/coordinates-a-difference-empty-is-a *test-dimensions* '((10 . 20) (0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-difference-a-is-empty *test-dimensions* '((10 . 20) (0 . 100) (a b))))

(def test test/dimensional/value/d-value-equal ()
  (is (not (d-value-equal
            (make-single-d-value
             (list *time-dimension* *validity-dimension*)
             `((,+end-of-time+ . ,+end-of-time+) (,+beginning-of-time+ . ,+end-of-time+))
             1)
            (make-single-d-value
             (list *time-dimension* *validity-dimension*)
             `((,+end-of-time+ . ,+end-of-time+) (,+beginning-of-time+ . ,+end-of-time+))
             2)))))

(def test test/dimensional/value/iter-in-d-values (d-value-1 d-value-2 expected-d-values)
  (iter (with dimensions = (prc::dimensions-of d-value-1))
        (with test = [d-value= !1 !2 :test #'equal])
        (for (coordinates values) :in-d-values (d-value-1 d-value-2) :unspecified-value nil)
        (for d-value = (make-single-d-value dimensions coordinates values))
        (is (member d-value expected-d-values :test test) "Unexpected: ~S" d-value)
        (removef expected-d-values d-value :test test)
        (finally (is (null expected-d-values) "Missing: ~S" expected-d-values))))

(def test test/dimensional/value/iter-in-d-values/1 ()
  (bind ((dimensions (list *time-dimension* *validity-dimension*)))
    (test/dimensional/value/iter-in-d-values
     (make-single-d-value
      dimensions
      (list (make-empty-coordinate-range +end-of-time+)
            (make-coordinate-range +beginning-of-time+ (parse-datestring "2001-01-01")))
      1)
     (make-single-d-value
      dimensions
      (list (make-coordinate-range (parse-datestring "2000-01-01") +end-of-time+)
            (make-coordinate-range (parse-datestring "2000-01-01") +end-of-time+))
      2)
     (list
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range +beginning-of-time+ (parse-datestring "2000-01-01")))
       (list 1 nil))
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range (parse-datestring "2000-01-01") (parse-datestring "2001-01-01")))
       (list 1 2))
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range (parse-datestring "2000-01-01") +end-of-time+))
       (list nil 2))
      (make-single-d-value
       dimensions
       (list (make-coordinate-range (parse-datestring "2000-01-01") +end-of-time+)
             (make-coordinate-range (parse-datestring "2000-01-01") +end-of-time+))
       (list nil 2))))))