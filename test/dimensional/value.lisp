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
    (is (null (coordinate-intersection dimension '(ie 0 . 10) '(ie 10 . 20))))
    (is (null (coordinate-intersection dimension '(ie 0 . 10) '(ii 10 . 10))))
    (is (equal '(ii 0 . 0) (coordinate-intersection dimension '(ie 0 . 10) '(ii 0 . 0))))
    (is (equal '(ie 5 . 10) (coordinate-intersection dimension '(ie 0 . 10) '(ie 5 . 15))))
    (is (equal '(ie 0 . 5) (coordinate-intersection dimension '(ie 0 . 10) '(ie -5 . 5))))
    (is (equal '(ie 0 . 10) (coordinate-intersection dimension '(ie 0 . 10) '(ie -5 . 15))))))

(def test test/dimensional/value/range-union ()
  (bind ((dimension (find-dimension 'validity)))
    (is (equal '(ie 0 . 20) (coordinate-union dimension '(ie 0 . 10) '(ie 10 . 20))))
    (is (equal '(ii 0 . 10) (coordinate-union dimension '(ie 0 . 10) '(ii 10 . 10))))
    (is (equal '(ie 0 . 10) (coordinate-union dimension '(ie 0 . 10) '(ii 0 . 0))))
    (is (equal '(ie 0 . 15) (coordinate-union dimension '(ie 0 . 10) '(ie 5 . 15))))
    (is (equal '(ie -5 . 10) (coordinate-union dimension '(ie 0 . 10) '(ie -5 . 5))))
    (is (equal '(ie -5 . 15) (coordinate-union dimension '(ie 0 . 10) '(ie -5 . 15))))))

(def test test/dimensional/value/range-difference ()
  (bind ((dimension (find-dimension 'validity)))
    (is (equal '((ie 0 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ie 10 . 20))))
    (is (equal '((ie 0 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ii 10 . 10))))
    (is (equal '((ee 0 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ii 0 . 0))))
    (is (equal '((ie 0 . 5)) (coordinate-difference dimension '(ie 0 . 10) '(ie 5 . 15))))
    (is (equal '((ie 5 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ie -5 . 5))))
    (is (equal '((ie 0 . 1) (ie 9 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ie 1 . 9))))
    (is (null (coordinate-difference dimension '(ie 0 . 10) '(ie -5 . 15))))))

;;;;;;
;;; Coordinates union

(def test test/dimensional/value/coordinates-a-union-empty-is-a (dimensions coordinates-a)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         (make-empty-coordinates dimensions)))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-union-a-is-a (dimensions coordinates-a)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         coordinates-a))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-union-b-is-b-union-a (dimensions coordinates-a coordinates-b)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         coordinates-b))
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-b
                                                         coordinates-a)))))

(def test test/dimensional/value/coordinates-a-union-b-union-c-is-a-union-b-union-c (dimensions coordinates-a coordinates-b coordinates-c)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
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
  (test/dimensional/value/coordinates-a-union-empty-is-a *test-dimensions* '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-union-a-is-a *test-dimensions* '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-union-b-is-b-union-a *test-dimensions*
                                                             '((ie 10 . 20) (ie 0 . 100) (a))
                                                             '((ie 20 . 40) (ie 0 . 100) (a)))
  (test/dimensional/value/coordinates-a-union-b-union-c-is-a-union-b-union-c *test-dimensions*
                                                                             '((ie 10 . 20) (ie 0 . 100) (a))
                                                                             '((ie 20 . 40) (ie 0 . 100) (a))
                                                                             '((ie 40 . 80) (ie 0 . 100) (a))))

;;;;;;
;;; Coordinates intersecion

(def test test/dimensional/value/coordinates-a-intersection-empty-is-empty (dimensions coordinates-a)
  (is (null (coordinates-intersection dimensions
                                      coordinates-a
                                      (make-empty-coordinates dimensions)))))

(def test test/dimensional/value/coordinates-a-intersection-a-is-a (dimensions coordinates-a)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                coordinates-a))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-intersection-b-is-b-intersection-a (dimensions coordinates-a coordinates-b)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                coordinates-b))
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-b
                                                                coordinates-a)))))

(def test test/dimensional/value/coordinates-a-intersection-b-intersection-c-is-a-intersection-b-intersection-c (dimensions coordinates-a coordinates-b coordinates-c)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-intersection dimensions
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
  (test/dimensional/value/coordinates-a-intersection-empty-is-empty
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-intersection-a-is-a
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-intersection-b-is-b-intersection-a
   *test-dimensions*
   '((ie 10 . 40) (ie 0 . 100) (a b))
   '((ie 20 . 60) (ie 50 . 200) (a c)))
  (test/dimensional/value/coordinates-a-intersection-b-intersection-c-is-a-intersection-b-intersection-c
   *test-dimensions*
   '((ie 10 . 40) (ie 0 . 100) (a b c))
   '((ie 20 . 60) (ie 50 . 200) (b c d))
   '((ie 0 . 100) (ie 20 . 60) (c))))

;;;;;;
;;; Coordinates difference

(def test test/dimensional/value/coordinates-a-difference-empty-is-a (dimensions coordinates-a)
  (is (prc::every* [coordinates-equal dimensions !1 !2]
                   (is-not-null (coordinates-difference dimensions
                                                        coordinates-a
                                                        (make-empty-coordinates dimensions)))
                   (list coordinates-a))))

(def test test/dimensional/value/coordinates-a-difference-a-is-empty (dimensions coordinates-a)
  (is (null (coordinates-difference dimensions
                                    coordinates-a
                                    coordinates-a))))

(def test test/dimensional/value/coordinates-difference-invariants ()
  (test/dimensional/value/coordinates-a-difference-empty-is-a
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-difference-a-is-empty
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b))))

(def test test/dimensional/value/coordinates-difference (dimensions coordinates-a coordinates-b expected)
  (is (null (set-exclusive-or
             (coordinates-difference dimensions coordinates-a coordinates-b)
             expected
             :test [coordinates-equal dimensions !1 !2]))))

(def test test/dimensional/value/coordinates-difference-samples ()
  (test/dimensional/value/coordinates-difference
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a))
   '((ie 10 . 20) (ie 10 . 90) (a))
   '(((ie 10 . 20) (ie 0 . 10) (a))
     ((ie 10 . 20) (ie 90 . 100) (a))))

  (test/dimensional/value/coordinates-difference
   (mapcar 'find-dimension '(enumerated validity))
   `((a) (ie 0 . 100))
   `((a) (ie 10 . 90))
   `(((a) (ie 0 . 10))
     ((a) (ie 90 . 100))))
  
  (test/dimensional/value/coordinates-difference
   (mapcar 'find-dimension '(enumerated validity))
   `((a) (ie ,+beginning-of-time+ . ,+end-of-time+))
   `((a) (ie ,(parse-datestring "2008-01-01") . ,(parse-datestring "2009-01-01")))
   `(((a) (ie ,+beginning-of-time+ . ,(parse-datestring "2008-01-01")))
     ((a) (ie ,(parse-datestring "2009-01-01") . ,+end-of-time+)))))

(def test test/dimensional/value/d-value-equal/positive ()
  (is (d-value-equal
            (make-single-d-value
             '(time validity)
             (list
              (make-empty-coordinate-range +end-of-time+)
              (make-coordinate-range 'ie +beginning-of-time+ +end-of-time+))
             1)
            (make-single-d-value
             '(time validity)
             (list
              (make-empty-coordinate-range +end-of-time+)
              (make-coordinate-range 'ie +beginning-of-time+ +end-of-time+))
             1)))
  (is (d-value-equal
       (make-empty-d-value '(time validity))
       (make-empty-d-value '(time validity)))))

(def test test/dimensional/value/d-value-equal/negative ()
  (is (not (d-value-equal
            (make-single-d-value '(time validity)
                                 (list
                                  (make-empty-coordinate-range +end-of-time+)
                                  (make-coordinate-range 'ie +beginning-of-time+ +end-of-time+))
                                 1)
            (make-single-d-value '(time validity)
                                 (list
                                  (make-empty-coordinate-range +end-of-time+)
                                  (make-coordinate-range 'ie +beginning-of-time+ +end-of-time+))
                                 2))))
  (is (not (d-value-equal
            (make-empty-d-value '(time validity))
            (make-single-d-value '(time validity)
                                 (list
                                  (make-empty-coordinate-range +end-of-time+)
                                  (make-coordinate-range 'ie +beginning-of-time+ +end-of-time+))
                                 2))))
  (is (not (d-value-equal
            (make-single-d-value '(time validity)
                                 (list
                                  (make-empty-coordinate-range +end-of-time+)
                                  (make-coordinate-range 'ie +beginning-of-time+ +end-of-time+))
                                 1)
            (make-empty-d-value '(time validity))))))

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
            (make-coordinate-range 'ie +beginning-of-time+ (parse-datestring "2001-01-01")))
      1)
     (make-single-d-value
      dimensions
      (list (make-coordinate-range 'ii (parse-datestring "2000-01-01") +end-of-time+)
            (make-coordinate-range 'ie (parse-datestring "2000-01-01") +end-of-time+))
      2)
     (list
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range 'ie +beginning-of-time+ (parse-datestring "2000-01-01")))
       (list 1 nil))
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range 'ie (parse-datestring "2000-01-01") (parse-datestring "2001-01-01")))
       (list 1 2))
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range 'ie (parse-datestring "2001-01-01") +end-of-time+))
       (list nil 2))
      (make-single-d-value
       dimensions
       (list (make-coordinate-range 'ie (parse-datestring "2000-01-01") +end-of-time+)
             (make-coordinate-range 'ie (parse-datestring "2000-01-01") +end-of-time+))
       (list nil 2))))))

(def test test/dimensional/value/d-project (d-value projecting-dimensions expected-d-value)
  (is (d-value-equal
       (break "~S ~S" expected-d-value (d-project
         (lambda (dimensions coordinates-list values)
           (iter (for dimension :in dimensions)
                 (for coordinates :in coordinates-list)
                 (for value :in values)
                 (for volume = (reduce #'* coordinates
                                       :initial-value 1
                                       :key (lambda (coordinate)
                                              (etypecase dimension
                                                (ordering-dimension (- (coordinate-range-end coordinate)
                                                                       (coordinate-range-begin coordinate)))
                                                (dimension (length coordinate))))))
                 (summing (* volume value))))
         (mapcar #'lookup-dimension projecting-dimensions)
         d-value))
       expected-d-value)))

(def test test/dimensional/value/d-project/1 ()
  (bind ((d-value (make-d-value *test-dimensions*
                                '(((ie 0 . 1) (ie 0 . 1) (a))
                                  ((ie 0 . 1) (ie 1 . 2) (a))
                                  ((ie 1 . 2) (ie 0 . 2) (a))
                                  ((ie 0 . 1) (ie 0 . 2) (b))
                                  ((ie 1 . 2) (ie 0 . 1) (b))
                                  ((ie 1 . 2) (ie 1 . 2) (b)))
                                '(1 2 3 4 5 6))))
    ;;(test/dimensional/value/d-project d-value '() d-value)
    (test/dimensional/value/d-project d-value '(time)
                                      (make-d-value '(validity enumerated)
                                                    '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                    `(,(+ (* 1 1) (* 1 3))
                                                      ,(+ (* 1 2) (* 1 3))
                                                      ,(+ (* 1 4) (* 1 5))
                                                      ,(+ (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(validity)
                                      (make-d-value '(time enumerated)
                                                    '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                    `(,(+ (* 1 1) (* 1 2))
                                                      ,(+ (* 2 3))
                                                      ,(+ (* 2 4))
                                                      ,(+ (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(enumerated)
                                      (make-d-value '(time validity)
                                                    '(((ie 0 . 1) (ie 0 . 1)) ((ie 0 . 1) (ie 1 . 2))
                                                      ((ie 1 . 2) (ie 0 . 1)) ((ie 1 . 2) (ie 1 . 2)))
                                                    `(,(+ (* 1 1) (* 1 4))
                                                      ,(+ (* 1 2) (* 1 4))
                                                      ,(+ (* 1 3) (* 1 5))
                                                      ,(+ (* 1 3) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(validity enumerated)
                                      (make-d-value '(time)
                                                    '(((ie 0 . 1)) ((ie 1 . 2)))
                                                    `(,(+ (* 1 1) (* 1 2) (* 2 4))
                                                      ,(+ (* 2 3) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(time enumerated)
                                      (make-d-value '(validity)
                                                    '(((ie 0 . 1)) ((ie 1 . 2)))
                                                    `(,(+ (* 1 1) (* 1 3) (* 1 4) (* 1 5))
                                                      ,(+ (* 1 2) (* 1 3) (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(time validity)
                                      (make-d-value '(enumerated)
                                                    '(((a)) ((b)))
                                                    `(,(+ (* 1 1) (* 1 2) (* 2 3))
                                                      ,(+ (* 2 4) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(time validity enumerated)
                                      (make-d-value '()
                                                    '(())
                                                    `(,(+ (* 1 1) (* 1 2) (* 2 3) (* 2 4) (* 1 5) (* 1 6)))))))

(def test test/dimensional/value/d-fold (d-value projecting-dimensions expected-d-value)
  (setf projecting-dimensions (mapcar #'lookup-dimension projecting-dimensions))
  (is (d-value-equal
       (d-fold
        (lambda (accumulator coordinates value)
          (aprog1
              (+ accumulator
                 (* value
                    (reduce #'* (mapcar #'cons projecting-dimensions coordinates)
                            :initial-value 1
                            :key (lambda (dimension-and-coordinate)
                                   (bind ((dimension (car dimension-and-coordinate))
                                          (coordinate (cdr dimension-and-coordinate)))
                                     (etypecase dimension
                                       (ordering-dimension (- (coordinate-range-end coordinate)
                                                              (coordinate-range-begin coordinate)))
                                       (dimension (length coordinate))))))))))
        projecting-dimensions
        d-value
        :initial-value 0)
       expected-d-value)))

(def test test/dimensional/value/d-fold/1 ()
  (bind ((d-value (make-d-value *test-dimensions*
                                '(((ie 0 . 1) (ie 0 . 1) (a))
                                  ((ie 0 . 1) (ie 1 . 2) (a))
                                  ((ie 1 . 2) (ie 0 . 2) (a))
                                  ((ie 0 . 1) (ie 0 . 2) (b))
                                  ((ie 1 . 2) (ie 0 . 1) (b))
                                  ((ie 1 . 2) (ie 1 . 2) (b)))
                                '(1 2 3 4 5 6))))
    (test/dimensional/value/d-fold d-value '() d-value)
    (test/dimensional/value/d-fold d-value '(time)
                                         (make-d-value '(validity enumerated)
                                                       '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                       `(,(+ (* 1 1) (* 1 3))
                                                          ,(+ (* 1 2) (* 1 3))
                                                          ,(+ (* 1 4) (* 1 5))
                                                          ,(+ (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(validity)
                                         (make-d-value '(time enumerated)
                                                       '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                       `(,(+ (* 1 1) (* 1 2))
                                                          ,(+ (* 2 3))
                                                          ,(+ (* 2 4))
                                                          ,(+ (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(enumerated)
                                         (make-d-value '(time validity)
                                                       '(((ie 0 . 1) (ie 0 . 1)) ((ie 0 . 1) (ie 1 . 2))
                                                         ((ie 1 . 2) (ie 0 . 1)) ((ie 1 . 2) (ie 1 . 2)))
                                                       `(,(+ (* 1 1) (* 1 4))
                                                          ,(+ (* 1 2) (* 1 4))
                                                          ,(+ (* 1 3) (* 1 5))
                                                          ,(+ (* 1 3) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(validity enumerated)
                                         (make-d-value '(time)
                                                       '(((ie 0 . 1)) ((ie 1 . 2)))
                                                       `(,(+ (* 1 1) (* 1 2) (* 2 4))
                                                          ,(+ (* 2 3) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(time enumerated)
                                         (make-d-value '(validity)
                                                       '(((ie 0 . 1)) ((ie 1 . 2)))
                                                       `(,(+ (* 1 1) (* 1 3) (* 1 4) (* 1 5))
                                                          ,(+ (* 1 2) (* 1 3) (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(time validity)
                                         (make-d-value '(enumerated)
                                                       '(((a)) ((b)))
                                                       `(,(+ (* 1 1) (* 1 2) (* 2 3))
                                                          ,(+ (* 2 4) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(time validity enumerated)
                                         (make-d-value '()
                                                       '(())
                                                       `(,(+ (* 1 1) (* 1 2) (* 2 3) (* 2 4) (* 1 5) (* 1 6)))))))
