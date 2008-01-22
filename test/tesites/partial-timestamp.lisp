(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;
;;; Partial timestamp

(defsuite* (test/tesites/partial-timestamp :in test/tesites))

(deftest test/tesites/partial-timestamp/first-moment ()
  (is (equal "2006-01-01T00:00:00Z" (first-moment-for-partial-timestamp "2006")))
  (is (equal "2006-06-01T00:00:00Z" (first-moment-for-partial-timestamp "2006-06")))
  (is (equal "2006-06-06T00:00:00Z" (first-moment-for-partial-timestamp "2006-06-06")))
  (is (equal "2006-06-06T06:00:00Z" (first-moment-for-partial-timestamp "2006-06-06T06")))
  (is (equal "2006-06-06T06:06:00Z" (first-moment-for-partial-timestamp "2006-06-06T06:06")))
  (is (equal "2006-06-06T06:06:06Z" (first-moment-for-partial-timestamp "2006-06-06T06:06:06"))))

(deftest test/tesites/partial-timestamp/last-moment ()
  (is (equal "2007-01-01T00:00:00Z" (last-moment-for-partial-timestamp "2006")))
  (is (equal "2006-07-01T00:00:00Z" (last-moment-for-partial-timestamp "2006-06")))
  (is (equal "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06")))
  (is (equal "2006-06-06T07:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T06")))
  (is (equal "2006-06-06T06:07:00Z" (last-moment-for-partial-timestamp "2006-06-06T06:06")))
  (is (equal "2006-06-06T06:06:07Z" (last-moment-for-partial-timestamp "2006-06-06T06:06:06"))))

(deftest test/tesites/partial-timestamp/overflow ()
  (is (equal "2007-01-01T00:00:00Z" (last-moment-for-partial-timestamp "2006-12")))
  (is (equal "2004-03-01T00:00:00Z" (last-moment-for-partial-timestamp "2004-02-29")))
  (is (equal "2005-03-01T00:00:00Z" (last-moment-for-partial-timestamp "2005-02-28")))
  (is (equal "2007-01-01T00:00:00Z" (last-moment-for-partial-timestamp "2006-12-31")))
  (is (equal "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T23")))
  (is (equal "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T23:59")))
  (is (equal "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T23:59:59"))))
