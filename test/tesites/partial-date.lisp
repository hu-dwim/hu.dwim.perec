(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;
;;; Partial date

(defsuite* (test/tesites/partial-date :in test/tesites))

(deftest test/tesites/partial-date/first-day ()
  (is (equal "2006-01-01" (date-of-first-day-for-partial-date "2006")))
  (is (equal "2006-06-01" (date-of-first-day-for-partial-date "2006-06")))
  (is (equal "2006-08-12" (date-of-first-day-for-partial-date "2006-08-12"))))

(deftest test/tesites/partial-date/last-day ()
  (is (equal "2006-12-31" (date-of-last-day-for-partial-date "2006")))
  (is (equal "2006-06-30" (date-of-last-day-for-partial-date "2006-06")))
  (is (equal "2005-02-28" (date-of-last-day-for-partial-date "2005-02")))
  (is (equal "2004-02-29" (date-of-last-day-for-partial-date "2004-02")))
  (is (equal "2006-08-12" (date-of-last-day-for-partial-date "2006-08-12"))))
