(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Values having validity

(defsuite* (test/tesites/values-having-validty :in test/tesites))

(deftest test/tesites/values-having-validty/collect/single-value ()
  (is (= 1000
         (collect-values-having-validity
          `((1000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-02TZ")))
          'first 'second 'third
          (lambda (validity-start validity-end)
            (error "No value for validity range: ~A - ~A" validity-start validity-end))
          (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(deftest test/tesites/values-having-validty/collect/no-value ()
  (is (= 1
         (collect-values-having-validity
          nil
          'first 'second 'third
          (lambda (validity-start validity-end)
            (declare (ignore validity-start validity-end))
            1)
          (parse-timestring "2007-01-01TZ") (parse-timestring "2007-01-02TZ")))))

(deftest test/tesites/values-having-validty/collect/multiple-value ()
  (collect-values-having-validity
   `((1000 ,(parse-timestring "2007-01-10TZ") ,(parse-timestring "2007-01-20TZ"))
     (2000 ,(parse-timestring "2007-01-01TZ") ,(parse-timestring "2007-01-15TZ"))
     (3000 ,(parse-timestring "2007-01-05TZ") ,(parse-timestring "2007-01-30TZ")))
   'first 'second 'third
   (lambda (validity-start validity-end)
     (declare (ignore validity-start validity-end))
     nil)
   (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))

(deftest test/tesites/values-having-validity/extract ()
  (iter (for (value validity-start validity-end index) :in-values-having-validity
             (extract-values-having-validity
              (make-instance 'values-having-validity
                             :values (make-array 2 :initial-contents '(1000 2000))
                             :validity-starts (make-array 2 :initial-contents (list (parse-timestring "2006-01-01TZ")
                                                                                    (parse-timestring "2007-01-01TZ")))
                             :validity-ends (make-array 2 :initial-contents (list (parse-timestring "2007-01-01TZ")
                                                                                  (parse-timestring "2008-01-01TZ"))))
              (parse-timestring "2006-06-06TZ")
              (parse-timestring "2007-07-07TZ")))
        (ecase index
          (0 (is (= value 1000))
             (is (local-time= validity-start (parse-timestring "2006-06-06TZ")))
             (is (local-time= validity-end (parse-timestring "2007-01-01TZ"))))
          (1 (is (= value 2000))
             (is (local-time= validity-start (parse-timestring "2007-01-01TZ")))
             (is (local-time= validity-end (parse-timestring "2007-07-07TZ")))))))
