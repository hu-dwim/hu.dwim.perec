(in-package :cl-perec-test)

(defsuite* (test/query/select/order-by :in test/query/select))

(defun check-ordered (classes order-spec)
  (bind ((compare-fn (generate-compare-fn order-spec))
;; PORT: is does not support macros inside, yet to come
         (pass (iter (for class in classes)
                     (for prev previous class)
                     (always (or (null prev)
                                 (funcall compare-fn prev class))))))
;; PORT:
;;"Ordering of ~a by ~a failed." classes order-spec)))
    (is pass)))

(defun generate-compare-fn (order-spec)
  (lambda (class1 class2)
    (iter (for (dir attr) on order-spec by 'cddr)
          (for attr-value-1 = (slot-value class1 attr))
          (for attr-value-2 = (slot-value class2 attr))
          (for compare-fn = (ecase dir (:asc 'prc::less-or-equal-p) (:desc 'prc::greater-or-equal-p)))
          (cond
            ((funcall compare-fn attr-value-1 attr-value-2) (return #t))
            ((not (equal attr-value-1 attr-value-2)) (return #f)))
          (finally (return #t)))))

(defmacro run-order-by-test (&body body)
  `(progn
    (fill-data-6)
    (run-queries
      ,@body)))

(defpclass* order-by-test ()
  ((int-attr :type integer-32)
   (str-attr :type (text 10))
   (date-attr :type date)))

(defixture fill-data-6
  (with-transaction
    (purge-instances 'order-by-test)
    (bind ((count 10)
           (int-values (iter (for i from 0 below count) (collect i)))
           (str-values (iter (for i from 0 below count) (collect (string (digit-char i)))))
           (date-values (iter (for i from 0 below count) (collect (make-local-time
                                                                   :day i
                                                                   :timezone +utc-zone+)))))
      (macrolet ((random-element (list)
                   (with-unique-names (element)
                     `(let ((,element (nth (random (length ,list)) ,list)))
                       (setf ,list (remove ,element ,list))
                       ,element))))
        (iter (for i from 0 below count)
              (make-instance 'order-by-test
                             :int-attr (random-element int-values)
                             :str-attr (random-element str-values)
                             :date-attr (random-element date-values)))))))

(deftest test/query/select/order-by/integer/asc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :asc (int-attr-of o)))
     (list :asc 'int-attr))))

(deftest test/query/select/order-by/integer/desc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :desc (int-attr-of o)))
     (list :desc 'int-attr))))

(deftest test/query/select/order-by/string/asc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :asc (str-attr-of o)))
     (list :asc 'str-attr))))

(deftest test/query/select/order-by/string/desc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :desc (str-attr-of o)))
     (list :desc 'str-attr))))

(deftest test/query/select/order-by/date/asc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :asc (date-attr-of o)))
     (list :asc 'date-attr))))

(deftest test/query/select/order-by/date/desc ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :desc (date-attr-of o)))
     (list :desc 'date-attr))))

(deftest test/query/select/order-by/all ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :asc (int-attr-of o) :desc (str-attr-of o)))
     (list :asc 'int-attr :desc 'str-attr))))

(deftest test/query/select/order-by/expression ()
  (run-order-by-test
    (check-ordered
     (select (o)
       (from (o order-by-test))
       (order-by :asc (- (int-attr-of o))))
     (list :desc 'int-attr))))

(deftest test/query/select/order-by/error ()
  (run-order-by-test
    (signals error
      (select (o)
        (from (o order-by-test))
        (order-by :asc (int-attr-of 'o))))))

