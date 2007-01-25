(in-package :cl-perec-test)

(defsuite* test/cache :in test)

(deftest test/cache/with-caching-slot-values ()
  (with-caching-slot-values
    (test/cache/slot-access)))

(deftest test/cache/without-caching-slot-values ()
  (without-caching-slot-values
    (test/cache/slot-access)))

(defsuite* test/cache/slot-access :in nil)

(defun counter+ (counter value)
  (if prc::*cache-slot-values* counter (+ counter value)))

(defpclass* cache-test ()
  ((name nil :type (or null (string 20)))))

(deftest test/cache/slot/read-initial-value ()
  (with-transaction
    (bind ((object (make-instance 'cache-test :name "the one"))
           (select-counter (current-select-counter)))
      (name-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/slot/multiple-read ()
  (bind ((object
          (with-transaction
            (make-instance 'cache-test :name "the one")))
         select-counter)
    (with-transaction
      (revive-object object)
      (name-of object)
      (setf select-counter (current-select-counter))
      (name-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/slot/write-and-read ()
  (with-transaction
    (bind ((object (make-instance 'cache-test))
           (select-counter))
      (setf (name-of object) "the one")
      (setf select-counter (current-select-counter))
      (name-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/reference/read-initial-value ()
  (with-transaction
    (bind ((object (make-instance 'reference-test :referred (make-instance 'referred-test)))
           (select-counter (current-select-counter)))
      (referred-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/reference/multiple-read ()
  (bind ((object
          (with-transaction
            (make-instance 'reference-test :referred (make-instance 'referred-test))))
         select-counter)
    (with-transaction
      (revive-object object)
      (referred-of object)
      (setf select-counter (current-select-counter))
      (referred-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/reference/write-and-read ()
  (with-transaction
    (bind ((object (make-instance 'reference-test))
           (select-counter))
      (setf (referred-of object) (make-instance 'referred-test))
      (setf select-counter (current-select-counter))
      (referred-of object)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/association/1-1/read-initial-value ()
  (with-transaction
    (bind ((sister (make-instance 'sister-test))
           (brother (make-instance 'brother-test :sister sister))
           (select-counter (current-select-counter)))
      (sister-of brother)
      (brother-of sister)
      (is (= (counter+ select-counter 2) (current-select-counter))))))

(deftest test/cache/association/1-1/multiple-read ()
  (bind (((sister brother)
          (with-transaction
            (bind ((sister (make-instance 'sister-test))
                   (brother (make-instance 'brother-test :sister sister)))
              (list sister brother))))
         select-counter)
    (with-transaction
      (revive-object sister)
      (revive-object brother)
      (sister-of brother)
      (brother-of sister)
      (setf select-counter (current-select-counter))
      (sister-of brother)
      (brother-of sister)
      (is (= (counter+ select-counter 2) (current-select-counter))))))

(deftest test/cache/association/1-1/write-and-read ()
  (with-sister-and-brother-transaction
    (bind (select-counter)
      (setf (sister-of brother) sister)
      (setf select-counter (current-select-counter))
      (sister-of brother)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/association/1-n/read-initial-value ()
  (with-transaction
    (bind ((parent (make-instance 'parent-test))
           (child (make-instance 'child-test :parent parent))
           (select-counter (current-select-counter)))
      (parent-of child)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/association/1-n/multiple-read ()
  (bind ((child
          (with-transaction
            (make-instance 'child-test :parent (make-instance 'parent-test))))
         select-counter)
    (with-transaction
      (revive-object child)
      (parent-of child)
      (setf select-counter (current-select-counter))
      (parent-of child)
      (is (= (counter+ select-counter 1) (current-select-counter))))))

(deftest test/cache/association/1-n/write-and-read ()
  (with-parent-and-child-transaction
    (bind (select-counter)
      (setf (parent-of child) parent)
      (setf select-counter (current-select-counter))
      (parent-of child)
      (is (= (counter+ select-counter 1) (current-select-counter))))))
