(in-package :cl-perec-test)

;;;; TODO: specify expected result in tests

(defpclass* topic-test ()
  ((title :type (string 50))))

(defpclass* message-test ()
  ((subject :type (string 50))
   (content :type (string 50))))

(defpclass* ad-test (message-test)
  ())

(defpclass* spam-test (ad-test)
  ((score :type integer-32)
   (spam-type :type (member phishing money-from-africa viagra))))

(defpclass* owner-test ()
  ((name :type (string 50)))
  (:abstract #t))

(defpclass* user-test (owner-test)
  ((password :type (string 50))
   (birthday :type date)
   (age 32 :persistent #f :type integer-32)))

(defassociation*
  ((:class topic-test :slot messages :type (set message-test))
   (:class message-test :slot topic :type topic-test)))

(defassociation*
  ((:class owner-test :slot topics :type (set topic-test))
   (:class topic-test :slot owner :type owner-test)))

;; PORT:
(defixture fill-data-1
  (with-transaction
    (purge-objects 'owner-test)
    (purge-objects 'topic-test)
    (purge-objects 'message-test)
    (bind ((user1 (make-instance 'user-test
                                 :name "user1"
                                 :birthday (encode-local-time 0 0 0 0 22 4 1984 :timezone +utc-zone+)
                                 :password "secret"))
           (topic1 (make-instance 'topic-test :title "topic1" :owner user1)))
      (make-instance 'user-test
                     :name "user2"
                     :birthday (encode-local-time 0 0 0 0 2 7 1975 :timezone +utc-zone+)
                     :password "sglF$%3D")
      (make-instance 'topic-test :title "topic2" :owner user1)
      (make-instance 'message-test :subject "subject1" :content "content1" :topic topic1)
      (make-instance 'message-test :subject "subject2" :content "content2" :topic topic1)
      (make-instance 'ad-test :subject "ad1" :content "content3" :topic topic1)
      (make-instance 'spam-test :subject "spam1" :content "content4" :topic topic1 :score 10 :spam-type 'viagra))))

(deftest test/query/select/simple ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (select (o)
      (assert (typep o 'message-test))
      (collect o))))

(deftest test/query/select/short ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (select ((o message-test))
      (collect o))))

(deftest test/query/select/all ()
  (test-query (:fixture fill-data-1)
    (select (o)
      (collect o))))

(deftest test/query/select/symbol-enum ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-1)
    (select ((s spam-test))
      (assert (eq (spam-type-of s) 'viagra))
      (collect s))))

(deftest test/query/select/nested-ands ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (select (object)
      (assert (and t (and t (typep object 'message-test))))
      (collect object))))

(deftest test/query/select/<= ()
  (test-query (:select-count 1 :record-count 0 :fixture fill-data-1)
    (select ((s spam-test))
      (assert (<= 50 (score-of s) 100))
      (collect s)))
  (test-query (:select-count 1 :record-count 1)
    (select ((s spam-test))
      (assert (not (<= 50 (score-of s) 100)))
      (collect s))))

(deftest test/query/select/multiple-objects ()
  (test-query (:record-count (* 2 4 2) :fixture fill-data-1)
    (select ((user user-test) (message message-test) (topic topic-test))
      (collect user message topic))))

(deftest test/query/select/with-lexical-variables-1 ()
  (test-query (:record-count 1 :fixture fill-data-1)
    (let ((user-name "user1"))
      (select ((user user-test))
        (assert (equal (name-of user) user-name))
        (collect user)))))

(deftest test/query/select/with-lexical-variables-2 ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (let ((class (find-class 'message-test)))
      (select (o)
        (assert (typep o (class-name class)))
        (collect o)))))

(deftest test/query/select/with-dynamic-variables ()
  (with-fixture fill-data-1
      (let ((user (with-transaction (select-first-matching user-test))))
        (test-query (:select-count nil :record-count 2)
          (revive-object user)          ; for eq
          (select ((o topic-test))
            (assert (eq (owner-of o) user))
            (collect o))))))

(deftest test/query/select/polimorph-association-end ()
  (test-query (:select-count (+ 2 1) :record-count 2 :fixture fill-data-1)
    (let ((topic (select-first-matching topic-test)))
      (select ((o ad-test))
        (assert (eq (topic-of o) topic))
        (collect o)))))

(deftest test/query/select/slot ()
  (test-query (:record-count 2 :fixture fill-data-1)
    (select ((user user-test))
      (collect (name-of user)))))

(deftest test/query/select/association ()
  (test-query (:select-count nil :record-count 2 :fixture fill-data-1)
    (select ((topic topic-test))
      (collect (owner-of topic)))))

(deftest test/query/select/or ()
  (test-query (:record-count (+ 4 2) :fixture fill-data-1)
    (select (o)
      (assert (or (typep o 'message-test) (typep o 'topic-test)))
      (collect o))))

(deftest test/query/select/builder ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (let ((query (make-query nil)))
      (add-query-variable query 'm)
      (add-assert query '(typep m 'message-test))
      (add-collect query 'm)
      (execute-query query))))

(deftest test/query/select/join-1 ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (select ((message message-test))
      (assert (equal (title-of (topic-of message)) "topic1"))
      (collect message))))

(deftest test/query/select/join-n/collect-child ()
  (test-query (:record-count 4 :fixture fill-data-1)
    (select ((topic topic-test) (message message-test))
      (assert (equal (title-of topic) "topic1"))
      (assert (member message (messages-of topic)))
      (collect message))))

(deftest test/query/select/join-n/collect-parent ()
  (test-query (:record-count 1 :fixture fill-data-1)
    (select ((topic topic-test) (message message-test))
      (assert (equal (title-of topic) "topic1"))
      (assert (member message (messages-of topic)))
      (assert (equal (subject-of message) "subject1"))
      (collect topic))))

(deftest test/query/select/join-n-in-collect ()
  (test-query (:select-count nil :record-count 1 :fixture fill-data-1)
    (select ((topic topic-test))
      (assert (equal (title-of topic) "topic1"))
      (collect (messages-of topic)))))

(deftest test/query/select/general ()
  (test-query (:select-count nil :record-count 1 :fixture fill-data-1)
    (select ((topic topic-test))
      (when (equal (title-of topic) "topic1")
        (collect topic)))))

(deftest test/query/select/with-lisp-filter ()
  (test-query (:select-count nil :record-count 1 :fixture fill-data-1)
    (let ((predicate (lambda (message) (equal (subject-of message) "subject1"))))
      (select ((message message-test))
        (assert (funcall predicate message))
        (collect message)))))

(deftest test/query/select/cnf ()
  (test-query (:record-count 1 :fixture fill-data-1)
    (select ((m message-test))
      (assert (or (not (equal (title-of (topic-of m)) "topic1"))
                  (and (equal (subject-of m) "subject1")
                       (equal (content-of m) "content1"))))
      (collect m))))

(deftest test/query/select/typep ()
  (test-query (:record-count 2 :fixture fill-data-1)
    (let ((subject "subject1"))
      (select ((m message-test))
        (assert (and (or (typep m 'ad-test)
                         (equal (subject-of m) subject))
                     (not (typep m 'spam-test))))
        (collect m)))))

(deftest test/query/select/macro ()
  (define-query-macro s-of (message)
    `(subject-of ,message))
  (test-query (:record-count 1 :fixture fill-data-1)
    (select ((m message-test))
      (assert (equal (s-of m) "subject1"))
      (collect m))))

(deftest test/query/select/count ()
  (test-query (:record-count 1 :fixture fill-data-1)
    (select ((topic topic-test))
      (assert (>= (length (messages-of topic)) 2))
      (collect topic))))

(deftest test/query/select/member-1 ()
  (test-query (:select-count 2 :record-count 3 :fixture fill-data-1)
    (let ((messages (cdr (prc::select-objects message-test))))
      (select ((m message-test))
        (assert (member m messages))
        (collect m)))))

(deftest test/query/select/member-2 ()
  (test-query (:record-count 1 :fixture fill-data-1)
    (select ((m message-test))
      (assert (member (subject-of m) '("subject1" "no-such-subject") :test 'equal))
      (collect m))))

(deftest test/query/select/member-3 ()
  (test-query (:select-count 0 :record-count 0 :fixture fill-data-1)
    (select ((m message-test))
      (assert (member (subject-of m) nil))
      (collect m))))

(deftest test/query/select/lisp-expr ()
  (test-query (:record-count 1 :fixture fill-data-1)
    (let ((num "1"))
      (select ((topic topic-test))
        (assert (equal (title-of topic) (strcat "topic" num)))
        (collect topic)))))

(deftest test/query/select/regex-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-1)
    (select ((topic topic-test))
      (assert (scan "t.*picx?1" (title-of topic)))
      (collect topic))))

(deftest test/query/select/regex-2 ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-1)
    (select ((topic topic-test))
      (assert (scan "picx?1" (title-of topic) :start 2 :end 6))
      (collect topic))))

(deftest test/query/select/like ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-1)
    (select ((topic topic-test))
      (assert (like (title-of topic) "t%pi_1"))
      (collect topic))))

(deftest test/query/select/strcat ()
  (test-query (:select-count 1 :record-count 1 :fixture fill-data-1)
    (select ((tc topic-test) (m message-test) (u user-test))
      (assert (string= (strcat (title-of tc) ":" (subject-of m) ":" (name-of u))
                       "topic1:subject1:user1"))
      (collect tc m u))))

(deftest test/query/select/non-persistent-slot ()
  (test-query (:record-count 2 :fixture fill-data-1) ; TODO: should select 1
    (select ((u user-test))
      (assert (> (age-of u) 30))
      (collect u))))
