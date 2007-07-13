(in-package :cl-perec-test)

;;;; TODO: specify expected result in tests

(defsuite* (test/query/select :in test/query))

(defpclass* topic-test ()
  ((title :type (text 50))))

(defpclass* message-test ()
  ((subject :type (text 50))
   (content :type (text 50))))

(defpclass* ad-test (message-test)
  ())

(defpclass* spam-test (ad-test)
  ((score :type integer-32)
   (spam-type :type (member phishing money-from-africa viagra))))

(defpclass* owner-test ()
  ((name :type (text 50)))
  (:abstract #t))

(defpclass* user-test (owner-test)
  ((password :type (text 50))
   (birthday :type date)
   (age 32 :persistent #f :type integer-32)))

(defassociation*
  ((:class topic-test :slot messages :type (set message-test))
   (:class message-test :slot topic :type topic-test)))

(defassociation*
  ((:class owner-test :slot topics :type (set topic-test))
   (:class topic-test :slot owner :type owner-test)))

;; PORT:
(defixture forum-data
  (with-transaction
    (purge-instances 'owner-test)
    (purge-instances 'topic-test)
    (purge-instances 'message-test)
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
  (test-query (:record-count 4 :fixture forum-data)
    (select (o)
      (from o)
      (where (typep o 'message-test)))))

(deftest test/query/select/short ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (o)
      (from (o message-test)))))

(deftest test/query/select/all ()
  (test-query (:fixture forum-data)
    (select (o)
      (from o))))

(deftest test/query/select/symbol-enum ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (s)
      (from (s spam-test))
      (where (eq (spam-type-of s) 'viagra)))))

(deftest test/query/select/nested-ands ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (object)
      (from object)
      (where (and t (and t (typep object 'message-test)))))))

(deftest test/query/select/<= ()
  (test-query (:select-count 1 :record-count 0 :fixture forum-data)
    (select (s)
      (from (s spam-test))
      (where (<= 50 (score-of s) 100))))
  
  (test-query (:select-count 1 :record-count 1)
    (select (s)
      (from (s spam-test))
      (where (not (<= 50 (score-of s) 100))))))

(deftest test/query/select/multiple-objects ()
  (test-query (:record-count (* 2 4 2) :fixture forum-data)
    (select (user message topic)
      (from (user user-test) (message message-test) (topic topic-test)))))

(deftest test/query/select/with-lexical-variables-1 ()
  (test-query (:record-count 1 :fixture forum-data)
    (let ((user-name "user1"))
      (select (user)
        (from (user user-test))
        (where (equal (name-of user) user-name))))))

(deftest test/query/select/with-lexical-variables-2 ()
  (test-query (:record-count 4 :fixture forum-data)
    (let ((class (find-class 'message-test)))
      (select (o)
        (from o)
        (where (typep o (class-name class)))))))

(deftest test/query/select/with-dynamic-variables ()
  (with-fixture forum-data
      (let ((user (with-transaction (select-first-matching-instance user-test))))
        (test-query (:select-count nil :record-count 2)
          (revive-instance user)          ; for eq
          (select (o)
            (from (o topic-test))
            (where (eq (owner-of o) user)))))))

(deftest test/query/select/with-literal-object ()
  (with-fixture forum-data
      (bind ((user (with-transaction (select-first-matching-instance user-test))))
        (test-query (:select-count nil :record-count 2)
          (execute-query
           (make-query
            `(select (o)
              (from (o topic-test))
              (where (eq (owner-of o) (first-arg ,user o))))))))))

(deftest test/query/select/polimorph-association-end ()
  (test-query (:select-count (+ 2 1) :record-count 2 :fixture forum-data)
    (let ((topic (select-first-matching-instance topic-test)))
      (select (o)
        (from (o ad-test))
        (where (eq (topic-of o) topic))))))

(deftest test/query/select/slot ()
  (test-query (:record-count 2 :fixture forum-data)
    (select ((name-of user))
      (from (user user-test)))))

(deftest test/query/select/simple-association ()
  (test-query (:select-count nil :record-count 2 :fixture forum-data)
    (select ((owner-of topic))
      (from (topic topic-test)))))

(deftest test/query/select/or ()
  (test-query (:record-count (+ 4 2) :fixture forum-data)
    (select (o)
      (from o)
      (where (or (typep o 'message-test) (typep o 'topic-test))))))

(deftest test/query/select/builder ()
  (test-query (:record-count 4 :fixture forum-data)
    (let ((query (make-query nil)))
      (add-query-variable query 'm)
      (add-assert query '(typep m 'message-test))
      (add-collect query 'm)
      (execute-query query))))

(deftest test/query/select/join-1 ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (message)
      (from (message message-test))
      (where (equal (title-of (topic-of message)) "topic1")))))

(deftest test/query/select/join-n/collect-child ()
  (test-query (:record-count 4 :fixture forum-data)
    (select (message)
      (from (topic topic-test) (message message-test))
      (where (and
              (equal (title-of topic) "topic1")
              (member message (messages-of topic)))))))

(deftest test/query/select/join-n/collect-parent ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test) (message message-test))
      (where (and
              (equal (title-of topic) "topic1")
              (member message (messages-of topic))
              (equal (subject-of message) "subject1"))))))

(deftest test/query/select/join-n-in-collect ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (select ((messages-of topic))
      (from (topic topic-test))
      (where (equal (title-of topic) "topic1")))))

#|
(deftest test/query/select/general ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (select ((topic topic-test))
      (when (equal (title-of topic) "topic1")
        (collect topic)))))
|#

(deftest test/query/select/with-lisp-filter ()
  (test-query (:select-count nil :record-count 1 :fixture forum-data)
    (let ((predicate (lambda (message) (equal (subject-of message) "subject1"))))
      (select (message)
        (from (message message-test))
        (where (funcall predicate message))))))

(deftest test/query/select/cnf ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (or (not (equal (title-of (topic-of m)) "topic1"))
                 (and (equal (subject-of m) "subject1")
                      (equal (content-of m) "content1")))))))

(deftest test/query/select/typep ()
  (test-query (:record-count 2 :fixture forum-data)
    (let ((subject "subject1"))
      (select (m)
        (from (m message-test))
        (where (and (or (typep m 'ad-test)
                         (equal (subject-of m) subject))
                     (not (typep m 'spam-test))))))))

(deftest test/query/select/macro ()
  (define-query-macro s-of (message)
    `(subject-of ,message))
  (test-query (:record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (equal (s-of m) "subject1")))))

(deftest test/query/select/count ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test))
      (where (>= (length (messages-of topic)) 2)))))

(deftest test/query/select/member-1 ()
  (test-query (:select-count 2 :record-count 3 :fixture forum-data)
    (let ((messages (cdr (prc::select-instances message-test))))
      (select (m)
        (from (m message-test))
        (where (member m messages))))))

(deftest test/query/select/member-2 ()
  (test-query (:record-count 1 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (member (subject-of m) '("subject1" "no-such-subject") :test 'equal)))))

(deftest test/query/select/member-3 ()
  (test-query (:select-count 0 :record-count 0 :fixture forum-data)
    (select (m)
      (from (m message-test))
      (where (member (subject-of m) nil)))))

(deftest test/query/select/member-4 ()
  (test-query (:select-count 1 :record-count 0 :fixture forum-data)
    (let ((topics (prc::select-instances topic-test)))
      (execute-query
       (make-query
        `(select (m)
          (from (m message-test))
          (where (member m ',topics))))))))

(deftest test/query/select/member-5 ()
  (test-query (:select-count 3 :record-count 1 :fixture forum-data)
    (let ((list (append (select-instances topic-test) (select-instances spam-test))))
      (execute-query
       (make-query
        `(select (m)
          (from (m message-test))
          (where (member m ',list))))))))

(deftest test/query/select/lisp-expr ()
  (test-query (:record-count 1 :fixture forum-data)
    (let ((num "1"))
      (select (topic)
        (from (topic topic-test))
        (where (equal (title-of topic) (strcat "topic" num)))))))

(deftest test/query/select/regex-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test))
      (where (scan "t.*picx?1" (title-of topic))))))

(deftest test/query/select/regex-2 ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test))
      (where (scan "picx?1" (title-of topic) :start 2 :end 6)))))

(deftest test/query/select/like ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (topic)
      (from (topic topic-test))
      (where (like (title-of topic) "t%pi_1")))))

(deftest test/query/select/strcat ()
  (test-query (:select-count 1 :record-count 1 :fixture forum-data)
    (select (tc m u)
      (from (tc topic-test) (m message-test) (u user-test))
      (where (equal (strcat (title-of tc) ":" (subject-of m) ":" (name-of u))
                    "topic1:subject1:user1")))))

(deftest test/query/select/non-persistent-slot ()
  (test-query (:record-count 2 :fixture forum-data) ; TODO: should select 1
    (select (u)
      (from (u user-test))
      (where (> (age-of u) 30)))))

(deftest test/query/select/max ()
  (test-query (:record-count 1 :fixture forum-data)
    (select ((max (score-of s)))
      (from (s spam-test)))))
