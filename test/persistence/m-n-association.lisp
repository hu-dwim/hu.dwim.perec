(in-package :cl-perec-test)

(defsuite* test/association/m-n :in test/association)

(defpclass* user-test ()
  ())
   
(defpclass* role-test ()
  ())

(defassociation*
  ((:class user-test :slot roles :type (set role-test))
   (:class role-test :slot users :type (set user-test))))

(defmacro with-user-and-role-transaction (&body body)
  `(with-transaction
    (bind ((user (make-instance 'user-test))
           (role (make-instance 'role-test)))
      ,@body)))

(deftest test/association/m-n/initial-value/1 ()
  (with-user-and-role-transaction
    (is (null (roles-of user)))
    (is (null (users-of role)))
    (is (= 0 (size (roles-of* user))))
    (is (= 0 (size (users-of* role))))))

(deftest test/association/m-n/initial-value/2 ()
  (with-transaction
    (bind ((role (make-instance 'role-test))
           (user (make-instance 'user-test :roles (list role))))
      (is (equal (roles-of user) (list role))))))

(deftest test/association/m-n/store-value/1 ()
  (with-user-and-role-transaction
    (setf (roles-of user) (list role))
    (is (equal (list role) (roles-of user)))))

(deftest test/association/m-n/referential-integrity/1 ()
  (with-user-and-role-transaction
    (setf (roles-of user) (list role))
    (bind ((users (users-of* role)))
      (is (= 1 (size users)))
      (is (eq user (first (list-of users)))))))

(deftest test/association/m-n/collection/1 ()
  (with-user-and-role-transaction
    (bind ((roles (roles-of* user)))
      (insert-item roles role)
      (is (= 1 (size roles)))
      (is (equal (list role) (roles-of user)))
      (delete-item roles role)
      (is (= 0 (size roles)))
      (is (null (roles-of user))))))

(deftest test/association/m-n/collection/2 ()
  (with-user-and-role-transaction
    (bind ((roles (roles-of* user))
           (other-role (make-instance 'role-test)))
      (insert-item roles role)
      (insert-item roles other-role)
      (delete-item roles role)
      (is (= 1 (size roles)))
      (is (equal (list other-role) (list-of roles))))))
