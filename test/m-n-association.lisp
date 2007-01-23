(in-package :cl-perec-test)

(in-suite test/association)

(defsuite test/association/m-n)

(in-suite test/association/m-n)

(defmacro with-transaction-for-user-and-role (&body body)
  `(with-transaction
    (bind ((user (make-instance 'user))
           (role (make-instance 'role)))
      ,@body)))

(defpclass* user ()
  ())
   
(defpclass* role ()
  ())

(defassociation
  ((user :0..N)
   (role :0..N)))

(deftest test/association/m-n/initial-value/1 ()
  (with-transaction-for-user-and-role
      (is (null (roles-of user)))
    (is (null (users-of role)))
    (is (= 0 (size (roles-of* user))))
    (is (= 0 (size (users-of* role))))))

(deftest test/association/m-n/initial-value/2 ()
  (with-transaction
    (bind ((role (make-instance 'role))
           (user (make-instance 'user :roles (list role))))
      (is (equal (roles-of user) (list role))))))

(deftest test/association/m-n/store-value/1 ()
  (with-transaction-for-user-and-role
      (setf! (roles-of user) (list role))
    (is (equal (list role) (roles-of user)))))

(deftest test/association/m-n/referential-integrity/1 ()
  (with-transaction-for-user-and-role
      (setf! (roles-of user) (list role))
    (bind ((users (users-of* role)))
      (is (= 1 (size users)))
      (is (eq user (first-item users))))))

(deftest test/association/m-n/collection/1 ()
  (with-transaction-for-user-and-role
      (bind ((roles (roles-of* user)))
	(insert-item roles role)
	(is (= 1 (size roles)))
        (is (equal (list role) (roles-of user)))
	(delete-item roles role)
	(is (= 0 (size roles)))
        (is (null (roles-of user))))))

(deftest test/association/m-n/collection/2 ()
  (with-transaction-for-user-and-role
      (bind ((roles (roles-of* user))
             (other-role (make-instance 'role)))
        (insert-item roles role)
        (insert-item roles other-role)
        (delete-item roles role)
        (is (= 1 (size roles)))
        (is (equal (list other-role) (list-of roles))))))
