;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.perec.documentation
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.perec
        :hu.dwim.perec.test
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui))

#|
-*- outline -*-

* hu.dwim.perec: an Object Relational Mapping (ORM) from CLOS to RDBMS

** Environment for the test suite (using the PostgreSQL backend)

Create the database and user:

$ createdb perec-test
$ createuser -P perec-test

Give "test123" as password.

** Running the tests using the PostgreSQL backend

(asdf:test-system :hu.dwim.perec.postgresql)

or use 'asdf:load-op to merely load it.

** Some simple examples

(defpclass foo ()
  ((bar :type integer-16)))

(with-transaction
  (make-instance 'foo :bar 1))

(with-transaction
  (select (i)
    (from (i foo))
    (where (= 1 (bar-of i)))))

** Loading the system without the test environment

(load-system :hu.dwim.perec)

For some details on how to set up the PostgreSQL specific configuration see the hu.dwim.perec.test.postgresql.asd file.
|#

#| ;; TODO:
Columns
=======
Status Priority Description 

Status
======
- todo
+ done

Priorities
==========
0 - highest
1 - high
2 - normal
3 - low
4 - lowest

Entries
=======
use M-x sort-lines to keep the order

-0 subclassing a dimensional class does not make the subclass dimensional if there are no dimensional slots in the subclass
-1 change-class, add-class, remove-class, ensure-class, desure-class (support anonymous persistent classes)
-1 add delayed updates using mark dirty-slots and provide a flush function
-1 revise perec persistent-p/debug-persistent-p 
-2 add asserts in update/select statements for the number of affected rows in store
-2 prefetching 1-1 association does not always work due to not knowing that both ends are persistent (why do we assume broken references there) and thus executing a query upon persistent-p
-3 finish sqlite support (issues: there are no sequences; no reflection on the column level; no date, time, timestamp data type)
-3 what about purge-instance making references broken?
-4 eliminate barely used (by increasing duplicates) system and package dependencies
-4 eliminate superfluous duplicates (which are covered by other systems)
|#











#|
Testbed default parameters (port is set to PostgreSQL default port):
   host: localhost
   port: 5432
   database: perec-test
   user-name: perec-test
   password: test123

To install postgresql:
   sudo apt-get install postgresql

To setup the test database:
   sudo su - postgres
   createdb perec-test
   createuser -d -r -l -P perec-test
   ;; type in 'test123' for password

In emacs do: 
   ;; the swank server uses utf-8, so
   M-S-: (setq slime-net-coding-system 'utf-8-unix)
   M-x slime-connect
   ;; 'localhost' and default port 4005 should be ok

To test hu.dwim.perec:
   (in-package :hu.dwim.perec.test) ; this is the default when you connect
   (retest) ; should print a lot of dots and stuff and takes a while

To play around:
   ;; to turn on logging of SQL statements in SLIME
   (start-sql-recording)
   ;; to create a persistent class
   (defpclass* test ()
     ((name :type (text 20))
      (age :type integer-32)
      (flag :type boolean)))
   ;; to make an instance 
   ;; this should automatically create/update the tables needed for the class
   ;; note: if you have run the test suite, this might execute several queries
   ;;       to check all persistent classes present in your lisp image
   (defvar p
     (with-transaction
        (make-instance 'test :name \"Hello\" :age 42 :flag t)))
   ;; to reuse the instance in another transaction
   (with-transaction
     (with-revived-instance p
       (describe p)))
   ;; to query instances of the class just defined
   (with-transaction
     (select (instance)
       (from (instance test))
       (where (and (equal (name-of instance) \"Hello\")
                   (< (age-of instance) 100)))
       (order-by :descending (age-of instance))))
   ;; queries are polimorph by default (this should actually return all persistent instances)
   ;; use macroexpand to see how it compiles down to straight SQL
   (with-transaction
     (select (:compile-at-macroexpand t) (instance)
       (from (instance persistent-object))))
   ;; see the tests in the repository at http://common-lisp.net/cgi-bin/darcsweb/darcsweb.cgi?r=hu.dwim.perec-hu.dwim.perec;a=tree;f=/test
   ;; see a somewhat more complicated example at: http://common-lisp.net/project/hu.dwim.perec/shop.html
   ;; and also check the showcase on the website at http://common-lisp.net/project/hu.dwim.perec/showcase.html

To read more about the project:
   http://common-lisp.net/project/hu.dwim.perec

There is some form of documentation at:)
   http://common-lisp.net/project/hu.dwim.perec/documentation/index.html

Suggestions, bug reports are welcomed at:
   hu.dwim.perec-devel@common-lisp.net
|#
