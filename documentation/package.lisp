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

(asdf:test-system :hu.dwim.perec.test.postgresql)

or use 'asdf:load-op to merely load it (including the environment setup).

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
