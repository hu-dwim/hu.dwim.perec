;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: #<VALUES-HAVING-VALIDITY {NIL, 96, 82, 17, NIL} {1006F31581}> and test value: #<VALUES-HAVING-VALIDITY {NIL, 96, 29, 82, 17, NIL} {1006F32521}> are different
in the slot TIME-DEPENDENT-SLOT of #<TESITES-COMPLEX-TEST :persistent #t 253771 {1006EFA941}>
with t 2008-02-19T15:26:52.274905Z and with validity range 1000-01-01T00:00:00Z -> 3000-01-01T00:00:00Z
with 104 history entries: "
         (BIND
             ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0) (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-TEST)))))
           (break "START")
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1919-12-16T16:47:41Z"
                 (WITH-VALIDITY-RANGE "1915-04-11T04:57:41Z" "1943-08-14T14:58:11Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 10)))))
           (break "2")
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1919-12-16T16:47:41Z"
                 (WITH-VALIDITY-RANGE "1915-04-11T04:57:41Z" "1943-08-14T14:58:11Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 29)))))
           (break "3")
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1907-11-05T13:45:42Z"
                 (WITH-VALIDITY-RANGE "1928-11-24T05:14:12Z" "1956-07-04T11:45:59Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 66)))))
           (break "KESZ")
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2008-02-19T15:26:52.274905Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TIME-DEPENDENT-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TIME-DEPENDENT-SLOT PERSISTENT-VALUE
                                                        TEST-VALUE))))))))
