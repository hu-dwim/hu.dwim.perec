;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;
;;;; Generated tests

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: #<VALUES-HAVING-VALIDITY {NIL, 96, 82, 17, NIL} {1006F31581}> and test value: #<VALUES-HAVING-VALIDITY {NIL, 96, 29, 82, 17, NIL} {1006F32521}> are different
in the slot TIME-DEPENDENT-SLOT of #<TESITES-COMPLEX-TEST :persistent #t 253771 {1006EFA941}>
with t 2008-02-19T15:26:52.274905Z and with validity range 1000-01-01T00:00:00Z -> 3000-01-01T00:00:00Z
with 104 history entries: "
         (BIND ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0) (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-TEST)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1919-12-16T16:47:41Z"
                 (WITH-VALIDITY-RANGE "1915-04-11T04:57:41Z" "1943-08-14T14:58:11Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 10)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1919-12-16T16:47:41Z"
                 (WITH-VALIDITY-RANGE "1915-04-11T04:57:41Z" "1943-08-14T14:58:11Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 29)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1907-11-05T13:45:42Z"
                 (WITH-VALIDITY-RANGE "1928-11-24T05:14:12Z" "1956-07-04T11:45:59Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 66)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2008-02-19T15:26:52.274905Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TIME-DEPENDENT-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TIME-DEPENDENT-SLOT PERSISTENT-VALUE
                                                        TEST-VALUE))))))))

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: #<VALUES-HAVING-VALIDITY {NIL, 67, NIL, 6, NIL} {10075C2511}> and test value: #<VALUES-HAVING-VALIDITY {NIL, 67, 6, NIL, NIL} {10075C2E11}> are different
in the slot TIME-DEPENDENT-SLOT of #<TESITES-COMPLEX-TEST :persistent #t 254216 {10075837F1}>
with t 2008-02-19T17:32:58.699811Z and with validity range 1000-01-01T00:00:00Z -> 3000-01-01T00:00:00Z
with 8 history entries: "
         (BIND ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0) (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-TEST)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1980-02-12T07:57:50Z"
                 (WITH-VALIDITY-RANGE "1920-12-19T03:02:15Z" "1960-02-10T04:03:08Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 67)))
               (WITH-T "1902-08-03T23:47:50Z"
                 (WITH-VALIDITY-RANGE "2011-12-31T11:43:35Z" "2170-01-06T08:04:05Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 51)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2008-02-19T17:32:58.699811Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TIME-DEPENDENT-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TIME-DEPENDENT-SLOT PERSISTENT-VALUE
                                                        TEST-VALUE))))))))

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: #<VALUES-HAVING-VALIDITY {NIL, 95, 78, 53, NIL} {10076C1781}> and test value: #<VALUES-HAVING-VALIDITY {NIL, 95, 53, NIL} {10076C1D91}> are different
in the slot TIME-DEPENDENT-SLOT of #<TESITES-COMPLEX-TEST :persistent #t 254404 {1007393471}>
with t 2008-02-19T17:46:44.264928Z and with validity range 1000-01-01T00:00:00Z -> 3000-01-01T00:00:00Z
with 13 history entries: "
         (BIND ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0) (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-TEST)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1902-05-04T04:03:32Z"
                 (WITH-VALIDITY-RANGE "1981-07-11T10:23:11Z" "2058-11-04T20:50:23Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 53)))
               (WITH-T "1909-02-19T10:54:56Z"
                 (WITH-VALIDITY-RANGE "1923-05-21T02:22:57Z" "2041-01-22T21:41:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 5)))
               (WITH-T "1909-02-19T10:54:56Z"
                 (WITH-VALIDITY-RANGE "1923-05-21T02:22:57Z" "2041-01-22T21:41:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 95)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2008-02-19T17:46:44.264928Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TIME-DEPENDENT-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TIME-DEPENDENT-SLOT PERSISTENT-VALUE
                                                        TEST-VALUE))))))))

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: 63 and test value: NIL are different
in the slot TEMPORAL-SLOT of #<TESITES-COMPLEX-TEST :persistent #t 258220 {10068C1FB1}>
with t 1940-03-24T03:28:29Z and with validity range 1000-01-01T00:00:00Z -> 1000-01-01T00:00:00.001000Z
with 20 history entries: "
         (BIND ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0) (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-TEST)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2008-02-20T16:56:28.940703Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) NIL)))
               (WITH-T "2008-02-20T16:56:28.940703Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) NIL)))
               (WITH-T "2008-02-20T16:56:28.940703Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) NIL)))
               (WITH-T "2022-09-10T10:00:29Z"
                 (WITH-VALIDITY-RANGE "1981-03-31T15:14:22Z" "2049-09-16T09:55:12Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 84)))
               (WITH-T "2022-09-10T10:00:29Z"
                 (WITH-VALIDITY-RANGE "1981-03-31T15:14:22Z" "2049-09-16T09:55:12Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 59)))
               (WITH-T "2030-07-20T16:35:59Z"
                 (WITH-VALIDITY-RANGE "1909-05-18T22:37:06Z" "2022-01-15T05:30:43Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 39)))
               (WITH-T "2030-07-20T16:35:59Z"
                 (WITH-VALIDITY-RANGE "1909-05-18T22:37:06Z" "2022-01-15T05:30:43Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 40)))
               (WITH-T "2030-07-20T16:35:59Z"
                 (WITH-VALIDITY-RANGE "1909-05-18T22:37:06Z" "2022-01-15T05:30:43Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 22)))
               (WITH-T "2030-07-20T16:35:59Z"
                 (WITH-VALIDITY-RANGE "1909-05-18T22:37:06Z" "2022-01-15T05:30:43Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 65)))
               (WITH-T "1940-03-24T03:28:29Z"
                 (WITH-VALIDITY-RANGE "1974-05-19T03:29:53Z" "1995-02-18T10:01:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 65)))
               (WITH-T "1940-03-24T03:28:29Z"
                 (WITH-VALIDITY-RANGE "1974-05-19T03:29:53Z" "1995-02-18T10:01:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 40)))
               (WITH-T "1940-03-24T03:28:29Z"
                 (WITH-VALIDITY-RANGE "1974-05-19T03:29:53Z" "1995-02-18T10:01:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 93)))
               (WITH-T "1940-03-24T03:28:29Z"
                 (WITH-VALIDITY-RANGE "1974-05-19T03:29:53Z" "1995-02-18T10:01:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 63)))
               (WITH-T "2035-03-24T02:19:51Z"
                 (WITH-VALIDITY-RANGE "2017-05-14T02:14:02Z" "2038-02-22T12:56:28Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 11)))
               (WITH-T "2035-03-24T02:19:51Z"
                 (WITH-VALIDITY-RANGE "2017-05-14T02:14:02Z" "2038-02-22T12:56:28Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 27)))
               (WITH-T "2035-03-24T02:19:51Z"
                 (WITH-VALIDITY-RANGE "2017-05-14T02:14:02Z" "2038-02-22T12:56:28Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 12)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1940-03-24T03:28:29Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "1000-01-01T00:00:00.001000Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TEMPORAL-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TEMPORAL-SLOT PERSISTENT-VALUE
                                                        TEST-VALUE))))))))

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: #<VALUES-HAVING-VALIDITY {NIL, 91} {1007DB3611}> and test value: #<VALUES-HAVING-VALIDITY {NIL, 91, 68} {1007DB3C01}> are different
in the slot TEMPORAL-AND-TIME-DEPENDENT-SLOT of #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1007D40891}>
with t 2014-07-22T03:43:39Z and with validity range 1000-01-01T00:00:00Z -> 1908-03-30T04:32:48.001000Z
with 29 history entries: (#S(HISTORY-ENTRY
                             :STEP 29
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093D49C1}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 1983-10-01T03:10:59Z
                             :VALIDITY-START 2058-04-03T21:15:45Z
                             :VALIDITY-END 2087-11-03T15:35:53Z
                             :VALUE 80)
                          #S(HISTORY-ENTRY
                             :STEP 28
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093D49C1}>
                             :SLOT-NAME TEMPORAL-AND-TIME-DEPENDENT-SLOT
                             :T-VALUE 1983-10-01T03:10:59Z
                             :VALIDITY-START 2058-04-03T21:15:45Z
                             :VALIDITY-END 2087-11-03T15:35:53Z
                             :VALUE 8)
                          #S(HISTORY-ENTRY
                             :STEP 27
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093D49C1}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 1983-10-01T03:10:59Z
                             :VALIDITY-START 2058-04-03T21:15:45Z
                             :VALIDITY-END 2087-11-03T15:35:53Z
                             :VALUE 68)
                          #S(HISTORY-ENTRY
                             :STEP 26
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093D49C1}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 1983-10-01T03:10:59Z
                             :VALIDITY-START 2058-04-03T21:15:45Z
                             :VALIDITY-END 2087-11-03T15:35:53Z
                             :VALUE 93)
                          #S(HISTORY-ENTRY
                             :STEP 25
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093D49C1}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 1983-10-01T03:10:59Z
                             :VALIDITY-START 2058-04-03T21:15:45Z
                             :VALIDITY-END 2087-11-03T15:35:53Z
                             :VALUE 21)
                          #S(HISTORY-ENTRY
                             :STEP 24
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308621}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 2013-02-22T18:22:17Z
                             :VALIDITY-START 1901-02-20T09:23:02Z
                             :VALIDITY-END 2013-05-04T07:54:56Z
                             :VALUE 87)
                          #S(HISTORY-ENTRY
                             :STEP 23
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308621}>
                             :SLOT-NAME SLOT
                             :T-VALUE 2013-02-22T18:22:17Z
                             :VALIDITY-START 1901-02-20T09:23:02Z
                             :VALIDITY-END 2013-05-04T07:54:56Z
                             :VALUE 61)
                          #S(HISTORY-ENTRY
                             :STEP 22
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308621}>
                             :SLOT-NAME TEMPORAL-AND-TIME-DEPENDENT-SLOT
                             :T-VALUE 2013-02-22T18:22:17Z
                             :VALIDITY-START 1901-02-20T09:23:02Z
                             :VALIDITY-END 2013-05-04T07:54:56Z
                             :VALUE 91)
                          #S(HISTORY-ENTRY
                             :STEP 21
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308621}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 2013-02-22T18:22:17Z
                             :VALIDITY-START 1901-02-20T09:23:02Z
                             :VALIDITY-END 2013-05-04T07:54:56Z
                             :VALUE 90)
                          #S(HISTORY-ENTRY
                             :STEP 20
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308621}>
                             :SLOT-NAME TEMPORAL-AND-TIME-DEPENDENT-SLOT
                             :T-VALUE 2013-02-22T18:22:17Z
                             :VALIDITY-START 1901-02-20T09:23:02Z
                             :VALIDITY-END 2013-05-04T07:54:56Z
                             :VALUE 10)
                          #S(HISTORY-ENTRY
                             :STEP 19
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308681}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 1991-07-31T16:21:39Z
                             :VALIDITY-START 1988-08-23T03:39:35Z
                             :VALIDITY-END 2078-06-28T03:11:04Z
                             :VALUE 4)
                          #S(HISTORY-ENTRY
                             :STEP 18
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308681}>
                             :SLOT-NAME TIME-DEPENDENT-SLOT
                             :T-VALUE 1991-07-31T16:21:39Z
                             :VALIDITY-START 1988-08-23T03:39:35Z
                             :VALIDITY-END 2078-06-28T03:11:04Z
                             :VALUE 53)
                          #S(HISTORY-ENTRY
                             :STEP 17
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308681}>
                             :SLOT-NAME TIME-DEPENDENT-SLOT
                             :T-VALUE 1991-07-31T16:21:39Z
                             :VALIDITY-START 1988-08-23T03:39:35Z
                             :VALIDITY-END 2078-06-28T03:11:04Z
                             :VALUE 63)
                          #S(HISTORY-ENTRY
                             :STEP 16
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308681}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 1991-07-31T16:21:39Z
                             :VALIDITY-START 1988-08-23T03:39:35Z
                             :VALIDITY-END 2078-06-28T03:11:04Z
                             :VALUE 7)
                          #S(HISTORY-ENTRY
                             :STEP 15
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308681}>
                             :SLOT-NAME SLOT
                             :T-VALUE 1991-07-31T16:21:39Z
                             :VALIDITY-START 1988-08-23T03:39:35Z
                             :VALIDITY-END 2078-06-28T03:11:04Z
                             :VALUE 21)
                          #S(HISTORY-ENTRY
                             :STEP 14
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093086E1}>
                             :SLOT-NAME SLOT
                             :T-VALUE 1994-05-12T05:12:28Z
                             :VALIDITY-START 1984-05-05T06:52:52Z
                             :VALIDITY-END 2010-10-30T13:37:13Z
                             :VALUE 17)
                          #S(HISTORY-ENTRY
                             :STEP 13
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093086E1}>
                             :SLOT-NAME TEMPORAL-AND-TIME-DEPENDENT-SLOT
                             :T-VALUE 1994-05-12T05:12:28Z
                             :VALIDITY-START 1984-05-05T06:52:52Z
                             :VALIDITY-END 2010-10-30T13:37:13Z
                             :VALUE 12)
                          #S(HISTORY-ENTRY
                             :STEP 12
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093086E1}>
                             :SLOT-NAME TIME-DEPENDENT-SLOT
                             :T-VALUE 1994-05-12T05:12:28Z
                             :VALIDITY-START 1984-05-05T06:52:52Z
                             :VALIDITY-END 2010-10-30T13:37:13Z
                             :VALUE 52)
                          #S(HISTORY-ENTRY
                             :STEP 11
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093086E1}>
                             :SLOT-NAME SLOT
                             :T-VALUE 1994-05-12T05:12:28Z
                             :VALIDITY-START 1984-05-05T06:52:52Z
                             :VALIDITY-END 2010-10-30T13:37:13Z
                             :VALUE 59)
                          #S(HISTORY-ENTRY
                             :STEP 10
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {10093086E1}>
                             :SLOT-NAME SLOT
                             :T-VALUE 1994-05-12T05:12:28Z
                             :VALIDITY-START 1984-05-05T06:52:52Z
                             :VALIDITY-END 2010-10-30T13:37:13Z
                             :VALUE 15)
                          #S(HISTORY-ENTRY
                             :STEP 9
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308761}>
                             :SLOT-NAME SLOT
                             :T-VALUE 2014-07-22T03:43:39Z
                             :VALIDITY-START 1908-03-30T04:32:48Z
                             :VALIDITY-END 1936-11-20T09:42:39Z
                             :VALUE 3)
                          #S(HISTORY-ENTRY
                             :STEP 8
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308761}>
                             :SLOT-NAME TIME-DEPENDENT-SLOT
                             :T-VALUE 2014-07-22T03:43:39Z
                             :VALIDITY-START 1908-03-30T04:32:48Z
                             :VALIDITY-END 1936-11-20T09:42:39Z
                             :VALUE 78)
                          #S(HISTORY-ENTRY
                             :STEP 7
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308761}>
                             :SLOT-NAME TEMPORAL-AND-TIME-DEPENDENT-SLOT
                             :T-VALUE 2014-07-22T03:43:39Z
                             :VALIDITY-START 1908-03-30T04:32:48Z
                             :VALIDITY-END 1936-11-20T09:42:39Z
                             :VALUE 68)
                          #S(HISTORY-ENTRY
                             :STEP 6
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308761}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 2014-07-22T03:43:39Z
                             :VALIDITY-START 1908-03-30T04:32:48Z
                             :VALIDITY-END 1936-11-20T09:42:39Z
                             :VALUE 60)
                          #S(HISTORY-ENTRY
                             :STEP 5
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {1009308761}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 2014-07-22T03:43:39Z
                             :VALIDITY-START 1908-03-30T04:32:48Z
                             :VALIDITY-END 1936-11-20T09:42:39Z
                             :VALUE 72)
                          #S(HISTORY-ENTRY
                             :STEP 4
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {100BBB3CC1}>
                             :SLOT-NAME SLOT
                             :T-VALUE 2008-02-20T17:08:38.385770Z
                             :VALIDITY-START 1000-01-01T00:00:00Z
                             :VALIDITY-END 3000-01-01T00:00:00Z
                             :VALUE NIL)
                          #S(HISTORY-ENTRY
                             :STEP 3
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {100BBB3CC1}>
                             :SLOT-NAME TEMPORAL-SLOT
                             :T-VALUE 2008-02-20T17:08:38.385770Z
                             :VALIDITY-START 1000-01-01T00:00:00Z
                             :VALIDITY-END 3000-01-01T00:00:00Z
                             :VALUE NIL)
                          #S(HISTORY-ENTRY
                             :STEP 2
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {100BBB3CC1}>
                             :SLOT-NAME TIME-DEPENDENT-SLOT
                             :T-VALUE 2008-02-20T17:08:38.385770Z
                             :VALIDITY-START 1000-01-01T00:00:00Z
                             :VALIDITY-END 3000-01-01T00:00:00Z
                             :VALUE NIL)
                          #S(HISTORY-ENTRY
                             :STEP 1
                             :INSTANCE #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 258430 {100BBB3CC1}>
                             :SLOT-NAME TEMPORAL-AND-TIME-DEPENDENT-SLOT
                             :T-VALUE 2008-02-20T17:08:38.385770Z
                             :VALIDITY-START 1000-01-01T00:00:00Z
                             :VALIDITY-END 3000-01-01T00:00:00Z
                             :VALUE NIL))"
         (BIND
             ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0)
              (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-INHERITANCE-TEST)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2008-02-20T17:08:38.385770Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) NIL)))
               (WITH-T "2008-02-20T17:08:38.385770Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) NIL)))
               (WITH-T "2008-02-20T17:08:38.385770Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) NIL)))
               (WITH-T "2008-02-20T17:08:38.385770Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) NIL)))
               (WITH-T "2014-07-22T03:43:39Z"
                 (WITH-VALIDITY-RANGE "1908-03-30T04:32:48Z" "1936-11-20T09:42:39Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 72)))
               (WITH-T "2014-07-22T03:43:39Z"
                 (WITH-VALIDITY-RANGE "1908-03-30T04:32:48Z" "1936-11-20T09:42:39Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 60)))
               (WITH-T "2014-07-22T03:43:39Z"
                 (WITH-VALIDITY-RANGE "1908-03-30T04:32:48Z" "1936-11-20T09:42:39Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 68)))
               (WITH-T "2014-07-22T03:43:39Z"
                 (WITH-VALIDITY-RANGE "1908-03-30T04:32:48Z" "1936-11-20T09:42:39Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 78)))
               (WITH-T "2014-07-22T03:43:39Z"
                 (WITH-VALIDITY-RANGE "1908-03-30T04:32:48Z" "1936-11-20T09:42:39Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) 3)))
               (WITH-T "1994-05-12T05:12:28Z"
                 (WITH-VALIDITY-RANGE "1984-05-05T06:52:52Z" "2010-10-30T13:37:13Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) 15)))
               (WITH-T "1994-05-12T05:12:28Z"
                 (WITH-VALIDITY-RANGE "1984-05-05T06:52:52Z" "2010-10-30T13:37:13Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) 59)))
               (WITH-T "1994-05-12T05:12:28Z"
                 (WITH-VALIDITY-RANGE "1984-05-05T06:52:52Z" "2010-10-30T13:37:13Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 52)))
               (WITH-T "1994-05-12T05:12:28Z"
                 (WITH-VALIDITY-RANGE "1984-05-05T06:52:52Z" "2010-10-30T13:37:13Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 12)))
               (WITH-T "1994-05-12T05:12:28Z"
                 (WITH-VALIDITY-RANGE "1984-05-05T06:52:52Z" "2010-10-30T13:37:13Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) 17)))
               (WITH-T "1991-07-31T16:21:39Z"
                 (WITH-VALIDITY-RANGE "1988-08-23T03:39:35Z" "2078-06-28T03:11:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) 21)))
               (WITH-T "1991-07-31T16:21:39Z"
                 (WITH-VALIDITY-RANGE "1988-08-23T03:39:35Z" "2078-06-28T03:11:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 7)))
               (WITH-T "1991-07-31T16:21:39Z"
                 (WITH-VALIDITY-RANGE "1988-08-23T03:39:35Z" "2078-06-28T03:11:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 63)))
               (WITH-T "1991-07-31T16:21:39Z"
                 (WITH-VALIDITY-RANGE "1988-08-23T03:39:35Z" "2078-06-28T03:11:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TIME-DEPENDENT-SLOT) 53)))
               (WITH-T "1991-07-31T16:21:39Z"
                 (WITH-VALIDITY-RANGE "1988-08-23T03:39:35Z" "2078-06-28T03:11:04Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 4)))
               (WITH-T "2013-02-22T18:22:17Z"
                 (WITH-VALIDITY-RANGE "1901-02-20T09:23:02Z" "2013-05-04T07:54:56Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 10)))
               (WITH-T "2013-02-22T18:22:17Z"
                 (WITH-VALIDITY-RANGE "1901-02-20T09:23:02Z" "2013-05-04T07:54:56Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 90)))
               (WITH-T "2013-02-22T18:22:17Z"
                 (WITH-VALIDITY-RANGE "1901-02-20T09:23:02Z" "2013-05-04T07:54:56Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 91)))
               (WITH-T "2013-02-22T18:22:17Z"
                 (WITH-VALIDITY-RANGE "1901-02-20T09:23:02Z" "2013-05-04T07:54:56Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'SLOT) 61)))
               (WITH-T "2013-02-22T18:22:17Z"
                 (WITH-VALIDITY-RANGE "1901-02-20T09:23:02Z" "2013-05-04T07:54:56Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 87)))
               (WITH-T "1983-10-01T03:10:59Z"
                 (WITH-VALIDITY-RANGE "2058-04-03T21:15:45Z" "2087-11-03T15:35:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 21)))
               (WITH-T "1983-10-01T03:10:59Z"
                 (WITH-VALIDITY-RANGE "2058-04-03T21:15:45Z" "2087-11-03T15:35:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 93)))
               (WITH-T "1983-10-01T03:10:59Z"
                 (WITH-VALIDITY-RANGE "2058-04-03T21:15:45Z" "2087-11-03T15:35:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 68)))
               (WITH-T "1983-10-01T03:10:59Z"
                 (WITH-VALIDITY-RANGE "2058-04-03T21:15:45Z" "2087-11-03T15:35:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 8)))
               (WITH-T "1983-10-01T03:10:59Z"
                 (WITH-VALIDITY-RANGE "2058-04-03T21:15:45Z" "2087-11-03T15:35:53Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-SLOT) 80)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "2014-07-22T03:43:39Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "1908-03-30T04:32:48.001000Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT
                                                        PERSISTENT-VALUE TEST-VALUE))))))))


;;;
;;;

(DEFTEST TEST/TESITES/COMPLEX/GENERATED NIL "Test assertion failed:

The persistent value: #<VALUES-HAVING-VALIDITY {NIL, 3} {DC64B61}> and test value: #<VALUES-HAVING-VALIDITY {NIL, 3, 48} {DC65841}> are different
in the slot TEMPORAL-AND-TIME-DEPENDENT-SLOT of #<TESITES-COMPLEX-INHERITANCE-TEST :persistent #t 13070 {DC33D51}>
with t 1978-01-16T19:18:19Z and with validity range 1000-01-01T00:00:00Z -> 2051-05-07T20:14:21.001000Z"
         (BIND
             ((*HISTORY-ENTRIES* NIL) (*HISTORY-ENTRY-COUNTER* 0)
              (INSTANCE (WITH-TRANSACTION (WITH-DEFAULT-T (MAKE-INSTANCE 'TESITES-COMPLEX-INHERITANCE-TEST)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1978-01-16T19:18:19Z"
                 (WITH-VALIDITY-RANGE "2051-05-07T20:14:21Z" "2104-05-08T11:32:44Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 48)))
               (WITH-T "1984-02-03T16:35:29Z"
                 (WITH-VALIDITY-RANGE "1978-05-07T12:27:26Z" "2118-07-02T00:56:36Z"
                   (SETF (SLOT-VALUE-AND-SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT) 21)))))
           (WITH-TRANSACTION
             (WITH-REVIVED-INSTANCE INSTANCE
               (WITH-T "1978-01-16T19:18:19Z"
                 (WITH-VALIDITY-RANGE "1000-01-01T00:00:00Z" "2051-05-07T20:14:21.001000Z"
                   (BIND
                       ((PERSISTENT-VALUE (SLOT-VALUE INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT))
                        (TEST-VALUE (SLOT-VALUE* INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT)))
                     (ASSERT-PERSISTENT-AND-TEST-VALUES INSTANCE 'TEMPORAL-AND-TIME-DEPENDENT-SLOT
                                                        PERSISTENT-VALUE TEST-VALUE))))))))