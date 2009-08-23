;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(load (merge-pathnames "workspace/environment/environment.lisp" (user-homedir-pathname)))

(push (merge-pathnames "workspace/_slime-cvs" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:oos 'asdf:load-op :swank)
(asdf:oos 'asdf:load-op :hu.dwim.rdbms)

(defvar *file-name* "perec-testbed")

(defun create-swank-server ()
  (with-simple-restart (continue "Ok, go on without a Swank server")
    (swank:create-server :style nil :dont-close t :coding-system "utf-8-unix")))

(defun build-image ()
  (load (merge-pathnames "workspace/environment/swank-sprof.lisp" (user-homedir-pathname)))

  (when (ignore-errors
          (fdefinition (read-from-string "(setf swank:swank-print-right-margin)")))
    (eval (read-from-string "(setf (swank:swank-print-right-margin) 150
                                   swank:*globally-redirect-io* t)")))

  (asdf:oos 'asdf:load-op :hu.dwim.perec.test.postgresql)

  (if (probe-file *file-name*)
      (delete-file *file-name*))

  (sb-ext::save-lisp-and-die
   *file-name*
   :executable t
   :toplevel (lambda ()
               (with-simple-restart (quit "Ok, give up, it failed this time...")
                 (let ((arguments (subseq sb-ext:*posix-argv*
                                          ;; checking for "--end-toplevel-options" shouldn't be necessary; SBCL bug.
                                          (1+ (or (position "--end-toplevel-options" sb-ext:*posix-argv*
                                                            :test #'string=)
                                                  0)))))
                   (flet ((get-argument (name &optional has-value?)
                            "Removes and returns the argument value when found or the position if this argument has no value."
                            (let ((position (position name arguments :test #'string=)))
                              (if position
                                  (if has-value?
                                      (progn
                                        (unless (> (length arguments)
                                                   (1+ position))
                                          (error "~S requires an argument" name))
                                        (prog1 (elt arguments (1+ position))
                                          (setf (elt arguments position) nil)
                                          (setf (elt arguments (1+ position)) nil)))
                                      (progn
                                        (setf (elt arguments position) nil)
                                        t)))))
                          (fail (&optional message &rest args)
                            (when message
                              (apply #'format *error-output* message args))
                            (sb-ext:quit :unix-status 1)))
                     (let* ((connection-specification (hu.dwim.rdbms::connection-specification-of hu.dwim.rdbms::*database*))
                            (host (or (get-argument "--host" t)
                                      (getf connection-specification :host)))
                            (port (or (let ((port (get-argument "--port" t)))
                                        (when port
                                          (parse-integer port)))
                                      (getf connection-specification :port)
                                      5432))
                            (database (or (get-argument "--database" t)
                                          (getf connection-specification :database)))
                            (user-name (or (get-argument "--user-name" t)
                                           (getf connection-specification :user-name)))
                            (password (or (get-argument "--password" t)
                                          (getf connection-specification :password))))
                       (setf (hu.dwim.rdbms::connection-specification-of hu.dwim.rdbms::*database*)
                             `(:host ,host :port ,port :database ,database :user-name ,user-name :password ,password)))))
                 (format *debug-io*
"Testbed Usage:
   perec-testbed [--host <host>] [--port <port>] [--database <database>] [--user-name <user-name>] [--password <password>]

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

The current PostgreSQL connection specification is:
   ~S

To exit press Control-C.
" (hu.dwim.rdbms::connection-specification-of rdbms::*database*))
                 (labels ((signal-handler (signal code scp)
                            (declare (ignore signal code scp))
                            (format *debug-io* "SIGTERM/SIGINT was received, exiting~&")
                            (force-output *debug-io*)
                            (sb-ext:quit :recklessly-p t :unix-status -1)))
                   (sb-sys:enable-interrupt sb-unix:sigterm #'signal-handler)
                   (sb-sys:enable-interrupt sb-unix:sigint #'signal-handler)
                   (create-swank-server)))
               0)))

(build-image)
