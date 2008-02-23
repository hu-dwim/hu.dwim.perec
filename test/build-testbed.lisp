;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(load (merge-pathnames "workspace/environment/environment.lisp" (user-homedir-pathname)))

(push (merge-pathnames "workspace/_slime-cvs" (user-homedir-pathname)) asdf:*central-registry*)
(asdf:oos 'asdf:load-op :swank)
(asdf:oos 'asdf:load-op :cl-rdbms)

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

  (asdf:oos 'asdf:load-op :cl-perec-test.postgresql)

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
                     (let* ((connection-specification (rdbms::connection-specification-of rdbms::*database*))
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
                       (setf (rdbms::connection-specification-of rdbms::*database*)
                             `(:host ,host :port ,port :database ,database :user-name ,user-name :password ,password)))))
                 (format *debug-io*
"Usage:
   perec-testbed [--host <host>] [--port <port>] [--user-name <user-name>] [--password <password>] [--name name]

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

To test cl-perec:
   (in-package :cl-perec-test) ; this is the default when you connect
   (retest) ; should print a lot of dots and stuff and takes a while

To play around:
   (start-sql-recording)
   ;; or a simple example
   (defpclass* test ()
     ((name :type (text 20))
      (age :type integer-32)
      (flag :type boolean)))
   ;; make an instance (should automatically update table)
   (with-transaction
     (make-instance 'test :name \"Hello\" :age 42 :flag t))
   ;; reuse the instance
   (with-transaction
     (with-revived-instance *
       (describe *)))
   ;; query instances
   (with-transaction
     (select (instance)
       (from (instance test))
       (where (and (equal (name-of instance) \"Hello\")
                   (< (age-of instance) 100)))
       (order-by :descending (age-of instance))))
   ;; queries are polimorph by default, use macroexpand to see how it compiles down to straight SQL
   (with-transaction
     (select (:compile-at-macroexpand t) (instance)
       (from (instance persistent-object))))
   ;; see the tests in the repository at http://common-lisp.net/cgi-bin/darcsweb/darcsweb.cgi?r=cl-perec-cl-perec-helium;a=tree;f=/test
   ;; also check the showcase on the website at http://common-lisp.net/project/cl-perec/showcase.html

To read more:
   http://common-lisp.net/project/cl-perec

Some form of documentation :)
   http://common-lisp.net/project/cl-perec/documentation/index.html

PostgreSQL connection specification:
   ~S
" (rdbms::connection-specification-of rdbms::*database*))
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
