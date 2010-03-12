;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;;
;;;; Steps to prepare your database for free text search with hu.dwim.perec using PostgreSQL
;;;;
;;;; 1. Install postgresql-contrib module using your package manager (this will provide pg_trgm)
;;;;    sudo aptitude install postgresql-contrib
;;;; 2. Install pg_trgm package into your database
;;;;    sudo su - postgres
;;;;    psql -d <database-name> -f /usr/share/postgresql/8.4/contrib/pg_trgm.sql
;;;; 3. Install tsearch package into your database
;;;;    sudo su - postgres
;;;;    psql -d <database-name> -f /usr/share/postgresql/8.4/contrib/tsearch2.sql
;;;; 4. Rebuild the text search database every now and then
;;;;    (rebuild-text-search-index)
;;;; 5. Do text searches
;;;;    (text-search-instances ...)

;;;;;;;
;;; text-search-instances

(def (function e) text-search-instances (text &key limit threshold)
  (set-text-search-threshold threshold)
  (bind ((limit-clause (if limit
                         (format nil "LIMIT ~A" limit)
                         ""))
         (records (execute (format nil "
SELECT _inner_select.*, _rank * _similarity AS _rank_similarity
FROM (SELECT _oid, _text_search_dictionary._word, ts_rank_cd(_tsv, plainto_tsquery(_text_search_dictionary._word)) AS _rank, _text_search_dictionary._similarity
      FROM _text_search_object, (SELECT _word, similarity(_word, '~A') _similarity from _text_search_dictionary where _word % '~A' ORDER BY _similarity DESC, _word) AS _text_search_dictionary
      WHERE _tsv @@ plainto_tsquery(_text_search_dictionary._word)) AS _inner_select
ORDER BY _rank_similarity DESC
~A;" text text limit-clause))))
    (map 'list (lambda (record)
                 (list (load-instance (first-elt record) :skip-existence-check #t)
                       (elt record 1)
                       (elt record 2)
                       (elt record 3)))
         records)))

(def (function e) text-search-dictionary (text &key limit threshold)
  (set-text-search-threshold threshold)
  (bind ((limit-clause (if limit
                           (format nil "LIMIT ~A" limit)
                           "")))
    (execute (format nil "
SELECT _word, similarity(_word, '~A') _similarity
FROM _text_search_dictionary
WHERE _word % '~A'
ORDER BY _similarity DESC, _word
~A;" text text limit-clause))))

(def function set-text-search-threshold (threshold)
  (when threshold
    (execute (format nil "SELECT set_limit(~A);" threshold))))

;;;;;;;
;;; rebuild-text-search-index

(def (function e) rebuild-text-search-index (&key (configuration "english"))
  (check-type *database* hu.dwim.rdbms.postgresql:postgresql)
  (with-transaction
    (ignore-errors (execute "DROP TABLE _text_search_object")))
  (with-transaction
    (execute (format nil "CREATE TABLE _text_search_object AS ~{~A~^ UNION ALL ~}"
                     (iter (for (name class) :in-hashtable *persistent-classes*)
                           (for direct-instances-prefetch-view = (direct-instances-prefetch-view-of class))
                           (when direct-instances-prefetch-view
                             (when-bind column-names
                                 (iter slot-loop
                                       (for slot :in (prefetched-slots-of class))
                                       (when (subtypep (canonical-type-of slot) 'string)
                                         (iter column-loop
                                               (for column :in (columns-of slot))
                                               (when (typep (hu.dwim.rdbms::type-of column) 'hu.dwim.rdbms::sql-string-type)
                                                 (in slot-loop (collect (hu.dwim.rdbms::name-of column)))))))
                               (collect (format nil "SELECT _oid AS _oid, to_tsvector('~A', ~{coalesce(~A,'')~^ || ~}) AS _tsv FROM ~A"
                                                configuration column-names (name-of direct-instances-prefetch-view))))))))
    (execute "CREATE INDEX _text_search_object_idx ON _text_search_object USING gin(_tsv)"))
  (with-transaction
    (ignore-errors (execute "DROP TABLE _text_search_dictionary")))
  (with-transaction
    (execute "CREATE TABLE _text_search_dictionary AS SELECT word AS _word FROM stat('SELECT _tsv FROM _text_search_object')")
    (execute "CREATE INDEX _text_search_dictionary_ids ON _text_search_dictionary USING gist(_word gist_trgm_ops);")))
