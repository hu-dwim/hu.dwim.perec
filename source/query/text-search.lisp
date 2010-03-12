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
;;;; 4. Rebuild the text search index every now and then
;;;;    (rebuild-text-search-index)
;;;; 5. Do text searches
;;;;    (text-search-instances ...)
;;;;
;;;; TODO: FIXME: use query variable binding

;;;;;;;
;;; rebuild-text-search-index

;;; TODO: include the localized class names from the class precedence list
(def (function e) rebuild-text-search-index (&key (configuration "english") (class-name-provider #'class-name))
  (check-type *database* hu.dwim.rdbms.postgresql:postgresql)
  (with-transaction
    (ignore-errors (execute "DROP TABLE _text_search_object")))
  (with-transaction
    (execute (format nil "CREATE TABLE _text_search_object AS ~{~A~^ UNION ALL ~}"
                     (iter (for (name class) :in-hashtable *persistent-classes*)
                           (for class-names = (mapcar 'string (mapcar class-name-provider (class-precedence-list class))))
                           (for direct-instances-prefetch-view = (direct-instances-prefetch-view-of class))
                           (when direct-instances-prefetch-view
                             (when-bind column-names
                                 (iter slot-loop
                                       (for slot :in (prefetched-slots-of class))
                                       (when (subtypep (canonical-type-of slot) '(or unbound null string))
                                         (iter column-loop
                                               (for column :in (columns-of slot))
                                               (when (typep (hu.dwim.rdbms::type-of column) 'hu.dwim.rdbms::sql-string-type)
                                                 (in slot-loop (collect (hu.dwim.rdbms::name-of column)))))))
                               (collect (format nil "SELECT _oid AS _oid, to_tsvector('~A', '~{~A~^ ~}' || ~{coalesce(~A,'')~^ || ~}) AS _tsv FROM ~A"
                                                configuration class-names column-names (name-of direct-instances-prefetch-view))))))))
    (execute "CREATE INDEX _text_search_object_idx ON _text_search_object USING gin(_tsv)"))
  (with-transaction
    (ignore-errors (execute "DROP TABLE _text_search_dictionary")))
  (with-transaction
    (execute "CREATE TABLE _text_search_dictionary AS SELECT word AS _word FROM stat('SELECT _tsv FROM _text_search_object')")
    (execute "CREATE INDEX _text_search_dictionary_ids ON _text_search_dictionary USING gist(_word gist_trgm_ops);")))

;;;;;;;
;;; text-search-instances

(def (function e) text-search-dictionary (text &key limit threshold)
  (set-text-search-threshold threshold)
  (execute (make-text-search-dictionary-query text :limit limit)))

(def (function e) text-search-instances (text &key limit threshold)
  ;; TODO: support AND/OR/NOT using to_tsquery instead of plainto_tsquery
  ;; TODO: need to make a parser for that
  (set-text-search-threshold threshold)
  (bind ((words (split-sequence:split-sequence #\Space text))
         (table-names (iter (for index :from 0)
                            (for word :in words)
                            (for table-name = (format nil "_text_search_word~A" index))
                            (execute (format nil "CREATE TEMPORARY TABLE ~A AS ~A" table-name (make-text-search-dictionary-query word)))
                            (collect table-name)))
         (word-column-names (iter (for table-name :in table-names)
                                  (collect (format nil "~A._word" table-name))))
         (similarity-column-names (iter (for table-name :in table-names)
                                        (collect (format nil "~A._similarity" table-name))))
         (records (progn
                    (execute (format nil "
CREATE TEMPORARY TABLE _text_search_text AS
SELECT ~{~A~^ || ' ' || ~} AS _text, ~{~A~^ * ~} AS _similarity
FROM ~{~A~^, ~}
ORDER BY _similarity" word-column-names similarity-column-names table-names))
                    (execute (format nil "
SELECT _inner_select.*, _rank * _similarity AS _rank_similarity
FROM (SELECT _oid, _text_search_text._text, ts_rank_cd(_tsv, plainto_tsquery(_text_search_text._text)) AS _rank, _text_search_text._similarity
      FROM _text_search_object, _text_search_text
      WHERE _tsv @@ plainto_tsquery(_text_search_text._text)) AS _inner_select
ORDER BY _rank_similarity DESC
~A;" (make-limit-clause limit))))))
    (foreach 'drop-table table-names)
    (drop-table "_text_search_text")
    (map 'list [load-instance (first-elt !1) :skip-existence-check #t] records)))

(def function make-text-search-dictionary-query (text &key limit)
  (format nil "
SELECT _word, similarity(_word, '~A') AS _similarity
FROM _text_search_dictionary
WHERE _word % '~A'
ORDER BY _similarity DESC, _word
~A;" text text (make-limit-clause limit)))

(def function set-text-search-threshold (threshold)
  (when threshold
    (execute (format nil "SELECT set_limit(~A);" threshold))))

(def function make-limit-clause (limit)
  (if limit
      (format nil "LIMIT ~A" limit)
      ""))
