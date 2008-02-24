;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define the classes for the shop

(defpclass* basket ()
  ((created-at (transaction-timestamp) :type timestamp)
   (ordered #f :type boolean))
  (:documentation "Holds a list of product, quantity pairs"))

(defpclass* product ()
  ((name :type (text 30) :unique #t)
   (unit-price :type number))
  (:abstract #t)
  (:documentation "Serves as base class for products"))

(defpclass* products-in-basket ()
  ((quantity :type integer-16))
  (:documentation "Specifies the quantity of a product in a basket"))

(defassociation*
  ((:class basket :slot products-in-basket :type (set products-in-basket))
   (:class products-in-basket :slot basket :type basket)))

(defassociation*
  ((:class product :slot products-in-basket :type (set products-in-basket))
   (:class products-in-basket :slot product :type product)))

(defpclass* computer (product)
  ((kind :type (member :desktop :notebook))
   (memory :type integer-32)))

(defpclass* bycicle (product)
  ((size :type integer-16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions require a transaction context, use with-transaction

(defun purge-shop ()
  "Purges all data from the shop"
  (purge-instances 'product) ;; this is polymorph
  (purge-instances 'basket)
  (purge-instances 'products-in-basket))

(defun create-products ()
  "Creates some products which can be bought"
  (list
   (make-instance 'computer
                  :name "Apple"
                  :kind :desktop
                  :unit-price 1200
                  :memory 1024)
   (make-instance 'computer
                  :name "Orange"
                  :kind :notebook
                  :unit-price 1400
                  :memory 2048)
   (make-instance 'bycicle
                  :name "Csengi"
                  :unit-price 400
                  :size 26)
   (make-instance 'bycicle
                  :name "Nandi"
                  :unit-price 500
                  :size 28)))

(defun find-product (name)
  (first
   (select (p)
     (from (p product))
     (where (equal (name-of p) name))
     (limit 1))))

(defun create-baskets ()
  "Creates hypotetical baskets with products"
  (bind ((b1 (make-instance 'basket :ordered #t))
         (b2 (make-instance 'basket))
         (b3 (make-instance 'basket :ordered #t)))
    (make-instance 'products-in-basket
                   :basket b1
                   :product (find-product "Apple")
                   :quantity 1)
    (make-instance 'products-in-basket
                   :basket b1
                   :product (find-product "Csengi")
                   :quantity 2)
    (make-instance 'products-in-basket
                   :basket b2
                   :product (find-product "Nandi")
                   :quantity 1)
    (make-instance 'products-in-basket
                   :basket b3
                   :product (find-product "Orange")
                   :quantity 3)
    (list b1 b2 b3)))

(defun select-ordered-baskets (created-before)
  "Selects the baskets which have been ordered and created before the provided timestamp.
Returns a list of basket and total price pairs.

This query compiles into the following SQL either at compile time or
at runtime based on the :compile-at-macroexpand parameter. The compiled
query is always cached, so subsequent calls reuse the result. Use macroexpand
when the parameter is set to #t or trace cl-perec::compile-query when it is set
to #f to see how the query compiler compiles down parts to static SQL and how
it leaves other parts in lisp.

SELECT _pib._basket_id, SUM((_pib._quantity * _product3355._unit_price))
FROM _product _product3355, _basket _basket3354, _products_in_basket _pib
WHERE ((_product3355._id = _pib._product_id) AND
       (_basket3354._id = _pib._basket_id) AND
        _basket3354._ordered AND
       ((_basket3354._created_at = $1::TIMESTAMP WITH TIME ZONE)))
GROUP BY _pib._basket_id"
  (select ((basket-of pib)
           (sum (* (quantity-of pib)
                   (unit-price-of (product-of pib)))))
    (from (pib products-in-basket))
    (where (and (ordered-p (basket-of pib))
                (local-time<= (created-at-of (basket-of pib)) created-before)))
    (group-by (basket-of pib))))

(deftest test/shop/1 ()
  (with-transaction
    (purge-shop)
    (create-products)
    (create-baskets)
    (bind ((result (select-ordered-baskets (transaction-timestamp))))
      (is (= 2 (length result)))
      (is (= 2000 (second (first result))))
      (is (= 4200 (second (second result))))))
  (with-transaction
    (dolist (basket
              (select-instances (b basket)
                (where (not (ordered-p b)))))
      (setf (ordered-p basket) #t))
    (bind ((result (select-ordered-baskets (transaction-timestamp))))
      (is (= 3 (length result)))
      result)))
