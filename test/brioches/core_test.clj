(ns brioches.core-test
  "Just a taste of property-based testing in Clojure."
  (:require [clojure.test :refer :all]
            [brioches.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def iterations 100)


;; Let’s test good old `assoc' with properties
(defspec assoc-left-identity
  iterations
  (prop/for-all [v (gen/vector gen/int)]
                (= (concat [] v) v)))

(defspec assoc-right-identity
  iterations
  (prop/for-all [v (gen/vector gen/int)]
                (= (concat v []) v)))

(defspec assoc-associativity
  iterations
  (prop/for-all [v1 (gen/vector gen/int)
                 v2 (gen/vector gen/int)
                 v3 (gen/vector gen/int)]
                (= (concat (concat v1 v2) v3)
                   (concat v1 (concat v2 v3)))))

(defspec assoc-and-cons-invariant
  iterations
  (prop/for-all [n gen/int
                 v1 (gen/vector gen/int)
                 v2 (gen/vector gen/int)]
                (= (concat (cons n v1) v2)
                   (cons n (concat v1 v2)))))

(defspec assoc-closure?
  iterations
  (prop/for-all [v1 (gen/vector gen/int)
                 v2 (gen/vector gen/int)]
                (sequential? (concat v1 v2))))


;; `sum' of a collection. Is this set of properties the minimal (i.e.
;; necessary and sufficient) one?
(defspec sum-base-case
  ;; The sum of a singleton vector is equal to the single item.
  iterations
  (prop/for-all [n gen/int]
                (= (sum [n]) n)))

(defspec sum-monotonicity
  ;; Adding a strictly positive integer to a collection should
  ;; strictly increase the sum of the extended collection.
  iterations
  (prop/for-all [v (gen/vector gen/int)
                 delta (gen/such-that #(> % 0) gen/pos-int)]
                (> (sum (cons delta v)) (sum v))))

(defspec sum-identity
  ;; `0' is the identity value for `sum'.
  iterations
  (prop/for-all [v (gen/vector gen/int)
                 identity (gen/return 0)]
                (= (sum (cons identity v)) (sum v))))
