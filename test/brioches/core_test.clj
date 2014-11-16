(ns brioches.core-test
  "Just a taste of property-based testing in Clojure."
  (:require [clojure.test :refer :all]
            [brioches.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics    :as combo]))

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


;; knapsack problem
;; http://www.learningclojure.com/2013/09/the-knapsack-problem-another-classic.html
(defn knapsack-brute-force
  "Given a collection of things `ts' and a `budget' value, finds the
  best candidate, if any, among all the possibile subsets of `ts'
  within the given `budget'.

  The best candidate is the collection with maximum `rate', using
  `cost' as a tie breaker.

  We use the brute-force approach as the reference implementation."
  [ts budget]
  (let [find-candidate
        (fn [acc candidate]
          (if (or (< (rate acc) (rate candidate))
                  (and (= (rate acc) (rate candidate))
                       (> (cost acc) (cost candidate))))
            candidate
            acc))
        lte-max-budget?
        (fn [sub-set] (<= (cost sub-set) budget))]
    (reduce find-candidate
            []
            (filter lte-max-budget?
                    (combo/subsets ts)))))

(def thing-gen
  "`Thing' generator. We generate `Thing's always having a cost
  greater than zero."
  (gen/fmap (partial apply ->Thing)
            (gen/tuple (gen/such-that #(> % 0) gen/pos-int) gen/pos-int)))

(defspec knapsack-output-is-subset-of-input
  iterations
  (prop/for-all [t (gen/vector thing-gen)
                 w-max gen/pos-int]
                (every? (fn [item]
                          (not (nil? (some #{item} t))))
                        (greedy-value t w-max))))

(defspec knapsack-weight-is-<-knapsack-weight
  iterations
  (prop/for-all [t (gen/vector thing-gen)
                 w-max gen/pos-int]
                (>= w-max (reduce (fn [acc item]
                                    (+ acc (:cost item)))
                                  0
                                  (greedy-value t w-max)))))

(defspec knapsack-result-is-optimal
  ;; This fails showing the value of the property-based tests. This
  ;; value of `ts', with a budget of 1, fails the test because
  ;; `greedy-value' tries the item with highest `:value' first, which
  ;; is already overbudget.
  ;;
  ;; [{:cost 1, :value 1} {:cost 2, :value 2}]
  10
  (prop/for-all [ts (gen/vector thing-gen)
                 budget gen/pos-int]
                (= (rate (greedy-value ts budget))
                   (rate (knapsack-brute-force ts budget)))))
