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

(defspec assoc-closure?
  iterations
  (prop/for-all [v1 (gen/vector gen/int)
                 v2 (gen/vector gen/int)]
                (sequential? (concat v1 v2))))
