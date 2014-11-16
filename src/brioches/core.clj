(ns brioches.core
  "Standard generators and generators built upon Prismatic’s Schema.

  - https://github.com/Prismatic/schema/issues/103

  - https://gist.github.com/cd4025963581ec3e0a12"
  (:require [schema.core :as s]
            [clojure.test.check.generators :as gen]))


;; schema.core interop
(defprotocol Generatable
  (generate [x] "return a generator for x"))

(defn g-by [f & args]
  (gen/fmap
   (partial apply f)
   (apply gen/tuple args)))

(defn g-apply-by [f args]
  (gen/fmap f (apply gen/tuple args)))

(defn g-or [& args]
  (gen/one-of args))

(defn optional-entry [[k v]]
  (g-or
   (gen/return {})
   (g-by hash-map (generate k) (generate v))))

(extend-type schema.core.RequiredKey
  Generatable
  (generate [x]
    (gen/return (:k x))))

(extend-type schema.core.OptionalKey
  Generatable
  (generate [x]
    (gen/return (:k x))))

(extend-type clojure.lang.APersistentVector
  Generatable
  (generate [x]
    (let [[ones [repeated]] (split-with #(instance? schema.core.One %) x)
          [required optional] (split-with (comp not :optional?) ones)]
      (g-by
       concat
       (g-or
        (apply gen/tuple (map generate required))
        (apply gen/tuple (map generate (concat required optional))))
       (if repeated
         (gen/vector (generate repeated))
         (gen/return []))))))

(extend-type clojure.lang.APersistentMap
  Generatable
  (generate [x]
    (let [[required other]
          (split-with
           (fn [[k v]]
             (or (keyword? k)
                 (instance? schema.core.RequiredKey k))) x)

          [optional [repeated]]
          (split-with
           (fn [[k v]] (instance? schema.core.OptionalKey k)) other)]
      (g-by
       merge
       (g-apply-by (partial into {}) (map optional-entry optional))
       (if repeated
         (->> repeated (map generate) (apply gen/map))
         (gen/return {}))
       (g-apply-by
        (partial into {})
        (for [entry required]
          (apply gen/tuple (map generate entry))))))))

(extend-type Object
  Generatable
  (generate [x]
    (cond
     ;; base types
     (= x double)
     (gen/bind gen/int
               (fn [x] (gen/return (* x (.nextDouble (gen/random))))))
     (= x float)
     (gen/bind gen/int
               (fn [x] (gen/return (float (* x (.nextFloat (gen/random)))))))
     (= x long) gen/int
     (= x boolean) gen/boolean
     ;; longs, ints...?
     (= x s/Str) gen/string-ascii
     (= x s/Bool) gen/boolean
     (= x s/Num) (gen/one-of [gen/int gen/ratio])
     (= x s/Int) gen/int
     (= x s/Keyword) gen/keyword
     ;; Plausible(?), non-namespaced symbols
     (= x s/Symbol)
     (gen/such-that #(re-find #"^[^0-9]\w" %)
                    gen/string-alpha-numeric)
     ;; use `gen/int' to offset dates. Good enough?
     (= x s/Inst)
     (gen/bind gen/int
               (fn [x] (gen/return (java.util.Date. (* x 1000000000000)))))
     (= x s/Uuid)
     (gen/bind gen/bytes
               (fn [x] (gen/return (java.util.UUID/nameUUIDFromBytes x)))))))


(extend-protocol Generatable
  ;; Hack. Notes below
  java.util.regex.Pattern
  (generate [this] (gen/such-that #(re-find this %) gen/string-ascii 1000))
  clojure.lang.Keyword
  (generate [x] (gen/return x))
  schema.core.AnythingSchema
  (generate [_] gen/any)
  schema.core.NamedSchema
  (generate [x] (generate (:schema x)))
  schema.core.ConditionalSchema
  (generate [x]
    (gen/one-of (mapv (comp generate peek) (:preds-and-schemas x))))
  schema.core.Maybe
  (generate [x] (gen/one-of [(gen/return nil)
                             (generate (:schema x))]))
  schema.core.EqSchema
  (generate [x] (gen/return (:v x)))
  schema.core.EnumSchema
  (generate [x] (gen/elements (vec (:vs x))))
  schema.core.One
  (generate [x] (generate (:schema x)))
  schema.core.Either
  (generate [x] (gen/one-of (map generate (:schemas x))))
  schema.core.Both
  (generate [x]
    ;; Hack. Notes below
    (let [{predicates true generators false}
          (group-by (comp  nil? generate) (:schemas x))]
      (gen/such-that #(every? true? ((apply juxt (map :p? predicates)) %))
                     (gen/one-of (map generate generators))
                     1000)))
  schema.core.RequiredKey
  (generate [x] (gen/return (:k x)))
  schema.core.OptionalKey (generate [x] (gen/return (:k x))))


;; schema.core examples

;; scalars
(def gen-whatever (gen/sample (generate s/Any)))
(def gen-doubles (gen/sample (generate double)))
(def gen-floats (gen/sample (generate float)))
(def gen-longs (gen/sample (generate long)))
(def gen-booleans (gen/sample (generate boolean)))
(def gen-bools (gen/sample (generate s/Bool)))
(def gen-strings (gen/sample (generate s/Str)))
(def gen-nums (gen/sample (generate s/Num)))
(def gen-ints (gen/sample (generate s/Int)))
(def gen-keywords (gen/sample (generate s/Keyword)))
(def gen-symbols (gen/sample (generate s/Symbol)))
(def gen-timestamps (gen/sample (generate s/Inst)))
(def gen-uuids (gen/sample (generate s/Uuid)))
;; some hacks with regexp. Too fragile, not a good idea
(def gen-with-regexp (gen/sample (generate #"^\w+$")))
(def gen-constant-keyword (gen/sample (generate :k)))
(def gen-maybe-int (gen/sample (generate (s/maybe s/Int))))
(def gen-constant-int-by-predicate (gen/sample (generate (s/eq 10))))
(def gen-from-collection  (gen/sample (generate (s/enum :a :b :c :d))))
(def gen-from-one (gen/sample (generate (s/one s/Int "int"))))
(def gen-with-either (gen/sample (generate (s/either s/Int s/Keyword))))
(def gen-with-both (gen/sample (generate (s/both (s/pred #(< % 0)) s/Int))))
(def gen-negative-3-mult (gen/sample (generate (s/both (s/pred #(< % 0))
                                                       (s/pred #(= (mod % 3) 0))
                                                       s/Int))))
;; collections
(def gen-sequence (gen/sample (generate [s/Int])))
;; if `:foo' use a number, if `:bar' use a string
(def gen-associative (gen/sample (generate {:this s/Int
                                            :that s/Uuid
                                            :those {:a s/Inst
                                                    :b double}})))
(def gen-associative-with-conditional
  (gen/sample (generate (s/conditional
                         #(= (:type %) :foo) {:type (s/eq :foo) :baz s/Num}
                         #(= (:type %) :bar) {:type (s/eq :bar) :baz s/Str}))))
;; combinations, compositions, and so on
(def OddLong (s/both long (s/pred odd?)))
(def EvenLong (s/both long (s/pred even?)))
(def Pk {:int-value OddLong})
(gen/sample (generate Pk))

(def gen-broken (gen/sample (generate (s/both
                                       (s/pred
                                        (fn equal-keys? [m]
                                          (every? (fn [[k v]] (= k v)) m)) 'equal-keys?)
                                       {s/Keyword s/Keyword}))))


;; A rather contrived example with sum. The purpose was to break it in
;; a way not detectable by the stated properties. We were unable to do
;; it, but it remains to be seen if the properties set is
;; necessary and sufficient.
(defn sum
  ([acc xs]
     (if (seq xs)
       (recur (+ (first xs) acc) (rest xs))
       acc))
  ([xs] (sum 0 xs)))


;; The Knapsack Problem
;; http://www.learningclojure.com/2013/09/the-knapsack-problem-another-classic.html
;; The rationale of this choice was to try to find a problem with some
;; hard to verify but reasonably easy to write properties.

;; Everything has a cost and a value.
(defrecord Thing [cost value])

(defn rate
  "Return the total value of the collection of `Thing's `ts'."
  [ts]
  (reduce (fn [acc item] (+ acc (:value item))) 0 ts))

(defn cost
  "Return the total cost of the collection of `Thing's `ts'."
  [ts]
  (reduce (fn [acc item] (+ acc (:cost item))) 0 ts))

(defn best-evaluation
  "Finds the subsequence of `sorted-things' with the maximum value
  within `budget'.

  `sorted-things' should be sorted according the value that we want to
  maximize. In this context, it could be `:value', `:cost' or some
  kind of ratio between the two.

  1. Create all the (progressively longer) subsequences of
  `sorted-things'

  2. Take the created subsequences whose total cost is within
  budget.

  3. Return the one the maximum value."
  [sorted-things budget]
  (->> sorted-things
       (reductions conj '())              ; 1
       (take-while #(<= (cost %) budget)) ; 2
       last))                             ; 3

(defn greedy-value
  "Returns the subset of `ts' with the `best-evaluation' within
  budget. The criterium used is to maximize `:value'."
  [ts budget]
  (best-evaluation (sort-by :value > ts) budget))
