(ns grammate.edn-test
  (:require [clojure.test :refer :all]
            [grammate.edn :as edn :refer [patterns]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))
            
(defn match? [re s]
  "Returns true if regex re matches the complete string s"
  (if-let [ma (re-matches re s)]
    (if (string? ma)
      (= ma s)
      (= (first ma) s))
    false))

(defn get-re [k]
  (-> patterns k :match))
  
(def str-generators
  { :boolean (gen/fmap str gen/boolean)
    :symbol (gen/fmap str gen/symbol)
    :keyword (gen/fmap str gen/keyword) })
    
(defn other-str-gen [key]
  "Combines all string generators except k"
  (->> (filter (fn [p] (not= (first p) key)) str-generators)
       (mapv (fn [p] (last p)))
       (gen/one-of)))
       
(defn prop-match [k]
  (prop/for-all [s (str-generators k)]
    (match? (get-re k) s)))

(defn prop-not-match [k]
  (prop/for-all [s (other-str-gen k)]
    ((complement match?) (get-re k) s)))

(deftest test-all-matches
  (are [k] (= ((tc/quick-check 100 (prop-match k)) :result) true)
    :boolean
    :symbol
    :keyword))
    
(deftest test-all-non-matches
  (are [k] (= ((tc/quick-check 100 (prop-not-match k)) :result) true)
    :boolean
    :symbol
    :keyword))

(deftest nil-test
  (is (match? (get-re :nil) "nil"))
  (is (not (match? (get-re :nil) "nil-test"))))      
