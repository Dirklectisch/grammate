(ns grammate.edn-test
  (:require [clojure.test :refer :all]
            [grammate.edn :as edn :refer [patterns]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

; Helpers

(defn match? [re s]
  "Returns true if regex re matches the complete string s"
  (if-let [ma (re-matches re s)]
    (if (string? ma)
      (= ma s)
      (= (first ma) s))
    false))

(defn get-re [k]
  (-> patterns k :match))
  
; Unit tests

(deftest nil-test
  (is (match? (get-re :nil) "nil"))
  (is (not (match? (get-re :nil) "nil-test"))))
  
(deftest keyword-test
  (is (not (match? (get-re :keyword) ":/")))
  (is (not (match? (get-re :keyword) "::foo"))))
  
; Generative tests

; test.check documentation here: https://github.com/clojure/test.check/blob/master/README.md

(defn gen-join-str [& gs]
  "Returns a generators that joins the string results of several others"
  (gen/fmap clojure.string/join (apply gen/tuple gs)))

(def str-generators
  (let [gen-int-str (gen/one-of [(gen/fmap str gen/int)
                                 (gen/fmap #(str "+" %) gen/pos-int)])
        gen-exp-str (gen-join-str (gen/elements ["e" "E"])
                                  (gen/elements ["" "+" "-"])
                                  gen/pos-int)
        gen-frac-str (gen/fmap #(str (float %)) gen/ratio)]
    { :boolean (gen/fmap str gen/boolean)                          
      :character (gen/frequency [[4 (gen/elements ["\\newline" "\\return" "\\space" "\\tab"])]
                                 [96 (gen/fmap #(str "\\" %) gen/char-ascii)]])
      :integer (gen/one-of [gen-int-str
                            (gen/fmap #(str % "N") gen-int-str)])
      :floating-point-number (gen/frequency [[25 (gen/fmap #(str % "M") gen-int-str)]
                                             [75 (gen-join-str (gen/one-of [gen-frac-str
                                                                            (gen-join-str gen-int-str gen-exp-str)
                                                                            (gen-join-str gen-frac-str gen-exp-str)])
                                                                (gen/elements ["M" ""]))]])
      :symbol (gen/fmap str gen/symbol)
      :keyword (gen/fmap str gen/keyword)}))
    
(defn other-str-gen [k]
  "Combines all string generators except k"
  (->> (filter (fn [p] (not= (first p) k)) str-generators)
       (mapv (fn [p] (last p)))
       (gen/one-of)))
       
(defn prop-match [k]
  (prop/for-all [s (str-generators k)]
    (match? (get-re k) s)))

(defn prop-not-match [k]
  (prop/for-all [s (other-str-gen k)]
    ((complement match?) (get-re k) s)))

(deftest test-all-matches
  (are [k x] (= ((tc/quick-check x (prop-match k)) :result) true)
    :boolean 10
    :character 1000
    :integer 100
    :floating-point-number 100
    :symbol 100
    :keyword 100))
    
(deftest test-all-non-matches
  (are [k] (= ((tc/quick-check 100 (prop-not-match k)) :result) true)
    :boolean
    :integer
    :floating-point-number
    :symbol
    :keyword))
