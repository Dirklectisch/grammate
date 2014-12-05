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
  
(deftest keyword-test
  (is (not (match? (get-re :keyword) ":/")))
  (is (not (match? (get-re :keyword) "::foo"))))
  
; Generative tests

; test.check documentation here: https://github.com/clojure/test.check/blob/master/README.md

(defn gen-join-str [& gs]
  "Returns a generator that joins the string results of several others"
  (gen/fmap clojure.string/join (apply gen/tuple gs)))

(def str-generators
  (let [gen-int-str (gen/one-of [(gen/fmap str gen/int)
                                 (gen/fmap #(str "+" %) gen/pos-int)])
        gen-exp-str (gen-join-str (gen/elements ["e" "E"])
                                  (gen/elements ["" "+" "-"])
                                  gen/pos-int)
        gen-frac-str (gen/fmap #(str (float %)) gen/ratio)]
    { :nil (gen/return "nil")
      :boolean (gen/fmap str gen/boolean)                          
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

(defn malformed-str-gen [k]
  "Returns a generator for a string of type k enclosed by two strings of another type.
   Example: niltrue-99 for type :boolean. Only generates strings that do not match completely."
   (let [others (other-str-gen k)]
     (gen/such-that 
       (fn [s] ((complement match?) (get-re k) s))
       (gen-join-str others (str-generators k) others))))

(defn within-coll-str-gen [k]
  (let [this-gen (str-generators k)]
    (gen/one-of [(gen/fmap #(str "[" % "]") this-gen)
                 (gen/fmap #(str "(" % ")") this-gen)
                 (gen/fmap #(str "#{" % "}") this-gen)])))
   
(defn prop-match [k]
  (prop/for-all [s (str-generators k)]
    (match? (get-re k) s)))

(defn prop-not-match [k]
  (prop/for-all [s (other-str-gen k)]
    ((complement match?) (get-re k) s)))
    
(defn prop-not-part-of [k]
  (prop/for-all [s (malformed-str-gen k)]
    (if (re-find (get-re k) s) false true)))

(defn prop-is-part-of [k]
  (prop/for-all [s (within-coll-str-gen k)]
    (= 1 (count (re-seq (get-re k) s)))))

(deftest test-all-matches
  (are [k x] (= ((tc/quick-check x (prop-match k)) :result) true)
    :nil 1
    :boolean 10
    :character 1000
    :integer 100
    :floating-point-number 100
    :symbol 100
    :keyword 100))
    
(deftest test-all-non-matches
  (are [k] (= ((tc/quick-check 100 (prop-not-match k)) :result) true)
    :nil
    :boolean
    :integer
    :floating-point-number
    :symbol
    :keyword))
    
(deftest test-malformed-boundries
  (are [k] (= ((tc/quick-check 1000 (prop-is-part-of k)) :result) true)
    :nil
    :boolean
    :character
    :integer
    :floating-point-number
    :symbol
    :keyword))    

(deftest test-colletion-boundries
  (are [k] (= ((tc/quick-check 100 (prop-not-part-of k)) :result) true)
    :nil
    :boolean
    :character
    :integer
    :floating-point-number
    :symbol
    :keyword))    
