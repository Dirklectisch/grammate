(ns grammate.edn-test
  (:require [clojure.test :refer :all]
            [grammate.edn :as edn :refer [patterns]]))
            
(defn match? [re s]
  "Returns true if regex re matches the complete string s"
  (if-let [ma (re-matches re s)]
    (if (string? ma)
      (= ma s)
      (= (first ma) s))
    false))

(defn get-re [k]
  (-> patterns k :match))
  
(def examples
  { :boolean ["true" "false"] 
    :symbol ["hG0MsbMQwCP06" "BT69jey_IC.jM0qJNNFNHw.iBnT3_JGa7.I5E2Wn04Od.MqMN5_Ugj+P/_e_73jNGVPi"]})
    
(deftest nil-test
  (is (match? (get-re :nil) "nil"))
  (is (not (match? (get-re :nil) "nil-test"))))  

(deftest boolean-test
  (is (match?  (get-re :boolean) "true"))
  (is (match?  (get-re :boolean) "false"))
  (is (not (match? (get-re :boolean) "foo"))))
    
(deftest symbol-test
  (is (match?  (get-re :symbol) "hG0MsbMQwCP06"))
  (is (match?  (get-re :symbol) "BT69jey_IC.jM0qJNNFNHw.iBnT3_JGa7.I5E2Wn04Od.MqMN5_Ugj+P/_e_73jNGVPi"))
  (is (not (match? (get-re :symbol) "#foo"))))
    