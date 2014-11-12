(ns grammate.edn-test
  (:require [clojure.test :refer :all]
            [grammate.edn :as edn :refer [exprs]]))

(deftest boolean-test
  (testing "FIXME, I fail."
    (is (re-matches (exprs :boolean) "true"))
    (is (re-matches (exprs :boolean) "false"))))
    
(deftest label-test
  (testing "FIXME, I fail."
    (is (re-matches (exprs :label) "sN_Ddl+g3Vz.WzI_QdsHHs.JKuN_t8URwz.cJCGYPxlQws.Ucq7e9TZZF6.bLH5PCDvJi"))))
    

