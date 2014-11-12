(ns grammate.edn
  (:require [clojure.string :refer [join]]))

(defn re-join [coll]
  (re-pattern (join coll)))

(def exprs 
  {:boolean #"true|false"
   :label (re-join [ "(?:" #"[\-+][:#.*+!\-_?$%&=A-Za-z]"
                     #"|[.*+!\-_?$%&=A-Za-z]" ")"
                     #"[:#.*+!\-_?$%&=A-Za-z0-9]*"])})
                     
(def patterns
  {:symbol {:name "variable.other.symbol.edn"
            :match (re-join [ (join ["(" (exprs :label) "/" ")?"])
                              (join ["(" (exprs :label) ")?"])])}})
