(ns grammate.pprint
  "A pretty printer that prints Clojure data as Texmate flavoured property lists"
  (:require [fipp.printer :as printer :refer (pprint-document)]))

; Format specification can be found here: http://manual.macromates.com/en/appendix#property_list_format.html

; {  name = 'name';
;   begin = '';
;   end = '';
; },

; {  name = 'name';
;   match = 'pattern';
; },

; {  include = ''; },

; beginCaptures = {
;   1 = { name = ''; };
; };

(defprotocol IPretty
  (-pretty [x ctx]))

(extend-protocol IPretty

  nil
  (-pretty [x ctx]
    [:text "nil"])

  java.lang.Object
  (-pretty [x ctx]
    [:text (pr-str x)])
    
  java.lang.String
  (-pretty [x ctx]
    [:text (str "'" x "'")])

  clojure.lang.Keyword
  (-pretty [x ctx]
    [:text (subs (str x) 1)])
    
  clojure.lang.IPersistentVector
  (-pretty [v ctx]
    [:group "(" [:align (interpose [:span "," :line] (map #(-pretty % ctx) v))] :line ")"])
    
  clojure.lang.IPersistentMap
  (-pretty [m ctx]
    (let [kvps (for [[k v] m]
                 [:span (-pretty k ctx) " = " (-pretty v ctx) ";"])]
      [:group "{" [:align (interpose [:span :line] kvps) :line]  "}"])))
      
(defn ppd [doc]
  (pprint-document doc {:width 10}))
  
(defn pp [x]
  (ppd (-pretty x nil)))
  