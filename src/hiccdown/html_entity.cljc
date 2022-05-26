(ns hiccdown.html-entity
  #?(:cljs (:require-macros [hiccdown.html-entity :refer [inline-entities]]))
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

#?(:clj
   (defmacro inline-entities [uri]
     (-> (slurp uri)
         json/read-json)))

(def entities-raw
  (inline-entities "https://html.spec.whatwg.org/entities.json"))

(def entities
  (->> entities-raw
       (into []
             (keep (fn [[k v]]
                     (let [n (name k)]
                       (when (str/ends-with? n ";")
                         {}
                         [k (:characters v)])))))))
