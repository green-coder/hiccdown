(ns hiccdown.spec-test
  #?(:cljs (:require-macros [hiccdown.spec-test :refer [inline-spec-tests]]))
  (:require [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.data.json :as json])
            [clojure.set :as set]
            [hiccdown.common-mark.core :as cm]
            [hiccdown.core :as hd]
            [taipei-404.html :as html]
            [mate.core :as mate]))

#?(:clj
   (defmacro inline-spec-tests [uri]
     (-> (slurp uri)
         json/read-json)))

(def examples-raw
  (inline-spec-tests
    "test/hiccdown/spec.json"
    #_"https://spec.commonmark.org/0.30/spec.json"))

(def examples
  (->> examples-raw
       (mapv (fn [example]
               (-> example
                   (dissoc :start_line :end_line)
                   (set/rename-keys {:example :id})
                   (assoc :hiccup (->> (html/html->hiccup (:html example))
                                       vec)))))))

#_
(-> (mate/group-by :section :id examples)
    (update-vals (fn [ids]
                   [(count ids)
                    (first ids)
                    (last ids)])))

(defn select-examples-fn
  ([section] (select-examples-fn section 0 1000))
  ([section id] (select-examples-fn section id id))
  ([section min-id max-id]
   (fn [example]
     (and (= (:section example) section)
          (<= min-id (:id example) max-id)))))

(defn select-examples-id [id]
  (comp #{id} :id))

#_
(next
  (hd/minify-spaced-hiccup
    (hd/ast->spaced-hiccup
      (hd/markdown->ast
        (:markdown
          (examples 333))))))

(deftest specification-examples-new-test
  (doseq [example (->> examples
                       (filter (some-fn
                                 ;;(select-examples-fn "Link reference definitions" 192)
                                 ;;(select-examples-fn "Link reference definitions" 192 218)

                                 (select-examples-fn "Tabs")
                                 ;;(select-examples-fn "Backslash escapes") ;; xxx ?
                                 ;;(select-examples-fn "Entity and numeric character references")
                                 (select-examples-fn "Precedence")
                                 (select-examples-fn "Thematic breaks")
                                 (select-examples-fn "ATX headings")
                                 (select-examples-fn "Setext headings")
                                 (select-examples-fn "Indented code blocks")
                                 (select-examples-fn "Fenced code blocks")

                                 ;;(select-examples-fn "HTML blocks" 148) ;; xxx inline html
                                 (select-examples-fn "HTML blocks" 149 167)
                                 ;;(select-examples-fn "HTML blocks" 168) ;; xxx inline html
                                 (select-examples-fn "HTML blocks" 169 186)
                                 ;;(select-examples-fn "HTML blocks" 187) ;; xxx href
                                 (select-examples-fn "HTML blocks" 188 191)

                                 ;;(select-examples-fn "Link reference definitions")

                                 (select-examples-fn "Paragraphs")
                                 (select-examples-fn "Blank lines")
                                 (select-examples-fn "Block quotes")
                                 (select-examples-fn "List items")

                                 (select-examples-fn "Lists" 304 316)
                                 ;;(select-examples-fn "Lists" 317) ;; xxx html link ref
                                 (select-examples-fn "Lists" 318 326)

                                 (select-examples-fn "Inlines")

                                 (select-examples-fn "Code spans" 328 342)
                                 ;;(select-examples-fn "Code spans" 343) ;; xxx unsafe html chars
                                 ;;(select-examples-fn "Code spans" 344) ;; xxx undefined html nodes
                                 ;;(select-examples-fn "Code spans" 345) ;; xxx unsafe html chars
                                 ;;(select-examples-fn "Code spans" 346) ;; xxx unsafe html chars
                                 (select-examples-fn "Code spans" 347 349)

                                 (select-examples-fn "Emphasis and strong emphasis" 350 402)
                                 ;;(select-examples-fn "Emphasis and strong emphasis" 403) ;; xxx href
                                 (select-examples-fn "Emphasis and strong emphasis" 404 417)
                                 ;;(select-examples-fn "Emphasis and strong emphasis" 418) ;; xxx href
                                 (select-examples-fn "Emphasis and strong emphasis" 419 420)
                                 ;;(select-examples-fn "Emphasis and strong emphasis" 421) ;; xxx href
                                 (select-examples-fn "Emphasis and strong emphasis" 422 431)
                                 ;;(select-examples-fn "Emphasis and strong emphasis" 432) ;; xxx href
                                 (select-examples-fn "Emphasis and strong emphasis" 433 471)
                                 ;;(select-examples-fn "Emphasis and strong emphasis" 472 476) ;; xxx
                                 ;;(select-examples-fn "Emphasis and strong emphasis" 477 478) ;; xxx `a`*
                                 (select-examples-fn "Emphasis and strong emphasis" 479 478) ;; xxx

                                 ;;(select-examples-fn "Links")
                                 ;;(select-examples-fn "Images")
                                 ;;(select-examples-fn "Autolinks")
                                 ;;(select-examples-fn "Raw HTML")

                                 ;;(select-examples-fn "Hard line breaks") ;; xxx html
                                 (select-examples-fn "Soft line breaks")
                                 (select-examples-fn "Textual content")
                                 ,)))]
    (testing (str "Example " (:id example))
      #_
      (is (= (:hiccup example)
             (-> (:markdown example)
                 hd/markdown->ast
                 cm/ast->spaced-hiccup
                 cm/minify-spaced-hiccup
                 next)))
      (is (= (:html example)
             (-> (:markdown example)
                 hd/markdown->ast
                 cm/ast->spaced-hiccup
                 cm/minify-spaced-hiccup
                 next
                 (->> (mapv cm/hiccup->html))
                 (->> (apply str))))))))
