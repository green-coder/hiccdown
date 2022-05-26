(ns hiccdown.common-mark.core
  (:require [clojure.string :as str]
            [hiccdown.core :as hd]))

(defn ast->spaced-hiccup [ast]
  (cond
    (string? ast)
    [(hd/post-process-text ast)]

    (vector? ast)
    [ast]

    (map? ast)
    (case (:type ast)
      :document
      (into [:div]
            (comp (mapcat ast->spaced-hiccup)
                  (drop-while #{"\n"}))
            (:children ast))

      :blank
      [(:text ast)]

      :block-quote
      ["\n"
       (-> [:blockquote]
           (conj "\n")
           (into (mapcat ast->spaced-hiccup) (:children ast))
           (conj "\n"))
       "\n"]

      :paragraph
      ["\n"
       (into [:p]
             (mapcat ast->spaced-hiccup)
             (:children ast))
       "\n"]

      :thematic-break
      ["\n"
       [:hr]
       "\n"]

      :soft-line-break
      ["\n"]

      :hard-line-break
      [[:br]
       "\n"]

      :header
      ["\n"
       (into [(keyword (str "h" (:depth ast)))]
             (mapcat ast->spaced-hiccup)
             (:children ast))
       "\n"]

      :list
      (let [attributes (when-some [item-num (:item-num ast)]
                         (when-not (= item-num 1)
                           {:start (str item-num)}))]
        ["\n"
         (-> [(if (:ordered ast) :ol :ul)]
             (cond-> (seq attributes) (conj attributes))
             (conj "\n")
             (into (mapcat ast->spaced-hiccup) (:children ast))
             (conj "\n"))
         "\n"])

      :list-item
      [(into [:li]
             (comp (mapcat (fn [child-ast]
                             (if (and (:tight ast)
                                      (map? child-ast)
                                      (= (:type child-ast) :paragraph))
                               (-> child-ast :children)
                               [child-ast])))
                   (mapcat ast->spaced-hiccup))
             (:children ast))
       "\n"]

      :indented-code-block
      ["\n"
       [:pre (into [:code]
                   (comp (mapcat ast->spaced-hiccup)
                         (hd/reduce-consecutive string? str))
                   (:children ast))]
       "\n"]

      :fenced-code-block
      (let [attributes (when-some [info-string (:info-string ast)]
                         {:class (str "language-" info-string)})]
        ["\n"
         [:pre (-> [:code]
                   (cond-> (seq attributes) (conj attributes))
                   (into (comp (mapcat ast->spaced-hiccup)
                               (hd/reduce-consecutive string? str))
                         (:children ast)))]
         "\n"])

      (:html-block-1
       :html-block-2
       :html-block-3
       :html-block-4
       :html-block-5
       :html-block-6
       :html-block-7)
      (-> ["\n"]
          (into (mapcat (fn [child]
                          [child "\n"]))
                (:children ast)))

      :code-span
      [[:code (:text ast)]]

      :emphasis
      [(into [:em]
             (mapcat ast->spaced-hiccup)
             (:children ast))]

      :strong-emphasis
      [(into [:strong]
             (mapcat ast->spaced-hiccup)
             (:children ast))]

      :inline-link
      [(into [:a (cond-> {:href (:destination ast)}
                         (some? (:title ast))
                         (assoc :title (:title ast)))]
             (mapcat ast->spaced-hiccup)
             (:children ast))]

      ,)))

(defn minify-spaced-hiccup [hiccup]
  (cond
    (string? hiccup) hiccup
    (map? hiccup) hiccup
    (vector? hiccup) (let [[tag-kw & children] hiccup]
                       (if (contains? #{:pre :code} tag-kw)
                         hiccup
                         (let [[first-child & rest-children] children
                               [props children] (if (or (nil? first-child)
                                                        (map? first-child))
                                                  [first-child rest-children]
                                                  [nil children])]
                           (-> [tag-kw]
                               (cond-> (seq props) (conj props))
                               (into (->> children
                                          (map minify-spaced-hiccup)
                                          ;; Remove consecutive "\n" outside of :pre and :code
                                          (partition-all 2 1)
                                          (mapcat (fn [[a b]]
                                                    (if (= a b "\n")
                                                      []
                                                      [a])))
                                          (hd/reduce-consecutive string? str)))))))
    :else hiccup))

(defn- attributes->html [attrs]
  (transduce (map (fn [[k v]]
                    (str " " (name k) "=\"" v "\"")))
             str
             attrs))

(defn hiccup->html [hiccup]
  (cond
    (nil? hiccup) nil
    (string? hiccup) hiccup
    (vector? hiccup)
    (let [[tag-kw & children] hiccup
          [first-child & rest-children] children
          [attrs children] (if (or (nil? first-child)
                                   (map? first-child))
                             [first-child rest-children]
                             [nil children])
          tag (name tag-kw)]
      (if (contains? #{:hr :br} tag-kw)
        (str "<" tag " />")
        (str "<" tag
             (when attrs
               (attributes->html attrs))
             ">"
             (transduce (map hiccup->html) str children)
             "</" tag ">")))))

#_
(-> "<del>*foo*</del>"
    hd/markdown->ast
    ast->spaced-hiccup
    minify-spaced-hiccup
    next
    (->> (mapv hiccup->html))
    (->> (apply str)))
