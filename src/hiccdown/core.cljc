(ns hiccdown.core
  #?(:cljs (:require-macros [hiccdown.core :refer [prn-after]]))
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [better-cond.core :as b]
            ;;[hashp.core]
            #?(:clj [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])))

(defparser parser
           "
           document-line = blank-line /
                           indented-code-block-line /
                           block-quote-line /
                           (* setext-heading-underline / *)
                           thematic-break /
                           list-item-line /
                           code-fence-line /
                           atx-heading /
                           html-start-line /
                           link-reference-definition /
                           paragraph-line

           block-quote-next-line = blank-line /
                                   (* indented-code-block-line / *)
                                   block-quote-line /
                                   thematic-break /            (* before setext-heading *)
                                   (* setext-heading-underline / *)
                                   list-item-line /
                                   code-fence-line /
                                   atx-heading /
                                   html-start-line /
                                   link-reference-definition /
                                   paragraph-line

           paragraph-next-line = blank-line /
                                 (* indented-code-block-line / *)
                                 block-quote-line /
                                 setext-heading-underline /    (* before thematic-break *)
                                 thematic-break /
                                 list-item-line /
                                 code-fence-line /
                                 atx-heading /
                                 html-start-line-0-6 /
                                 link-reference-definition /
                                 paragraph-line

           block-quote-line = space-0-3 <#'>'> text-line

           list-item-line = space-0-3 list-marker (& <#'[ \\t]'> text-line)?
           <list-marker> = (bullet-list-marker | ordered-list-marker)
           bullet-list-marker = '-' | '+' | '*'
           ordered-list-marker = #'[0-9]{1,9}' ('.' | ')')

           blank-line = maybe-spaces

           indented-code-block-line = <'    '> text-line

           code-fence-line = space-0-3 (#'[~]{3,}' <maybe-spaces> (#'[^ \\t]+' <#'.*'>)? |
                                        #'[`]{3,}' <maybe-spaces> (#'[^` \\t]+' <#'[^`]*'>)?)

           <html-start-line-0-6> = html-start-line-1 /
                                   html-start-line-2 /
                                   html-start-line-3 /
                                   html-start-line-4 /
                                   html-start-line-5 /
                                   html-start-line-6

           <html-start-line> = html-start-line-0-6 /
                               html-start-line-7

           html-start-line-1 = space-0-3 &html-start-condition-1 text-line
           html-start-condition-1 = '<' html-start-tags-1 (' ' | '\t' | '>' | !#'.')
           <html-start-tags-1> = 'pre' | 'script' | 'style' | 'textarea'

           html-start-line-2 = space-0-3 <'<!--'> text-line

           html-start-line-3 = space-0-3 <'<?'> text-line

           html-start-line-4 = space-0-3 <'<!'> &#'[a-zA-Z]' text-line

           html-start-line-5 = space-0-3 <'<![CDATA['> text-line

           html-start-line-6 = space-0-3 &html-start-condition-6 text-line
           html-start-condition-6 = '<' '/'? html-start-condition-6-tag (' ' | '\t' | '>' | '/>' | !#'.')
           html-start-condition-6-tag = 'address' | 'article' | 'aside' | 'base' | 'basefont' | 'blockquote' | 'body' | 'caption' |
                                        'center' | 'col' | 'colgroup' | 'dd' | 'details' | 'dialog' | 'dir' | 'div' | 'dl' | 'dt' |
                                        'fieldset' | 'figcaption' | 'figure' | 'footer' | 'form' | 'frame' | 'frameset' | 'h1' |
                                        'h2' | 'h3' | 'h4' | 'h5' | 'h6' | 'head' | 'header' | 'hr' | 'html' | 'iframe' | 'legend' |
                                        'li' | 'link' | 'main' | 'menu' | 'menuitem' | 'nav' | 'noframes' | 'ol' | 'optgroup' |
                                        'option' | 'p' | 'param' | 'section' | 'source' | 'summary' | 'table' | 'tbody' | 'td' |
                                        'tfoot' | 'th' | 'thead' | 'title' | 'tr' | 'track' | 'ul'

           html-start-line-7 = space-0-3 &html-start-condition-7 text-line
           html-start-condition-7 = &('<' '/'? !html-start-tags-1) (open-tag | closing-tag) maybe-spaces eos

           open-tag = <'<'> tag-name html-attribute* <maybe-spaces> '/'? <'>'>
           html-attribute = <spaces> attribute-name attribute-value-specification?

           <attribute-value-specification> = <maybe-spaces> <'='> <maybe-spaces> attribute-value
           attribute-value = unquoted-attribute-value | single-quoted-attribute-value | double-quoted-attribute-value
           unquoted-attribute-value = #'[^ \\t\"\\'=<>/]*'
           single-quoted-attribute-value = #'\\'[^\\']*\\''
           double-quoted-attribute-value = #'\"[^\"]*\"'

           closing-tag = <'</'> tag-name <maybe-spaces> <'>'>

           link-reference-definition = space-0-3 link-label <':'> <maybe-spaces> link-destination (spaces link-title)?
           link-label = <'['> !(spaces (']' | !#'.')) #'[^\\[\\]]*' <']'>
           link-destination = (<'<'> #'[^<>]*' <'>'>) |
                              (!'<' #'[^ ]*')
           link-title = (<'\"'> #'[^\"]*' <'\"'>) |
                        (<'\\''> #'[^\\']*' <'\\''>) |
                        (<'('> #'[^()]*' <')'>)

           thematic-break = <space-0-3> (<#'[*][ \\t]*[*][ \\t]*[*][ \\t*]*'> |
                                         <#'-[ \\t]*-[ \\t]*-[ \\t-]*'> |
                                         <#'_[ \\t]*_[ \\t]*_[ \\t_]*'>)

           atx-heading = <space-0-3> sharp-1-6 heading-text (<spaces> <sharps>)? <maybe-spaces>
           heading-text = (<spaces> #'.'+)?

           setext-heading-underline = <space-0-3> (#'=+' | #'--+') <maybe-spaces>

           paragraph-line = indentation text-line


           (* Tokens *)
           attribute-name = #'[:_a-zA-Z][-:_0-9a-zA-Z]*'
           tag-name = #'[a-zA-Z][-0-9a-zA-Z]*'

           sharp-1-6 = #'#{1,6}'
           sharps = #'#+'
           space-0-3 = #'[ ]{0,3}'
           maybe-spaces = #'[ \\t]*'
           spaces = #'[ \\t]+'
           indentation = #'[ ]*'
           text-line = #'[^\\n]*'
           eol = <'\n'>
           eos = <#'$'>
           ")

(comment
  [;; Multiple line parse
   :code
   :blockquote
   :html
   :table

   :table-cells
   :table-alignments
   :table-cells

   ;; single line parse
   :heading
   :paragraph
   :blockquote
   :list-item
   :checkbox
   :hr

   ;; inline parse
   :strong
   :em
   :codespan
   :br
   :del
   :link
   :image
   :text])

(def char->escaped-string
  {\! "!"
   \" "&quot;"
   \# "#"
   \$ "$"
   \% "%"
   \& "&amp;"
   \' "'"
   \( "("
   \) ")"
   \* "*"
   \+ "+"
   \, ","
   \- "-"
   \. "."
   \/ "/"
   \: ":"
   \; ";"
   \< "&lt;"
   \= "="
   \> "&gt;"
   \? "?"
   \@ "@"
   \[ "["
   \\ "\\"
   \] "]"
   \^ "^"
   \_ "_"
   \` "`"
   \{ "{"
   \| "|"
   \} "}"
   \~ "~"})

(defn post-process-text
  "Returns an escaped and transformed string according to the table `char->escaped-string`."
  [s]
  (if (< (count s) 2)
    s
    (->> (loop [result []
                c0 (first s)
                c1 (second s)
                s (next (next s))]
           (if (nil? c1)
             (conj result (char->escaped-string c0 c0))
             (if (= c0 \\)
               (if (contains? char->escaped-string c1)
                 (recur (conj result (char->escaped-string c1))
                        (first s)
                        (second s)
                        (next (next s)))
                 (recur (-> result
                            (conj c0)
                            (conj c1))
                        (first s)
                        (second s)
                        (next (next s))))
               (recur (conj result (char->escaped-string c0 c0))
                      c1
                      (first s)
                      (next s)))))
         (apply str))))

(defn- expanded-tabs-length
  "Returns the length of an indentation (made of spaces and tabs) where the tabs
   are expanded to change the current length to the next multiple of 4."
  [indentation col-index]
  (-> (reduce (fn [index c]
                (if (= c \space)
                  (inc index)
                  (+ (- index (mod index 4)) 4)))
              col-index
              indentation)
      (- col-index)))

(defn- n-spaces
  "Returns a string made of `n` spaces."
  [n]
  (apply str (repeat n \space)))

(defn heading-tabs-expanded [line-str col-index]
  (if (nil? line-str)
    ""
    (let [[_ indent text] (re-find #"([ \t]*)(.*)" line-str)]
      (str (n-spaces (expanded-tabs-length indent col-index)) text))))

(defn triml-up-to-n-spaces [line-str n-spaces]
  (let [left-spaces (count (re-find #"[ ]*" line-str))]
    (subs line-str (min left-spaces n-spaces))))

(defn process-line-breaks
  "Replaces the hard line breaks by [:br], prepends \\n otherwise."
  [text-lines]
  (-> (mapcat (fn [line-str]
                (cond
                  (str/ends-with? line-str "  ")
                  [(str/trim (subs line-str 0 (- (count line-str) 2)))
                   [:br]]

                  (str/ends-with? line-str "\\")
                  [(str/trim (subs line-str 0 (dec (count line-str))))
                   [:br]]

                  :else
                  [(str/trim line-str) "\n"]))
              (butlast text-lines))
      vec
      (conj (str/trim (last text-lines)))))

(defn prepend-soft-line-breaks
  "Prepends \\n on each line."
  [text-lines]
  (->> text-lines
       (mapv (fn [code-line]
               (str code-line "\n")))
       str/join))

(defn drop-surrounding-blank-lines
  [text-lines]
  (->> text-lines
       (drop-while str/blank?)
       reverse
       (drop-while str/blank?)
       reverse))

(defn parse-using [line-str start-rule]
  (let [parse-result (insta/parse parser line-str :start start-rule)]
    (when-not (insta/failure? parse-result)
      (second parse-result))))



(defn transform-consecutive [pred transform-fn]
  (comp (partition-by pred)
        (mapcat (fn [group]
                  (if (pred (first group))
                    [(transform-fn group)]
                    group)))))

(defn reduce-consecutive
  ([pred reduce-fn]
   (transform-consecutive pred
                          (fn [group]
                            (reduce reduce-fn group))))
  ([pred reduce-fn coll]
   (into []
         (reduce-consecutive pred reduce-fn)
         coll)))

(defn strip-one [s]
  (if (and (not (str/blank? s))
           (str/starts-with? s " ")
           (str/ends-with? s " "))
    (subs s 1 (dec (count s)))
    s))

(defn parse-code-span [s]
  (loop [result []
         s0 s]
    ;; Look for the opening backticks
    (let [[match text-before opening-backticks s1] (re-matches #"(?s)([^`]*)(`+)(.*)" s0)]
      (if (nil? match)
        (-> result
            (cond-> (seq s0) (conj s0)))
        ;; Look for the closing backticks
        (let [[match code-span-text s2] (re-matches (re-pattern (str "(?s)(.+?(?<!`)(?="
                                                                     opening-backticks
                                                                     "(?!`)))"
                                                                     opening-backticks
                                                                     "(.*)"))
                                                    s1)]
          (if (nil? match)
            (recur (-> result
                       (conj (str text-before opening-backticks)))
                   s1)
            (recur (-> result
                       (cond-> (seq text-before) (conj text-before))
                       (conj {:type :code-span
                              :text (-> code-span-text
                                        (str/replace #"\n" " ")
                                        strip-one)}))
                   s2)))))))

(defn mapcat-strings [f coll]
  (->> coll
       (mapcat (fn [item]
                 (if (string? item)
                   (f item)
                   [item])))))

(defn map-strings [f coll]
  (->> coll
       (map (fn [item]
              (if (string? item)
                (f item)
                item)))))

#?(:clj
   (defn re-find-map [s ^java.util.regex.Pattern re f]
     (let [matcher (re-matcher re s)]
       (loop [result []
              index 0]
         (if (.find matcher)
           (let [group (.group matcher)
                 start (.start matcher)
                 end   (.end matcher)]
             (recur (-> result
                        (cond-> (< index start) (conj (subs s index start)))
                        (conj (f group s start end)))
                    end))
           (-> result
               (cond-> (< index (count s)) (conj (subs s index))))))))
   :cljs
   (defn re-find-map [s ^js/RegExp re f]
     (loop [result []
            s0 s
            index 0]
       (let [matches (.exec re s0)]
         (if (nil? matches)
           (-> result
               (cond-> (pos? (count s0)) (conj s0)))
           (let [group (aget matches 0)
                 start (.-index matches)
                 end (+ start (count group))]
             (recur (-> result
                        (cond-> (pos? start) (conj (subs s0 0 start)))
                        (conj (f group s (+ index start) (+ index end))))
                    (subs s0 end)
                    (+ index end))))))))


(defn parse-delimiter-runs [text]
  (re-find-map text
               #"(?m)(?<!\\)(?:[*]+|[_]+)"
               (fn [group s start end]
                 {:type           :delimiter-run
                  :marker         group
                  :preceding      (get s (dec start))
                  :following      (get s end)})))

(defn flanking-space? [c]
  (contains? #{;; Zs unicode category
               \space \u00a0 \u1680 \u2000 \u2001 \u2002
               \u2003 \u2004 \u2005 \u2006 \u2007 \u2008
               \u2009 \u200a \u202f \u205f \u3000

               nil \tab \newline \formfeed \return} c))

(defn flanking-punctuation? [c]
  ;; FIXME: add the Pc, Pd, Pe, Pf, Pi, Po and Ps unicode categories
  (contains? #{\! \" \# \$ \% \& \' \( \) \* \+ \, \- \. \/
               \: \; \< \= \> \? \@
               \[ \\ \] \^ \_ \`
               \{ \| \} \~} c))

(defn left-flanking? [{:keys [preceding following] :as delimiter}]
  (and (not (flanking-space? following))
       (or (not (flanking-punctuation? following))
           (and (or (flanking-space? preceding)
                    (flanking-punctuation? preceding))))))

(defn right-flanking? [{:keys [preceding following] :as delimiter}]
  (and (not (flanking-space? preceding))
       (or (not (flanking-punctuation? preceding))
           (and (or (flanking-space? following)
                    (flanking-punctuation? following))))))

(defn can-open-emphasis? [delimiter]
  (or (and (str/starts-with? (:marker delimiter) "*")
           (left-flanking? delimiter))
      (and (str/starts-with? (:marker delimiter) "_")
           (left-flanking? delimiter)
           (or (not (right-flanking? delimiter))
               (flanking-punctuation? (:preceding delimiter))))))

(defn can-close-emphasis? [delimiter]
  (or (and (str/starts-with? (:marker delimiter) "*")
           (right-flanking? delimiter))
      (and (str/starts-with? (:marker delimiter) "_")
           (right-flanking? delimiter)
           (or (not (left-flanking? delimiter))
               (flanking-punctuation? (:following delimiter))))))

(defn can-open-strong-emphasis? [delimiter]
  (or (and (str/starts-with? (:marker delimiter) "**")
           (left-flanking? delimiter))
      (and (str/starts-with? (:marker delimiter) "__")
           (left-flanking? delimiter)
           (or (not (right-flanking? delimiter))
               (flanking-punctuation? (:preceding delimiter))))))

(defn can-close-strong-emphasis? [delimiter]
  (or (and (str/starts-with? (:marker delimiter) "**")
           (right-flanking? delimiter))
      (and (str/starts-with? (:marker delimiter) "__")
           (right-flanking? delimiter)
           (or (not (left-flanking? delimiter))
               (flanking-punctuation? (:following delimiter))))))

(defn delimiter-kind-matches? [opening-delimiter closing-delimiter]
  (= (get (:marker opening-delimiter) 0)
     (get (:marker closing-delimiter) 0)))

(defn delimiter-rule-of-3? [opening-delimiter closing-delimiter]
  (let [a (-> opening-delimiter :marker count)
        b (-> closing-delimiter :marker count)]
    (or (not (zero? (mod (+ a b) 3)))
        (and (zero? (mod a 3))
             (zero? (mod b 3))))))

(defn delimiter-run? [x]
  (= (:type x) :delimiter-run))

(defn unwrap-delimiters [coll]
  (->> coll
       (mapv (fn [x]
               (cond-> x
                       (delimiter-run? x)
                       (:marker x))))
       (reduce-consecutive string? str)
       vec))

(defn match-delimiter-runs [coll]
  (-> (reduce (fn [acc elm]
                (if (not (delimiter-run? elm))
                  (conj acc elm)

                  (let [elm-can-close-emphasis (can-close-emphasis? elm)
                        elm-can-close-strong-emphasis (can-close-strong-emphasis? elm)]
                    (if (and (not elm-can-close-emphasis)
                             (not elm-can-close-strong-emphasis))
                      (conj acc elm)

                      ;; Looks for an opening delimiter that elm can close.
                      (let [elm-can-open-emphasis (can-open-emphasis? elm)
                            elm-can-open-strong-emphasis (can-open-strong-emphasis? elm)
                            [index opening-delimiter matches-strong-emphasis]
                            (-> (for [[index x] (map-indexed vector (rseq acc))
                                      :when (delimiter-kind-matches? x elm)
                                      :let [matches-strong-emphasis (and elm-can-close-strong-emphasis
                                                                         (can-open-strong-emphasis? x)
                                                                         (let [can-both-open-close (or elm-can-open-strong-emphasis
                                                                                                       (can-close-strong-emphasis? x))]
                                                                           (or (not can-both-open-close)
                                                                               (delimiter-rule-of-3? x elm))))
                                            matches-emphasis (when-not matches-strong-emphasis
                                                               (and elm-can-close-emphasis
                                                                    (can-open-emphasis? x)
                                                                    (let [can-both-open-close (or elm-can-open-emphasis
                                                                                                  (can-close-emphasis? x))]
                                                                      (or (not can-both-open-close)
                                                                          (delimiter-rule-of-3? x elm)))))]

                                      :when (or matches-strong-emphasis matches-emphasis)
                                      :let [index (- (count acc) index 1)]]
                                  [index x matches-strong-emphasis])
                                first)]
                        ;; If no opening delimiter was found
                        (if (nil? index)
                          (conj acc elm)

                          (if matches-strong-emphasis
                            ;; Matches a strong emphasis
                            (let [open-marker  (subs (:marker opening-delimiter) 2)
                                  close-marker (subs (:marker elm) 2)]
                              (-> (subvec acc 0 index)
                                  (cond->
                                    (not (str/blank? open-marker))
                                    (conj (assoc opening-delimiter :marker open-marker)))
                                  (conj {:type     :strong-emphasis
                                         :children (-> (subvec acc (inc index))
                                                       unwrap-delimiters)})
                                  (cond->
                                    (not (str/blank? close-marker))
                                    (recur (conj (assoc elm :marker close-marker))))))

                            ;; Matches an emphasis
                            (let [open-marker  (subs (:marker opening-delimiter) 1)
                                  close-marker (subs (:marker elm) 1)]
                              (-> (subvec acc 0 index)
                                  (cond->
                                    (not (str/blank? open-marker))
                                    (conj (assoc opening-delimiter :marker open-marker)))
                                  (conj {:type     :emphasis
                                         :children (-> (subvec acc (inc index))
                                                       unwrap-delimiters)})
                                  (cond->
                                    (not (str/blank? close-marker))
                                    (recur (conj (assoc elm :marker close-marker)))))))))))))
              []
              coll)
      unwrap-delimiters))

;;(match-delimiter-runs (parse-delimiter-runs "a ***bc*** d"))


(defn hard-line-breaks [text-segment]
  (re-find-map text-segment #"[ \t]{2,}\n|\\\n" (constantly {:type :hard-line-break})))

(defn soft-line-breaks [text-segment]
  (re-find-map text-segment #"[ \t^\\]*\n[ \t]*" (constantly {:type :soft-line-break})))

(defn process-inlines [node]
  (update node :children
          (fn [children]
            (->> children
                 (str/join "\n")
                 str/trimr
                 parse-code-span
                 (reduce-consecutive string? str)
                 (mapcat-strings hard-line-breaks)
                 (reduce-consecutive string? str)
                 (mapcat-strings soft-line-breaks)
                 (mapcat-strings parse-delimiter-runs)
                 match-delimiter-runs
                 vec))))

;;(process-inlines {:children ["*a `*`*"]})
;;(process-inlines {:children ["_a `_`_"]})

(defmacro prn-after [debug-expr after-expr]
  `(let [result# ~after-expr]
     ;;(prn ~debug-expr)
     result#))

(defn append-child [node child]
  (update node :children conj child))

(defn get-last-child [node]
  (-> node :children peek))

(defn pop-last-child [node]
  (if (seq (:children node))
    (update node :children pop)
    node))

(defn update-last-child [node f & args]
  (let [children (:children node)
        last-child-index (dec (count children))]
    (if (neg? last-child-index)
      node
      (apply update-in node [:children last-child-index] f args))))

(defn assoc-last-child [node new-child]
  (let [children (:children node)
        last-child-index (dec (count children))]
    (if (neg? last-child-index)
      node
      (assoc-in node [:children last-child-index] new-child))))

(def block-branch-node-types
  "Types of block nodes which can contain other block nodes."
  #{:document
    :block-quote
    :list
    :list-item})

(defn find-last-leaf-node [node]
  (if (contains? block-branch-node-types (:type node))
    (find-last-leaf-node (get-last-child node))
    node))

(defn update-last-leaf-node [node f & args]
  (if (contains? block-branch-node-types (:type node))
    (apply update-last-child node update-last-leaf-node f args)
    (apply f node args)))

(defn get-deepest-block-node [block-node]
  (let [last-child (get-last-child block-node)]
    (if (contains? block-branch-node-types (:type last-child))
      (get-deepest-block-node last-child)
      block-node)))

(defn update-deepest-block-node [block-node f & args]
  (let [last-child (get-last-child block-node)]
    (if (contains? block-branch-node-types (:type last-child))
      (apply update-last-child block-node update-deepest-block-node f args)
      (apply f block-node args))))

(defn drop-surrounding-blanks
  [children]
  (->> children
       (drop-while #{{:type :blank}})
       reverse
       (drop-while #{{:type :blank}})
       reverse
       vec))

(defn remove-blank-children [node]
  (update node :children
          (fn [children]
              (filterv (complement #{{:type :blank}}) children))))

(defn ends-with-blank-line? [list-item]
  (= (peek (:children list-item)) {:type :blank}))

(declare ast-append-child)

(defn append-block-quote-line [node line-ast col-index]
  (let [[_:block-quote-line [_:space-0-3 indent] [_:text-line text-line]] line-ast
        indent (inc (count indent)) ;; +1 for the mark
        expanded-text (heading-tabs-expanded text-line (+ col-index indent))
        [indent expanded-text] (if (str/starts-with? expanded-text " ")
                                 [(inc indent) (subs expanded-text 1)]
                                 [indent expanded-text])]
    (-> node
        (ast-append-child expanded-text (+ col-index indent)))))

(defn change-to-setext-heading [node line-ast]
  (let [[_:setext-heading-underline marker] line-ast]
    (-> node
        (assoc :type :header
               :depth ({\= 1, \- 2} (first marker)))
        process-inlines)))

(defn create-block-quote [line-ast col-index]
  (-> {:type     :block-quote
       :children []}
      (append-block-quote-line line-ast col-index)))

(defn create-list-item [line-ast col-index]
  (let [[_:list-item-line
         [_:space-0-3 pre-marker-spaces]
         [_marker-type item-num-str dot-or-paren]
         [_:text-line text-line]] line-ast
        marker (str item-num-str dot-or-paren)

        starting-on-blank-line (str/blank? text-line)
        pre-marker-indent (+ (count pre-marker-spaces)
                             (count marker))
        expanded-text (heading-tabs-expanded text-line (+ col-index pre-marker-indent))

        ;; Skip the post-mark single space.
        expanded-text (cond-> expanded-text
                              (str/starts-with? expanded-text " ") (subs 1))

        post-spaces (re-find #"[ ]*" expanded-text)
        post-marker-indent (cond
                             ;; When the list item starts with a blank line, the number of spaces
                             ;; following the list marker does not change the required indentation.
                             starting-on-blank-line 0

                             ;; Indented code block
                             (> (count post-spaces) 3) 0

                             :else (count post-spaces))
        content-text (subs expanded-text post-marker-indent)

        item-indent (+ pre-marker-indent
                       1 ;; the required space after the marker
                       post-marker-indent)]
    (-> {:type                   :list-item
         :children               []
         :indent                 item-indent}
        (ast-append-child content-text (+ col-index item-indent)))))

(defn create-list [line-ast col-index]
  (-> (let [[_:list-item-line
             _:pre-marker-spaces
             [marker-type item-num-str dot-or-paren]
             _:text-line] line-ast
            ordered (= marker-type :ordered-list-marker)]
        (-> {:type             :list
             :children         []
             :ordered          ordered}
            (cond-> ordered
                    (assoc :item-num (edn/read-string item-num-str)
                           :dot-or-paren dot-or-paren))
            (cond-> (not ordered)
                    (assoc :bullet-type item-num-str))))
      (append-child (create-list-item line-ast col-index))))

(defn create-indented-code-block [line-ast col-index]
  (let [[_:indented-code-block-line [_:text-line text-line]] line-ast]
    {:type      :indented-code-block
     :children  [text-line]}))

(defn create-fenced-code-block [line-ast col-index]
  (let [[_:code-fence-line
         [_:space-0-3 indent]
         fence info-string] line-ast]
    {:type        :fenced-code-block
     :indent      (count indent)
     :fence-type  (first fence)
     :fence-count (count fence)
     :info-string info-string
     :children  []}))

(defn create-atx-heading [line-ast col-index]
  (let [[_:atx-heading
         [_:sharp-1-6 sharps]
         [_:heading-text & text-line]] line-ast
        heading-text (-> (apply str text-line)
                         str/trim)]
    (-> {:type     :header
         :depth    (count sharps)
         :children (if (str/blank? heading-text)
                     []
                     [heading-text])}
        process-inlines)))

(defn create-thematic-break [line-ast col-index]
  {:type :thematic-break})

(defn is-closing-html-block-1? [line-str]
  (let [low-line-str (str/lower-case line-str)]
    (or (some? (str/index-of low-line-str "</pre>"))
        (some? (str/index-of low-line-str "</script>"))
        (some? (str/index-of low-line-str "</style>"))
        (some? (str/index-of low-line-str "</textarea>")))))

(defn create-html-1 [line-ast col-index]
  (let [[_:html-start-line-1
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation text-line)]
    (cond-> {:type     :html-block-1
             :children [(str indentation text-line)]}
      (is-closing-html-block-1? line-str)
      (assoc :closed true))))

(defn is-closing-html-block-2? [line-str]
  (some? (str/index-of line-str "-->")))

(defn create-html-2 [line-ast col-index]
  (let [[_:html-start-line-2
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation "<!--" text-line)]
    (cond-> {:type     :html-block-2
             :children [line-str]}
      (is-closing-html-block-2? line-str)
      (assoc :closed true))))

(defn is-closing-html-block-3? [line-str]
  (some? (str/index-of line-str "?>")))

(defn create-html-3 [line-ast col-index]
  (let [[_:html-start-line-3
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation "<?" text-line)]
    (cond-> {:type     :html-block-3
             :children [line-str]}
      (is-closing-html-block-3? line-str)
      (assoc :closed true))))

(defn is-closing-html-block-4? [line-str]
  (some? (str/index-of line-str ">")))

(defn create-html-4 [line-ast col-index]
  (let [[_:html-start-line-4
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation "<!" text-line)]
    (cond-> {:type     :html-block-4
             :children [line-str]}
      (is-closing-html-block-4? line-str)
      (assoc :closed true))))

(defn is-closing-html-block-5? [line-str]
  (some? (str/index-of line-str "]]>")))

(defn create-html-5 [line-ast col-index]
  (let [[_:html-start-line-5
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation "<![CDATA[" text-line)]
    (cond-> {:type     :html-block-5
             :children [line-str]}
      (is-closing-html-block-5? line-str)
      (assoc :closed true))))

(defn is-closing-html-block-5-6? [line-str]
  (str/blank? line-str))

(defn create-html-6 [line-ast col-index]
  (let [[_:html-start-line-6
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation text-line)]
    (cond-> {:type     :html-block-6
             :children [line-str]}
      (is-closing-html-block-5-6? line-str)
      (assoc :closed true))))

(defn create-html-7 [line-ast col-index]
  (let [[_:html-start-line-7
         [_:space-0-3 indentation]
         [_:text-line text-line]] line-ast
        line-str (str indentation text-line)]
    (cond-> {:type     :html-block-7
             :children [line-str]}
      (is-closing-html-block-5-6? line-str)
      (assoc :closed true))))

(defn create-paragraph [line-ast col-index]
  (let [[_:paragraph-line [_:indentation _indentation] [_:text-line text-line]] line-ast]
    {:type     :paragraph
     :children [text-line]}))

(defn create-blank [line-ast col-index]
  {:type :blank})

(defn create-node
  ([line-ast col-index]
   (create-node line-ast col-index {}))
  ([line-ast col-index line-type->create-fn]
   (let [line-type (first line-ast)
         create-fn (or (line-type->create-fn line-type)
                       ({:block-quote-line create-block-quote
                         ;;:list-item-line create-list-item
                         :list-item-line create-list
                         :indented-code-block-line create-indented-code-block
                         :code-fence-line create-fenced-code-block
                         :atx-heading create-atx-heading
                         :thematic-break create-thematic-break
                         :html-start-line-1 create-html-1
                         :html-start-line-2 create-html-2
                         :html-start-line-3 create-html-3
                         :html-start-line-4 create-html-4
                         :html-start-line-5 create-html-5
                         :html-start-line-6 create-html-6
                         :html-start-line-7 create-html-7
                         :paragraph-line create-paragraph
                         :blank-line create-blank} line-type))]
     (create-fn line-ast col-index))))

(declare close-node)

(defn close-document [node]
  (prn-after [:close-document node]
    (-> node
        (update-last-child close-node)
        remove-blank-children)))

(defn close-block-quote [node]
  (prn-after [:close-block-quote node]
    (-> node
        (update-last-child close-node)
        remove-blank-children)))

(defn close-list [node]
  (prn-after [:close-list node]
    (-> node
        (update-last-child close-node)
        ((fn [node]
           (let [children (:children node)
                 tight (and (not (some :ends-with-blank (butlast children)))
                            (every? :tight children))]
             (-> node
                 (assoc :tight tight
                        :children (->> children
                                       (mapv (fn [list-item]
                                               (assoc list-item :tight tight)))))
                 ;; If the last item ends with a blank, then this list ends with a blank.
                 (cond->
                   (:ends-with-blank (last children))
                   (assoc :ends-with-blank true)))))))))

(defn close-list-item [node]
  (prn-after [:close-list-item node]
    (-> node
        (update-last-child close-node)
        ((fn [node]
           (let [children (:children node)]
             (assoc node :ends-with-blank
                         (or (:ends-with-blank (peek children))
                             (and (> (count children) 1)
                                  (= (peek children) {:type :blank})))))))
        (update :children drop-surrounding-blanks)
        ((fn [node]
           (let [children (:children node)
                 ;; A list item is tight if none of its children ends with a blank.
                 tight (and (not (some :ends-with-blank (butlast children)))
                            (< (count (partition-by #{{:type :blank}} children)) 3))]
             (assoc node :tight tight))))
        remove-blank-children)))

(defn close-indented-code-block [node]
  (prn-after [:indented-code-block node]
    (-> node
        (update-last-child close-node)
        (update :children (fn [children]
                            (let [children (drop-surrounding-blank-lines children)]
                              (if (seq children)
                                [(prepend-soft-line-breaks children)]
                                [])))))))

(defn close-fenced-code-block [node]
  (prn-after [:fenced-code-block node]
    (-> node
        (update-last-child close-node)
        (update :children (fn [children]
                            (if (seq children)
                              [(prepend-soft-line-breaks children)]
                              []))))))

(defn close-paragraph [node]
  (prn-after [:close-paragraph node]
    (process-inlines node)))

(defn close-node [node]
  ;;(prn-after [:close-node node])
  (if (map? node)
    (case (:type node)
      :document (close-document node)
      :block-quote (close-block-quote node)
      :list (close-list node)
      :list-item (close-list-item node)
      :indented-code-block (close-indented-code-block node)
      :fenced-code-block (close-fenced-code-block node)
      :paragraph (close-paragraph node)
      node) ;(update-last-child node close-node)
    node))

#_
(defn close-all-nodes [node]
  (cond-> (close-node node)
    (and (map? node)
         (contains? node :children))
    (update-last-child close-all-nodes)))

(defn- starts-with-<=-1-blank-line [list-item]
  (let [children (:children list-item)]
    (or (<= (count children) 1)
        (not= (children 0) {:type :blank})
        (not= (children 1) {:type :blank}))))

(defn ast-append-child [node line-str col-index]
  (let [last-child (get-last-child node)
        last-child-type (when-not (:closed last-child)
                          (:type last-child))]
    ;;(prn [:ast-append-child line-str col-index last-child-type])
    (case last-child-type
      #_
      (if (closes-node? line-ast last-child)
        (-> node
            (update-last-child close-node)
            (append-child (create-node line-ast col-index)))
        (-> node
            (update-last-child ast-append-child (remove-marker line-ast) 0)))

      :block-quote
      (let [expanded-line-str (-> line-str
                                  (heading-tabs-expanded col-index))
            [line-type :as line-ast] (-> expanded-line-str
                                         (parse-using :block-quote-next-line))]
        ;;(prn [:block-quote node line-ast])
        (cond
          (= line-type :block-quote-line)
          (-> node
              (update-last-child append-block-quote-line line-ast col-index))

          ;; FIXME: port the code from the continuation in lists.
          ;; Lazy paragraph continuation
          (and (= line-type :paragraph-line)
               (let [leaf-node (find-last-leaf-node last-child)]
                 (= (:type leaf-node) :paragraph)))
          (-> node
              (update-last-leaf-node (fn [paragraph-node]
                                       (let [[_:paragraph-line
                                              [_:indentation indentation]
                                              [_:text-line text-line]] line-ast]
                                         (-> paragraph-node
                                             (append-child text-line))))))

          :else
          (-> node
              (update-last-child close-block-quote)
              (append-child (create-node (-> expanded-line-str
                                             (parse-using :document-line))
                                         col-index)))))

      :list
      (let [expanded-line-str (-> line-str
                                  (heading-tabs-expanded col-index))
            last-list-item (get-last-child last-child)
            line-indent (count (re-find #"[ ]*" expanded-line-str))
            [line-type :as line-ast] (-> expanded-line-str
                                         (parse-using :document-line))]
        ;;(prn [:list node line-ast])
        (cond
          ;; Is this new list item belongs to the same list?
          (= line-type :list-item-line)
          (let [[_:list-item-line
                      _:pre-marker-spaces
                      [marker-type item-num-str dot-or-paren]
                      _:text-line] line-ast
                     ordered (= marker-type :ordered-list-marker)]
            (if (and (= ordered (:ordered last-child))
                     (or ordered (= item-num-str (:bullet-type last-child)))
                     (= dot-or-paren (:dot-or-paren last-child)))
              (-> node
                  (update-last-child ast-append-child line-str col-index))
              (-> node
                  (update-last-child close-list)
                  (append-child (create-node line-ast col-index)))))

          (= line-type :blank-line)
          ;; This blank line should land somewhere inside the list.
          (-> node
              (update-last-child ast-append-child line-str col-index))

          ;; This line of whatever is indented enough to be sent inside the list.
          (and (<= (:indent last-list-item) line-indent)
               (starts-with-<=-1-blank-line last-list-item))
          (-> node
              (update-last-child ast-append-child line-str col-index))

          ;; Lazy continuation line, to be sent inside the list.
          (and (contains? #{:paragraph-line
                            :indented-code-block-line} line-type)
               (let [deepest-block-node (get-deepest-block-node last-list-item)]
                 (case (:type deepest-block-node)
                   :block-quote
                   (not (ends-with-blank-line? deepest-block-node))
                   :list-item
                   (and (not (ends-with-blank-line? deepest-block-node))
                        (starts-with-<=-1-blank-line deepest-block-node)))))
          (-> node
              (update-deepest-block-node ast-append-child line-str col-index))

          ;; Does not belong to the list. Let's close it and create a new node.
          :else
          (-> node
              (update-last-child close-list)
              (append-child (create-node line-ast col-index)))))

      :list-item
      (let [expanded-line-str (-> line-str
                                  (heading-tabs-expanded col-index))
            list-item-indent (:indent last-child)
            line-indent (count (re-find #"[ ]*" expanded-line-str))
            [line-type :as line-ast] (-> expanded-line-str
                                         (parse-using :document-line))]
        ;;(prn [:list-item node line-ast])
        (cond
          (= line-type :blank-line)
          (-> node
              (update-last-child ast-append-child
                                 (triml-up-to-n-spaces expanded-line-str list-item-indent)
                                 (+ col-index (min list-item-indent line-indent))))

          ;; Is it a new list item sibling?
          (and (= line-type :list-item-line)
               (not (<= list-item-indent line-indent)))
          (-> node
              (update-last-child close-list-item)
              (append-child (create-list-item line-ast col-index)))

          ;; This line of whatever is indented enough to be sent inside the list-item.
          (<= list-item-indent line-indent)
          (-> node
              (update-last-child ast-append-child
                                 (triml-up-to-n-spaces expanded-line-str list-item-indent)
                                 (+ col-index (min list-item-indent line-indent))))

          :else
          (-> node
              (update-last-child close-list-item)
              (append-child (create-node line-ast col-index)))))

      :indented-code-block
      (let [expanded-line-str (-> line-str
                                  ;; FIXME: We should only expand the tabs until (+ col-index 4)
                                  (heading-tabs-expanded col-index))
            [line-type :as line-ast] (-> expanded-line-str
                                         (parse-using :document-line))]
        (case line-type
          :indented-code-block-line
          (let [[_:indented-code-block-line [_:text-line text-line]] line-ast]
            (-> node
                (update-last-child append-child text-line)))

          :blank-line
          (-> node
              (update-last-child append-child (triml-up-to-n-spaces expanded-line-str 4)))

          ;; else
          (-> node
              (update-last-child close-indented-code-block)
              (append-child (create-node line-ast col-index)))))

      :fenced-code-block
      (let [expanded-line-str (-> line-str
                                  (heading-tabs-expanded col-index))
            [line-type :as line-ast] (-> expanded-line-str
                                         (parse-using :document-line))
            [_:code-fence-line _:indent fence info-string] line-ast]
        (if (and (= line-type :code-fence-line)
                 (= (:fence-type last-child) (first fence))
                 (<= (:fence-count last-child) (count fence))
                 (nil? info-string))
          (-> node
              (update-last-child close-fenced-code-block)
              (append-child nil))
          (-> node
              (update-last-child append-child
                                 (triml-up-to-n-spaces expanded-line-str (:indent last-child))))))

      :html-block-1
      (if (is-closing-html-block-1? line-str)
        (-> node
            (update-last-child append-child line-str)
            (update-last-child close-node)
            (append-child nil))
        (-> node
            (update-last-child append-child line-str)))

      :html-block-2
      (if (is-closing-html-block-2? line-str)
        (-> node
            (update-last-child append-child line-str)
            (update-last-child close-node)
            (append-child nil))
        (-> node
            (update-last-child append-child line-str)))

      :html-block-3
      (if (is-closing-html-block-3? line-str)
        (-> node
            (update-last-child append-child line-str)
            (update-last-child close-node)
            (append-child nil))
        (-> node
            (update-last-child append-child line-str)))

      :html-block-4
      (if (is-closing-html-block-4? line-str)
        (-> node
            (update-last-child append-child line-str)
            (update-last-child close-node)
            (append-child nil))
        (-> node
            (update-last-child append-child line-str)))

      :html-block-5
      (if (is-closing-html-block-5? line-str)
        (-> node
            (update-last-child append-child line-str)
            (update-last-child close-node)
            (append-child nil))
        (-> node
            (update-last-child append-child line-str)))

      (:html-block-6
       :html-block-7)
      (if (is-closing-html-block-5-6? line-str)
        (-> node
            (update-last-child close-node)
            (append-child nil))
        (-> node
            (update-last-child append-child line-str)))

      :paragraph
      (let [expanded-line-str (-> line-str
                                  (heading-tabs-expanded col-index))
            [line-type :as line-ast] (-> expanded-line-str
                                         (parse-using :paragraph-next-line))]
        (cond
          (= line-type :paragraph-line)
          (let [[_:paragraph-line
                 [_:indentation _indentation]
                 [_:text-line text-line]] line-ast]
            (-> node
                (update-last-child append-child text-line)))

          (= line-type :setext-heading-underline)
          (-> node
              (update-last-child change-to-setext-heading line-ast))

          ;; An empty list item cannot interrupt a paragraph.
          ;; Ordered lists not starting with "1" cannot interrupt a paragraph.
          (and (= line-type :list-item-line)
               (let [[_:list-item-line
                      _:pre-marker-spaces
                      [marker-type item-num-str dot-or-paren]
                      [_:text-line text-line]] line-ast
                     ordered (= marker-type :ordered-list-marker)]
                 (or (str/blank? text-line)
                     (and ordered
                          (not= item-num-str "1")))))
          (-> node
              (update-last-child append-child expanded-line-str))

          #_
          (contains? #{:blank-line
                       :list-item-line
                       :block-quote-line
                       :html-block-0-to-6} line-type)
          :else
          (-> node
              (update-last-child close-paragraph)
              (append-child (create-node (-> expanded-line-str
                                             (parse-using :document-line))
                                         col-index)))))

      (:thematic-break
       :header)
      (let [line-ast (-> line-str
                         (heading-tabs-expanded col-index)
                         (parse-using :document-line))]
        (-> node
            (append-child (create-node line-ast col-index))))

      (nil ;; Last child does not exist.
       :blank)
      (let [line-ast (-> line-str
                         (heading-tabs-expanded col-index)
                         (parse-using :document-line))]
        (-> node
            (append-child (create-node line-ast col-index))))

      ,)))

(def empty-document-ast-node
  {:type :document
   :children []})

(defn markdown->ast [document]
  (-> (str/split-lines document)
      (->> (reduce (fn [node line-str]
                     (ast-append-child node line-str 0))
                   empty-document-ast-node))
      close-document))

(defn ast->hiccup [ast]
  (cond
    (string? ast)
    (post-process-text ast)

    (vector? ast)
    [ast]

    (map? ast)
    (case (:type ast)
      :document
      (into [:div]
            (comp (map ast->hiccup)
                  (drop-while #{"\n"}))
            (:children ast))

      :blank
      (:text ast)

      :block-quote
      (into [:blockquote] (map ast->hiccup) (:children ast))

      :paragraph
      (into [:p]
            (map ast->hiccup)
            (:children ast))

      :thematic-break
      [:hr]

      :soft-line-break
      "\n"

      :hard-line-break
      [:br]

      :header
      (into [(keyword (str "h" (:depth ast)))]
            (map ast->hiccup)
            (:children ast))

      :list
      (let [attributes (when-some [item-num (:item-num ast)]
                         (when-not (= item-num 1)
                           {:start (str item-num)}))]
        (-> [(if (:ordered ast) :ol :ul)]
            (cond-> (seq attributes) (conj attributes))
            (into (map ast->hiccup) (:children ast))))

      :list-item
      (into [:li]
            (comp (mapcat (fn [child-ast]
                            (if (and (:tight ast)
                                     (map? child-ast)
                                     (= (:type child-ast) :paragraph))
                              (-> child-ast :children)
                              [child-ast])))
                  (map ast->hiccup))
            (:children ast))

      :indented-code-block
      [:pre (into [:code]
                  (comp (map ast->hiccup)
                        (reduce-consecutive string? str))
                  (:children ast))]

      :fenced-code-block
      (let [attributes (when-some [info-string (:info-string ast)]
                         {:class (str "language-" info-string)})]
        [:pre (-> [:code]
                  (cond-> (seq attributes) (conj attributes))
                  (into (comp (map ast->hiccup)
                              (reduce-consecutive string? str))
                        (:children ast)))])

      (:html-block-1
       :html-block-2
       :html-block-3
       :html-block-4
       :html-block-5
       :html-block-6
       :html-block-7)
      (into [:html] (:children ast))

      :code-span
      [:code (:text ast)]

      :emphasis
      (into [:em]
            (map ast->hiccup)
            (:children ast))

      :strong-emphasis
      (into [:strong]
            (map ast->hiccup)
            (:children ast))

      :inline-link
      (into [:a (cond-> {:href (:destination ast)}
                  (some? (:title ast))
                  (assoc :title (:title ast)))]
            (map ast->hiccup)
            (:children ast))

      ,)))

#_
(-> "*foo *bar**"
    markdown->ast
    ast->hiccup
    next)
