(ns hiccdown.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.string :as str]
            [hiccdown.core :as hd]))

(deftest strip-one-test
  (is (= " " (hd/strip-one " ")))
  (is (= "  " (hd/strip-one "  ")))
  (is (= "   " (hd/strip-one "   ")))
  (is (= "abc " (hd/strip-one "abc ")))
  (is (= "abc" (hd/strip-one "abc")))
  (is (= " abc" (hd/strip-one " abc")))
  (is (= "abc" (hd/strip-one " abc ")))
  (is (= " abc " (hd/strip-one "  abc  "))))

(deftest parse-code-span-test
  (let [parse-code-span* (fn [s]
                           (->> (hd/parse-code-span s)
                                (hd/reduce-consecutive string? str)))]
    (is (= ["abc"
            {:text "def````ghi```jkl"
             :type :code-span}
            "mn```op"
            {:text "qr`st``uv"
              :type :code-span}
            "x"]
           (parse-code-span* "abc``def````ghi```jkl``mn```op````qr`st``uv````x")))

    (is (= ["abcdef"]
           (parse-code-span* "abcdef")))

    (is (= ["abc``def"]
           (parse-code-span* "abc``def")))

    (is (= ["a"
            {:type :code-span
             :text "b"}
            "c``def"]
           (parse-code-span* "a`b`c``def")))

    (is (= ["a"
            {:type :code-span
             :text "b"}
            "c"
            {:type :code-span
             :text "de"}
            "f"]
           (parse-code-span* "a`b`c`de`f")))

    (is (= [{:type :code-span
             :text " "}
            "\n"
            {:type :code-span
             :text "  "}]
           (parse-code-span* "` `\n`  `")))))


(deftest parse-delimiter-runs-test
  (is (= ["abc"
          {:type :delimiter-run, :marker "*", :preceding \c, :following \\}
          "\\*"
          {:type :delimiter-run, :marker "*", :preceding \*, :following \d}
          "de"
          {:type :delimiter-run, :marker "*", :preceding \e, :following \\}
          "\\*"]
         (hd/parse-delimiter-runs "abc*\\**de*\\*"))))


(deftest left-right-flanking-test
  (testing "left-flanking but not right-flanking"
    (are [text]
      (let [delimiter-run (-> text hd/parse-delimiter-runs first)]
        (and (hd/left-flanking? delimiter-run)
             (not (hd/right-flanking? delimiter-run))))

      "***abc"
      "_abc"
      "**\"abc\""
      "_\"abc\""))

  (testing "right-flanking but not left-flanking"
    (are [text]
      (let [delimiter-run (-> text hd/parse-delimiter-runs last)]
        (and (hd/right-flanking? delimiter-run)
             (not (hd/left-flanking? delimiter-run))))

      "abc***"
      "abc_"
      "\"abc\"**"
      "\"abc\"_"))

  (testing "Both left and right-flanking"
    (are [text]
      (let [delimiter-run (-> text hd/parse-delimiter-runs second)]
        (and (hd/left-flanking? delimiter-run)
             (hd/right-flanking? delimiter-run)))

      "abc***def"
      "\"abc\"_\"def\""))

  (testing "Neither left nor right-flanking"
    (are [text]
      (let [delimiter-run (-> text hd/parse-delimiter-runs second)]
        (and (not (hd/left-flanking? delimiter-run))
             (not (hd/right-flanking? delimiter-run))))
      "abc *** def"
      "a _ b")))


(deftest lazy-continuation-test
  (testing "after a quote"
    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children []}
                       {:type     :paragraph
                        :children ["a"]}]}

           (-> "
>
a
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"
                                               {:type :soft-line-break}
                                               "b"]}]}]}
           (-> "
> a
b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"]}]}
                       {:type     :paragraph
                        :children ["b"]}]}
           (-> "
> a
>
b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"]}]}
                       {:type     :paragraph
                        :children ["b"]}]}
           (-> "
> a

b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"]}]}
                       {:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["b"]}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}]}
           (-> "
> a
- b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"
                                               {:type :soft-line-break}
                                               "- b"]}]}]}
           (-> "
> a
    - b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"]}
                                   {:type        :list
                                    :children    [{:type            :list-item
                                                   :children        [{:type     :paragraph
                                                                      :children ["b"
                                                                                 {:type :soft-line-break}
                                                                                 "c"]}]
                                                   :indent          2
                                                   :ends-with-blank false
                                                   :tight           true}]
                                    :ordered     false
                                    :bullet-type "-"
                                    :tight       true}]}]}
           (-> "
> a
> - b
c
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["a"]}
                                   {:type        :list
                                    :children    [{:type            :list-item
                                                   :children        [{:type     :paragraph
                                                                      :children ["b"]}]
                                                   :indent          2
                                                   :ends-with-blank false
                                                   :tight           true}]
                                    :ordered     false
                                    :bullet-type "-"
                                    :tight       true}]}
                       {:type     :paragraph
                        :children ["c"]}]}
           (-> "
> a
> - b

c
"
               hd/markdown->ast))))


  (testing "after a list-item"
    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        []
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}
                       {:type     :paragraph
                        :children ["a"]}]}
           (-> "
-
a
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["a"]}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}]}
           (-> "
-
  a
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type            :list
                        :children        [{:type            :list-item
                                           :children        []
                                           :indent          2
                                           :ends-with-blank true
                                           :tight           true}]
                        :ordered         false
                        :bullet-type     "-"
                        :ends-with-blank true
                        :tight           true}
                       {:type     :paragraph
                        :children ["a"]}]}
           (-> "
-

  a
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["a"
                                                                     {:type :soft-line-break}
                                                                     "b"]}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}]}
           (-> "
- a
b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type            :list
                        :children        [{:type            :list-item
                                           :children        [{:type     :paragraph
                                                              :children ["a"]}]
                                           :indent          2
                                           :ends-with-blank true
                                           :tight           true}]
                        :ordered         false
                        :bullet-type     "-"
                        :tight           true
                        :ends-with-blank true}
                       {:type     :paragraph
                        :children ["b"]}]}
           (-> "
- a

b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["a"]}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}
                       {:type     :block-quote
                        :children [{:type     :paragraph
                                    :children ["b"]}]}]}
           (-> "
- a
> b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["a"]}
                                                         {:type     :block-quote
                                                          :children [{:type     :paragraph
                                                                      :children ["b"]}]}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}]}
           (-> "
- a
    > b
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["a"]}
                                                         {:type     :block-quote
                                                          :children [{:type     :paragraph
                                                                      :children ["b"
                                                                                 {:type :soft-line-break}
                                                                                 "c"]}]}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}]}
           (-> "
- a
  > b
c
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type            :list
                        :children        [{:type            :list-item
                                           :children        [{:type     :paragraph
                                                              :children ["a"]}
                                                             {:type     :block-quote
                                                              :children [{:type     :paragraph
                                                                          :children ["b"]}]}]
                                           :indent          2
                                           :ends-with-blank true
                                           :tight           true}]
                        :ordered         false
                        :bullet-type     "-"
                        :tight           true
                        :ends-with-blank true}
                       {:type :paragraph
                        :children ["c"]}]}
           (-> "
- a
  > b

c
"
               hd/markdown->ast)))


    (is (= {:type     :document
            :children [{:type        :list
                        :children    [{:type            :list-item
                                       :children        [{:type     :paragraph
                                                          :children ["a"]}
                                                         {:type        :list
                                                          :children    [{:type            :list-item
                                                                         :children        [{:type     :paragraph
                                                                                            :children ["b"]}]
                                                                         :indent          4
                                                                         :ends-with-blank false
                                                                         :tight           true}]
                                                          :ordered     false
                                                          :bullet-type "-"
                                                          :tight       true}]
                                       :indent          2
                                       :ends-with-blank false
                                       :tight           true}]
                        :ordered     false
                        :bullet-type "-"
                        :tight       true}]}
           (-> "
- a
    - b
"
               hd/markdown->ast)))))
