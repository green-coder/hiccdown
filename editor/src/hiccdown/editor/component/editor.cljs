(ns hiccdown.editor.component.editor
  (:require [reagent.core :as r]
            [hiccdown.core :as hd]))

(defonce edited-text (r/atom "
# Header 1
This is a paragraph.
## Header 2
This is a paragraph.
### Header 3
This is a paragraph.
#### Header 4
This is a paragraph.
##### Header 5
This is a paragraph.
###### Header 6

> This is a quote from myself.

I love to write `defn` like this:

```clojure
(defn foobar []
  ,,,)
```

This is a long  long  long  long  long  long
long  long  long  long  long  long  long  long  long
long  long  long  long  long  long  long  long  long
long  long  long  long  long  long  long  long  long
long  long  long  long  long  long  long  long  long
long  long  long  long  long  long  long  long  long paragraph.

This is another one.

Things to do in this editor:
- Stuff 1
  - Stuff 1a
  - Stuff 1b
- Stuff 2
- Stuff 3

Things to do in this editor:
1. Stuff 1
   1. Stuff 1a
   1. Stuff 1b
1. Stuff 2
1. Stuff 3
"))

(defn markdown-input []
  [:textarea.box-border.w-full.h-full.resize-none.font-size-6
   {:rows      20
    :cols      80
    :on-change (fn [^js event]
                 (.preventDefault event)
                 (reset! edited-text (-> event .-target .-value)))
    :value @edited-text}])

(defn markdown-output []
  [:div.h-full.overflow-auto
   (try (-> @edited-text hd/markdown->ast hd/ast->hiccup)
        (catch js/Error e "[WIP]: Input currently not handled."))])

(defn editor []
  [:div
   [:h1.font-size-10 "Hiccdown editor"]
   [:main
    ;; Text editor | Markdown as HTML
    [:div.flex.h-80vh
     [:section.flex-1
      [markdown-input]]
     [:section.md.flex-1
      [markdown-output]]]]])
