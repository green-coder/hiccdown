(ns hiccdown.editor.core
  (:require [reagent.dom :as rdom]
            [hiccdown.editor.component.editor :refer [editor]]))

(defn render []
  (rdom/render [editor] (js/document.getElementById "app")))

(defn init []
  (println "(init)")
  (render))

(defn ^:dev/before-load stop []
  (println "(stop)"))

(defn ^:dev/after-load start []
  (println "(start)")
  (render))
