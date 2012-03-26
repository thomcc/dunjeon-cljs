(ns dunjeon-cljs.views.main
  (:require [dunjeon-cljs.views.common :as common])
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]]))

(defpage "/" []
  (common/layout
   [:div#content
    [:div#wrapper
     [:h1 "(dunjeon)"]
     [:div.clearfix
      [:canvas#canvas {:width 550, :height 550}]
      [:div#status]]
     [:div.msgs]]]))
