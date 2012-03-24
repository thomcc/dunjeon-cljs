(ns dunjeon-cljs.views.main
  (:require [dunjeon-cljs.views.common :as common])
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]]))

(defpage "/" []
         (common/layout
           [:div#content]))
