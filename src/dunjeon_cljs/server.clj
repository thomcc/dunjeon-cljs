(ns dunjeon-cljs.server
  (:require [noir.server :as server]
            [noir.cljs.core :as cljs]
            [cljs.repl :as repl]
            [cljs.repl.browser :as browser]))

(server/load-views-ns 'dunjeon-cljs.views)
(def cljs-options {:advanced {:externs ["externs/jquery.js"]}})

(defn -main [& m]
  (let [mode (keyword (or (first m) :dev)), port (Integer. (get (System/getenv) "PORT" "8090"))]
    (cljs/start mode cljs-options)
    (server/start port {:mode mode, :ns 'dunjeon-cljs})
    (repl/repl (browser/repl-env))))





