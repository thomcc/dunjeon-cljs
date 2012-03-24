(defproject dunjeon-cljs "0.1.0-SNAPSHOT"
  :description "A port of dunjeon to cljs"
  :url "http://exampl.com/FIXME"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [noir-cljs "0.2.5"]
                 [jayq "0.1.0-alpha1"]
                 [fetch "0.1.0-alpha2"]
                 [crate "0.1.0-alpha2"]
                 [noir "1.3.0-alpha10"]]
  :cljsbuild {:builds [{}]}
  :main ^{:skip-aot true} dunjeon-cljs.server)
