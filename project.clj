(defproject connect4 "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [org.clojure/core.async "0.1.338.0-5c5012-alpha"]
                 [om "0.7.0"]
                 [secretary "1.2.0"]
                 [prismatic/om-tools "0.3.2"]
                 ]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "connect4"
              :source-paths ["src"]
              :compiler {
                :output-to "connect4.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
