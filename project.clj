(defproject algorithmuss "0.1.0-SNAPSHOT"
  :description "Music written in overtone and Leipzig by LeMilonkh"
  :url "http://github.com/lemilonkh/algorithmuss"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    [overtone "0.10.1"]
    [leipzig "0.10.0"]]
  :jvm-opts ^:replace []
  :main algorithmuss.core
)
