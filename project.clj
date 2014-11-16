(defproject brioches "0.1.0-SNAPSHOT"
  :description "Let them eat cakes"
  :url "https://github.com/giorgio-v/brioches"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.3.2"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/tools.namespace "0.2.7"]]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.5.9"]]
              :source-paths ["dev"]}})
