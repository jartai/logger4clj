(defproject logger4clj "0.2"
  :description "Clojure logging API"
  :url "http://github.com/jkauzlar/logger4clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :target-path "target/"
  :plugins [[lein2-eclipse "2.0.0"]]
  :profiles {:dev {:dependencies [[midje "1.4.0"]]}}  
  :codox {:output-dir "doc/codox"})
