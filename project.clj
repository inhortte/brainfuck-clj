(defproject brainfuck-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot brainfuck-clj.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[speclj "3.3.0"]]}
             :uberjar {:aot :all}}
  :plugins [[speclj "3.3.0"]]
  :test-paths ["spec"])
