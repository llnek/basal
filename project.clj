;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/basal "2.2.0"

  :license {:url "https://www.apache.org/licenses/LICENSE-2.0.txt"
            :name "Apache License"}

  :description "General clojure helper functions"
  :url "https://github.com/llnek/basal"

  :dependencies [[org.apache.logging.log4j/log4j-slf4j2-impl "2.24.1"]
                 [org.apache.logging.log4j/log4j-core "2.24.1"]
                 [org.slf4j/slf4j-api "2.0.16" ]
                 [io.aviso/pretty "1.4.4"]
                 [org.clojure/core.async "1.6.681"]
                 [org.flatland/ordered "1.15.12"]
                 [org.clojure/data.json "2.5.0"]
                 [org.clojure/tools.logging "1.3.0"]
                 [org.clojure/clojurescript "1.11.132"]]

  :XXXexclusions [org.clojure/clojure]

  :plugins [[cider/cider-nrepl "0.50.2" :exclusions [nrepl/nrepl]]
            [lein-codox "0.10.8"]
            [lein-cljsbuild "1.1.8"]]

  :cljsbuild {
    :builds [{
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src/main/clojure"]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          ;:output-to "war/javascripts/main.js"
          ; default: target/cljsbuild-main.js
          :optimizations :whitespace
          :pretty-print true}}]}

  :profiles {:uberjar {:aot :all}
             :provided {:dependencies [[org.clojure/clojure "1.12.0"]]} }

  :global-vars {*warn-on-reflection* true}
  :target-path "out/%s"
  :aot :all

  :coordinate! "czlab"
  :omit-source true

  :java-source-paths ["src/main/java" "src/test/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]

  :test-selectors {:core :test-core
                   :str :test-str
                   :misc :test-misc
                   :psub :test-psub
                   :proc :test-proc
                   :ini :test-ini
                   :io :test-io
                   :meta :test-meta
                   :dates :test-dates
                   :util :test-util}

  :jvm-opts ["-Dczlabloggerflag=true"
             "-Dlog4j.configurationFile=file:attic/log4j2.xml"]

  :javac-options ["-source" "16"
                  "-target" "22"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

