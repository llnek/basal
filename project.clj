;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/basal "2.1.0"

  :license {:url "https://www.apache.org/licenses/LICENSE-2.0.txt"
            :name "Apache License"}

  :description "General clojure helper functions"
  :url "https://github.com/llnek/basal"

  :dependencies [[org.apache.logging.log4j/log4j-slf4j-impl "2.17.2"]
                 [org.apache.logging.log4j/log4j-core "2.17.2"]
                 [org.slf4j/slf4j-api "1.7.36" ]
                 [io.aviso/pretty "1.1.1"]
                 [org.clojure/core.async "1.5.648"]
                 [org.flatland/ordered "1.15.10"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/tools.logging "1.2.4"]
                 [org.clojure/clojurescript "1.11.4"]]

  :exclusions [org.clojure/clojure]

  :plugins [[cider/cider-nrepl "0.28.3"]
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

  :profiles {:provided {:dependencies [[org.clojure/clojure
                                        "1.11.1" :scope "provided"]]}
             :uberjar {:aot :all}}

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
                  "-target" "16"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

