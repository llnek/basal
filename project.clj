;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/basal "1.1.0"

  :license {:url "http://www.eclipse.org/legal/epl-v10.html"
            :name "Eclipse Public License"}

  :description "General clojure helper functions"
  :url "https://github.com/llnek/basal"

  :dependencies [[org.apache.logging.log4j/log4j-slf4j-impl "2.12.1"]
                 [org.apache.logging.log4j/log4j-core "2.12.1"]
                 [org.slf4j/slf4j-api "1.7.29" ]
                 [org.clojure/core.async "0.5.527"]
                 [org.flatland/ordered "1.5.7"]
                 [org.clojure/data.json "0.2.7"]
                 [org.clojure/tools.logging "0.5.0"]
                 [org.clojure/clojurescript "1.10.597"]]

  :exclusions [org.clojure/clojure]

  :plugins [[cider/cider-nrepl "0.22.4"]
            [lein-cljsbuild "1.1.7"]
            [lein-codox "0.10.7"]
            [lein-cprint "1.3.2"]]

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
                                        "1.10.1" :scope "provided"]]}
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

  :jvm-opts ["-Dlog4j.configurationFile=file:attic/log4j2.xml"
             "-Dczlabloggerflag=true"]

  :javac-options [;"-source" "8"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

