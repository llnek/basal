(set-env!

  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :description ""
  :url "https://github.com/llnek/xlib"

  :exclusions '[javax.servlet/servlet-api]

  :dependencies '[

    [org.apache.ant/ant-apache-log4j "1.9.7" :exclusions [log4j]]
    [ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]

    [org.apache.logging.log4j/log4j-core "2.5" ]
    [org.slf4j/slf4j-api "1.7.21" ]
    [ch.qos.logback/logback-classic "1.1.7" ]
    [ch.qos.logback/logback-core "1.1.7" ]

    [org.apache.ant/ant-launcher "1.9.7" ]
    [org.apache.ant/ant-junit4 "1.9.7" ]
    [org.apache.ant/ant-junit "1.9.7" ]
    [org.apache.ant/ant "1.9.7" ]

    [commons-fileupload/commons-fileupload "1.3.1" ]
    [org.apache.commons/commons-compress "1.11" ]
    [org.apache.commons/commons-lang3 "3.4" ]
    [org.apache.commons/commons-exec "1.3" ]
    [commons-io/commons-io "2.5" ]
    [commons-logging/commons-logging "1.2" ]
    [commons-codec/commons-codec "1.10" ]

    ;;[org.apache.httpcomponents/httpclient "4.5.2" ]
    ;;[org.apache.httpcomponents/httpcore "4.4.4" ]
    ;;[javax.servlet/javax.servlet-api "3.1.0"]
    [joda-time/joda-time "2.9.3" ]

    ;;[org.mozilla/rhino "1.7.7.1" ]
    ;;[jline/jline "2.14.1" ]

    [com.google.code.gson/gson "2.6.2" ]
    [com.google.guava/guava "19.0" ]

    [org.clojure/data.priority-map "0.0.7" ]
    [org.clojure/data.xml "0.0.8" ]
    [org.clojure/tools.reader "0.10.0" ]
    [org.clojure/tools.logging "0.3.1" ]
    [org.clojure/core.memoize "0.5.9" ]
    [org.clojure/core.async "0.2.374" ]
    [org.clojure/data.json "0.2.6" ]
    [org.clojure/core.cache "0.6.5" ]
    [org.flatland/ordered "1.5.3"]
    [com.cemerick/pomegranate "0.3.1"]

    ;;[hiccup/hiccup "1.0.5"]
    ;;[enlive/enlive "1.1.6"]
    [codox/codox "0.9.5" :scope "provided"]

    ;;[org.clojure/clojurescript "1.8.51" ]
    [org.clojure/clojure "1.8.0" ]

    [net.mikera/cljunit "0.4.1" ]
    [junit/junit "4.12"  ]

    ;;[ring/ring-core "1.4.0"]

    ;;[org.projectodd.shimdandy/shimdandy-impl "1.1.0"]
    ;;[org.projectodd.shimdandy/shimdandy-api "1.2.0"]

    ;; boot/clj stuff
    [boot/base "2.6.0" :scope "provided"]
    [boot/core "2.6.0" :scope "provided"]
    ;;[boot/pod "2.6.0" :scope "provided"]
    [boot/worker "2.6.0" :scope "provided"]
    ;; this is causing the RELEASE_6 warning
    ;;[boot/aether "2.6.0" :scope "provided"]

  ]

  :source-paths #{"src/main/clojure" "src/main/java"}
  :test-runner "czlabtest.xlib.ClojureJUnit"
  :version "1.0.0"
  :debug true
  :project 'czlab/czlab-xlib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require
  '[boot.task.built-in :refer [pom target]]
  '[czlab.tpcl.boot
    :as b
    :refer [artifactID fp! se! ge]]
  '[clojure.tools.logging :as log]
  '[clojure.java.io :as io]
  '[clojure.string :as cs]
  '[czlab.xlib.antlib :as a]
  '[boot.pom :as bp]
  '[boot.core :as bc])

(import '[org.apache.tools.ant
          Project
          Target
          Task]
        '[java.io File])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(b/bootEnv!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  task defs below !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask cljTpcl

  ""
  []

  (bc/with-pre-wrap fileset
    (a/runTarget*
      "clj/tpcl"
      (a/antJava
        (ge :CLJC_OPTS)
        (concat [[:argvalues
                  (b/listCljNsps (fp! (ge :srcDir)
                                      "clojure") "czlab/tpcl")]]
                (ge :CJNESTED_RAW))))
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask tst

  "test all"
  []

  (comp (b/testJava)
        (b/testClj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask dev

  "for dev only"
  []

  (comp (b/initBuild)
        (b/libjars)
        (cljTpcl)
        (b/buildr)
        (b/pom!)
        (b/jar!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask release

  ""
  [d doco bool "Generate doc"]

  (b/toggleDoco doco)
  (comp (dev)
        (b/localInstall)
        (b/packDistro)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(doseq [[k v] (get-env)] (println k "=" v))
;;(doseq [k (sort (keys (get-sys-env)))] (println k))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


