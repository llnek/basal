(set-env!

  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :description ""
  :url "https://github.com/llnek/xlib"

  :exclusions '[javax.servlet/servlet-api]

  :dependencies '[

    [ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]

    [org.slf4j/slf4j-api "1.7.21" ]
    [ch.qos.logback/logback-classic "1.1.7" ]
    [ch.qos.logback/logback-core "1.1.7" ]

    [org.apache.ant/ant-launcher "1.9.7" ]
    [org.apache.ant/ant-junit4 "1.9.7" ]
    [org.apache.ant/ant-junit "1.9.7" ]
    [org.apache.ant/ant "1.9.7" ]

    [org.clojure/tools.logging "0.3.1" ]
    [org.clojure/data.json "0.2.6" ]
    [org.flatland/ordered "1.5.3"]

    [com.cemerick/pomegranate "0.3.1" :scope "provided"]
    [codox/codox "0.9.5" :scope "provided"]

    [org.clojure/clojure "1.8.0" ]

    [net.mikera/cljunit "0.4.1" :scope "provided"]
    [junit/junit "4.12"  :scope "provided"]

    ;;[org.projectodd.shimdandy/shimdandy-impl "1.1.0"]
    ;;[org.projectodd.shimdandy/shimdandy-api "1.2.0"]

    ;; this is causing the RELEASE_6 warning
    ;; boot/clj stuff
    [boot/base "2.6.0" :scope "provided"]
    [boot/core "2.6.0" :scope "provided"]
    [boot/pod "2.6.0" :scope "provided"]
    [boot/worker "2.6.0" :scope "provided"]
    [boot/aether "2.6.0" :scope "provided"]

  ]

  :source-paths #{"src/main/clojure" "src/main/java"}
  :test-runner "czlabtest.xlib.ClojureJUnit"
  :version "1.0.0"
  :debug true
  :project 'czlab/czlab-xlib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require
  '[czlab.tpcl.boot
    :as b
    :refer [artifactID fp! se! ge]]
  '[clojure.tools.logging :as log]
  '[clojure.java.io :as io]
  '[clojure.string :as cs]
  '[czlab.xlib.antlib :as a]
  '[boot.core :as bc])

(import '[java.io File])

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
(deftask rel

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


