(set-env!

  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :description ""
  :url "https://github.com/llnek/xlib"

  :exclusions '[javax.servlet/servlet-api]

  :dependencies '[

    [org.apache.ant/ant-apache-log4j "1.9.7" :exclusions [log4j]]
    [ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]
    [org.apache.ant/ant "1.9.7" ]
    [org.apache.ant/ant-launcher "1.9.7" ]
    [org.apache.ant/ant-junit4 "1.9.7" ]
    [org.apache.ant/ant-junit "1.9.7" ]
    [org.apache.logging.log4j/log4j-core "2.5" ]
    [org.slf4j/slf4j-api "1.7.21" ]

    [ch.qos.logback/logback-classic "1.1.7" ]
    [ch.qos.logback/logback-core "1.1.7" ]

    [commons-fileupload/commons-fileupload "1.3.1" ]
    [org.apache.commons/commons-compress "1.11" ]
    [org.apache.commons/commons-lang3 "3.4" ]
    [org.apache.commons/commons-exec "1.3" ]
    [commons-io/commons-io "2.5" ]
    [commons-logging/commons-logging "1.2" ]
    [commons-codec/commons-codec "1.10" ]

    [org.apache.httpcomponents/httpclient "4.5.2" ]
    [org.apache.httpcomponents/httpcore "4.4.4" ]
    [javax.servlet/javax.servlet-api "3.1.0"]
    [joda-time/joda-time "2.9.3" ]

    [org.mozilla/rhino "1.7.7.1" ]
    [jline/jline "2.14.1" ]

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

    [hiccup/hiccup "1.0.5"]
    [enlive/enlive "1.1.6"]
    [codox/codox "0.9.5" ]

    [org.clojure/clojurescript "1.8.51" ]
    [org.clojure/clojure "1.8.0" ]

    [net.mikera/cljunit "0.4.0" ]
    [junit/junit "4.12"  ]

    [ring/ring-core "1.4.0"]

    ;; boot/clj stuff
    [boot/base "2.5.5"]
    [boot/core "2.5.5"]
    [boot/pod "2.5.5"]
    [boot/worker "2.5.5"]
    ;; this is causing the RELEASE_6 warning
    [boot/aether "2.5.5"]

  ]

  :source-paths #{"src/main/clojure" "src/main/java"}
  :test-runner "czlabtest.xlib.ClojureJUnit"
  :version "0.9.0-SNAPSHOT"
  :debug true
  :project 'czlab.xlib
  :PID "czlab-xlib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require
  '[boot.task.built-in :refer [pom target]]
  '[czlab.tpcl.boot
    :as b
    :refer [fp! ge testjava testclj]]
  '[clojure.tools.logging :as log]
  '[clojure.java.io :as io]
  '[clojure.string :as cs]
  '[czlab.tpcl.antlib :as a]
  '[boot.core :as bc])

(import '[org.apache.tools.ant Project Target Task]
        '[java.io File])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:dynamic *genjars* false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(b/bootEnv!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cljXLib ""

  [& args]

  (a/cleanDir (fp! (ge :czzDir) "czlab/xlib"))
  (let [ t1 (a/antJava
              (ge :CLJC_OPTS)
              (concat [[:argvalues (b/listCljNsps
                                     (fp! (ge :srcDir) "clojure")
                                     "czlab/xlib")]]
                      (ge :CJNESTED)))
        t2 (a/antCopy
             {:todir (fp! (ge :czzDir) "czlab/xlib")}
             [[:fileset {:dir (fp! (ge :srcDir) "clojure/czlab/xlib")
                         :excludes "**/*.clj"}]])
        t3 (a/antJar
             {:destFile (fp! (ge :distDir)
                             (str "xlib-" (ge :version) ".jar"))}
             [[:fileset {:dir (ge :czzDir)
                         :includes "czlab/xlib/**"
                         :excludes (str "**/log4j.properties,"
                                        "**/logback.xml")}]])]
    (->> (if *genjars*
           [t1 t2 t3]
           [t1 t2])
         (a/runTarget "clj/xlib"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cljTpcl ""

  [& args]

  (a/cleanDir (fp! (ge :czzDir) "czlab/tpcl"))
  (let [t1 (a/antJava
              (ge :CLJC_OPTS)
              (concat [[:argvalues
                        (b/listCljNsps (fp! (ge :srcDir)
                                           "clojure")
                                      "czlab/tpcl")]]
                      (ge :CJNESTED_RAW)))
        t2 (a/antCopy
              {:todir (fp! (ge :czzDir) "czlab/tpcl")}
              [[:fileset {:dir (fp! (ge :srcDir) "clojure/czlab/tpcl")
                          :excludes "**/*.clj"}]])
        t3 (a/antJar
              {:destFile (fp! (ge :distDir)
                              (str "tpcl-" (ge :version) ".jar"))}
              [[:fileset {:dir (ge :czzDir)
                          :includes "czlab/tpcl/**"
                          :excludes (str "**/log4j.properties,"
                                         "**/logback.xml")}]]) ]
    (->> (if *genjars*
           [t1 t2 t3]
           [t1 t2])
         (a/runTarget "clj/tpcl"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- distroInit ""

  [& args]

  (let [root (io/file (ge :packDir))]
    (a/cleanDir root)
    (doseq [d ["dist" "lib" "docs"]]
      (.mkdirs (io/file root d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- packDocs ""

  [& args]

  (a/cleanDir (fp! (ge :packDir) "docs" "api"))

  (a/runTarget*
    "pack/docs"
    (a/antJavadoc
      {:destdir (fp! (ge :packDir) "docs/api")
       :access "protected"
       :author true
       :nodeprecated false
       :nodeprecatedlist false
       :noindex false
       :nonavbar false
       :notree false
       :source "1.8"
       :splitindex true
       :use true
       :version true}
       [[:fileset {:dir (fp! (ge :srcDir) "java")
                   :includes "**/*.java"}]
        [:classpath (ge :CPATH) ]])

    (a/antJava
      (ge :CLJC_OPTS)
      (concat [[:argvalues ["czlab.tpcl.codox"]]]
              (ge :CJNESTED_RAW)))

    (a/antJava
      {:classname "czlab.tpcl.codox"
       :fork true
       :failonerror true}
      [[:argvalues [(ge :basedir)
                    (fp! (ge :srcDir) "clojure")
                    (fp! (ge :packDir) "docs/api")]]
       [:classpath (ge :CJPATH) ]]) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- packSrc ""

  [& args]

  (a/runTarget*
    "pack/src"
    (a/antCopy
      {:todir (fp! (ge :packDir) "src/main/clojure")}
      [[:fileset {:dir (fp! (ge :srcDir) "clojure")} ]])
    (a/antCopy
      {:todir (fp! (ge :packDir) "src/main/java")}
      [[:fileset {:dir (fp! (ge :srcDir) "java")} ]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- packLics ""

  [& args]

  (a/runTarget*
    "pack/lics"
    (a/antCopy
      {:todir (ge :packDir)}
      [[:fileset {:dir (ge :basedir)
                  :includes "pom.xml,*.md,*.html,*.txt,LICENSE"}]]))
  (b/replaceFile (fp! (ge :packDir) "pom.xml")
                 #(cs/replace % "@@VERSION@@" (ge :version))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- packDist ""

  [& args]

  (a/runTarget*
    "pack/dist"
    (a/antCopy
      {:todir (fp! (ge :packDir) "dist")}
      [[:fileset {:dir (ge :distDir)
                  :includes "*.jar"}]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- packLibs ""

  [& args]

  (a/runTarget*
    "pack/lib"
    (a/antCopy
      {:todir (fp! (ge :packDir) "lib")}
      [[:fileset {:dir (ge :libDir)} ]])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- packAll ""

  [& args]

  (a/runTarget*
    "pack/all"
    (a/antTar
      {:destFile (fp! (ge :distDir)
                      (str (ge :PID)
                           "-"
                           (ge :version) ".tar.gz"))
       :compression "gzip"}
      [[:tarfileset {:dir (ge :packDir)
                     :excludes "bin/**"}]
       [:tarfileset {:dir (ge :packDir)
                     :mode "755"
                     :includes "bin/**"}]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  task defs below !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask prebuild

  "prepare build environment"
  []

  (bc/with-pre-wrap fileset
    ((comp b/preBuild
           (fn [& x]
             (a/cleanDir (ge :packDir)))
           b/clean4Build))
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask javacmp

  "compile java files"
  []

  (bc/with-pre-wrap fileset
    (b/compileJava)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask cljcmp

  "compile clojure files"
  []

  (bc/with-pre-wrap fileset
    (cljTpcl)
    (cljXLib)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask jar!

  "jar all classes"
  []

  (bc/with-pre-wrap fileset
    (b/replaceFile (fp! (ge :jzzDir) "czlab/xlib/version.properties")
                   #(cs/replace % "@@pom.version@@" (ge :version)))
    (b/jarFiles)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask dev
  "for dev only"
  []

  (comp (prebuild)
        (b/libjars)
        (javacmp)
        (cljcmp)
        ;;(pom :project (ge :project) :version (ge :version))
        ;;(target :dir #{(ge :jzzDir)})
        (jar!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask pack
  "internal use only"
  []

  (bc/with-pre-wrap fileset
    (distroInit)
    (packLics)
    (packSrc)
    (packDist)
    (packLibs)
    ;;(packDocs)
    (packAll)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask release
  "release bundle"
  []
  (comp (dev) (pack)))


(deftask poo []
   (bc/with-pre-wrap fileset

     (let [p (output-files fileset)]
       (doseq [x (seq p)]
         (println x)))

    fileset))

(deftask shit []
  (comp (pom :project 'abc/def
             :version "10.1")
        (poo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(doseq [[k v] (get-env)] (println k "=" v))
;;(doseq [k (sort (keys (get-sys-env)))] (println k))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


