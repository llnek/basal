(set-env!
  :dependencies '[

    [org.bouncycastle/bcprov-jdk15on "1.54" ]
    [org.bouncycastle/bcmail-jdk15on "1.54" ]
    [org.bouncycastle/bcpkix-jdk15on "1.54" ]
    [org.jasypt/jasypt "1.9.2" ]
    ;;[org.mindrot/jbcrypt "0.3m" ]

    [org.slf4j/slf4j-api "1.7.21" ]
    [org.apache.logging.log4j/log4j-core "2.5" ]

    [ch.qos.logback/logback-classic "1.1.7" ]
    [ch.qos.logback/logback-core "1.1.7" ]

    [net.sourceforge.jregex/jregex "1.2_01" ]
    [net.sf.jopt-simple/jopt-simple "5.0.1" ]
    [com.google.guava/guava "19.0" ]
    [com.google.code.findbugs/jsr305 "3.0.1" ]
    [joda-time/joda-time "2.9.3" ]
    ;;[org.zeroturnaround/zt-exec "1.8" ]
    ;;[org.zeroturnaround/zt-zip "1.8" ]
    [org.apache.axis/axis "1.4" ]
    [org.apache.axis/axis-jaxrpc "1.4" ]
    ;;[org.jetlang/jetlang "0.2.12" ]

    [com.fasterxml.jackson.core/jackson-core "2.7.3" ]
    [com.fasterxml.jackson.core/jackson-databind "2.7.3" ]
    [com.fasterxml.jackson.core/jackson-annotations "2.7.3" ]
    [org.jdom/jdom2 "2.0.6" ]

    [com.google.code.gson/gson "2.6.2" ]

    [org.apache.commons/commons-compress "1.11" ]
    [org.apache.commons/commons-lang3 "3.4" ]
    [org.apache.commons/commons-exec "1.3" ]
    [commons-net/commons-net "3.4" ]
    [commons-io/commons-io "2.5" ]

    [commons-logging/commons-logging "1.2" ]
    [org.apache.commons/commons-email "1.4" ]
    [commons-codec/commons-codec "1.10" ]
    [commons-fileupload/commons-fileupload "1.3.1" ]
    [commons-dbutils/commons-dbutils "1.6" ]
    [com.sun.mail/javax.mail "1.5.5" ]

    ;;[org.apache.ivy/ivy "2.4.0" ]
    [org.apache.ant/ant "1.9.7" ]
    [org.apache.ant/ant-launcher "1.9.7" ]
    [org.apache.ant/ant-junit4 "1.9.7" ]
    [org.apache.ant/ant-junit "1.9.7" ]
    [org.apache.ant/ant-apache-log4j "1.9.7" :exclusions [log4j]]

    [ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]

    [com.jolbox/bonecp "0.8.0.RELEASE" ]

    [org.apache.httpcomponents/httpcore-nio "4.4.4" ]
    [org.apache.httpcomponents/httpcore "4.4.4" ]
    [org.apache.httpcomponents/httpclient "4.5.2" ]
    [io.netty/netty-all "4.0.36.Final" ]

    ;;[com.corundumstudio.socketio/netty-socketio "1.7.10" :exclusions [io.netty]]

    [org.eclipse.jetty/jetty-xml "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-server "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-continuation "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-servlet "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-server "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-util "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-security "9.3.8.v20160314"  ]
    [org.eclipse.jetty/jetty-webapp "9.3.8.v20160314"  ]
    [org.eclipse.jetty.websocket/websocket-api "9.3.8.v20160314"  ]
    [org.eclipse.jetty.websocket/websocket-common "9.3.8.v20160314"  ]
    [org.eclipse.jetty.websocket/websocket-servlet "9.3.8.v20160314"  ]
    [org.eclipse.jetty.websocket/websocket-client "9.3.8.v20160314"  ]
    [org.eclipse.jetty.websocket/websocket-server "9.3.8.v20160314"  ]

    ;;[org.codehaus.ga/gant_groovy2.4 "1.9.12" ]
    ;;[org.codehaus.groovy/groovy-all "2.4.6" ]

    [com.sun.tools/tools "1.8.0" ]
    [org.javassist/javassist "3.20.0-GA"  ]

    [com.github.spullara.mustache.java/compiler "0.9.1" ]

    [org.freemarker/freemarker "2.3.23" ]

    [com.yahoo.platform.yui/yuicompressor "2.4.8"  :exclusions [rhino]]

    [org.apache.geronimo.specs/geronimo-jms_1.1_spec "1.1.1" ]
    [com.h2database/h2 "1.4.191" ]
    [org.postgresql/postgresql "9.4.1208.jre7" ]

    [org.clojure/math.numeric-tower "0.0.4" ]
    [org.clojure/math.combinatorics "0.1.1" ]
    [org.clojure/tools.logging "0.3.1" ]
    [org.clojure/tools.nrepl "0.2.12" ]
    [org.clojure/tools.reader "0.10.0" ]
    [org.clojure/data.codec "0.1.0" ]
    [org.clojure/data.csv "0.1.3" ]
    [org.clojure/java.jdbc "0.5.8" ]
    [org.clojure/java.data "0.1.1" ]
    [org.clojure/java.jmx "0.3.1" ]
    [org.clojure/data.json "0.2.6" ]
    [org.clojure/data.xml "0.0.8" ]
    [org.clojure/core.cache "0.6.5" ]
    [org.clojure/core.match "0.2.2" ]
    [org.clojure/core.memoize "0.5.9" ]
    [org.clojure/tools.analyzer.jvm "0.6.9"]
    [org.clojure/tools.analyzer "0.6.7"]
    [org.clojure/tools.cli "0.3.3" ]
    [org.clojure/data.generators "0.1.2" ]
    [org.clojure/data.priority-map "0.0.7" ]
    [org.clojure/core.async "0.2.374" ]
    [org.clojure/core.logic "0.8.10" ]
    [org.clojure/algo.monads "0.1.5" ]
    [org.clojure/algo.generic "0.1.2" ]
    [org.clojure/core.memoize "0.5.9" ]
    [org.flatland/ordered "1.5.3"]
    [com.cemerick/pomegranate "0.3.1"]
    [codox/codox "0.9.5" ]

    ;; 1.2.0 screws up skaro runtime
    [org.projectodd.shimdandy/shimdandy-impl "1.1.0"]
    [org.projectodd.shimdandy/shimdandy-api "1.2.0"]

    ;; boot/clj stuff

    [boot/base "2.5.5"
     :exclusions [javax.servlet/servlet-api
                  org.projectodd.shimdandy/shimdandy-impl
                  org.projectodd.shimdandy/shimdandy-api]]
    [boot/core "2.5.5"
     :exclusions [javax.servlet/servlet-api
                  org.projectodd.shimdandy/shimdandy-impl
                  org.projectodd.shimdandy/shimdandy-api]]
    [boot/pod "2.5.5"
     :exclusions [javax.servlet/servlet-api
                  org.projectodd.shimdandy/shimdandy-impl
                  org.projectodd.shimdandy/shimdandy-api]]
    [boot/worker "2.5.5"
     :exclusions [javax.servlet/servlet-api
                  org.projectodd.shimdandy/shimdandy-impl
                  org.projectodd.shimdandy/shimdandy-api]]
    ;; this is causing the RELEASE_6 warning
    [boot/aether "2.5.5"
     :exclusions [javax.servlet/servlet-api
                  org.projectodd.shimdandy/shimdandy-impl
                  org.projectodd.shimdandy/shimdandy-api]]

    [org.clojure/clojure "1.8.0" ]
    [org.clojure/clojurescript "1.8.51" ]

    [org.apache.shiro/shiro-core "1.2.4" ]
    [org.mozilla/rhino "1.7.7.1" ]
    [jline/jline "2.14.1" ]

    [net.mikera/cljunit "0.4.0" ]
    [junit/junit "4.12"  ]
    [com.googlecode.jslint4java/jslint4java "2.0.5" ]

  ]

  :source-paths #{"src/main/clojure" "src/main/java"}
  :buildVersion "0.9.0-SNAPSHOT"
  :buildDebug true
  :test-runner "czlabtest.xlib.ClojureJUnit"
  :DOMAIN "czlab.xlib"
  :PID "xlib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require
  '[czlab.tpcl.boot
    :as b
    :refer [fp! ge testjava testclj]]
  '[clojure.tools.logging :as log]
  '[clojure.java.io :as io]
  '[clojure.string :as cs]
  '[czlab.tpcl.antlib :as a]
  '[czlab.xlib.files :as fs]
  '[czlab.xlib.core :as co]
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
              (concat [[:argvalues (b/fmtCljNsps
                                     (fp! (ge :srcDir) "clojure")
                                     "czlab/xlib")]]
                      (ge :CJNESTED)))
        t2 (a/antCopy
             {:todir (fp! (ge :czzDir) "czlab/xlib")}
             [[:fileset {:dir (fp! (ge :srcDir) "clojure/czlab/xlib")
                         :excludes "**/*.clj"}]])
        t3 (a/antJar
             {:destFile (fp! (ge :distDir)
                             (str "xlib-" (ge :buildVersion) ".jar"))}
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
                        (b/fmtCljNsps (fp! (ge :srcDir)
                                           "clojure")
                                      "czlab/tpcl")]]
                      (ge :CJNESTED_RAW)))
        t2 (a/antCopy
              {:todir (fp! (ge :czzDir) "czlab/tpcl")}
              [[:fileset {:dir (fp! (ge :srcDir) "clojure/czlab/tpcl")
                          :excludes "**/*.clj"}]])
        t3 (a/antJar
              {:destFile (fp! (ge :distDir)
                              (str "tpcl-" (ge :buildVersion) ".jar"))}
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
    (a/antCopy
      {:todir (fp! (ge :packDir) "docs")}
      [[:fileset {:dir (fp! (ge :basedir) "docs")
                  :excludes "dummy.txt"}]])
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
                 #(cs/replace % "@@VERSION@@" (ge :buildVersion))))

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
                           (ge :buildVersion) ".tar.gz"))
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
                   #(cs/replace % "@@pom.version@@" (ge :buildVersion)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


