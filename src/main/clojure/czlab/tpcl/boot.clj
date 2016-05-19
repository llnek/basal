;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Copyright (c) 2013-2016, Kenneth Leung. All rights reserved.

(ns ^{:doc ""
      :author "kenl" }

  czlab.tpcl.boot

  (:require
    [boot.task.built-in :refer [uber aot]]
    [czlab.xlib.logging :as log]
    [clojure.data.json :as js]
    [cemerick.pomegranate :as pom]
    [clojure.java.io :as io]
    [boot.core :as bc :refer :all]
    [clojure.string :as cs]
    [czlab.tpcl.antlib :as a])

  (:import
    [java.util Stack]
    [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replaceFile ""

  [file work]

  {:pre [(fn? work)]}

  (spit file
        (-> (slurp file :encoding "utf-8")
              (work))
        :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- grep-paths ""

  [top bin fext]

  (doseq [f (.listFiles (io/file top))]
    (cond
      (.isDirectory f)
      (grep-paths f bin fext)
      (.endsWith (.getName f) fext)
      (let [p (.getParentFile f)]
        (when-not (contains? @bin p))
          (swap! bin assoc p p))
      :else nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn grepFolderPaths

  "Recurse a folder, picking out sub-folders
   which contain files with the given extension"
  [rootDir ext]

  (let [rpath (.getCanonicalPath (io/file rootDir))
        rlen (.length rpath)
        out (atom [])
        bin (atom {})]
    (grep-paths rootDir bin ext)
    (doseq [[k v] @bin]
      (let [kp (.getCanonicalPath ^File k)]
        (swap! out conj (.substring kp (+ rlen 1)))))
    @out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ge

  "get-env"
  [expr]

  `(bc/get-env ~expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fp!

  "Constructs a file path"
  [& args]

  (cs/join "/" args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn se!

  "Local version of set-env!"

  [options k dv]

  (let [v (get options k)]
    (if (fn? v)
      (v options k)
      (set-env! k (or v dv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro minitask

  "Wraps it like an ant task"
  [func & forms]

  `(do (println (str ~func ":")) ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listCljNsps

  "Format a list of clojure namespaces"

  [root & paths]

  (let [base #(cs/replace (.getName %) #"\.[^\.]+$" "")
        dot #(cs/replace % "/" ".")
        ffs #(and (.isFile %)
                  (.endsWith (.getName %) ".clj"))]
    (sort
      (reduce
        (fn [memo path]
          (let [nsp (dot path)]
            (concat
              memo
              (map #(str nsp "." (base %))
                   (filter ffs
                           (.listFiles (io/file root path)))))))
        []
        paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn clean4Build ""

  [& args]

  (a/runTarget*
    "clean/build"
    (a/antDelete {:dir (ge :bootBuildDir)
                  :excludes (str (ge :czz) "/**")})
    (a/antDelete {}
      [[:fileset {:dir (ge :czzDir)
                  :excludes "clojure/**"}]]))
  (a/cleanDir (io/file (ge :libDir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn preBuild ""

  [& args]

  (minitask "prebuild"
    (doseq [s [(ge :bootBuildDir)
               (ge :distDir)
               (ge :libDir)
               (ge :qaDir)
               ;;(ge :wzzDir)
               (ge :czzDir)
               (ge :jzzDir)]]
      (.mkdirs (io/file s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn compileJava ""

  []

  (a/runTarget*
    "compile/java"
    (a/antJavac
      (ge :JAVAC_OPTS)
      [[:compilerarg (ge :COMPILER_ARGS)]
       [:include "**/*.java"]
       [:classpath (ge :CPATH)]])
    (a/antCopy
      {:todir (ge :jzzDir)}
      [[:fileset {:dir (fp! (ge :srcDir) "java")
                  :excludes "**/*.java"}]])
    (a/antCopy
      {:todir (ge :jzzDir)}
      [[:fileset {:dir (fp! (ge :srcDir) "resources")}]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn compileClj ""

  []

  (let [root (io/file (ge :srcDir) "clojure")
        ps (grepFolderPaths root ".clj")
        bin (atom '())]
    (doseq [p ps]
      (swap! bin concat (partition-all 25 (listCljNsps root p))))
    (minitask "compile/clj"
      (doseq [p @bin]
        (a/runTasks*
          (a/antJava
            (ge :CLJC_OPTS)
            (concat [[:argvalues p ]] (ge :CJNESTED)))))
      (a/runTasks*
        (a/antCopy
              {:todir (ge :czzDir)}
              [[:fileset {:dir root
                          :excludes "**/*.clj"}]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jarFiles ""

  []

  (let [j [:fileset
           {:dir (ge :jzzDir)
            :excludes "**/log4j.properties,**/logback.xml"} ]
        c [:fileset
           {:dir (ge :czzDir)
            :excludes "**/log4j.properties,**/logback.xml"} ]]
    (a/runTarget*
      "jar/files"
      (a/antJar
        {:destFile (fp! (ge :distDir)
                        (str (ge :PID)
                             "-"
                             (ge :buildVersion) ".jar"))}
        [j c]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn preTest ""

  []

  (minitask
    "pretest"
    (.mkdirs (io/file (ge :buildTestDir)))
    (.mkdirs (io/file (ge :reportTestDir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn compileJavaTest ""

  []

  (a/runTarget*
    "compile/test/java"
    (a/antJavac
      (merge (ge :JAVAC_OPTS)
             {:srcdir (fp! (ge :tstDir) "java")
              :destdir (ge :buildTestDir)})
      [[:include "**/*.java"]
       [:classpath (ge :TPATH)]
       [:compilerarg (ge :COMPILER_ARGS)]])
    (a/antCopy
      {:todir (ge :buildTestDir)}
      [[:fileset {:dir (fp! (ge :tstDir) "java")
                  :excludes "**/*.java"}]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn compileCljTest ""

  []

  (let [root (io/file (ge :tstDir) "clojure")
        ps (grepFolderPaths root ".clj")
        bin (atom '())]
    (doseq [p ps]
      (swap! bin concat (partition-all 25 (listCljNsps root p))))
    (minitask
      "compile/test/clj"
      (doseq [p @bin]
        (a/runTasks*
          (a/antJava
            (ge :CLJC_OPTS)
            [[:sysprops (assoc (ge :CLJC_SYSPROPS)
                               :clojure.compile.path (ge :buildTestDir))]
             [:classpath (ge :TJPATH)]
             [:argvalues p]])))
      (a/runTasks*
        (a/antCopy
          {:todir (ge :buildTestDir)}
          [[:fileset {:dir root
                      :excludes "**/*.clj"}]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runCljTest ""

  []

  (a/runTarget*
    "run/test/clj"
    (a/antJunit
      {:logFailedTests true
       :showOutput false
       :printsummary true
       :fork true
       :haltonfailure true}
      [[:classpath (ge :TJPATH)]
       [:formatter {:type "plain"
                    :useFile false}]
       [:test {:name (ge :test-runner)
               :todir (ge :reportTestDir)}
              [[:formatter {:type "xml"}]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runJavaTest ""

  []

  (a/runTarget*
    "run/test/java"
    (a/antJunit
      {:logFailedTests true
       :showOutput false
       :printsummary true
       :fork true
       :haltonfailure true}
      [[:classpath (ge :TPATH)]
       [:formatter {:type "plain"
                    :useFile false}]
       [:batchtest {:todir (ge :reportTestDir)}
                   [[:fileset {:dir (ge :buildTestDir)}
                              [[:include "**/JUnit.*"]]]
                    [:formatter {:type "xml"}]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn buildCljTest ""

  []

  (a/cleanDir (io/file (ge :buildTestDir)))
  (preTest)
  (compileJavaTest)
  (compileCljTest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn buildJavaTest ""

  []

  (a/cleanDir (io/file (ge :buildTestDir)))
  (preTest)
  (compileJavaTest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runCmd ""

  [cmd workDir args]

  (a/runTarget*
    (str "cmd:" cmd)
    (a/antExec {:executable cmd
                :dir workDir
                :spawn false}
                [[:argvalues (or args [])]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootEnvVars! "Basic vars"

  [& [options]]

  (let [options (or options {})]

    (se! options :basedir (System/getProperty "user.dir"))

    (se! options :target-path "target")

    (se! options
         :warnonref
         :clojure.compile.warn-on-reflection)
    (se! options :warn-reflection true)


    ;;(se! options :classes "classes")
    (se! options :cout "z")
    (se! options :jzz "j")
    (se! options :czz "c")
    ;;(se! options :wzz "w")

    (se! options :bld "build")
    (se! options :pmode "dev")

    (se! options :bootBuildDir (fp! (ge :basedir) (ge :bld)))

    ;;(se! options :clzDir (fp! (ge :bootBuildDir) (ge :classes)))
    (se! options :jzzDir (fp! (ge :bootBuildDir) (ge :jzz)))
    (se! options :czzDir (fp! (ge :bootBuildDir) (ge :czz)))
    ;;(se! options :wzzDir (fp! (ge :bootBuildDir) (ge :wzz)))

    (se! options :distDir (fp! (ge :bootBuildDir) "d"))
    (se! options :qaDir (fp! (ge :bootBuildDir) "t"))
    (se! options :docs (fp! (ge :bootBuildDir) "docs"))

    (se! options :libDir (fp! (ge :basedir)
                              (ge :target-path)))

    (se! options :srcDir (fp! (ge :basedir) "src" "main"))
    (se! options :tstDir (fp! (ge :basedir) "src" "test"))

    (se! options :reportTestDir (fp! (ge :qaDir) "reports"))
    (se! options :buildTestDir (fp! (ge :qaDir) (ge :cout)))
    (se! options :packDir (fp! (ge :bootBuildDir) "p"))

    (doseq [[k v] options]
      (when (nil? (get-env k))
        (se! options k v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSyncCPath ""

  [& paths]

  (doseq [p paths]
    (pom/add-classpath p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootEnvPaths! "Compiler paths and options"

  [& [options]]

  (let [options (or options {})]

    (se! options
         :COMPILER_ARGS
         {:line "-Xlint:deprecation -Xlint:unchecked"})

    (se! options
         :COMPILE_OPTS
         {:includeantruntime false
          :debug (ge :buildDebug)
          :fork true})

    (se! options
         :CPATH
         [;;[:location (ge :clzDir)]
          [:location (ge :jzzDir)]
          [:location (ge :czzDir)]
          [:fileset {:dir (ge :libDir)
                     :includes "**/*.jar"}]])

    (se! options
         :TPATH
         (->> (ge :CPATH)
              (cons [:location (ge :buildTestDir)])
              (into [])))

    (se! options
         :JAVAC_OPTS
         (merge {:srcdir (fp! (ge :srcDir) "java")
                 :destdir (ge :jzzDir)
                 :target "1.8"
                 :debugLevel "lines,vars,source"}
                (ge :COMPILE_OPTS)))

    (se! options
         :CJPATH
         (->> (ge :CPATH)
              (cons [:location (fp! (ge :srcDir) "clojure")])
              (into [])))

    (se! options
         :TJPATH
         (->> (ge :CJPATH)
              (concat [[:location (fp! (ge :tstDir) "clojure")]
                       [:location (ge :buildTestDir)]])
              (into [])))

    (se! options
         :CLJC_OPTS
         {:classname "clojure.lang.Compile"
          :fork true
          :failonerror true
          :maxmemory "2048m"})

    (se! options
         :CLJC_SYSPROPS
         {(ge :warnonref) (ge :warn-reflection)
          :clojure.compile.path (ge :czzDir)})

    (se! options
         :CJNESTED
         [[:sysprops (ge :CLJC_SYSPROPS)]
          [:classpath (ge :CJPATH)]])

    (se! options
         :CJNESTED_RAW
         [[:sysprops
           (-> (ge :CLJC_SYSPROPS)
               (assoc (ge :warnonref) false))]
          [:classpath (ge :CJPATH)]])

    (doseq [[k v] options]
      (when (nil? (get-env k))
        (se! options k v)))

    (bootSyncCPath (str (ge :jzzDir) "/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootEnv! "Setup env-vars and paths"

  [& [options]]

  (let [options (or options {})]
    (bootEnvVars! options)
    (bootEnvPaths! options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSpit ""

  [^String s file]

  (spit file s :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSpitJson ""

  [json file]

  (bootSpit (js/write-str json) file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSlurp ""

  ^String
  [file]

  (slurp file :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSlurpJson ""

  [file]

  (-> (bootSlurp file)
      (js/read-str :key-fn keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(task-options!
  uber {:as-jars true}
  aot {:all true})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask testjava

  "test java"

  []

  (bc/with-pre-wrap fileset
    (buildJavaTest)
    (runJavaTest)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask testclj

  "test clj"

  []

  (bc/with-pre-wrap fileset
    (buildCljTest)
    (runCljTest)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask juber

  "my own uber"

  []

  (bc/with-pre-wrap fileset
    (let [from (io/file (ge :basedir)
                        (ge :target-path))
          jars (output-files fileset)
          to (io/file (ge :libDir))]
      (a/runTarget
        "juber"
        (for [j (seq jars)
              :let [dir (:dir j)
                    pn (:path j)
                    ;;boot prepends a hash to the jar file, dunno why,
                    ;;but i dont like it, so ripping it out
                    mt (re-matches #"^[0-9a-z]*-(.*)" pn)]]
          (if (= (count mt) 2)
            (a/antCopy {:file (fp! dir pn)
                        :tofile (fp! to (last mt))})
            (a/antCopy {:file (fp! dir pn)
                        :todir to}))))
      (println (format "copied (%d) jars to %s" (count jars) to))
    fileset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask libjars

  "resolve all dependencies (jars)"

  []

  (a/cleanDir (io/file (ge :libDir)))
  (comp (uber)(juber)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask clean4build

  "clean,pre-build"

  []

  (bc/with-pre-wrap fileset
    (clean4Build)
    (preBuild)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask buildr

  "compile"

  []

  (bc/with-pre-wrap fileset
    (compileJava)
    (compileClj)
  fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask dev

  "clean,resolve,build"

  []

  (comp (clean4build)
        (libjars)
        (buildr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftask jar!

  "jar!"

  []

  (bc/with-pre-wrap fileset
    (jarFiles)
  fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


