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

(ns ^{:doc "Various boot-clj helpers."
      :author "Kenneth Leung" }

  czlab.pariah.boot

  (:require [boot.task.built-in :refer [install pom aot uber target]]
            [boot.core :as bc]
            [cemerick.pomegranate :as pom]
            [flatland.ordered.map :as fom]
            [flatland.ordered.set :as fos]
            [clojure.data.json :as js]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [czlab.pariah.antlib :as a])

  (:import [clojure.lang APersistentMap APersistentVector]
           [java.util Stack]
           [java.io File]
           [java.util.regex Pattern]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;pull in flatland stuff to force things compiled
(def ^:private dummy-1 (fom/ordered-map))
(def ^:private dummy-2 (fos/ordered-set))

;;default local vars
(defonce ^:private l-vars (atom {}))
;;user vars
(defonce ^:private u-vars (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private spit*
  "Slurp utf-8" [f c] `(spit ~f ~c :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private slurp*
  "Spit utf-8" [f] `(slurp ~f :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private lsfs
  "List files"
  [a & args] `(.listFiles (io/file ~a ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private ficp
  "File's canonical path name"
  [a & args] `(.getCanonicalPath (io/file ~a ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn se! "Set a local var" [k v] (swap! l-vars assoc k v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn localVars "Get the local vars" ^APersistentMap [] @l-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replaceFile!
  "Replace content of a file"
  [file work]
  {:pre [(fn? work)]}
  (spit* file (-> (slurp* file) (work))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gpaths
  "Recurse and look for folders containing
  files with this extension"
  [top out ^String ext]
  (doseq [^File f (lsfs top)
          :let [p (.getParentFile f)
                n (.getName f)]]
    (cond
      (.isDirectory f)
      (gpaths f out ext)
      (.endsWith n ext)
      (when-not
        (contains? @out p)
        (swap! out conj p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- grepFolderPaths
  "Recurse a folder, picking out sub-folders
   which contain files with the given extension"
  [root ^String ext]
  (let [rlen (-> (ficp root)
                 (.length ))
        out (atom [])
        bin (atom #{})]
    (gpaths root bin ext)
    (doseq [k @bin
            :let [kp (ficp k)]]
      (swap! out
             conj
             (.substring kp (inc rlen))))
    @out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fp!
  "Constructs a file path"
  [& args]
  (clojure.string/join "/" args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- glocal
  "Get the value for this local var"
  [k]
  (let [v (@l-vars k)] (if (fn? v) (v k) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ge
  "Get the value for this local var"
  ([k] (ge k false))
  ([k local?]
   (let [rc
         (if local?
           (glocal k)
           (if-some [v (@u-vars k)]
             (if (fn? v) (v k) v)
             (glocal k)))]
     ;;sometimes we really want nothing to be returned
     (if (not= :nichts rc)
       (or rc (bc/get-env k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private minitask
  "Wraps it like an ant task"
  [func & forms]
  `(do (println (str ~func ":")) ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro artifactID
  "Get the name of this artifact" [] `(name (ge :project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro idAndVer
  "Id and version" [] `(str (artifactID) "-" (ge :version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- listCljNsps
  "Generate a list of clojure namespaces
  based on scanning for .clj files recursively"
  ^APersistentVector
  [root & paths]
  (let
    [base #(cs/replace (.getName ^File %) #"\.[^\.]+$" "")
     dot #(cs/replace % "/" ".")
     ffs #(let [^File f %]
            (and (.isFile f)
                 (.endsWith (.getName f) ".clj")))]
    (sort
      (reduce
        (fn [memo path]
          (let [nsp (dot path)]
            (concat
              memo
              (map #(str nsp "." (base %))
                   (filter ffs (lsfs root path))))))
        []
        paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn clrBuild
  "Clean build folders"
  []
  (minitask "clean/build"
    (a/cleanDir (io/file (ge :bootBuildDir)))
    (a/cleanDir (io/file (ge :libDir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn preBuild
  "Prepare build folders"
  []
  (minitask "pre/build"
    (doseq [s (ge :mdirs)]
      (.mkdirs (io/file s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compileJava
  "Compile java files and
  copy resources to output dir"
  []
  (let [ex (ge :exclude-java)]
    (a/runTarget*
      "compile/src/java"
      (a/antJavac
        (ge :javac-opts)
        [[:compilerarg (ge :compiler-args)]
         [:include "**/*.java"]
         [:exclude ex]
         [:classpath (ge :cpath)]])
      (a/antCopy
        {:todir (ge :jczDir)}
        [[:fileset
          {:dir (fp! (ge :srcDir) "java")
           :excludes "**/*.java"}]
         [:fileset
          {:dir (fp! (ge :srcDir) "resources")}]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compileClj
  "Compile clojure files and
  copy resources to output dir"
  []
  (let [root (io/file (ge :srcDir) "clojure")
        ps (grepFolderPaths root ".clj")
        ex (ge :exclude-clj)
        out (atom '())]
    ;;figure out all files(namespaces)
    (doseq [p ps]
      (swap! out
             concat
             (partition-all 24
                            (listCljNsps root p))))
    ;;compile each namespace
    (minitask "compile/src/clojure"
      (doseq
        [p @out
         :let
         [p (filter
              #(let [^String s1 %1]
                 (cond
                   (string? ex)
                   (not (.matches s1 ^String ex))
                   (instance? Pattern ex)
                   (nil? (re-matches ex s1))
                   :else true)) p)]
         :when (> (count p) 0)]
        (a/runTasks*
          (a/antJava
            (ge :cljc-opts)
            (concat [[:argvalues p]]
                    (ge :cjnested)))))
      (a/runTasks*
        (a/antCopy
          {:todir (ge :cczDir)}
           [[:fileset {:dir root
                       :excludes "**/*.clj"}]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- jarFiles
  "Create the jar file"
  []
  (let [d {:excludes "**/log4j.*,**/log4j2.*,**/logback.*"}
        j [:fileset (merge {:dir (ge :jczDir)} d)]
        c [:fileset (merge {:dir (ge :cczDir)} d)]]
    (a/runTarget*
      "jar/files"
      (a/antJar
        {:destFile (fp! (ge :distDir)
                        (str (idAndVer) ".jar"))}
        [j c]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- preTest
  "Prepare for test"
  []
  (minitask
    "pre/test"
    (.mkdirs (io/file (ge :buildTestDir)))
    (.mkdirs (io/file (ge :reportTestDir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compileJavaTest
  "Compile java test files"
  []
  (a/runTarget*
    "compile/test/java"
    (a/antJavac
      (merge (ge :javac-opts)
             {:srcdir (fp! (ge :tstDir) "java")
              :destdir (ge :buildTestDir)})
      [[:include "**/*.java"]
       [:classpath (ge :tpath)]
       [:compilerarg (ge :compiler-args)]])
    (a/antCopy
      {:todir (ge :buildTestDir)}
      [[:fileset {:dir (fp! (ge :tstDir) "java")
                  :excludes "**/*.java"}]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compileCljTest
  "Compile clojure test files"
  []
  (let [root (io/file (ge :tstDir) "clojure")
        ps (grepFolderPaths root ".clj")
        out (atom '())]
    (doseq [p ps]
      (swap! out
             concat
             (partition-all 24
                            (listCljNsps root p))))
    (minitask
      "compile/test/clojure"
      (doseq [p @out]
        (a/runTasks*
          (a/antJava
            (ge :cljc-opts)
            [[:sysprops (assoc (ge :cljc-sys-props)
                               :clojure.compile.path
                               (ge :buildTestDir))]
             [:classpath (ge :tjpath)]
             [:argvalues p]])))
      (a/runTasks*
        (a/antCopy
          {:todir (ge :buildTestDir)}
          [[:fileset {:dir root
                      :excludes "**/*.clj"}]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- runCljTest
  "Execute clojure test cases"
  []
  (a/runTarget*
    "run/test/clojure"
    (a/antJunit
      {:logFailedTests true
       :showOutput false
       :printsummary true
       :fork true
       :haltonfailure true}
      [[:classpath (ge :tjpath)]
       [:formatter {:type "plain"
                    :useFile false}]
       [:test {:name (ge :test-runner)
               :todir (ge :reportTestDir)}
              [[:formatter {:type "xml"}]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- runJavaTest
  "Execute java test cases"
  []
  (a/runTarget*
    "run/test/java"
    (a/antJunit
      {:logFailedTests true
       :showOutput false
       :printsummary true
       :fork true
       :haltonfailure true}
      [[:classpath (ge :tpath)]
       [:formatter {:type "plain"
                    :useFile false}]
       [:batchtest {:todir (ge :reportTestDir)}
                   [[:fileset {:dir (ge :buildTestDir)}
                              [[:include "**/JUnit.*"]]]
                    [:formatter {:type "xml"}]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- buildJavaTest
  "Build for java tests"
  []
  (a/cleanDir (io/file (ge :buildTestDir)))
  (preTest)
  (compileJavaTest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- buildCljTest
  "Build for clojure tests"
  []
  (buildJavaTest)
  (compileCljTest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runCmd
  "Run an external command"
  {:no-doc true}
  [cmd workDir args]
  (a/runTarget*
    (str "cmd:" cmd)
    (a/antExec {:executable cmd
                :dir workDir
                :spawn false}
                [[:argvalues (or args [])]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genDocs
  "Generate api docs"
  []
  (let [rootDir (fp! (ge :packDir) "docs")
        srcDir (ge :srcDir)]
    (a/cleanDir rootDir)
    (a/runTarget*
      "pack/docs"
      (a/antJavadoc
        {:destdir (fp! rootDir "java")
         :access "protected"
         :author true
         :nodeprecated false
         :nodeprecatedlist false
         :noindex false
         :nonavbar false
         :notree false
         :source (ge :java-ver)
         :splitindex true
         :use true
         :version true}
         [[:fileset {:dir (fp! srcDir "java")
                     :includes "**/*.java"}]
          [:classpath (ge :cpath)]])
      (a/antJava
        {:classname "czlab.pariah.codox"
         :fork true
         :failonerror true}
        [[:argvalues [(ge :basedir)
                      (fp! srcDir "clojure")
                      (fp! rootDir "clojure")]]
         [:classpath (ge :cjpath) ]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- bootEnvVars!
  "Basic vars"
  []
  (se! :warnonref :clojure.compile.warn-on-reflection)
  (se! :homedir (System/getProperty "user.home"))
  (se! :basedir (System/getProperty "user.dir"))
  (se! :java-ver "1.8")
  (se! :deps-path "lib")
  (se! :warn-reflection true)
  (se! :pmode "dev")
  (se! :bld "out")
  (se! :cout "z")
  (se! :jcz "j")
  (se! :ccz "c")
  (se! :web "w")
  (se! :bootBuildDir
       #(let [_ %] (fp! (ge :basedir) (ge :bld))))
  (se! :jczDir
       #(let [_ %] (fp! (ge :bootBuildDir) (ge :jcz))))
  (se! :cczDir
       #(let [_ %] (fp! (ge :bootBuildDir) (ge :ccz))))
  (se! :webDir
       #(let [_ %] (fp! (ge :bootBuildDir) (ge :web))))
  (se! :distDir
       #(let [_ %] (fp! (ge :bootBuildDir) "d")))
  (se! :qaDir
       #(let [_ %] (fp! (ge :bootBuildDir) "t")))
  (se! :docs
       #(let [_ %] (fp! (ge :bootBuildDir) "docs")))
  (se! :libDir
       #(let [_ %] (fp! (ge :basedir) (ge :deps-path))))
  (se! :srcDir
       #(let [_ %] (fp! (ge :basedir) "src/main")))
  (se! :tstDir
       #(let [_ %] (fp! (ge :basedir) "src/test")))
  (se! :buildTestDir
       #(let [_ %] (fp! (ge :qaDir) (ge :cout))))
  (se! :reportTestDir
       #(let [_ %] (fp! (ge :qaDir) "r")))
  (se! :packDir
       #(let [_ %] (fp! (ge :bootBuildDir) "p")))
  (se! :mdirs
       #(let [_ %]
          [(ge :bootBuildDir)
           (ge :distDir)
           (ge :libDir)
           (ge :qaDir)
           (ge :cczDir)
           (ge :jczDir)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toggleDoco "Toggle api docs" [b] (se! :wantDocs b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- bootSyncCPath
  "Add these file paths to class-path"
  [& paths] (doseq [p paths] (pom/add-classpath p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- bootEnvPaths!
  "File paths and class paths for builds"
  []
  (se!
    :compiler-args
    #(let [_ %]
       {:line "-Xlint:deprecation -Xlint:unchecked"}))
  (se!
    :compile-opts
    #(let [_ %]
       {:includeantruntime false
        :debug (ge :debug)
        :fork true}))
  (se!
    :cpath
    #(let [_ %]
       [[:location (fp! (ge :basedir) "attic")]
        [:location (ge :jczDir)]
        [:location (ge :cczDir)]
        [:fileset {:dir (ge :libDir)
                   :includes "**/*.jar"}]]))
  (se!
    :tpath
    #(let [_ %]
       (->> (ge :cpath)
            (cons [:location (ge :buildTestDir)])
            (into []))))
  (se!
    :javac-opts
    #(let [_ %]
       (merge {:srcdir (fp! (ge :srcDir) "java")
               :destdir (ge :jczDir)
               :target (ge :java-ver)
               :debugLevel "lines,vars,source"}
              (ge :compile-opts))))
  (se!
    :cjpath
    #(let [_ %]
       (->> (ge :cpath)
            (cons [:location (fp! (ge :srcDir) "clojure")])
            (into []))))
  (se!
    :tjpath
    #(let [_ %]
       (->> (ge :cjpath)
            (concat [[:location (fp! (ge :tstDir) "clojure")]
                     [:location (ge :buildTestDir)]])
            (into []))))
  (se!
    :cljc-opts
    #(let [_ %]
       {:classname "clojure.lang.Compile"
        :fork true
        :failonerror true
        :maxmemory "2048m"}))
  (se!
    :cljc-sys-props
    #(let [_ %]
       {(ge :warnonref) (ge :warn-reflection)
        :clojure.compile.path (ge :cczDir)}))
  (se!
    :cjnested
    #(let [_ %]
       [[:sysprops (ge :cljc-sys-props)]
        [:classpath (ge :cjpath)]]))
  (se!
    :cjnested-raw
    #(let [_ %]
       [[:sysprops (-> (ge :cljc-sys-props)
                       (assoc (ge :warnonref) false))]
        [:classpath (ge :cjpath)]]))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootEnv!
  "Setup env-vars and paths -
  must be called by the user"
  ([] (bootEnv! nil nil))
  ([options] (bootEnv! options nil))
  ([options post]
   (swap! u-vars merge options)
   (bootEnvVars!)
   (bootEnvPaths!)
   (bootSyncCPath (str (ge :jczDir) "/"))
   (if (fn? post) (post))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro bootSpit
  "Write to file"
  [^String data file] `(spit* ~file ~data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSpitJson
  "Write JSON object to file"
  [json file] (bootSpit (js/write-str json) file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro bootSlurp
  "Read file content as string" [file] `(slurp* ~file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bootSlurpJson
  "Read file content as JSON"
  [file]
  (-> (bootSlurp file)
      (js/read-str :key-fn keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn packAll
  "Pack all content"
  {:no-doc true}
  [initor]
  (let [root (ge :packDir)
        dist (ge :distDir)
        ver (ge :version)
        src (ge :srcDir)]
    ;;clean and init the pack dir
    (a/cleanDir root)
    (if (fn? initor)
      (initor root))
    ;; copy license stuff
    (a/runTarget*
      "pack/lics"
      (a/antCopy
        {:todir root}
        [[:fileset
          {:dir (ge :basedir)
           :includes "*.md,LICENSE"}]]))
    ;; copy source
    (a/runTarget*
      "pack/src"
      (a/antCopy
        {:todir (fp! root "src/main/clojure")}
        [[:fileset {:dir (fp! src "clojure")}]])
      (a/antCopy
        {:todir (fp! root "src/main/java")}
        [[:fileset {:dir (fp! src "java")}]]))
    ;; copy distro jars
    (a/runTarget*
      "pack/dist"
      (a/antCopy
        {:todir (fp! root "dist")}
        [[:fileset {:dir dist
                    :includes "*.jar"}]]))
    (a/runTarget*
      "pack/lib"
      (a/antCopy
        {:todir (fp! root "lib")}
        [[:fileset {:dir (ge :libDir)}]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tarAll
  "Tar everything"
  {:no-doc true}
  []
  (let [root (ge :packDir)
        dist (ge :distDir)]
    (a/runTarget*
      "pack/all"
      (a/antTar
        {:destFile (fp! dist (str (idAndVer) ".tar.gz"))
         :compression "gzip"}
        [[:tarfileset {:dir root
                       :excludes "bin/**"}]
         [:tarfileset {:dir root
                       :mode "755"
                       :includes "bin/**"}]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- dumpVars
  ""
  [m]
  (persistent!
    (reduce
      #(let [[k v] %2]
         (assoc! %1 k (ge k)))
      (transient {})
      m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dbgBootVars
  ""
  {:no-doc true}
  []
  {:l-vars (dumpVars @l-vars)
   :u-vars (dumpVars @u-vars)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/task-options!
  uber {:as-jars true}
  aot {:all true})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask testJava
  "Test java"
  []
  (bc/with-pre-wrap fileset
    (buildJavaTest)
    (runJavaTest)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask testClj
  "Test clojure"
  []
  (bc/with-pre-wrap fileset
    (buildCljTest)
    (runCljTest)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask juber
  "Pull all dependent files to target folder"
  []
  (bc/with-pre-wrap fileset
    (let [target (io/file (ge :basedir)
                          (ge :deps-path))
          jars (bc/output-files fileset)
          to (io/file (ge :libDir))]
      (a/runTarget
        "juber"
        (for [j (seq jars)
              :let [dir (:dir j)
                    pn (:path j)
                    ;;boot prepends a hash to the jar file, dunno why,
                    ;;but i dont like it, so ripping it out
                    mt (re-matches #"^[0-9a-z]*-(.*)" pn)]]
          (if (== (count mt) 2)
            (a/antCopy {:file (fp! dir pn)
                        :tofile (fp! to (last mt))})
            (a/antCopy {:file (fp! dir pn)
                        :todir to}))))
      (println (format "copied (%d) jars to %s" (count jars) to))
      fileset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask nullfs
  "Return a empty fileset"
  []
  (bc/with-pre-wrap fileset
    (bc/new-fileset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask libjars
  "Resolve all dependencies (jars)"
  []
  (a/cleanDir (io/file (ge :libDir)))
  (comp (uber)(juber)(nullfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask initBuild
  "Prepare for a build"
  []
  (bc/with-pre-wrap fileset
    (clrBuild)
    (preBuild)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask buildr
  "Compile all source files"
  []
  (bc/with-pre-wrap fileset
    (compileJava)
    (compileClj)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask jar!
  "Create final jar file"
  []
  (bc/with-pre-wrap fileset
    (let [p (str (ge :project))
          v (fp! (ge :jczDir)
                 p
                 "version.properties")]
      (if (.exists (io/file v))
        (replaceFile!
          v
          #(cs/replace %
                       "@@pom.version@@"
                       (ge :version)))))
    (jarFiles)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask postPOM!
  "Write out the pom files to output folder"
  []
  (bc/with-pre-wrap fileset
    (doseq [f (seq (bc/output-files fileset))]
      (let [^String pn (:path f)
            dir (:dir f)
            tf (io/file (ge :jczDir) pn)
            pd (.getParentFile tf)]
        (when (.startsWith pn "META-INF")
          (.mkdirs pd)
          (spit* tf
                (slurp* (fp! dir pn))))))
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask pom!
  "Run the default pom task"
  []
  (comp (nullfs)
        (pom :project (ge :project)
             :version (ge :version))
        (postPOM!)
        (nullfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask packDistro
  "Package all output files into a tar file"
  []
  (bc/with-pre-wrap fileset
    (let []
      (packAll
        (fn [root]
          (map #(.mkdirs (io/file root %))
               ["dist" "lib" "docs"])))
      (if (ge :wantDocs) (genDocs))
      (tarAll)
      nil)
    fileset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(bc/deftask localInstall
  "Install artifact to local maven repo"
  []
  (comp
    (install :file
             (str (ge :distDir)
                  "/" (idAndVer) ".jar"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

