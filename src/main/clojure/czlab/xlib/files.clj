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

(ns ^{:doc "File handling helpers."
      :author "Kenneth Leung" }

  czlab.xlib.files

  (:require
    [czlab.xlib.meta :refer [isBytes?]]
    [czlab.xlib.logging :as log]
    [clojure.java.io :as io]
    [clojure.string :as cs])

  (:use [czlab.xlib.consts]
        [czlab.xlib.core]
        [czlab.xlib.str]
        [czlab.xlib.io])

  (:import
    [java.nio.file Files CopyOption StandardCopyOption]
    [java.util.zip ZipFile ZipEntry]
    [java.util Stack ArrayList]
    [java.io
     ByteArrayOutputStream
     ByteArrayInputStream
     File
     InputStream
     OutputStream
     FileFilter
     FileInputStream
     FileOutputStream]
    [java.net URL URI]
    [czlab.xlib XData]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro slurpUtf8

  "Read contents of f with utf8 encoding"
  {:tag String}
  [f]

  `(slurp ~f :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro spitUtf8

  "Write data to f with utf8 encoding"
  [f c]

  `(spit ~f ~c :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileReadWrite?

  "true if file is readable & writable"
  [^File fp]

  (and (some? fp)
       (.exists fp)
       (.isFile fp)
       (.canRead fp)
       (.canWrite fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fileOK?

  "true if file exists"
  [fp]

  (boolean (some-> ^java.io.File ~fp (.exists))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileRead?

  "true if file is readable"
  [^File fp]

  (and (some? fp)
       (.exists fp)
       (.isFile fp)
       (.canRead fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirReadWrite?

  "true if directory is readable and writable"
  [^File dir]

  (and (some? dir)
       (.exists dir)
       (.isDirectory dir)
       (.canRead dir)
       (.canWrite dir) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirRead?

  "true if directory is readable"
  [^File dir]

  (and (some? dir)
       (.exists dir)
       (.isDirectory dir)
       (.canRead dir) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn canExec?

  "true if file or directory is executable"
  [^File fp]

  (and (some? fp)
       (.exists fp)
       (.canExecute fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro parentFile

  "Parent file"
  {:tag File}
  [f]

  `(some->
     ~(with-meta ~f {:tag 'java.io.File})  (.getParentFile )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parentPath

  "Path to the parent file"
  ^String
  [^String path]

  (if (empty? path)
    path
    (.getParent (io/file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn touch!

  "Touch a file"
  [file]

  (let [f (io/file file)]
    (if-not (.exists f)
      (with-open [os (FileOutputStream. f)])
      (when-not
        (.setLastModified f (System/currentTimeMillis))
        (throwIOE "Unable to set the lastmodtime: %s" f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn changeContent

  "Pass file content to the work function,
  returning new content"
  ^String
  [file work]
  {:pre [(fn? work)]}

  (work (slurpUtf8 file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replaceFile!

  "Update file with new content"
  [file work]
  {:pre [(fn? work)]}

  (spitUtf8 file
            (changeContent file work)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeFile

  "Write data to file"

  ([fout data] (writeFile fout data "utf-8"))

  ([fout data enc]
   (io/copy data fout :encoding (stror enc "utf-8"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cleanZEName

  "Remove leading separators from name"
  ^String
  [^ZipEntry en]

  (.replaceAll (.getName en) "^[\\/]+",""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doOneEntry

  ""
  [^ZipFile src ^File des ^ZipEntry en]

  (let [f (->> (cleanZEName en)
               (io/file des))]
    (if (.isDirectory en)
      (.mkdirs f)
      (do
        (.mkdirs (.getParentFile f))
        (with-open [inp (.getInputStream src en)
                    os (FileOutputStream. f)]
          (copy inp os))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn unzipToDir

  "Unzip zip file to a target folder"
  [^File srcZip ^File desDir]

  (let [z (ZipFile. srcZip)
        es (.entries z)]
    (.mkdirs desDir)
    (while
      (.hasMoreElements es)
      (-> (.nextElement es)
          (doOneEntry z desDir )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn slurpBytes

  "Read bytes from a file"
  ^bytes
  [^File fp]

  (with-open [out (ByteArrayOutputStream. BUF_SZ)]
    (io/copy fp out :buffer-size BUF_SZ)
    (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro readAsStr

  "Read data from source as string"
  {:tag String}

  ([s] `(slurp ~s :encoding "utf-8"))
  ([s enc]
   `(slurp ~s :encoding (stror ~enc "utf-8"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn saveFile

  "Save a file to a directory"

  ([^File dir ^String fname ^XData stuff]
   (saveFile dir fname stuff false))

  ([^File dir ^String fname ^XData stuff del?]
   ;;(log/debug "saving file: %s" fname)
   (let [fp (io/file dir fname)]
     (if del?
       (io/delete-file fp true)
       (if (.exists fp)
         (throwIOE "file %s exists" fp)))
     (if-not (.isFile stuff)
       (writeFile fp (.getBytes stuff))
       (let [opts (make-array CopyOption 1)]
         (aset #^"[Ljava.nio.file.CopyOption;"
               opts
               0 StandardCopyOption/REPLACE_EXISTING)
         (Files/move (.toPath (.fileRef stuff))
                     (.toPath fp)
                     opts)
         ;;since file has moved, update stuff
         (.setDeleteFlag stuff false)
         (.reset stuff fp false))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getFile

  "Get a file from a directory"
  ^XData
  [^File dir ^String fname]

  ;;(log/debug "getting file: %s" fname)
  (let [fp (io/file dir fname)
        xs (xdata<> )]
    (if (fileRead? fp)
      (.reset xs fp false))
    xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti mkdirs "Make directories" {:tag File} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod mkdirs

  String
  [^String f]

  (doto (io/file f) (.mkdirs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod mkdirs

  File
  [^File f]

  (doto f (.mkdirs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listFiles

  "List files with certain extension, without the dot"
  [dir ^String ext]

  (->> (reify FileFilter
         (accept [_ f] (.endsWith (.getName f) ext)))
       (.listFiles (io/file dir))
       (into [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listDirs

  "List sub-directories"
  [dir]

  (->> (reify FileFilter
         (accept [_ f] (.isDirectory f)))
       (.listFiles (io/file dir))
       (into [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- listXXX

  ""
  [^File root ^String ext dirs res]

  (->> (reify FileFilter
         (accept [_ f]
           (cond
             (.isDirectory f)
             (swap! dirs conj f)
             (.endsWith (.getName f) ext)
             (swap! res conj f)
             :else nil)
           false))
       (.listFiles root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listAnyFiles

  "List files with certain extension, without the dot"
  [dir ^String ext]

  (let [dirs (atom [])
        res (atom [])]
    (listXXX dir ext dirs res)
    (while (not-empty @dirs)
      (listXXX
        (first @dirs)
        ext
        (do (swap! dirs next ) dirs)
        res))
    @res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- grep-paths

  "Find folders containing files with this extension"
  [top out fext]

  (doseq [^File f (.listFiles (io/file top))]
    (cond
      (.isDirectory f)
      (grep-paths f out fext)
      (.endsWith (.getName f) fext)
      (let [p (.getParentFile f)]
        (when-not (contains? @out p))
          (swap! out conj p))
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
        bin (atom #{})]
    (grep-paths rootDir bin ext)
    (doseq [k @bin]
      (let [kp (.getCanonicalPath ^File k)]
        (swap! out conj (.substring kp (+ rlen 1)))))
    @out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scan-tree

  "Walk down folder hierarchies"
  [^Stack stk ext out seed]

  (let [^File top (or seed (.peek stk))]
    (doseq [^File f (.listFiles top)]
      (let [p (if (.empty stk)
                '()
                (for [x (.toArray stk)]
                  (.getName ^File x)))
            fid (.getName f)
            paths (conj (into [] p) fid) ]
        (if
          (.isDirectory f)
          (do
            (.push stk f)
            (scan-tree stk ext out nil))
          ;else
          (if (.endsWith fid ext)
            (swap! out conj (cs/join "/" paths)))))))
  (when-not (.empty stk) (.pop stk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn grepFilePaths

  "Recurse a folder, picking out files with the given extension"
  [rootDir ext]

  (let [out (atom [])]
    ;; the stack is used to store the folder hierarchy
    (scan-tree (Stack.) ext out (io/file rootDir))
    @out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn basename

  "Get the name of file without extension"
  [file]

  (let [n (.getName (io/file file))
        p (.lastIndexOf n ".")]
    (if (>= p 0)
      (.substring n 0 p)
      n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


