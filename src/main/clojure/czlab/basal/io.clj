;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "IO, streams and file helpers."
      :author "Kenneth Leung"}

  czlab.basal.io

  (:require [czlab.basal.logging :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [czlab.basal.meta]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.nio.file Files CopyOption StandardCopyOption]
           [java.util.zip GZIPInputStream GZIPOutputStream]
           [java.util Base64 Base64$Decoder Base64$Encoder]
           [clojure.lang APersistentVector]
           [java.util.zip ZipFile ZipEntry]
           [java.util Stack ArrayList]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset Charset]
           [java.net URL URI]
           [java.io
            ByteArrayOutputStream
            ByteArrayInputStream
            DataInputStream
            DataOutputStream
            FileInputStream
            FileOutputStream
            CharArrayWriter
            OutputStreamWriter
            IOException
            FileFilter
            File
            InputStream
            InputStreamReader
            Closeable
            OutputStream
            Reader
            Writer]
           [czlab.jasal XData XStream]
           [org.xml.sax InputSource]
           [java.nio.charset Charset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:dynamic *tempfile-repo* (io/file (sysTmpDir)))
(def ^:dynamic *membuf-limit* (* 4 MegaBytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn streamit
  "Wrapped these bytes in an input-stream"
  ^InputStream [^bytes bytess] (some-> bytess ByteArrayInputStream. ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn baos<>
  "Make a byte array output stream"
  {:tag ByteArrayOutputStream}

  ([] (baos<> nil))
  ([size]
   (ByteArrayOutputStream. (int (or size BUF_SZ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- copyAll
  "" [^InputStream input ^OutputStream output]

  (loop [buf (byte-array BUF_SZ)
         cnt 0
         n (.read input buf)]
    (if (< n 0)
      cnt
      (do
        (if (> n 0)
          (.write output buf 0 n))
        (recur buf
               (+ n cnt)
               (.read input buf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyBytes
  "Copy certain number of bytes to output"
  [^InputStream input ^OutputStream output ^long kount]

  (let [buf (byte-array BUF_SZ)
        bsz (alength buf)]
    (loop [remain kount
           total 0]
      (let [len (if (< remain bsz)
                  remain bsz)
            n (if (> len 0)
                (.read input buf 0 len) -1)]
        (if (< n 0)
          total
          (do
            (if (> n 0)
              (.write output buf 0 n))
            (recur (long (- remain n))
                   (long (+ total n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copy
  "Copy bytes to output"
  [^InputStream input ^OutputStream output]

  (let [cnt (copyAll input output)]
    (if (> cnt Integer/MAX_VALUE)
      (trap! IOException "size too large"))
    cnt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toBytes
  "Read from stream" ^bytes [^InputStream input]
  (let [os (baos<>)] (copy input os) (.toByteArray  os)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsToBytes
  "" ^bytes [^chars chs & [encoding]]

  (->> (stror encoding "utf-8")
       (.getBytes (String. chs) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toChars
  "" ^chars [^bytes bytess & [encoding]]
  (->> (stror encoding "utf-8") (String. bytess) .toCharArray ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readNumber
  "Deserialize a number"
  ^Number [^bytes bytess ^Class numType]

  (with-open [dis (-> (streamit bytess)
                      (DataInputStream. ))]
    (cond
      (or (= numType Double)
          (= numType Double/TYPE))
      (.readDouble dis)
      (or (= numType Float)
          (= numType Float/TYPE))
      (.readFloat dis)
      (or (= numType Integer)
          (= numType Integer/TYPE))
      (.readInt dis)
      (or (= numType Long)
          (= numType Long/TYPE))
      (.readLong dis)
      :else
      (throwBadData "Unsupported number type %s" numType))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeNumber
  "Serialize a number" ^bytes [nnum]

  (with-open [baos (baos<>)
              dos (DataOutputStream. baos)]
    (let [numType (class nnum)]
      (cond
        (or (= numType Double)
            (= numType Double/TYPE))
        (.writeDouble dos (double nnum))
        (or (= numType Float)
            (= numType Float/TYPE))
        (.writeFloat dos (float nnum))
        (or (= numType Integer)
            (= numType Integer/TYPE))
        (.writeInt dos (int nnum))
        (or (= numType Long)
            (= numType Long/TYPE))
        (.writeLong dos (long nnum))
        :else
        (throwBadData "Unsupported number type %s" numType))
      (.flush dos)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn closeQ
  "Close object (quietly)"
  [obj] (try! (some-> (cast? Closeable obj) .close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteQ "Delete file (quietly)" [f] (try! (io/delete-file f true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesToHex
  "Bytes into hex-chars" ^chars [^bytes bytess]

  (let [len (if (nil? bytess) 0 (* 2 (alength bytess)))
        hx (.toCharArray HEX_CHS)
        out (char-array len)]
    (loop [k 0 pos 0]
      (when-not (>= pos len)
        (let [n (bit-and (aget ^bytes bytess k) 0xff)]
          ;; high 4 bits
          (aset-char out pos
                     (aget hx (unsigned-bit-shift-right n 4)))
          ;; low 4 bits
          (aset-char out (+ pos 1)
                     (aget hx (bit-and n 0xf)))
          (recur (inc k) (+ 2 pos)) )))
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hexify
  "Turn bytes into hex string"
  ^String [^bytes bytess] (some-> bytess bytesToHex String. ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gzip
  "Gzip these bytes" ^bytes [^bytes bytess]

  (if (some? bytess)
    (with-open [baos (baos<>)
                g (GZIPOutputStream. baos)]
      (.write g bytess 0 (alength bytess))
      (.finish g)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gunzip
  "Gunzip these bytes" ^bytes [^bytes bytess]

  (if (some? bytess)
    (with-open [inp (GZIPInputStream.
                      (streamit bytess))] (toBytes inp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetStream!
  "Reset stream (safely)"
  [^InputStream inp] (try! (some-> inp .reset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn inputStream??
  "A tuple [you-close? stream]"
  ^APersistentVector
  [arg]
  (cond
    (ist? File arg) [true (FileInputStream. ^File arg)]
    (string? arg) [true (streamit (bytesit arg))]
    (instBytes? arg) [true (streamit arg)]
    (ist? InputStream arg) [false arg]
    (ist? URL arg) [true (.openStream ^URL arg)]))
    ;;(nil? arg) [false nil]
    ;;:else [false nil]))
    ;;:else (throwBadArg "Bad type %s" (class arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti openFile "Open this file" {:tag InputStream} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile String [fp] (openFile (io/file fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile File [^File f] (some-> f (io/input-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fromGZB64
  "Unzip content which is base64 encoded + gziped"
  ^bytes
  [^String gzb64]

  (if (hgl? gzb64)
    (-> (Base64/getDecoder) (.decode gzb64) gunzip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toGZB64
  "Zip content and then base64 encode it"
  ^String
  [^bytes bytess]

  (if (some? bytess)
    (-> (Base64/getEncoder)
        (.encodeToString (gzip bytess)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readableBytes
  "Get the available bytes in this stream"
  ^Integer
  [^InputStream inp]
  (if (nil? inp) (int 0) (.available inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tempFile
  "Create a temporary file" {:tag File}

  ([pfx sux] (tempFile pfx sux *tempfile-repo*))
  ([pfx sux ^File dir]
   (File/createTempFile
     ^String (if (> (count pfx) 2) pfx "czlab")
     (stror sux ".tmp")
     dir))
  ([] (tempFile "" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn openTempFile
  "A Tuple(2) [ File, OutputStream? ]" {:tag APersistentVector}

  ([pfx sux] (openTempFile pfx sux *tempfile-repo*))
  ([pfx sux dir]
   (let [fp (tempFile pfx sux dir)]
     [fp (FileOutputStream. fp)]))
  ([] (openTempFile "" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyStream
  "Copy content to a temp file" ^File [^InputStream inp]

  (let [[fp os] (openTempFile)]
    (try (copy inp os) (finally (closeQ os))) fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetSource!
  "Reset an input source" [^InputSource inpsrc]

  (if (some? inpsrc)
    (let [rdr (.getCharacterStream inpsrc)
          ism (.getByteStream inpsrc) ]
      (try! (some-> ism .reset))
      (try! (some-> rdr .reset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro xdata<>
  "Create XData with content"
  ([c b] `(XData. ~c ~b))
  ([c] `(XData. ~c))
  ([] `(XData. )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fdata<>
  "Create XData with temp-file"
  ([dir] `(XData. (tempFile "" "" ~dir)))
  ([] `(XData. (tempFile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapBytes "Swap bytes in buffer to file" [baos]
  (let [[^File fp ^OutputStream os]
        (openTempFile)]
    (doto os
      (.write (. ^ByteArrayOutputStream
                 baos
                 toByteArray))
      .flush)
    (closeQ baos)
    [fp os]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapChars "Swap chars in writer to file" [wtr]
  (let [[^File fp ^OutputStream out]
        (openTempFile)
        w (OutputStreamWriter. out "utf-8")]
    (doto w
      (.write (. ^CharArrayWriter
                 wtr
                 toCharArray))
      .flush)
    (closeQ wtr)
    [fp w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- slurpb
  "Bytes from stream" ^XData [^InputStream inp limit]

  (loop [bits (byte-array BUF_SZ)
         os (baos<>)
         fout nil
         cnt 0
         c (.read inp bits)]
    (if (< c 0)
      (try
        (xdata<> (or fout os))
        (finally
          (closeQ os)))
      (do
        (if (> c 0)
          (.write ^OutputStream os bits 0 c))
        (let
          [[f o]
           (if (and (nil? fout)
                    (> (+ c cnt) limit))
             (swapBytes os)
             [fout os])]
          (recur bits
                 o
                 f
                 (+ c cnt)
                 (.read inp bits)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- slurpc
  "Chars from reader" ^XData [^Reader rdr limit]

  (loop [wtr (CharArrayWriter. (int BUF_SZ))
         carr (char-array BUF_SZ)
         fout nil
         cnt 0
         c (.read rdr carr)]
    (if (< c 0)
      (try
        (xdata<> (or fout wtr))
        (finally
          (closeQ wtr)))
      (do
        (if (> c 0)
          (.write ^Writer wtr carr 0 c))
        (let
          [[f w]
           (if (and (nil? fout)
                    (> (+ c cnt) limit))
             (swapChars wtr)
             [fout wtr])]
          (recur w carr f
                 (+ c cnt)
                 (.read rdr carr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readBytes
  "Read bytes from stream" {:tag XData}

  ([inp] (readBytes inp false))
  ([^InputStream inp usefile?]
   (slurpb inp (if usefile? 1 *membuf-limit*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readChars
  "Read chars and return a XData" {:tag XData}

  ([rdr] (readChars rdr false))
  ([^Reader rdr usefile?]
   (slurpc rdr (if usefile? 1 *membuf-limit*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn workDirPath
  "The working directory" ^String [] (fpath *tempfile-repo*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convBytes "" ^bytes [data]
  (condp = (class data)
    InputStream (toBytes data)
    String (bytesit data)
    (bytesClass) data
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro slurpUtf8
  "Read f with utf8 encoding" [f] `(slurp ~f :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro spitUtf8
  "Write f with utf8 encoding" [f c] `(spit ~f ~c :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileReadWrite?
  "Is file readable & writable?" [^File fp]
  (and (some? fp) (.exists fp) (.isFile fp) (.canRead fp) (.canWrite fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fileOK?
  "If file exists?"
  [fp]
  `(boolean (some->
              ~(with-meta fp {:tag 'java.io.File}) .exists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileRead? "Is file readable?" [^File fp]
  (and (some? fp) (.exists fp) (.isFile fp) (.canRead fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirReadWrite?
  "Is dir readable and writable?" [^File dir]
  (and (some? dir) (.exists dir) (.isDirectory dir) (.canRead dir) (.canWrite dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirRead?
  "Is dir readable?" [^File dir]
  (and (some? dir) (.exists dir) (.isDirectory dir) (.canRead dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn canExec?
  "Is file executable?" [^File fp]
  (and (some? fp) (.exists fp) (.canExecute fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro parentFile
  "Parent file"
  [f]
  `(some->
     ~(with-meta f {:tag 'java.io.File})  .getParentFile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parentPath
  "Path to parent" ^String [^String path]
  (if (nichts? path) path (.getParent (io/file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn changeContent
  "Pass file content to the work function,
  returning new content"
  ^String [file work]
  {:pre [(some? file) (fn? work)]} (work (slurpUtf8 file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replaceFile!
  "Update file with new content"
  [file work]
  {:pre [(some? file)(fn? work)]}
  (spitUtf8 file (changeContent file work)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeFile
  "Write data to file"
  ([fout data] (writeFile fout data "utf-8"))
  ([fout data enc]
   (io/copy data
            fout
            :buffer-size FourK
            :encoding (stror enc "utf-8"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cleanZEName
  "Remove leading separators from name"
  ^String [^ZipEntry en] (-> (.getName en) (.replaceAll "^[\\/]+" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doOneEntry
  "" [^ZipFile src ^File des ^ZipEntry en]

  (let [f (->> (cleanZEName en)
               (io/file des))]
    (if (.isDirectory en)
      (.mkdirs f)
      (do
        (.. f getParentFile mkdirs)
        (with-open [inp (.getInputStream src en)
                    os (FileOutputStream. f)] (copy inp os))))))

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
          (doOneEntry z desDir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn slurpBytes
  "Read bytes from a file" ^bytes [f]
  (with-open [out (baos<> BUF_SZ)]
    (io/copy f out :buffer-size BUF_SZ)
    (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro readAsStr
  "Read data from source as string"
  ([s] `(readAsStr ~s "utf-8"))
  ([s enc] `(slurp ~s :encoding (stror ~enc "utf-8"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn saveFile
  "Save a file to a directory"

  ([dir fname stuff] (saveFile dir fname stuff false))
  ([dir ^String fname ^XData stuff del?]
   ;;(log/debug "saving file: %s" fname)
   (let [fp (io/file dir fname)]
     (if del?
       (deleteQ fp)
       (if (.exists fp)
         (throwIOE "file %s exists" fp)))
     (if-not (.isFile stuff)
       (writeFile fp (.getBytes stuff))
       (let [opts (make-array CopyOption 1)]
         (aset #^"[Ljava.nio.file.CopyOption;"
               opts 0 StandardCopyOption/REPLACE_EXISTING)
         (Files/move (.. stuff
                         fileRef
                         toPath)
                     (.toPath fp) opts)
         ;;since file has moved, update stuff
         (.setDeleteFlag stuff false)
         (.reset stuff fp false))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getFile
  "Get a file from a directory"
  ^XData [^File dir ^String fname]

  ;;(log/debug "getting file: %s" fname)
  (let [fp (io/file dir fname)
        xs (xdata<> )]
    (if (fileRead? fp)
      (.reset xs fp false)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti mkdirs "Make directories" {:tag File} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod mkdirs String [f] (mkdirs (io/file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod mkdirs File [^File f] (doto f .mkdirs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listFiles
  "List files with certain extension" [dir ext]

  (->> (reify FileFilter
         (accept [_ f] (cs/ends-with? (.getName f) ext)))
       (.listFiles (io/file dir))
       (into [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listDirs
  "List sub-directories" [dir]

  (->> (reify FileFilter
         (accept [_ f] (.isDirectory f)))
       (.listFiles (io/file dir))
       (into [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listAnyFiles
  "List files with certain extension recursively" [dir ext]

  (let [dir (io/file dir)
        res (atom [])
        dig
        (fn [root ext bin func]
          (doseq [^File f (.listFiles ^File root)]
            (cond
              (.isDirectory f)
              (func f ext bin func)
              (cs/ends-with? (.getName f) ext)
              (swap! bin conj f))))]
    (if (dirRead? dir)
      (dig dir ext res dig))
    @res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn grepFolderPaths
  "Recurse a dir, pick out dirs
   that have files of this extension" [rootDir ext]

  (let [rootDir (io/file rootDir)
        rlen  (.length ^String (fpath rootDir))
        rlen (inc rlen)
        bin (atom #{})
        dig
        (fn [top func]
          (doseq [^File f (.listFiles ^File top)]
            (cond
              (.isDirectory f)
              (func f func)
              (cs/ends-with? (.getName f) ext)
              (let [p (.getParentFile f)]
                (when-not (contains? @bin p)
                  (swap! bin conj p))))))]
    (dig rootDir dig)
    (mapv #(.substring ^String (fpath %) rlen) @bin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scan-tree
  "Walk down folder hierarchies"
  [^Stack stk ext out seed]

  (if-let [top (or seed (.peek stk))]
    (doseq [^File f (.listFiles ^File top)]
      (let [p (if (.empty stk)
                '()
                (for [x (.toArray stk)]
                  (. ^File x getName)))
            fid (.getName f)
            paths (conj (into [] p) fid)]
        (if
          (.isDirectory f)
          (do
            (.push stk f)
            (scan-tree stk ext out nil))
          ;else
          (if (.endsWith fid ext)
            (swap! out conj (cs/join "/" paths)))))))
  (when-not
    (.empty stk) (.pop stk))
  out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn grepFilePaths
  "Recurse a folder, picking out files with the given extension"
  [rootDir ext]

  ;; the stack is used to store the folder hierarchy
  @(scan-tree (Stack.) ext (atom []) (io/file rootDir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn basename
  "Get the name of file without extension"
  [file]
  (let [n (.getName (io/file file))
        p (.lastIndexOf n ".")]
    (if (> p 0) (.substring n 0 p) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn touch!
  "Touch a file"
  [file] {:pre [(some? file)]}

  (let [f (io/file file)]
    (if-not (.exists f)
      (do
        (.. f getParentFile mkdirs)
        (with-open [os (FileOutputStream. f)]))
      (when-not
        (. f setLastModified (System/currentTimeMillis))
        (throwIOE "Unable to set the lastmodtime: %s" f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytes??
  "Coerce to bytes" ^bytes [arg]
  (cond
    (ist? File arg) (slurpBytes arg)
    (string? arg) (bytesit arg)
    (instBytes? arg) arg
    (ist? InputStream arg) (toBytes arg)
    (ist? URL arg)
    (with-open [p (.openStream ^URL arg)] (toBytes p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

