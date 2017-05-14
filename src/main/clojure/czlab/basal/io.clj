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

  (:require [czlab.basal.log :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [czlab.basal.meta :as m]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

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
(def ^:dynamic *tempfile-repo* (io/file (c/sysTmpDir)))
(def ^:dynamic *membuf-limit* (* 4 c/MegaBytes))

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
   (ByteArrayOutputStream. (int (or size c/BUF-SZ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- copyAll
  "" [^InputStream in ^OutputStream out]

  (loop [buf (byte-array c/BUF-SZ)
         cnt 0
         n (.read in buf)]
    (if (< n 0)
      cnt
      (do
        (if (> n 0)
          (.write out buf 0 n))
        (recur buf
               (+ n cnt)
               (.read in buf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyBytes
  "Copy certain number of bytes to output"
  [^InputStream in ^OutputStream out ^long kount]

  (let [buf (byte-array c/BUF-SZ)
        bsz (alength buf)]
    (loop [remain kount
           total 0]
      (let [len (if (< remain bsz) remain bsz)
            n (if (> len 0) (.read in buf 0 len) -1)]
        (if (< n 0)
          total
          (do
            (if (> n 0)
              (.write out buf 0 n))
            (recur (long (- remain n))
                   (long (+ total n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copy
  "Copy bytes to output"
  [^InputStream in ^OutputStream out]

  (c/do-with [cnt (copyAll in out)]
             (if (> cnt Integer/MAX_VALUE)
               (c/throwIOE "size too large"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toBytes
  "Read from stream" ^bytes [in]
  (let [os (baos<>)] (copy in os) (.toByteArray os)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsToBytes
  "Coerce chars to bytes" {:tag "[B"}
  ([chs] (charsToBytes chs "utf-8"))
  ([chs enc]
   (->> (c/toCharset enc)
        (.getBytes (String. ^chars chs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toChars
  "Coerce bytes to chars" {:tag "[C"}
  ([bytess] (toChars bytess "utf-8"))
  ([bytess enc]
   (->> (c/toCharset enc) (String. ^bytes bytess) .toCharArray)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readNumber
  "Deserialize a number"
  ^Number [bytess numType]
  {:pre [(class? numType)]}
  (with-open [dis (DataInputStream.
                    (streamit bytess))]
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
      (c/throwBadData "Unsupported number type %s" numType))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeNumber
  "Serialize a number" ^bytes [nnum]
  {:pre [(number? nnum)]}
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
        (c/throwBadData "Unsupported number type %s" numType))
      (.flush dos)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn closeQ
  "Close object (quietly)"
  [obj] (c/trye!! nil (some-> (c/cast? Closeable obj) .close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteQ
  "Delete file (quietly)"
  [f] (c/trye!! nil (io/delete-file f true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesToHex
  "Bytes into hex-chars" ^chars [^bytes bytess]

  (let [len (if (nil? bytess)
              0 (* 2 (alength bytess)))
        hx (c/charsit c/HEX-CHS)]
    (c/do-with [out (char-array len)]
      (loop [k 0 pos 0]
        (when-not (>= pos len)
          (let [n (bit-and (aget ^bytes bytess k) 0xff)]
            ;; high 4 bits
            (aset-char out pos
                       (aget hx (unsigned-bit-shift-right n 4)))
            ;; low 4 bits
            (aset-char out (+ pos 1)
                       (aget hx (bit-and n 0xf)))
            (recur (inc k) (+ 2 pos)) ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hexify
  "Turn bytes into hex string"
  ^String [bytess] (some-> bytess bytesToHex c/strit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gzip
  "Gzip these bytes" ^bytes [^bytes bytess]

  (if bytess
    (with-open [baos (baos<>)
                g (GZIPOutputStream. baos)]
      (.write g bytess 0 (alength bytess))
      (.finish g)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gunzip
  "Gunzip these bytes" ^bytes [^bytes bytess]

  (if bytess
    (with-open [inp (GZIPInputStream.
                      (streamit bytess))] (toBytes inp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetStream!
  "Reset stream (safely)"
  [^InputStream inp] (c/trye!! nil (some-> inp .reset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn inputStream??
  "A tuple [you-close? stream]"
  ^APersistentVector
  [arg]
  (cond
    (c/ist? File arg) [true (FileInputStream. ^File arg)]
    (string? arg) [true (streamit (c/bytesit arg))]
    (m/instBytes? arg) [true (streamit arg)]
    (c/ist? InputStream arg) [false arg]
    (c/ist? URL arg) [true (.openStream ^URL arg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn openFile
  "Open this file"
  ^InputStream [arg]
  (cond
    (string? arg) (openFile (io/file arg))
    (c/ist? File arg) (openFile (io/as-url arg))
    (c/ist? URL arg) (some-> arg io/input-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fromGZB64
  "Unzip content which is base64 encoded + gziped"
  ^bytes
  [gzb64]
  (if (s/hgl? gzb64)
    (-> (Base64/getDecoder) (.decode ^String gzb64) gunzip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toGZB64
  "Zip content and then base64 encode it"
  ^String
  [^bytes bytess]

  (if bytess
    (-> (Base64/getEncoder) (.encodeToString (gzip bytess)))))

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

  ([pfx sux ^File dir]
   (File/createTempFile
     ^String (if (> (count pfx) 2) pfx "czlab")
     (s/stror sux ".tmp")
     dir))
  ([] (tempFile "" ""))
  ([pfx sux] (tempFile pfx sux *tempfile-repo*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn openTempFile
  "A Tuple(2) [File, OutputStream?]"
  {:tag APersistentVector}

  ([pfx sux dir]
   (let [fp (tempFile pfx sux dir)]
     [fp (FileOutputStream. fp)]))
  ([] (openTempFile "" ""))
  ([pfx sux] (openTempFile pfx sux *tempfile-repo*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyStream
  "Copy content to a temp file"
  ^File [^InputStream inp]

  (let [[fp os] (openTempFile)]
    (try (copy inp os) (finally (closeQ os))) fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetSource!
  "Reset an input source" [^InputSource inpsrc]

  (if inpsrc
    (let [rdr (.getCharacterStream inpsrc)
          ism (.getByteStream inpsrc) ]
      (c/trye!! nil (some-> ism .reset))
      (c/trye!! nil (some-> rdr .reset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro xdata<>
  "Create XData with content"
  ([c] `(xdata<> ~c nil))
  ([] `(xdata<> nil nil))
  ([c b] `(czlab.jasal.XData. ~c (boolean ~b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fdata<>
  "Create XData with temp-file"
  ([] `(xdata<> (czlab.basal.io/tempFile)))
  ([dir] `(xdata<> (czlab.basal.io/tempFile "" "" ~dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapBytes
  "Swap bytes in buffer to file"
  [^ByteArrayOutputStream baos]
  (c/do-with [rc (openTempFile)]
             (doto
               ^OutputStream
               (last rc)
               (.write (.toByteArray baos)) .flush)
             (closeQ baos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapChars
  "Swap chars in writer to file"
  [^CharArrayWriter wtr]
  (let [[fp ^OutputStream out] (openTempFile)
        w (OutputStreamWriter. out "utf-8")]
    (doto w
      (.write (.toCharArray wtr)) .flush)
    (closeQ wtr)
    [fp w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- slurpb
  "Bytes from stream"
  ^XData [^InputStream in limit]

  (loop [bits (byte-array c/BUF-SZ)
         os (baos<>)
         fout nil
         cnt 0
         c (.read in bits)]
    (if (< c 0)
      (try
        (xdata<> (or fout os))
        (finally (closeQ os)))
      (do
        (if (> c 0)
          (.write ^OutputStream os bits 0 c))
        (let
          [[f o']
           (if (and (nil? fout)
                    (> (+ c cnt) limit))
             (swapBytes os)
             [fout os])]
          (recur bits
                 o' f (+ c cnt) (.read in bits)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- slurpc
  "Chars from reader"
  ^XData [^Reader rdr limit]

  (loop [wtr (CharArrayWriter. (int c/BUF-SZ))
         carr (char-array c/BUF-SZ)
         fout nil
         cnt 0
         c (.read rdr carr)]
    (if (< c 0)
      (try
        (xdata<> (or fout wtr))
        (finally (closeQ wtr)))
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
                 (+ c cnt) (.read rdr carr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rxBytes
  "Read bytes from stream" {:tag XData}

  ([in] (rxBytes in false))
  ([in usefile?]
   (slurpb in (if usefile? 1 *membuf-limit*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rxChars
  "Read chars from reader" {:tag XData}

  ([rdr] (rxChars rdr false))
  ([rdr usefile?]
   (slurpc rdr (if usefile? 1 *membuf-limit*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn workDirPath
  "The working directory" ^String [] (c/fpath *tempfile-repo*))

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
  (and (some-> fp .exists)
       (.isFile fp) (.canRead fp) (.canWrite fp)))

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
  (and (some-> fp .exists) (.isFile fp) (.canRead fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirReadWrite?
  "Is dir readable and writable?" [^File dir]
  (and (some-> dir .exists)
       (.isDirectory dir) (.canRead dir) (.canWrite dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirRead?
  "Is dir readable?" [^File dir]
  (and (some-> dir .exists) (.isDirectory dir) (.canRead dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn canExec?
  "Is file executable?" [^File fp]
  (and (some-> fp .exists) (.canExecute fp)))

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
  "Path to parent" ^String [path]
  (if (s/nichts? path) path (.getParent (io/file path))))

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
  ([fout data enc]
   (io/copy data
            fout
            :buffer-size c/FourK
            :encoding (c/strCharset enc)))
  ([fout data] (writeFile fout data "utf-8")))

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
  (with-open [out (baos<> c/BUF-SZ)]
    (io/copy f
             out
             :buffer-size c/BUF-SZ)
    (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro readAsStr
  "Read data from source as string"
  ([s] `(readAsStr ~s "utf-8"))
  ([s enc] `(slurp ~s :encoding (s/stror ~enc "utf-8"))))

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
         (c/throwIOE "file %s exists" fp)))
     (if-not (.isFile stuff)
       (writeFile fp (.getBytes stuff))
       (let [opts (c/marray CopyOption 1)]
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
(defn mkdirs
  "Make directories"
  ^File [arg]
  (cond
    (string? arg) (mkdirs (io/file arg))
    (c/ist? File arg) (doto ^File arg .mkdirs)))

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
        rlen  (.length ^String (c/fpath rootDir))
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
    (mapv #(.substring ^String (c/fpath %) rlen) @bin)))

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
                  (.getName ^File x)))
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
        (.setLastModified f (System/currentTimeMillis))
        (c/throwIOE "Unable to set the lastmodtime: %s" f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytes??
  "Coerce to bytes" ^bytes [arg]
  (cond
    (c/ist? InputStream arg) (toBytes arg)
    (c/ist? File arg) (slurpBytes arg)
    (m/instChars? arg) (c/bytesit arg)
    (m/instBytes? arg) arg
    (string? arg) (c/bytesit arg)
    (c/ist? URL arg)
    (with-open [p (.openStream ^URL arg)] (bytes?? p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

