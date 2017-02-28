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
(defn streamify
  "Wrapped these bytes in an input-stream"
  ^InputStream
  [^bytes bytess]
  (some-> bytess ByteArrayInputStream. ))

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
  ""
  ^long
  [^InputStream input ^OutputStream output]

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
  ^long
  [^InputStream input ^OutputStream output ^long numToRead]

  (let [buf (byte-array BUF_SZ)
        bsz (alength buf)]
    (loop [remain numToRead
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
  ^long
  [^InputStream input ^OutputStream output]

  (let [cnt (copyAll input output)]
    (if (> cnt Integer/MAX_VALUE)
      (trap! IOException "size too large"))
    cnt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toBytes
  ""
  ^bytes
  [^InputStream input]
  (let [os (baos<>)]
    (copy input os)
    (.toByteArray  os)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsToBytes
  "Convert char[] to byte[]"
  ^bytes
  [^chars chs & [^String encoding]]

  (let [encoding (stror encoding "utf-8")]
    (comment
      (-> (Charset/forName encoding)
          (.encode (CharBuffer/wrap chs))
          (.array)))
    (.getBytes (String. chs) encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toChars
  "Convert byte[] to char[]"
  ^chars
  [^bytes bytess & [^String encoding]]

  (let [encoding (stror encoding "utf-8")]
   (comment
     (-> (Charset/forName encoding)
         (.decode (ByteBuffer/wrap bytess))
         (.array)))
   (.toCharArray (String. bytess encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readNumber
  "Deserialize a number"
  ^Number
  [^bytes bytess ^Class numType]
  (with-open [dis (-> (streamify bytess)
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
  "Serialize a number"
  ^bytes
  [nnum]
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
  "Close object"
  [obj]
  (try! (some-> (cast? Closeable obj) .close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteQ "Delete file" [f] (try! (io/delete-file f true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesToHex
  "Bytes into hex-chars"
  ^chars
  [^bytes bytess]

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
  ^String
  [^bytes bytess]
  (some-> bytess bytesToHex String. ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gzip
  "Gzip these bytes"
  ^bytes
  [^bytes bytess]
  (if (some? bytess)
    (with-open [baos (baos<>)
                g (GZIPOutputStream. baos)]
      (.write g bytess 0 (alength bytess))
      (.finish g)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gunzip
  "Gunzip these bytes"
  ^bytes
  [^bytes bytess]
  (if (some? bytess)
    (with-open [inp (GZIPInputStream.
                      (streamify bytess))]
      (toBytes inp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetStream!
  "Safely reset this stream"
  [^InputStream inp]
  (try! (some-> inp .reset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn coerceToInputStream
  "A tuple [bool stream]"
  ^APersistentVector
  [arg]
  (cond
    (inst? File arg) [true (FileInputStream. ^File arg)]
    (string? arg) [true (streamify (bytesify arg))]
    (instBytes? arg) [true (streamify arg)]
    (inst? InputStream arg) [false arg]
    (inst? URL arg) [true (.openStream ^URL arg)]
    (nil? arg) [false nil]
    :else (throwBadArg "Bad type %s" (class arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti openFile "Open this file" {:tag XStream} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile String [fp] (some-> fp io/file XStream. ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile File [^File f] (some-> f XStream. ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fromGZB64
  "Unzip content which is base64 encoded + gziped"
  ^bytes
  [^String gzb64]
  (if (some? gzb64)
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
  "Create a temporary file"
  {:tag File}

  ([^String pfx ^String sux ^File dir]
   (File/createTempFile
     (if (> (count pfx) 2) pfx "czlab")
     (if (> (count sux) 2) sux ".dat")
     dir))
  ([] (tempFile "" ""))
  ([pfx sux] (tempFile pfx sux *tempfile-repo*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn openTempFile
  "A Tuple(2) [ File, OutputStream? ]"
  {:tag APersistentVector}
  ([^String pfx ^String sux ^File dir]
   (let [fp (tempFile pfx sux dir)]
     [fp (FileOutputStream. fp)]))
  ([] (openTempFile "" ""))
  ([pfx sux]
   (openTempFile pfx sux *tempfile-repo*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyStream
  "Copy content from this input-stream to a temp file"
  ^File
  [^InputStream inp]
  (let [[fp os] (openTempFile)]
    (try
      (copy inp os)
      (finally (closeQ os)))
    fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetSource!
  "Reset an input source"
  [^InputSource inpsrc]
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
(defn- swapBytes
  "Swap bytes in buffer to file, returning a [File,OStream] tuple"
  [^ByteArrayOutputStream baos]
  (let [[^File fp ^OutputStream os]
        (openTempFile) ]
    (doto os (.write (.toByteArray baos)) .flush)
    (.close baos)
    [fp os]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapChars
  "Swap chars in writer to file, returning a [File,OWriter] tuple"
  [^CharArrayWriter wtr]
  (let [[^File fp ^OutputStream out]
        (openTempFile)
        w (OutputStreamWriter. out "utf-8")]
    (doto w (.write (.toCharArray wtr)) .flush)
    (closeQ wtr)
    [fp w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- slurpb
  "Read bytes from the stream"
  ^XData
  [^InputStream inp limit]
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
  "Read chars from the reader"
  ^XData
  [^Reader rdr limit]
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
  "Read bytes from stream"
  {:tag XData}

  ([inp] (readBytes inp false))
  ([^InputStream inp usefile?]
   (slurpb inp (if usefile? 1 *membuf-limit*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readChars
  "Read chars and return a XData"
  {:tag XData}
  ([rdr] (readChars rdr false))
  ([^Reader rdr usefile?]
   (slurpc rdr (if usefile? 1 *membuf-limit*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn workDirPath
  "The working directory" ^String [] (fpath *tempfile-repo*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convBytes
  ""
  ^bytes
  [data]
  (condp = (class data)
    InputStream (toBytes data)
    String (bytesify data)
    (bytesClass) data
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro slurpUtf8
  "Read contents of f with utf8 encoding"
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
  "If file is readable & writable"
  [^File fp]
  (and (some? fp)
       (.exists fp)
       (.isFile fp)
       (.canRead fp)
       (.canWrite fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fileOK?
  "If file exists"
  [fp]
  `(boolean (some->
              ~(with-meta fp {:tag 'java.io.File}) .exists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileRead?
  "If file is readable"
  [^File fp]
  (and (some? fp)
       (.exists fp)
       (.isFile fp)
       (.canRead fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirReadWrite?
  "If directory is readable and writable"
  [^File dir]
  (and (some? dir)
       (.exists dir)
       (.isDirectory dir)
       (.canRead dir)
       (.canWrite dir) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dirRead?
  "If directory is readable"
  [^File dir]
  (and (some? dir)
       (.exists dir)
       (.isDirectory dir)
       (.canRead dir) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn canExec?
  "If file or directory is executable"
  [^File fp]
  (and (some? fp)
       (.exists fp)
       (.canExecute fp)))

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
  "Path to the parent file"
  ^String
  [^String path]

  (if (nichts? path)
    path
    (.getParent (io/file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn changeContent
  "Pass file content to the work function,
  returning new content"
  ^String
  [file work]
  {:pre [(some? file)(fn? work)]}
  (work (slurpUtf8 file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replaceFile!
  "Update file with new content"
  [file work]
  {:pre [(some? file)(fn? work)]}
  (spitUtf8 file
            (changeContent file work)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeFile
  "Write data to file"
  ([fout data] (writeFile fout data "utf-8"))
  ([fout data enc]
   (io/copy data
            fout
            :buffer-size 4096
            :encoding (stror enc "utf-8"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cleanZEName
  "Remove leading separators from name"
  ^String
  [^ZipEntry en]
  (-> (.getName en)
      (.replaceAll "^[\\/]+" "")))

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
        (.. f getParentFile mkdirs)
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
  [f]
  (with-open [out (baos<> BUF_SZ)]
    (io/copy f out :buffer-size BUF_SZ)
    (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro readAsStr
  "Read data from source as string"
  ([s] `(readAsStr ~s "utf-8"))
  ([s enc]
   `(slurp ~s :encoding (stror ~enc "utf-8"))))

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
  ^XData
  [^File dir ^String fname]
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
(defmethod mkdirs String [^String f] (mkdirs (io/file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod mkdirs File [^File f] (doto f .mkdirs))

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
  "List files with certain extension recursively"
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
  (doseq [^File f
          (.listFiles (io/file top))]
    (cond
      (.isDirectory f)
      (grep-paths f out fext)
      (.endsWith (.getName f) fext)
      (let [p (.getParentFile f)]
        (when-not (contains? @out p))
          (swap! out conj p)))))

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
        (swap! out
               conj
               (.substring kp (+ rlen 1)))))
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
      (.substring n 0 p) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn touch!
  "Touch a file"
  [file]
  {:pre [(some? file)]}
  (let [f (io/file file)]
    (if-not (.exists f)
      (do
        (.. f getParentFile mkdirs)
        (with-open [os (FileOutputStream. f)]))
      (when-not
        (. f setLastModified (System/currentTimeMillis))
        (throwIOE "Unable to set the lastmodtime: %s" f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




