;; Copyright © 2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.io

  "Useful additions to clojure io."

  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as cs]
            [czlab.basal.util :as u]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [clojure.data.json :as js]
            [czlab.basal.indent :as in])

  (:import [java.nio.file
            CopyOption
            Files
            Path
            Paths
            FileVisitResult
            SimpleFileVisitor
            StandardCopyOption]
           [java.util.zip
            GZIPInputStream
            GZIPOutputStream]
           [java.nio
            ByteBuffer
            CharBuffer]
           [czlab.basal XData]
           [java.util
            Arrays
            Base64
            Base64$Decoder
            Base64$Encoder]
           [java.util.zip
            ZipFile
            ZipEntry]
           [java.util
            Stack]
           [java.net
            URI
            URL]
           [org.xml.sax
            InputSource]
           [java.io
            DataInputStream
            DataOutputStream
            FileInputStream
            FileOutputStream
            CharArrayWriter
            StringWriter
            FileFilter
            File
            InputStream
            Closeable
            OutputStream
            Reader
            Writer
            IOException
            InputStreamReader
            OutputStreamWriter
            BufferedOutputStream
            ByteArrayInputStream
            ByteArrayOutputStream]
           [java.nio.file.attribute BasicFileAttributes]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def ^:dynamic *membuf-limit* (* 4 c/MegaBytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/defmacro- os* [x] `(clojure.java.io/output-stream ~x))
(c/defmacro- is* [x] `(clojure.java.io/input-stream ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro istream

  ^{:arglists '([x])
    :doc "Same as clojure.java.io's input-stream."}

  [x] `(clojure.java.io/input-stream ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ostream

  ^{:arglists '([x])
    :doc "Same as clojure.java.io's output-stream."}

  [x] `(clojure.java.io/output-stream ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro file?

  ^{:arglists '([in])
    :doc "Is this a file?"}

  [in] `(instance? java.io.File ~in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro slurp-utf8

  ^{:arglists '([f])
    :doc "Read f with utf8 encoding."}

  [f] `(slurp ~f :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro spit-utf8

  ^{:arglists '([f c])
    :doc "Write f with utf8 encoding."}

  [f c] `(spit ~f (str ~c) :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-repo

  ^{:arglists '([])
    :doc "Java's system temporary folder."}

  []

  (io/file (u/sys-tmp-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn baos<>

  ^{:arglists '([][size])
    :doc "Make a byte array output stream."}

  {:tag ByteArrayOutputStream}

  ([]
   (baos<> nil))

  ([size]
   (ByteArrayOutputStream. (int (c/num?? size c/BUF-SZ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fsize

  ^{:arglists '([in])
    :doc "Get length of file."}

  [in]
  (some-> in io/file .length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fname

  ^{:arglists '([in])
    :tag String
    :doc "Get name of file."}

  [in]
  (some-> in io/file .getName))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn klose

  ^{:arglists '([obj])
    :doc "Close object (quietly)."}

  [obj]
  (c/try! (some-> (c/cast? Closeable obj) .close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn copy

  ^{:arglists '([in out]
                [in out count])
    :doc "Copy certain number of bytes to output."}

  ([in out]
   (copy in out -1))

  ([in out kount]
   {:pre [(number? kount)]}
   (if (neg? kount)
     (io/copy in out)
     (let [[di? ^InputStream is]
           (if (c/is? InputStream in)
             [false in] [true (is* in)])
           [do? ^OutputStream os]
           (if (c/is? OutputStream out)
             [false out] [true (os* out)])]
       (try
         (loop [remain kount
                total 0
                buf (byte-array c/BUF-SZ)]
           (let
             [len (if (< remain c/BUF-SZ) remain c/BUF-SZ)
              n (if-not (pos? len) -1 (.read is buf 0 len))]
             (if (neg? n)
               nil ;to be consistent with io/copy
               (do (if (pos? n)
                     (.write os buf 0 n))
                   (recur (long (- remain n))
                          (long (+ total n)) buf)))))
         (finally
           (if di? (klose is))
           (if do? (klose os))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->bytes

  ^{:arglists '([in][in enc])
    :doc "Convert almost anything to byte[]."}

  {:tag "[B"}

  ([in]
   (x->bytes in "utf-8"))

  ([in enc]
   (cond (c/is? u/CSCZ in)
         (let [^ByteBuffer
               bb (.encode (u/charset?? enc)
                           (CharBuffer/wrap ^chars in))]
           (Arrays/copyOfRange (.array bb)
                               (.position bb) (.limit bb)))
         (c/is? StringBuilder in)
         (x->bytes (.toString ^Object in) enc)
         (string? in)
         (.getBytes ^String in
                    (u/encoding?? enc))
         (or (nil? in)
             (bytes? in))
         in
         (c/is? XData in)
         (.getBytes ^XData in)
         (c/is? ByteArrayOutputStream in)
         (.toByteArray ^ByteArrayOutputStream in)
         :else
         (let [os (baos<>)] (io/copy in os) (.toByteArray os)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->chars

  ^{:arglists '([in][in enc])
    :doc "Convert almost anything to char[]."}

  {:tag "[C"}

  ([in]
   (x->chars in "utf-8"))

  ([in enc]
   (cond (c/is? CharArrayWriter in)
         (.toCharArray ^CharArrayWriter in)
         (or (nil? in)
             (c/is? u/CSCZ in))
         in
         (c/is? StringBuilder in)
         (x->chars (.toString ^Object in))
         (string? in)
         (.toCharArray ^String in)
         (c/is? XData in)
         (x->chars (.getBytes ^XData in))
         (bytes? in)
         (->> (u/encoding?? enc)
              (String. ^bytes in) .toCharArray)
         (c/is? ByteArrayOutputStream in)
         (x->chars (.toByteArray ^ByteArrayOutputStream in) enc)
         :else
         (let [os (baos<>)]
           (io/copy in os) (x->chars (.toByteArray os) enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->str

  ^{:arglists '([in][in enc])
    :doc "Convert almost anything to String."}

  {:tag String}

  ([in]
   (x->str in "utf-8"))

  ([in enc]
   (cond (or (nil? in)
             (string? in))
         in
         (bytes? in)
         (String. ^bytes in ^String enc)
         (c/is? InputStream in)
         (x->str (let [os (baos<>)]
                   (io/copy in os) os))
         (c/is? File in)
         (slurp in :encoding enc)
         (c/is? XData in)
         (.strit ^XData in)
         (c/is? StringBuilder in)
         (.toString ^Object in)
         (c/is? ByteArrayOutputStream in)
         (String. (.toByteArray
                    ^ByteArrayOutputStream in) ^String enc)
         :else (String. (x->chars in enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-number

  ^{:arglists '([in numType])
    :tag Number
    :doc "Deserialize a number."}

  [in numType]
  {:pre [(class? numType)]}

  (c/wo* [dis (DataInputStream. (is* in))]
    (cond (or (= numType Double)
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
          (u/throw-BadData "Unsupported number type %s." numType))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn write-number

  ^{:arglists '([nnum])
    :tag bytes
    :doc "Serialize a number."}

  [nnum]
  {:pre [(number? nnum)]}

  (c/wo* [baos (baos<>)
          dos (DataOutputStream. baos)]
    (let [numType (class nnum)]
      (cond (or (= numType Double)
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
            (u/throw-BadData "Unsupported number type %s." numType))
      (.flush dos)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fdelete

  ^{:arglists '([f])
    :doc "Delete file (quietly)."}

  [f] (c/try! (if (c/is? File f)
                (io/delete-file f true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bytes->hex

  ^{:arglists '([in])
    :tag chars
    :doc "Bytes into hex-chars."}

  [in]

  (let [b' (x->bytes in)
        len (if (nil? b')
              0 (* 2 (alength b')))]
    (loop [k 0
           pos 0
           out (char-array len)]
      (if (>= pos len)
        (if (nil? in) nil out)
        (let [n (bit-and (aget b' k) 0xff)]
          (aset-char out ;; high 4 bits
                     pos
                     (aget ^chars
                           c/hex-chars
                           (unsigned-bit-shift-right n 4)))
          (aset-char out ;; low 4 bits
                     (+ pos 1)
                     (aget ^chars c/hex-chars (bit-and n 0xf)))
          (recur (+ 1 k)
                 (+ 2 pos) out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hexify

  ^{:arglists '([in])
    :tag String
    :doc "Turn bytes into hex string."}

  [in] (some-> in bytes->hex x->str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gzip

  ^{:arglists '([in])
    :tag bytes
    :doc "Gzip input."}

  [in]

  (if-some [b (x->bytes in)]
    (with-open [baos (baos<>)
                g (GZIPOutputStream. baos)]
      (.write g b 0 (alength b))
      (.finish g)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gunzip

  ^{:arglists '([in])
    :tag bytes
    :doc "Gunzip input."}

  [in]

  (if-some [b (x->bytes in)]
    (with-open [inp (GZIPInputStream.
                      (io/input-stream b))] (x->bytes inp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reset-stream!

  ^{:arglists '([in])
    :doc "Reset stream (safely)."}

  [in]

  (if-some [inp (c/cast? InputStream in)] (c/try! (.reset inp))) in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn input-stream??

  ^{:arglists '([arg][arg enc])
    :doc "Returns a tuple [you-close? stream]."}

  ([arg]
   (input-stream?? arg "utf-8"))

  ([arg enc]
   (cond (or (bytes? arg)
             (c/is? URL arg)
             (c/is? File arg))
         [true (io/input-stream arg)]
         (c/is? ByteArrayOutputStream arg)
         (input-stream?? (x->bytes arg))
         (c/is? u/CSCZ arg)
         (input-stream?? (x->bytes arg enc))
         (or (nil? arg)
             (c/is? InputStream arg))
         [false arg]
         (string? arg)
         (if (cs/starts-with? arg "file:")
           (input-stream?? (io/as-url arg))
           (input-stream?? (x->bytes arg enc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gzb64->bytes

  ^{:arglists '([in])
    :tag bytes
    :doc "Unzip content which is base64 encoded + gziped."}

  [in]

  (some->> in x->str (.decode (Base64/getDecoder)) gunzip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bytes->gzb64

  ^{:arglists '([in])
    :tag String
    :doc "Zip content and then base64 encode it."}

  [in]

  (some->> (some-> in x->bytes gzip)
           (.encodeToString (Base64/getEncoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn readable-bytes

  ^{:arglists '([in][in enc])
    :doc "Get the available bytes in this stream."}

  {:tag Integer}

  ([in]
   (readable-bytes in "utf-8"))

  ([in enc]
   (cond (c/is? InputStream in) (.available ^InputStream in)
         (c/is? File in) (.length (c/cast? File in))
         (c/is? URL in) (c/wo* [i (is* in)]
                          (readable-bytes i enc))
         (string? in) (readable-bytes (x->bytes in enc) enc)
         (bytes? in) (alength ^bytes in)
         (c/is? u/CSCZ in) (readable-bytes (x->bytes in enc) enc)
         :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmpfile

  ^{:arglists '([f])
    :tag File
    :doc "Create f in temp dir."}

  [f]
  (->> (if-not (c/is? File f)
         (str f)
         (.getName ^File f))
       (io/file (file-repo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn temp-file

  ^{:arglists '([]
                [pfx sux]
                [pfx sux dir])
    :doc "Create a temporary file."}

  {:tag File}

  ([]
   (temp-file nil nil))

  ([pfx sux]
   (temp-file pfx sux (file-repo)))

  ([pfx sux dir]
   (c/do-with
     [f (File/createTempFile (c/stror pfx "czlab")
                             (c/stror sux ".tmp") (io/file dir))] (fdelete f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn open-temp-file

  ^{:arglists '([]
                [pfx sux]
                [pfx sux dir])
    :doc "A Tuple(2) [File, OutputStream?]."}

  ([] (open-temp-file nil nil))

  ([pfx sux] (open-temp-file pfx sux (file-repo)))

  ([pfx sux dir]
   (let [fp (temp-file pfx sux dir)] [fp (os* fp)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->file

  ^{:arglists '([in][in fout])
    :doc "Copy content to a temp file."}

  {:tag File}

  ([in]
   (x->file in nil))

  ([in fout]
   (let [fp (or fout (temp-file))]
     (c/pre (c/is? File fp))
     (try (io/copy in fp)
          fp
          (catch Throwable _
            (if (nil? fout) (fdelete fp)) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reset-source!

  ^{:arglists '([in])
    :doc "Reset an input source."}

  [in]

  (if-some [src (c/cast? InputSource in)]
    (let [ism (.getByteStream src)
          rdr (.getCharacterStream src)]
      (c/try! (some-> ism .reset))
      (c/try! (some-> rdr .reset))))
  in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- swap-bytes

  "Swap bytes in buffer to file."
  [baos enc]

  (let [[f ^OutputStream os :as rc] (open-temp-file)]
    (doto os (.write (x->bytes baos enc)) .flush)
    (klose baos)
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- swap-chars

  "Swap chars in writer to file."
  [wtr enc]

  (let [[fp ^OutputStream out] (open-temp-file)
        w (OutputStreamWriter. out (u/encoding?? enc))]
    (doto w (.write (x->chars wtr enc)) .flush)
    (klose wtr)
    [fp w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- slurp-inp

  [^InputStream inp limit enc]

  (loop [bits (byte-array c/BUF-SZ)
         os (baos<>) fo nil cnt 0 c (.read inp bits)]
    (if (neg? c)
      (try (if fo
             fo
             (x->bytes os enc))
           (finally (klose os)))
      (do (if (pos? c)
            (.write ^OutputStream os bits 0 c))
          (let [[f o']
                (if-not (and (nil? fo)
                             (> (+ c cnt) limit))
                  [fo os]
                  (swap-bytes os enc))]
            (recur bits o' f (+ c cnt) (.read inp bits)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- slurp-rdr

  [^Reader rdr limit enc]

  (loop [cw (CharArrayWriter. (int c/BUF-SZ))
         carr (char-array c/BUF-SZ)
         fo nil cnt 0 c (.read rdr carr)]
    (if (neg? c)
      (try (if fo
             fo
             (x->chars cw enc))
           (finally (klose cw)))
      (do (if (pos? c)
            (.write ^Writer cw carr 0 c))
          (let [[f w]
                (if-not (and (nil? fo)
                             (> (+ c cnt) limit))
                  [fo cw]
                  (swap-chars cw enc))]
            (recur w carr f (+ c cnt) (.read rdr carr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn slurpb

  ^{:arglists '([in]
                [in enc]
                [in enc useFile?])
    :doc "Like slurp but reads in bytes."}

  ([in enc] (slurpb in enc false))

  ([in] (slurpb in "utf-8"))

  ([in enc usefile?]
   (let [[c? i] (input-stream?? in)]
     (try (slurp-inp i (if usefile?
                         1 *membuf-limit*) enc) (finally
                                                  (if c? (klose i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn slurpc

  ^{:arglists '([in]
                [in enc]
                [in enc useFile?])
    :doc "Like slurp but reads in chars."}

  ([in enc] (slurpc in enc false))

  ([in] (slurpc in "utf-8"))

  ([in enc usefile?]
   (let [[c? i] (input-stream?? in)
         _ (assert (c/is? InputStream i)
                   "Expected input-stream!")
         rdr (InputStreamReader. i)]
     (try (slurp-rdr rdr (if usefile?
                           1 *membuf-limit*) enc) (finally
                                                    (if c? (klose rdr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-read-write?

  ^{:arglists '([in])
    :doc "Is file readable & writable?"}

  [in]

  (if-some [^File fp (some-> in (io/file))]
    (and (.exists fp)
         (.isFile fp) (.canRead fp) (.canWrite fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-ok?

  ^{:arglists '([in])
    :doc "If file exists?"}

  [in]

  (if-some [^File fp (some-> in (io/file))] (.exists fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-read?

  ^{:arglists '([in])
    :doc "Is file readable?"}

  [in]

  (if-some [^File fp (some-> in (io/file))]
    (and (.exists fp) (.isFile fp) (.canRead fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dir-read-write?

  ^{:arglists '([in])
    :doc "Is dir readable and writable?"}

  [in]

  (if-some [^File dir (some-> in (io/file))]
    (and (.exists dir)
         (.isDirectory dir) (.canRead dir) (.canWrite dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dir-read?

  ^{:arglists '([in])
    :doc "Is dir readable?"}

  [in]

  (if-some [^File dir (some-> in (io/file))]
    (and (.exists dir) (.isDirectory dir) (.canRead dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn can-exec?

  ^{:arglists '([in])
    :doc "Is file executable?"}

  [in]

  (if-some
    [^File fp (some-> in (io/file))]
    (and (.exists fp) (.canExecute fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parent-file

  ^{:arglists '([in])
    :tag File
    :doc "Parent file."}

  [in]
  (if-some [^File f (some-> in (io/file))] (.getParentFile f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parent-path

  ^{:arglists '([path])
    :tag String
    :doc "Path to parent."}

  [path]
  (if (c/nichts? path) "" (.getParent (io/file path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-file

  ^{:arglists '([dir fname])
    :doc "Get a file from a directory."}

  [dir fname]

  (let [fp (io/file dir fname)]
    (if (file-read? fp) (XData. fp false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn spit-file

  ^{:arglists '([file stuff]
                [file stuff del?])
    :doc "Save a file to a directory."}

  {:tag File}

  ([file stuff]
   (spit-file file stuff false))

  ([file stuff del?]
   (let [fp (io/file file)
         ^XData
         in (if (c/is? XData stuff)
              stuff (XData. stuff false))]
     (if del?
       (fdelete fp)
       (if (.exists fp)
         (u/throw-IOE "File %s exists." fp)))
     (if-not (.isFile in)
       (io/copy (.getBytes in) fp)
       (let [opts (c/marray CopyOption 1)]
         (aset #^"[Ljava.nio.file.CopyOption;"
               opts 0 StandardCopyOption/REPLACE_EXISTING)
         (Files/move (.. in fileRef toPath) (.toPath fp) opts)
         ;;since file has moved, update stuff
         (.setDeleteFlag in false)
         (.reset in nil)))
     fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn save-file

  ^{:arglists '([dir fname stuff]
                [dir fname stuff del?])
    :doc "Save a file to a directory."}

  {:tag File}

  ([dir fname stuff]
   (save-file dir fname stuff false))

  ([dir fname stuff del?]
   (spit-file (io/file dir fname) stuff del?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn change-content

  ^{:arglists '([file work]
                [file work enc])
    :doc "Pass file content - as string to
         the work function, returning new content."}

  {:tag String}

  ([file work]
   (change-content file work "utf-8"))

  ([file work enc]
   {:pre [(fn? work)]}
   (if (file-read? file)
     (work (slurp file :encoding (u/encoding?? enc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn replace-file!

  ^{:arglists '([file work]
                [file work enc])
    :doc "Update file with new content."}

  {:tag File}

  ([file work]
   (replace-file! file work "utf-8"))

  ([file work enc]
   {:pre [(fn? work)]}
   (spit file (change-content file work enc) :encoding (u/encoding?? enc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unzip->dir

  ^{:arglists '([srcZip desDir])
    :doc "Unzip zip file to a target folder."}

  [^File srcZip ^File desDir]
  (let [z (ZipFile. srcZip)
        es (.entries z)]
    (.mkdirs desDir)
    (while (.hasMoreElements es)
      (let [^ZipEntry en (.nextElement es)
            f (io/file desDir (-> (.getName en)
                                  (.replaceAll "^[\\/]+" "")))]
        (if (.isDirectory en)
          (.mkdirs f)
          (do (.. f getParentFile mkdirs)
              (c/wo* [inp (.getInputStream z en)] (io/copy inp f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mkdirs

  ^{:arglists '([arg])
    :tag File
    :doc "Make directories."}

  [arg]

  (cond (string? arg) (mkdirs (io/file arg))
        (c/is? File arg) (doto ^File arg .mkdirs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-files

  ^{:arglists '([dir ext])
    :doc "List files with certain extension."}

  [dir ext]

  (c/vec-> (.listFiles (io/file dir)
                       (reify FileFilter
                         (accept [_ f]
                           (cs/ends-with? (.getName f) ext))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-dirs

  ^{:arglists '([dir])
    :doc "List sub-directories."}

  [dir]

  (c/vec-> (.listFiles (io/file dir)
                       (reify FileFilter
                         (accept [_ f] (.isDirectory f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-any-files

  ^{:arglists '([dir ext])
    :doc "List files with certain extension recursively."}

  [dir ext]

  (c/do-with-atom [res (atom [])]
    (let [dir (io/file dir)
          dig (fn [^File root ext bin func]
                (doseq [^File f (.listFiles root)
                        :let [fn (.getName f)
                              d? (.isDirectory f)]]
                  (cond d?
                        (func f ext bin func)
                        (cs/ends-with? fn ext)
                        (swap! bin conj f))))]
      (if (dir-read? dir) (dig dir ext res dig)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn grep-folder-paths

  ^{:arglists '([rootDir ext])
    :doc "Recurse a dir, pick out dirs
         that have files of this extension."}

  [rootDir ext]

  (c/do-with-atom [bin (atom #{})]
    (let [rootDir (io/file rootDir)
          rlen (+ 1 (c/n# (u/fpath rootDir)))
          dig (fn [^File top func]
                (doseq [^File f (.listFiles top)
                        :let [fn (.getName f)
                              d? (.isDirectory f)]]
                  (cond d?
                        (func f func)
                        (cs/ends-with? fn ext)
                        (let [p (.getParentFile f)]
                          (if-not (c/in? @bin p)
                            (swap! bin conj p))))))] (dig rootDir dig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- scan-tree
  "Walk down folder hierarchies."
  [^Stack stk ext out seed]
  (if-let [^File top (or seed (.peek stk))]
    (doseq [^File f (.listFiles top)]
      (let [p (map #(.getName ^File %)
                   (if-not
                     (.empty stk)
                     (.toArray stk)))
            fid (.getName f)
            paths (conj (c/vec-> p) fid)]
        (if (.isDirectory f)
          (do (.push stk f)
              (scan-tree stk ext out nil))
          (if (cs/ends-with? fid ext)
            (swap! out conj (cs/join "/" paths)))))))
  (when-not (.empty stk) (.pop stk)) out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn grep-file-paths

  ^{:arglists '([rootDir ext])
    :doc "Recurse a folder, picking out
         files with the given extension."}

  [rootDir ext]
  ;; the stack is used to store the folder hierarchy
  @(scan-tree (Stack.) ext (atom []) (io/file rootDir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn basename

  ^{:arglists '([file])
    :doc "Get the name of file without extension."}

  [file]

  (let [n (.getName (io/file file))
        p (cs/last-index-of n ".")] (if p (subs n 0 p) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn touch!

  ^{:arglists '([file])
    :doc "Touch a file."}

  [file]

  (if-some [f (io/file file)]
    (if-not (.exists f)
      (do (.. f getParentFile mkdirs) (c/wo* [o (os* f)]))
      (when-not
        (.setLastModified f (u/system-time))
        (u/throw-IOE "Unable to set the lastmodtime: %s." f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn chunk-read-stream

  ^{:arglists '([data cb])
    :doc "Reads through this data, and for each chunk
         calls the function."}

  [data cb]

  (let [b (byte-array c/FourK)
        [z? ^InputStream inp] (input-stream?? data)]
    (try (loop [c (if inp (.read inp b) 0) t 0]
           (if (pos? c)
             (do (cb b 0 c false)
                 (recur (.read inp b) (long (+ t c))))
             (cb b 0 0 true)))
         (finally
           (if z? (klose inp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt->edn

  ^{:arglists '([obj])
    :tag String
    :doc "Format to EDN."}

  [obj]

  (let [w (StringWriter.)]
    (if obj
      (pp/with-pprint-dispatch
        in/indent-dispatch (pp/pprint obj w))) (str w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-edn

  ^{:arglists '([arg][arg enc])
    :doc "Parse EDN formatted text."}

  ([arg]
   (read-edn arg "utf-8"))

  ([arg enc]
   (if-some [s (x->str arg enc)] (edn/read-string s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt->json

  ^{:arglists '([data])
    :tag String
    :doc "Format to JSON."}

  [data] (some-> data js/write-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-json

  ^{:arglists '([data]
                [data enc]
                [data enc keyfn])
    :doc "Parses JSON."}

  ([data enc] (read-json data enc nil))

  ([data] (read-json data "utf-8"))

  ([data enc keyfn]
   (if-some [s (x->str data enc)]
     (if keyfn
       (js/read-str s :key-fn keyfn) (js/read-str s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn res->stream

  ^{:arglists '([path][path ldr])
    :doc "Load the resource as stream."}

  {:tag InputStream}

  ([path]
   (res->stream path nil))

  ([path ldr]
   {:pre [(string? path)]}
   (when-not (empty? path)
     (-> (u/get-cldr ldr)
         (.getResourceAsStream ^String path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn res->url

  ^{:arglists '([path][path ldr])
    :doc "Load the resource as URL."}

  {:tag URL}

  ([path]
   (res->url path nil))

  ([path ldr]
   {:pre [(string? path)]}
   (when-not (empty? path)
     (-> (u/get-cldr ldr)
         (.getResource ^String path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn res->str

  ^{:arglists '([path]
                [path enc]
                [path enc ldr])
    :doc "Load the resource as string."}

  {:tag String}

  ([path enc] (res->str path enc nil))

  ([path] (res->str path "utf-8" nil))

  ([path enc ldr]
   (if-some [r (res->stream path ldr)]
     (c/wo* [inp r
             out (baos<> c/BUF-SZ)]
       (io/copy inp
                out
                :buffer-size c/BUF-SZ) (x->str out enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn res->bytes

  ^{:arglists '([path][path ldr])
    :doc "Load the resource as bytes."}

  {:tag "[B"}

  ([path]
   (res->bytes path nil))

  ([path ldr]
   (if-some [r (res->stream path ldr)]
     (c/wo* [inp r
             out (baos<> c/BUF-SZ)]
       (io/copy inp
                out
                :buffer-size c/BUF-SZ) (x->bytes out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn res->file

  ^{:arglists '([path][path ldr])
    :doc "Load the resource and write it to a temp file."}

  {:tag File}

  ([path]
   (res->file path nil))

  ([path ldr]
   (if-some [r (res->stream path ldr)]
     (c/do-with [out (temp-file)]
       (c/wo* [inp r]
         (io/copy inp out :buffer-size c/BUF-SZ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn delete-dir

  ^{:arglists '([dir])
    :doc "Deleting recursively a directory with native Java.
         https://docs.oracle.com/javase/tutorial/essential/io/walk.html"}

  [dir]

  (if-some [root (Paths/get (-> dir io/file .toURI))]
    (Files/walkFileTree root
                        (proxy [SimpleFileVisitor][]
                          (visitFile [^Path file
                                      ^BasicFileAttributes attrs]
                            (Files/delete file)
                            FileVisitResult/CONTINUE)
                          (postVisitDirectory [^Path dir
                                               ^IOException ex]
                            (Files/delete dir)
                            FileVisitResult/CONTINUE)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clean-dir

  ^{:arglists '([dir])
    :doc "Clean out recursively a directory with native Java.
         https://docs.oracle.com/javase/tutorial/essential/io/walk.html"}

  [dir]

  (if-some [root (Paths/get (-> dir io/file .toURI))]
    (Files/walkFileTree root
                        (proxy [SimpleFileVisitor][]
                          (visitFile [^Path file
                                      ^BasicFileAttributes attrs]
                            (Files/delete file)
                            FileVisitResult/CONTINUE)
                          (postVisitDirectory [^Path dir
                                               ^IOException ex]
                            (if (not= dir root)
                              (Files/delete dir))
                            FileVisitResult/CONTINUE)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cmenu

  ^{:arglists '([cmdQs q1])
    :doc "A console menu, prompting a sequence of questions via console."}

  [cmdQs q1]
  {:pre [(map? cmdQs)]}

  (letfn
    [(read-data [^Reader in]
       (let [[ms bf]
             (loop [c (.read in)
                    bf (c/sbf<>)]
               (let [m (cond (or (== c 4) (== c -1)) #{:quit :break}
                             (== c (int \newline)) #{:break}
                             (c/or?? [== c] 27 (int \return) (int \backspace)) nil
                             :else (c/do#nil (c/sbf+ bf (char c))))]
                 (if (c/in? m :break) [m bf] (recur (.read in) bf))))]
         (if-not (c/in? ms :quit) (c/strim (str bf)))))
     (on-answer [^Writer cout answer props {:keys [result id
                                                   must? default next]}]
       (if (nil? answer)
         (c/do#nil (.write cout "\n"))
         (let [rc (c/stror answer default)]
           (cond (and must?
                      (c/nichts? rc)) id ;no answer, loop try again
                 (keyword? result)
                 (do (swap! props assoc result rc) next)
                 (fn? result)
                 (let [[nxt p] (result rc @props)]
                   (reset! props p)
                   (or nxt ::caio!!))
                 :else ::caio!!))))
     (popQ [^Writer cout ^Reader cin props {:as Q
                                            :keys [must? choices
                                                   default question]}]
       (if (nil? Q)
         ::caio!!
         (do (.write cout (str question (if must? "*") "? "))
             (if (c/hgl? choices) (.write cout (str "[" choices "]")))
             (if (c/hgl? default) (.write cout (str "(" default ")")))
             (doto cout (.write " ") .flush)
             ;; get the input from user, return the next question
             (-> (read-data cin) (on-answer cout props Q)))))
     (cycleQ [cout cin cmdQs start props]
       (loop [rc (popQ cout cin props (cmdQs start))]
         (cond (nil? rc) {}
               (= ::caio!! rc) @props
               :else (recur (popQ cout cin props (cmdQs rc))))))]
    (let [cout (-> System/out
                   BufferedOutputStream. OutputStreamWriter.)
          func (->> System/in InputStreamReader. (partial cycleQ cout))]
      (.write cout (str ">>> Press "
                        "<ctrl-c> or <ctrl-d>"
                        "<Enter> to cancel...\n"))
      (-> #(update-in %1 [%2] assoc :id %2)
          (reduce cmdQs (keys cmdQs)) (func q1 (atom {}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_
(def sample
  {:q1 {:question "hello ken"
        :choices "q|b|c"
        :default "c"
        :must? true
        :result :a1
        :next :q2}
   :q2 {:question "hello paul"
        :result :a2
        :next :q3}
   :q3 {:question "hello joe"
        :choices "2"
        :result (fn [answer result]
                  [nil (assoc result :zzz answer)])}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

