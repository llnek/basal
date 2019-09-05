;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "IO, streams and file helpers."
      :author "Kenneth Leung"}

  czlab.basal.io

  (:require [clojure.data.json :as js]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as cs]
            [clojure.edn :as edn]
            [czlab.basal.str :as s]
            [czlab.basal.util :as u]
            [czlab.basal.meta :as m]
            [czlab.basal.core :as c]
            [czlab.basal.indent :as in])

  (:import [java.nio.file
            Files
            CopyOption
            StandardCopyOption]
           [java.util.zip
            GZIPInputStream
            GZIPOutputStream]
           [java.nio
            ByteBuffer
            CharBuffer]
           [czlab.basal XData]
           [java.nio.charset
            Charset]
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
            InputStreamReader
            OutputStreamWriter
            ByteArrayInputStream
            ByteArrayOutputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def ^:dynamic *tempfile-repo* (io/file (u/sys-tmp-dir)))
(def ^:dynamic *membuf-limit* (* 4 c/MegaBytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private os* [x] `(clojure.java.io/output-stream ~x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private is* [x] `(clojure.java.io/input-stream ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro istream
  [x] `(clojure.java.io/input-stream ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ostream
  [x] `(clojure.java.io/output-stream ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro file?
  "Is file?" [in] `(instance? java.io.File ~in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro slurp-utf8
  "Read f with utf8 encoding" [f] `(slurp ~f :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro spit-utf8
  "Write f with utf8 encoding" [f c] `(spit ~f ~c :encoding "utf-8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn baos<>
  "Make a byte array output stream"
  {:tag ByteArrayOutputStream}
  ([] (baos<> nil))
  ([size]
   (ByteArrayOutputStream. (int (c/num?? size c/BUF-SZ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fsize
  "Get length of file." [in]
  (if-some [f (io/file in)] (.length f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fname
  "Get name of file." ^String [in]
  (if-some [f (io/file in)] (.getName f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn klose
  "Close object (quietly)."
  [obj] (c/try! (some-> (c/cast? Closeable obj) .close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn copy
  "Copy certain number of bytes to output."
  ([in out] (copy in out -1))
  ([in out kount]
   {:pre [(number? kount)]}
   (if (neg? kount)
     (io/copy in out)
     (let [[delis? ^InputStream is]
           (if (c/is? InputStream in)
             [false in] [true (is* in)])
           [delos? ^OutputStream os]
           (if (c/is? OutputStream out)
             [false out] [true (os* out)])]
       (try (loop [remain kount
                   total 0
                   buf (byte-array c/BUF-SZ)]
              (let [len (if (< remain c/BUF-SZ) remain c/BUF-SZ)
                    n (if (pos? len) (.read is buf 0 len) -1)]
                (if (neg? n)
                  nil ;to be consistent with io/copy, total
                  (do (if (pos? n)
                        (.write os buf 0 n))
                      (recur (long (- remain n))
                             (long (+ total n)) buf)))))
            (finally
              (if delis? (klose is))
              (if delos? (klose os))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->bytes
  "Convert almost anything to byte[]." {:tag "[B"}
  ([in] (x->bytes in "utf-8"))
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
         (c/is? ByteArrayOutputStream in)
         (.toByteArray ^ByteArrayOutputStream in)
         :else
         (let [os (baos<>)] (io/copy in os) (.toByteArray os)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->chars
  "Convert almost anything to char[]." {:tag "[C"}
  ([in] (x->chars in "utf-8"))
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
  "Convert almost anything to String."
  {:tag String}
  ([in] (x->str in "utf-8"))
  ([in enc] (cond (or (nil? in)
                      (string? in))
                  in
                  (c/is? StringBuilder in)
                  (.toString ^Object in)
                  :else (String. (x->chars in enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-number
  "Deserialize a number."
  ^Number [in numType]
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
  "Serialize a number" ^bytes [nnum]
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
  "Delete file (quietly)."
  [f] (c/try! (if (c/is? File f)
                (io/delete-file f true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bytes->hex
  "Bytes into hex-chars."
  ^chars [in]
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
  "Turn bytes into hex string."
  ^String [in] (some-> in bytes->hex x->str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gzip
  "Gzip input."
  ^bytes [in]
  (if-some [b (x->bytes in)]
    (with-open [baos (baos<>)
                g (GZIPOutputStream. baos)]
      (.write g b 0 (alength b))
      (.finish g)
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gunzip
  "Gunzip input."
  ^bytes [in]
  (if-some [b (x->bytes in)]
    (with-open [inp (GZIPInputStream.
                      (io/input-stream b))] (x->bytes inp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reset-stream!
  "Reset stream (safely)."
  [in]
  (if-some [inp (c/cast? InputStream in)] (c/try! (.reset inp))) in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn input-stream??
  "A tuple [you-close? stream]."
  ([arg] (input-stream?? arg "utf-8"))
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
  "Unzip content which is base64 encoded + gziped."
  ^bytes [in] (some->> in x->str (.decode (Base64/getDecoder)) gunzip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bytes->gzb64
  "Zip content and then base64 encode it."
  ^String [in] (some->> (some-> in x->bytes gzip)
                        (.encodeToString (Base64/getEncoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn readable-bytes
  "Get the available bytes in this stream."
  ^Integer
  ([in] (readable-bytes in "utf-8"))
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
  "Create f in temp dir."
  ^File
  [f] (io/file *tempfile-repo* f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn temp-file
  "Create a temporary file."
  {:tag File}
  ([pfx sux dir]
   (File/createTempFile (s/stror pfx "czlab")
                        (s/stror sux ".tmp") (io/file dir)))
  ([] (temp-file nil nil))
  ([pfx sux] (temp-file pfx sux *tempfile-repo*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn open-temp-file
  "A Tuple(2) [File, OutputStream?]."
  ([pfx sux dir]
   (let [fp (temp-file pfx sux dir)] [fp (os* fp)]))
  ([] (open-temp-file nil nil))
  ([pfx sux] (open-temp-file pfx sux *tempfile-repo*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->file
  "Copy content to a temp file."
  ([in] (x->file in nil))
  ([in fout]
   (let [fp (or fout (temp-file))]
     (try (io/copy in fp)
          fp
          (catch Throwable _
            (if (nil? fout) (fdelete fp)) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reset-source!
  "Reset an input source."
  [in]
  (if-some [src (c/cast? InputSource in)]
    (let [ism (.getByteStream src)
          rdr (.getCharacterStream src)]
      (c/try! (some-> ism .reset))
      (c/try! (some-> rdr .reset)))))

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
(defn- slurp-inp [^InputStream inp limit enc]
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
                (if (and (nil? fo)
                         (> (+ c cnt) limit))
                  (swap-bytes os enc) [fo os])]
            (recur bits o' f (+ c cnt) (.read inp bits)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- slurp-rdr [^Reader rdr limit enc]
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
                (if (and (nil? fo)
                         (> (+ c cnt) limit))
                  (swap-chars cw enc) [fo cw])]
            (recur w carr f (+ c cnt) (.read rdr carr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn slurpb
  "Like slurp but reads in bytes."
  ([in enc] (slurpb in enc false))
  ([in] (slurpb in "utf-8"))
  ([in enc usefile?]
   (let [[c? i] (input-stream?? in)]
     (try (slurp-inp i (if usefile?
                         1 *membuf-limit*) enc) (finally
                                                  (if c? (klose i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn slurpc
  "Like slurp but reads in chars."
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
  "Is file readable & writable?" [in]
  (if-some [^File fp (some-> in (io/file))]
    (and (.exists fp)
         (.isFile fp) (.canRead fp) (.canWrite fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-ok?
  "If file exists?" [in]
  (if-some [^File fp (some-> in (io/file))] (.exists fp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-read?
  "Is file readable?" [in]
  (if-some [^File fp (some-> in (io/file))]
    (and (.exists fp) (.isFile fp) (.canRead fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dir-read-write?
  "Is dir readable and writable?" [in]
  (if-some [^File dir (some-> in (io/file))]
    (and (.exists dir)
         (.isDirectory dir) (.canRead dir) (.canWrite dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dir-read?
  "Is dir readable?" [in]
  (if-some [^File dir (some-> in (io/file))]
    (and (.exists dir) (.isDirectory dir) (.canRead dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn can-exec?
  "Is file executable?" [in]
  (if-some
    [^File fp (some-> in (io/file))]
    (and (.exists fp) (.canExecute fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parent-file
  "Parent file." ^File [in]
  (if-some [^File f (some-> in (io/file))] (.getParentFile f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parent-path
  "Path to parent." ^String [path]
  (if (s/hgl? path) (.getParent (io/file path)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-file
  "Get a file from a directory."
  [dir fname]
  (let [fp (io/file dir fname)]
    (if (file-read? fp) (XData. fp false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn save-file
  "Save a file to a directory"

  ([dir fname stuff]
   (save-file dir fname stuff false))

  ([dir fname stuff del?]
   (let [fp (io/file dir fname)
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
         (.reset in nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn change-content
  "Pass file content - as string to
  the work function, returning new content."
  {:tag String}
  ([file work]
   (change-content file work "utf-8"))
  ([file work enc]
   {:pre [(fn? work)]}
   (if (file-read? file)
     (work (slurp file :encoding (u/encoding?? enc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn replace-file!
  "Update file with new content."
  ([file work]
   (replace-file! file work "utf-8"))
  ([file work enc]
   {:pre [(fn? work)]}
   (spit file (change-content file work enc) :encoding (u/encoding?? enc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unzip->dir
  "Unzip zip file to a target folder."
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
  "Make directories."
  ^File [arg]
  (cond (string? arg) (mkdirs (io/file arg))
        (c/is? File arg) (doto ^File arg .mkdirs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-files
  "List files with certain extension."
  [dir ext]
  (c/vec-> (.listFiles (io/file dir)
                       (reify FileFilter
                         (accept [_ f]
                           (cs/ends-with? (.getName f) ext))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-dirs
  "List sub-directories."
  [dir]
  (c/vec-> (.listFiles (io/file dir)
                       (reify FileFilter
                         (accept [_ f] (.isDirectory f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-any-files
  "List files with certain extension recursively."
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
  "Recurse a dir, pick out dirs
   that have files of this extension."
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
  "Recurse a folder, picking out files with the given extension."
  [rootDir ext]
  ;; the stack is used to store the folder hierarchy
  @(scan-tree (Stack.) ext (atom []) (io/file rootDir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn basename
  "Get the name of file without extension."
  [file]
  (let [n (.getName (io/file file))
        p (cs/last-index-of n ".")] (if p (subs n 0 p) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn touch!
  "Touch a file."
  [file]
  (if-some [f (io/file file)]
    (if-not (.exists f)
      (do (.. f getParentFile mkdirs) (c/wo* [o (os* f)]))
      (when-not
        (.setLastModified f (u/system-time))
        (u/throw-IOE "Unable to set the lastmodtime: %s." f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn chunk-read-stream
  "Reads through this data, and for each chunk
  calls the function."
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
  "Format to EDN."
  ^String [obj]
  (let [w (StringWriter.)]
    (if obj
      (pp/with-pprint-dispatch
        in/indent-dispatch (pp/pprint obj w))) (str w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-edn
  "Parse EDN formatted text."
  ([arg]
   (read-edn arg "utf-8"))
  ([arg enc]
   (if-some [s (x->str arg enc)] (edn/read-string s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt->json
  "Format to JSON."
  ^String [data] (some-> data js/write-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-json
  "Parses JSON."
  ([data enc] (read-json data enc nil))
  ([data] (read-json data "utf-8"))
  ([data enc keyfn]
   (if-some [s (x->str data enc)]
     (if keyfn
       (js/read-str s :key-fn keyfn) (js/read-str s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn res->stream
  "Load the resource as stream."
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
  "Load the resource as URL."
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
  "Load the resource as string."
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
  "Load the resource as bytes."
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
;;EOF
