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

(ns ^{:doc "IO & stream helpers."
      :author "Kenneth Leung" }

  czlab.xlib.io

  (:require
    [czlab.xlib.logging :as log]
    [clojure.java.io :as io]
    [clojure.string :as cs])

  (:use [czlab.xlib.consts]
        [czlab.xlib.meta]
        [czlab.xlib.core]
        [czlab.xlib.str])

  (:import
    [java.util.zip GZIPInputStream GZIPOutputStream]
    [java.util Base64 Base64$Decoder Base64$Encoder]
    [clojure.lang APersistentVector]
    [java.io
     ByteArrayOutputStream
     ByteArrayInputStream
     DataInputStream
     DataInputStream
     DataOutputStream
     FileInputStream
     FileOutputStream
     CharArrayWriter
     OutputStreamWriter
     IOException
     File
     InputStream
     InputStreamReader
     Closeable
     OutputStream
     Reader
     Writer]
    [java.nio ByteBuffer CharBuffer]
    [java.nio.charset Charset]
    [czlab.xlib XData XStream]
    [org.xml.sax InputSource]
    [java.nio.charset Charset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:dynamic *TEMPFILE_REPO* (io/file (sysProp "java.io.tmpdir")))
(def ^:dynamic *MEMBUF_LIMIT* (* 4 MegaBytes))

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
        (when (> n 0)
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
      (let [len (if (< remain bsz) remain bsz)
            n (if (> len 0) (.read input buf 0 len) -1)]
        (if (< n 0)
          total
          (do
            (when (> n 0)
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
      (trap! IOException "size too large")
      cnt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toBytes

  ""
  ^bytes
  [^InputStream input]

  (let [out (ByteArrayOutputStream.)]
    (copy input out)
    (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn baos<>

  "Make a byte array output stream"
  {:tag ByteArrayOutputStream }

  ([] (baos<> BUF_SZ))
  ([size]
   (ByteArrayOutputStream. (int (or size BUF_SZ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti writeBytes
  "Write this long value out as byte[]" {:tag bytes} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsToBytes

  "Convert char[] to byte[]"
  ^bytes
  [^chars chArray ^String encoding]

  (-> (Charset/forName encoding)
      (.encode (CharBuffer/wrap chArray))
      (.array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toChars

  "Convert byte[] to char[]"
  ^chars
  [^bytes byteArray ^String encoding]

  (-> (Charset/forName encoding)
      (.decode (ByteBuffer/wrap byteArray))
      (.array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readLong

  "A long by scanning the byte[]"
  [^bytes byteArray]

  (-> (ByteArrayInputStream. byteArray)
      (DataInputStream.)
      (.readLong)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readInt

  "An int by scanning the byte[]"
  [^bytes byteArray]

  (-> (ByteArrayInputStream. byteArray)
      (DataInputStream.)
      (.readInt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod writeBytes

  Integer
  [nnum]

  (with-open [baos (baos<>)]
    (doto
      (DataOutputStream. baos)
      (.writeInt (int nnum))
      (.flush))
    (.toByteArray baos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod writeBytes

  Long
  [nnum]

  (with-open [baos (baos<>) ]
    (doto
      (DataOutputStream. baos)
      (.writeLong ^long nnum)
      (.flush))
    (.toByteArray baos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn streamify

  "Wrapped these bytes in an input-stream"
  ^InputStream
  [^bytes bits]

  (if (some? bits)
    (ByteArrayInputStream. bits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn closeQ

  "Quietly close this object"
  [obj]

  (trye!
    nil
    (some-> (cast? Closeable obj)
            (.close))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteQ

  ""
  [file]

  (trye! nil (.delete (io/file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hexifyChars

  "Turn bytes into hex chars"
  ^chars
  [^bytes bits]

  (let [len (if (nil? bits) 0 (* 2 (alength bits)))
        hx (.toCharArray HEX_CHS)
        out (char-array len)]
    (loop [k 0 pos 0]
      (when-not (>= pos len)
        (let [n (bit-and (aget ^bytes bits k) 0xff) ]
          (aset-char out pos
                     (aget hx (unsigned-bit-shift-right n 4))) ;; high 4 bits
          (aset-char out (+ pos 1)
                     (aget hx (bit-and n 0xf))) ;; low 4 bits
          (recur (inc k) (+ 2 pos)) )))
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hexify

  "Turn bytes into hex string"
  ^String
  [^bytes bits]

  (if (some? bits)
    (String. (hexifyChars bits))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti openFile "Open this file path" {:tag XStream} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gzip

  "Gzip these bytes"
  ^bytes
  [^bytes bits]

  (if (some? bits)
    (let [baos (baos<>)]
      (with-open [g (GZIPOutputStream. baos)]
        (.write g bits 0 (alength bits)))
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gunzip

  "Gunzip these bytes"
  ^bytes
  [^bytes bits]

  (if (some? bits)
    (toBytes (GZIPInputStream. (streamify bits)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro resetStream!

  "Call reset on this input stream"
  [inp]

  `(trye!
     nil
     (some->
       ~(with-meta inp {:tag 'java.io.InputStream}) (.reset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile

  String
  [^String fp]

  (if (some? fp)
    (XStream. (io/file fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile

  File
  [^File f]

  (if (some? f) (XStream. f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fromGZB64

  "Unzip content which is base64 encoded + gziped"
  ^bytes
  [^String gzb64]

  (if (some? gzb64)
    (-> (Base64/getDecoder)
        (.decode gzb64)
        (gunzip ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toGZB64

  "Zip content and then base64 encode it"
  ^String
  [^bytes bits]

  (if (some? bits)
    (-> (Base64/getEncoder)
        (.encodeToString (gzip bits)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn available

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
  ([pfx sux] (tempFile pfx sux *TEMPFILE_REPO*)))

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
   (openTempFile pfx sux *TEMPFILE_REPO*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyStream

  "Copy content from this input-stream to a temp file"
  ^File
  [^InputStream inp]

  (let [[^File fp ^OutputStream os]
        (openTempFile) ]
    (try
      (copy inp os)
      (finally
        (closeQ os)))
    fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetSource!

  "Reset an input source"
  [^InputSource inpsrc]

  (if (some? inpsrc)
    (let [rdr (.getCharacterStream inpsrc)
          ism (.getByteStream inpsrc) ]
      (trye! nil (some-> ism (.reset)))
      (trye! nil (some-> rdr (.reset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro xdata<>

  "Create XData with content"
  {:tag XData}

  ([c] `(XData. ~c))
  ([] `(XData. )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro xdata<f>

  "Create XData with temp-file"
  {:tag XData}

  ([dir] `(XData. (tempFile "" "" ~dir)))
  ([] `(XData. (tempFile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapBytes

  "Swap bytes in buffer to file, returning a [File,OStream] tuple"
  [^ByteArrayOutputStream baos]

  (let [[^File fp ^OutputStream os]
        (openTempFile) ]
    (doto os
      (.write (.toByteArray baos))
      (.flush))
    (.close baos)
    [fp os]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- swapChars

  "Swap chars in writer to file, returning a [File,OWriter] tuple"
  [^CharArrayWriter wtr]

  (let [[^File fp ^OutputStream out]
        (openTempFile)
        w (OutputStreamWriter. out "utf-8") ]
    (doto w
      (.write (.toCharArray wtr))
      (.flush))
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

  ([^InputStream inp] (readBytes inp false))
  ([^InputStream inp usefile?]
   (slurpb inp (if usefile? 1 *MEMBUF_LIMIT*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readChars

  "Read chars and return a XData"
  {:tag XData}

  ([^Reader rdr] (readChars rdr false))
  ([^Reader rdr usefile?]
   (slurpc rdr (if usefile? 1 *MEMBUF_LIMIT*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn workDirPath
  "The working directory" ^String [] (fpath *TEMPFILE_REPO*))

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
;;EOF


