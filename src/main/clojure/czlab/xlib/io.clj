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

(ns ^{:doc "Util functions related to stream/io"
      :author "kenl" }

  czlab.xlib.io

  (:require
    [czlab.xlib.core :refer [spos? try!]]
    [czlab.xlib.logging :as log]
    [clojure.java.io :as io]
    [clojure.string :as cs])

  (:import
    [java.util.zip GZIPInputStream GZIPOutputStream]
    [org.apache.commons.codec.binary Base64]
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
     File
     InputStream
     InputStreamReader
     Closeable
     OutputStream
     Reader
     Writer]
    [java.nio ByteBuffer CharBuffer]
    [org.apache.commons.io IOUtils]
    [java.nio.charset Charset]
    [czlab.xlib XData XStream]
    [org.xml.sax InputSource]
    [java.nio.charset Charset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:private ^chars HEX_CHS (.toCharArray "0123456789ABCDEF"))
(def ^:private _slimit (atom (* 4 1024 1024)))
(def ^:private _wd
  (atom (io/file (System/getProperty "java.io.tmpdir"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn streamLimit

  "Beyond this limit, data will be swapped out to disk (temp file)"

  ^long
  []
  @_slimit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn setStreamLimit!

  "Set the limit to flush to disk"

  ^long
  [limit]

  (when (spos? limit)
    (reset! _slimit limit))
  @_slimit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn workDir "The working directory" ^File [] @_wd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn setWorkDir!

  "Set the working directory"

  ^File
  [dir]

  (->> (doto
         (io/file dir)
         (.mkdirs))
       (reset! _wd))
  @_wd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn byteOS

  "Make a byte array output stream"

  ^ByteArrayOutputStream
  [ & [size] ]

  (ByteArrayOutputStream. (int (or size 4096))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti writeBytes "Write this long value out as byte[]" class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toBytes

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
(defmethod writeBytes Integer

  ^bytes
  [nnum]

  (with-open [baos (byteOS)]
    (doto
      (DataOutputStream. baos)
      (.writeInt (int nnum))
      (.flush))
    (.toByteArray baos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod writeBytes Long

  ^bytes
  [nnum]

  (with-open [baos (byteOS) ]
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

  (when (some? bits)
    (ByteArrayInputStream. bits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn closeQ

  "Quietly close this object"

  [obj]

  (when
    (instance? Closeable obj)
    (try! (.close ^Closeable obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hexifyChars

  "Turn bytes into hex chars"

  ^chars
  [^bytes bits]

  (let [len (if (nil? bits) 0 (* 2 (alength bits)))
        out (char-array len)]
    (loop [k 0 pos 0]
      (when-not (>= pos len)
        (let [n (bit-and (aget ^bytes bits k) 0xff) ]
          (aset-char out pos
                     (aget ^chars HEX_CHS (bit-shift-right n 4))) ;; high 4 bits
          (aset-char out (+ pos 1)
                     (aget ^chars HEX_CHS (bit-and n 0xf))) ;; low 4 bits
          (recur (inc k) (+ 2 pos)) )))
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hexifyString

  "Turn bytes into hex string"

  ^String
  [^bytes bits]

  (when (some? bits)
    (String. (hexifyChars bits))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti openFile "Open this file path" class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gzip

  "Gzip these bytes"

  ^bytes
  [^bytes bits]

  (when (some? bits)
    (let [baos (byteOS)]
      (with-open [g (GZIPOutputStream. baos)]
        (.write g bits 0 (alength bits)))
      (.toByteArray baos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gunzip

  "Gunzip these bytes"

  ^bytes
  [^bytes bits]

  (when (some? bits)
    (IOUtils/toByteArray (GZIPInputStream. (streamify bits)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetStream!

  "Call reset on this input stream"

  [^InputStream inp]

  (try! (when (some? inp) (.reset inp)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile String

  ^XStream
  [^String fp]

  (when (some? fp)
    (XStream. (io/file fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod openFile File

  ^XStream
  [^File f]

  (when (some? f) (XStream. f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fromGZB64

  "Unzip content which is base64 encoded + gziped"

  ^bytes
  [^String gzb64]

  (when (some? gzb64)
    (gunzip (Base64/decodeBase64 gzb64))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toGZB64

  "Zip content and then base64 encode it"

  ^String
  [^bytes bits]

  (when (some? bits)
    (Base64/encodeBase64String (gzip bits))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn available

  "Get the available bytes in this stream"

  ;; int
  [^InputStream inp]

  (if (nil? inp) 0 (.available inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tempFile

  "Create a temporary file"

  ^File
  [ & [pfx sux] ]

  (File/createTempFile
    (if (> (count pfx) 2) pfx "tmp-")
    (if (> (count sux) 2) sux ".dat")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn openTempFile

  "A Tuple(2) [ File, OutputStream? ]"

  []

  (let [fp (tempFile)]
    [fp (FileOutputStream. fp)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyStream

  "Copy content from this input-stream to a temp file"

  ^File
  [^InputStream inp]

  (let [[^File fp ^OutputStream os]
        (openTempFile) ]
    (try
      (IOUtils/copy inp os)
      (finally
        closeQ os))
    fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyBytes

  "Copy x number of bytes from the source input-stream"

  [^InputStream src ^OutputStream out bytesToCopy]

  (when
    (> bytesToCopy 0)
    (IOUtils/copyLarge src out 0 ^long bytesToCopy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resetSource!

  "Reset an input source"

  [^InputSource inpsrc]

  (when (some? inpsrc)
    (let [rdr (.getCharacterStream inpsrc)
          ism (.getByteStream inpsrc) ]
      (try! (when (some? ism) (.reset ism)) )
      (try! (when (some? rdr) (.reset rdr)) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn newXData

  "A newly created XData"

  ^XData
  [ & [usefile] ]

  (if usefile
    (XData. (tempFile))
    (XData.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swapBytes

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
(defn swapChars

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
(defn- slurpBytes

  "Read bytes from the stream"

  ^XData
  [^InputStream inp limit]

  (with-local-vars
    [os (byteOS) fout nil]
    (loop [bits (byte-array 4096)
           cnt 0
           c (.read inp bits)]
      (if
        (< c 0)
        (try
          (if (some? @fout)
            (XData. @fout)
            (XData. @os))
          (finally
            (closeQ @os)))
        ;;else
        (do
          (when (> c 0)
            (.write ^OutputStream @os bits 0 c)
            (if (and (nil? @fout)
                     (> (+ c cnt) limit))
              (let [[f o] (swapBytes @os)]
                (var-set fout f)
                (var-set os o))))
          (recur bits
                 (+ c cnt)
                 (.read inp bits)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- slurpChars

  "Read chars from the reader"

  ^XData
  [^Reader rdr limit]

  (with-local-vars
    [wtr (CharArrayWriter. (int 4096))
     fout nil]
    (loop [carr (char-array 4096)
           cnt 0
           c (.read rdr carr)]
      (if
        (< c 0)
        (try
          (if (some? @fout)
            (XData. @fout)
            (XData. @wtr))
          (finally
            (closeQ @wtr)))
        ;;else
        (do
          (when (> c 0)
            (.write ^Writer @wtr carr 0 c)
            (if (and (nil? @fout)
                     (> (+ c cnt) limit))
              (let [[f w] (swapChars @wtr)]
                (var-set fout f)
                (var-set wtr w))))
          (recur carr
                 (+ c cnt)
                 (.read rdr carr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readBytes

  "Read bytes and return a XData"

  ^XData
  [^InputStream inp & [usefile]]

  (slurpBytes inp (if usefile 1 (streamLimit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readChars

  "Read chars and return a XData"

  ^XData
  [^Reader rdr & [usefile]]

  (slurpChars rdr (if usefile 1 (streamLimit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn morphChars

  "Convert these bytes to chars"

  ^chars
  [^bytes bits & [charSet]]

  (when (some? bits)
    (let [^Charset
          cs (or charSet (Charset/forName "utf-8")) ]
      (IOUtils/toCharArray (streamify bits) cs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


