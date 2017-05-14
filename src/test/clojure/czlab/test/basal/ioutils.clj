;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.ioutils

  (:require [clojure.java.io :as io]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s]
            [czlab.basal.io :as i])

  (:use [clojure.test])

  (:import [java.io
            FileReader
            File
            InputStream
            OutputStream
            InputStreamReader
            FileOutputStream
            ByteArrayInputStream
            ByteArrayOutputStream]
           [czlab.jasal XData XStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^File TMP_DIR (io/file (c/sysTmpDir)))
(def ^:private ^File TMP_FP
  (io/file TMP_DIR (str (c/jid<>) ".txt")))
(eval '(do (spit TMP_FP "heeloo" :encoding "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-ioutils

  (is (.exists TMP_DIR))

  (is (= "heeloo"
         (String. (i/toChars (i/charsToBytes
                             (.toCharArray "heeloo") "utf-8") "utf-8"))))

  (testing
    "related to: read/write numbers"
    (is (== 4 (alength (i/writeNumber Integer/MAX_VALUE))))
    (is (== 8 (alength (i/writeNumber Long/MAX_VALUE))))
    (is (== Long/MAX_VALUE
            (i/readNumber (i/writeNumber Long/MAX_VALUE) Long)))
    (is (== Integer/MAX_VALUE
            (i/readNumber (i/writeNumber Integer/MAX_VALUE) Integer)))
    (is (== Integer/MAX_VALUE
            (i/readNumber (i/writeNumber Integer/MAX_VALUE) Integer)))
    (is (== Long/MAX_VALUE
            (i/readNumber (i/writeNumber Long/MAX_VALUE) Long))))

  (testing
    "related to: string<->chars<->bytes"
    (is (= "ab"
           (let [baos (i/baos<>)
                 inp (i/streamit (c/bytesit "abcde"))]
             (i/copyBytes inp baos 2)
             (c/strit (.toByteArray baos)))))

    (is (= "abcde"
           (let [baos (i/baos<> 100)
                 inp (i/streamit (c/bytesit "abcde"))]
             (i/copyBytes inp baos 100)
             (c/strit (.toByteArray baos)))))

    (is (= "abcde"
           (let [baos (ByteArrayOutputStream.)
                 inp (i/streamit (c/bytesit "abcde"))]
             (i/copy inp baos)
             (c/strit (.toByteArray baos)))))

    (is (= "abcde"
           (let [inp (i/streamit (c/bytesit "abcde"))]
             (c/strit (i/toBytes inp)))))

    (is (= "hello"
           (c/strit (i/charsToBytes (.toCharArray "hello")))))

    (is (= "hello"
           (String. (i/toChars (.getBytes "hello" "utf-8"))))))

  (is (== 5 (i/readableBytes (i/streamit (c/bytesit "hello")))))
  (is (c/do->true (i/closeQ (i/baos<>))))

  (testing
    "related to: hex<->bytes"
    (is (some? (i/bytesToHex (.getBytes "abc"))))
    (is (= "616263" (i/hexify (.getBytes "abc")))))

  (testing
    "related to: gzip/gunzip"
    (is (= "hello"
           (c/strit (i/gunzip (i/gzip (c/bytesit "hello"))))))

    (is (= "helloworld"
           (c/strit
             (i/fromGZB64 (i/toGZB64 (c/bytesit "helloworld")))))))

  (testing
    "related to: temp files"
    (is (let [f (i/tempFile)
              _ (spit f "a")
              e? (.exists f)] (i/deleteQ f) e?))

    (is (let [[f os] (i/openTempFile)
              o? (some? os)] (i/closeQ os) (i/deleteQ f) o?))

    (is (= "hello"
           (let [f (i/copyStream (i/streamit (c/bytesit "hello")))
                 s (slurp f)] (i/deleteQ f) s))))

  (is (c/ist? XData (i/xdata<> nil)))
  (is (let [x (i/fdata<>)
            f? (some? (.fileRef x))] f?))

  (testing
    "related to: stream coersions"
    (is (nil? (i/inputStream?? 333)))

    (is (let [[c i] (i/inputStream?? "aaa")]
          (if c (i/closeQ i))
          (c/ist? InputStream i))))

  (testing
    "related to: mime properties"
    (is (with-open [inp (c/resStream "czlab/basal/etc/mime.properties")]
          (binding [i/*membuf-limit* (* 2 1024)]
            (let [z (.available inp)
                  x (i/rxBytes inp)
                  f (.fileRef x)
                  n (if (some? f)
                      (.length f) 0)]
              (.dispose x)
              (== z n)))))

    (is (with-open [inp (c/resStream "czlab/basal/etc/mime.properties")
                    rdr (InputStreamReader. inp)]
          (binding [i/*membuf-limit* (* 2 1024)]
            (let [z (.available inp)
                  x (i/rxChars rdr)
                  f (.fileRef x)
                  n (if (some? f)
                      (.length f) 0)]
              (.dispose x)
              (== z n))))))

  (is (== 5 (alength (i/bytes?? "hello"))))

  (testing
    "related to: file operations"

    (is (= "hello"
           (let [f (i/tempFile)
                 _ (i/spitUtf8 f "hello")
                 s (i/slurpUtf8 f)] (i/deleteQ f) s)))

    (is (let
          [f (i/tempFile)
           p (.getCanonicalPath f)
           pp (i/parentPath p)
           dd (io/file pp)
           d (i/parentFile f)
           _ (i/spitUtf8 f "hello")
           d? (i/dirRead? d)
           dw? (i/dirReadWrite? d)
           x? (i/canExec? d)
           ok? (i/fileOK? f)
           r? (i/fileRead? f)
           f? (i/fileReadWrite? f)]
          (i/deleteQ f)
          (and ;;(= d dd)
               d? dw? x? ok? r? f?)))

    (is (let [f (i/tempFile)
              _ (i/spitUtf8 f "hello")
              t (.lastModified f)
              p (.getCanonicalPath f)
              f2 (str p ".t")]
          (c/pause 1000)
          (i/touch! f)
          (i/touch! f2)
          (let [rc (and (> (.lastModified f) t)
                        (i/fileOK? (io/file f2)))]
            (i/deleteQ f)
            (i/deleteQ f2)
            rc)))

    (is (= "helloworld"
           (let [f (i/tempFile)
                 _ (i/spitUtf8 f "hello")
                 s (i/changeContent f #(str % "world"))]
             (i/deleteQ f)
             s)))

    (is (= "helloworld"
           (let [f (i/tempFile)
                 _ (i/spitUtf8 f "hello")
                 _ (i/replaceFile! f #(str % "world"))
                 s (i/slurpUtf8 f)]
             (i/deleteQ f)
             s)))

    (is (= "hello"
           (let [f (i/tempFile)
                 _ (i/writeFile f "hello")
                 s (i/slurpUtf8 f)]
             (i/deleteQ f)
             s)))

    (is (== 5
           (let [f (i/tempFile)
                 _ (i/spitUtf8 f "hello")
                 b (i/slurpBytes f)]
             (i/deleteQ f)
             (alength b))))

    (is (= "hello"
           (let [f (i/tempFile)
                 _ (i/spitUtf8 f "hello")
                 s (i/readAsStr f)]
             (i/deleteQ f)
             s)))

    (is (= "hello"
           (let [n (c/jid<>)
                 _ (i/saveFile i/*tempfile-repo* n (i/xdata<> "hello") true)
                 x (i/getFile i/*tempfile-repo* n)
                 s (i/readAsStr (.fileRef x))]
             (i/deleteQ (.fileRef x)) s)))

    (is (let [n (c/jid<>)
              d (io/file i/*tempfile-repo* n)
              _ (i/mkdirs d)
              e? (i/fileOK? d)]
          (i/deleteQ d) e?))

    (is (let [n0 (c/jid<>)
              n1 (c/jid<>)
              n2 (c/jid<>)
              f1 (str n1 ".txt")
              f2 (str n2 ".txt")
              root (io/file i/*tempfile-repo* n0)
              d1 (io/file root n1)
              d2 (io/file root n2)
              _ (i/mkdirs root)
              _ (i/mkdirs d1)
              _ (i/mkdirs d2)
              _ (i/spitUtf8 (io/file root f1) "a")
              _ (i/spitUtf8 (io/file root f2) "a")
              _ (i/spitUtf8 (io/file d1 f1) "a")
              _ (i/spitUtf8 (io/file d2 f2) "a")
              dz (count (i/listDirs root))
              fz (count (i/listFiles root ".txt"))
              tz (count (i/listAnyFiles root ".txt"))]
          (i/deleteQ (io/file d1 f1))
          (i/deleteQ (io/file d2 f2))
          (i/deleteQ (io/file root f1))
          (i/deleteQ (io/file root f2))
          (i/deleteQ (io/file d1))
          (i/deleteQ (io/file d2))
          (i/deleteQ root)
          (and (== 2 dz) (== 2 fz) (== 4 tz))))

    (is (let [n0 (c/jid<>) n1 (c/jid<>) n2 (c/jid<>)
              n3 (c/jid<>) n4 (c/jid<>) n5 (c/jid<>)
              root (io/file i/*tempfile-repo* n0)
              _ (i/mkdirs root)
              d1 (doto (io/file root n1) (i/mkdirs))
              d2 (doto (io/file root n2) (i/mkdirs))
              d3 (doto (io/file root n3) (i/mkdirs))
              d4 (doto (io/file d2 n4) (i/mkdirs))
              d5 (doto (io/file d4 n5) (i/mkdirs))
              _ (i/spitUtf8 (io/file d1 "d1.txt") "a")
              _ (i/spitUtf8 (io/file d4 "d4a.txt") "a")
              _ (i/spitUtf8 (io/file d4 "d4b.txt") "a")
              _ (i/spitUtf8 (io/file d5 "d5.txt") "a")
              ds (i/grepFolderPaths root ".txt")
              fs (i/grepFilePaths root ".txt")]
          (i/deleteQ (io/file d1 "d1.txt"))
          (i/deleteQ (io/file d4 "d4a.txt"))
          (i/deleteQ (io/file d4 "d4b.txt"))
          (i/deleteQ (io/file d5 "d5.txt"))
          (i/deleteQ d5)(i/deleteQ d4)
          (i/deleteQ d1)(i/deleteQ d2)(i/deleteQ d3)
          (i/deleteQ root)
          (and (== 3 (count ds))
               (== 4 (count fs)))))

    (is (let [n (c/jid<>)
              f (io/file i/*tempfile-repo* (str n ".txt"))
              _ (i/spitUtf8 f "hello")
              b (i/basename f)]
          (i/deleteQ f)
          (= n b))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


