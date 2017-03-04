;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.ioutils

  (:require [clojure.java.io :as io])

  (:use [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.io]
        [clojure.test])

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
(def ^:private ^File TMP_DIR (io/file (sysTmpDir)))
(def ^:private ^File TMP_FP (io/file TMP_DIR (str (jid<>) ".txt")))
(eval '(do (spit TMP_FP "heeloo" :encoding "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-ioutils

  (is (.exists TMP_DIR))

  (is (= "heeloo"
         (String. (toChars (charsToBytes
                             (.toCharArray "heeloo") "utf-8") "utf-8"))))

  (testing
    "related to: read/write numbers"
    (is (== 4 (alength (writeNumber Integer/MAX_VALUE))))
    (is (== 8 (alength (writeNumber Long/MAX_VALUE))))
    (is (== Long/MAX_VALUE
            (readNumber (writeNumber Long/MAX_VALUE) Long)))
    (is (== Integer/MAX_VALUE
            (readNumber (writeNumber Integer/MAX_VALUE) Integer)))
    (is (== Integer/MAX_VALUE
            (readNumber (writeNumber Integer/MAX_VALUE) Integer)))
    (is (== Long/MAX_VALUE
            (readNumber (writeNumber Long/MAX_VALUE) Long))))

  (testing
    "related to: string<->chars<->bytes"
    (is (= "ab"
           (let [baos (baos<>)
                 inp (ByteArrayInputStream. (bytesify "abcde"))]
             (copyBytes inp baos 2)
             (stringify (.toByteArray baos)))))

    (is (= "abcde"
           (let [baos (baos<> 100)
                 inp (ByteArrayInputStream. (bytesify "abcde"))]
             (copyBytes inp baos 100)
             (stringify (.toByteArray baos)))))

    (is (= "abcde"
           (let [baos (ByteArrayOutputStream.)
                 inp (ByteArrayInputStream. (bytesify "abcde"))]
             (copy inp baos)
             (stringify (.toByteArray baos)))))

    (is (= "abcde"
           (let [inp (ByteArrayInputStream. (bytesify "abcde"))]
             (stringify (toBytes inp)))))

    (is (= "hello"
           (stringify (charsToBytes (.toCharArray "hello")))))

    (is (= "hello"
           (String. (toChars (.getBytes "hello" "utf-8"))))))

  (is (== 5 (readableBytes (streamify (bytesify "hello")))))
  (is (do->true (closeQ (baos<>))))

  (testing
    "related to: hex<->bytes"
    (is (some? (bytesToHex (.getBytes "abc"))))
    (is (= "616263" (hexify (.getBytes "abc")))))

  (testing
    "related to: gzip/gunzip"
    (is (= "hello"
           (stringify (gunzip (gzip (bytesify "hello"))))))

    (is (= "helloworld"
           (stringify
             (fromGZB64 (toGZB64 (bytesify "helloworld")))))))

  (testing
    "related to: temp files"
    (is (let [f (tempFile)
              _ (spit f "a")
              e? (.exists f)] (deleteQ f) e?))

    (is (let [[f os] (openTempFile)
              o? (some? os)] (closeQ os) (deleteQ f) o?))

    (is (= "hello"
           (let [f (copyStream (streamify (bytesify "hello")))
                 s (slurp f)] (deleteQ f) s))))

  (is (ist? XData (xdata<> nil)))
  (is (let [x (fdata<>)
            f? (some? (.fileRef x))] f?))

  (testing
    "related to: stream coersions"
    (is (let [[d i] (inputStream?? 333)]
          (and (false? d)
               (nil? i))))

    (is (let [[c i] (inputStream?? "aaa")]
          (if c (closeQ i))
          (ist? InputStream i))))

  (testing
    "related to: mime properties"
    (is (with-open [inp (resStream "czlab/basal/etc/mime.properties")]
          (binding [*membuf-limit* (* 2 1024)]
            (let [z (.available inp)
                  x (readBytes inp)
                  f (.fileRef x)
                  n (if (some? f)
                      (.length f) 0)]
              (.dispose x)
              (== z n)))))

    (is (with-open [inp (resStream "czlab/basal/etc/mime.properties")
                    rdr (InputStreamReader. inp)]
          (binding [*membuf-limit* (* 2 1024)]
            (let [z (.available inp)
                  x (readChars rdr)
                  f (.fileRef x)
                  n (if (some? f)
                      (.length f) 0)]
              (.dispose x)
              (== z n))))))

  (is (== 5 (alength (convBytes "hello"))))

  (testing
    "related to: file operations"

    (is (= "hello"
           (let [f (tempFile)
                 _ (spitUtf8 f "hello")
                 s (slurpUtf8 f)] (deleteQ f) s)))

    (is (let
          [f (tempFile)
           p (.getCanonicalPath f)
           pp (parentPath p)
           dd (io/file pp)
           d (parentFile f)
           _ (spitUtf8 f "hello")
           d? (dirRead? d)
           dw? (dirReadWrite? d)
           x? (canExec? d)
           ok? (fileOK? f)
           r? (fileRead? f)
           f? (fileReadWrite? f)]
          (deleteQ f)
          (and ;;(= d dd)
               d? dw? x? ok? r? f?)))

    (is (let [f (tempFile)
              _ (spitUtf8 f "hello")
              t (.lastModified f)
              p (.getCanonicalPath f)
              f2 (str p ".t")]
          (pause 1000)
          (touch! f)
          (touch! f2)
          (let [rc (and (> (.lastModified f) t)
                        (fileOK? (io/file f2)))]
            (deleteQ f)
            (deleteQ f2)
            rc)))

    (is (= "helloworld"
           (let [f (tempFile)
                 _ (spitUtf8 f "hello")
                 s (changeContent f #(str % "world"))]
             (deleteQ f)
             s)))

    (is (= "helloworld"
           (let [f (tempFile)
                 _ (spitUtf8 f "hello")
                 _ (replaceFile! f #(str % "world"))
                 s (slurpUtf8 f)]
             (deleteQ f)
             s)))

    (is (= "hello"
           (let [f (tempFile)
                 _ (writeFile f "hello")
                 s (slurpUtf8 f)]
             (deleteQ f)
             s)))

    (is (== 5
           (let [f (tempFile)
                 _ (spitUtf8 f "hello")
                 b (slurpBytes f)]
             (deleteQ f)
             (alength b))))

    (is (= "hello"
           (let [f (tempFile)
                 _ (spitUtf8 f "hello")
                 s (readAsStr f)]
             (deleteQ f)
             s)))

    (is (= "hello"
           (let [n (jid<>)
                 _ (saveFile *tempfile-repo* n (xdata<> "hello") true)
                 x (getFile *tempfile-repo* n)
                 s (readAsStr (.fileRef x))]
             (deleteQ (.fileRef x)) s)))

    (is (let [n (jid<>)
              d (io/file *tempfile-repo* n)
              _ (mkdirs d)
              e? (fileOK? d)]
          (deleteQ d) e?))

    (is (let [n0 (jid<>)
              n1 (jid<>)
              n2 (jid<>)
              f1 (str n1 ".txt")
              f2 (str n2 ".txt")
              root (io/file *tempfile-repo* n0)
              d1 (io/file root n1)
              d2 (io/file root n2)
              _ (mkdirs root)
              _ (mkdirs d1)
              _ (mkdirs d2)
              _ (spitUtf8 (io/file root f1) "a")
              _ (spitUtf8 (io/file root f2) "a")
              _ (spitUtf8 (io/file d1 f1) "a")
              _ (spitUtf8 (io/file d2 f2) "a")
              dz (count (listDirs root))
              fz (count (listFiles root ".txt"))
              tz (count (listAnyFiles root ".txt"))]
          (deleteQ (io/file d1 f1))
          (deleteQ (io/file d2 f2))
          (deleteQ (io/file root f1))
          (deleteQ (io/file root f2))
          (deleteQ (io/file d1))
          (deleteQ (io/file d2))
          (deleteQ root)
          (and (== 2 dz) (== 2 fz) (== 4 tz))))

    (is (let [n0 (jid<>) n1 (jid<>) n2 (jid<>)
              n3 (jid<>) n4 (jid<>) n5 (jid<>)
              root (io/file *tempfile-repo* n0)
              _ (mkdirs root)
              d1 (doto (io/file root n1) (mkdirs))
              d2 (doto (io/file root n2) (mkdirs))
              d3 (doto (io/file root n3) (mkdirs))
              d4 (doto (io/file d2 n4) (mkdirs))
              d5 (doto (io/file d4 n5) (mkdirs))
              _ (spitUtf8 (io/file d1 "d1.txt") "a")
              _ (spitUtf8 (io/file d4 "d4a.txt") "a")
              _ (spitUtf8 (io/file d4 "d4b.txt") "a")
              _ (spitUtf8 (io/file d5 "d5.txt") "a")
              ds (grepFolderPaths root ".txt")
              fs (grepFilePaths root ".txt")]
          (deleteQ (io/file d1 "d1.txt"))
          (deleteQ (io/file d4 "d4a.txt"))
          (deleteQ (io/file d4 "d4b.txt"))
          (deleteQ (io/file d5 "d5.txt"))
          (deleteQ d5)(deleteQ d4)
          (deleteQ d1)(deleteQ d2)(deleteQ d3)
          (deleteQ root)
          (and (== 3 (count ds))
               (== 4 (count fs)))))

    (is (let [n (jid<>)
              f (io/file *tempfile-repo* (str n ".txt"))
              _ (spitUtf8 f "hello")
              b (basename f)]
          (deleteQ f)
          (= n b))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


