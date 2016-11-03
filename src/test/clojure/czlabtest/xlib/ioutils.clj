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

(ns czlabtest.xlib.ioutils

  (:require [clojure.java.io :as io])

  (:use [czlab.xlib.core]
        [czlab.xlib.str]
        [czlab.xlib.io]
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
           [czlab.xlib XData XStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^File TMP_DIR (io/file (sysTmpDir)))
(def ^:private ^File TMP_FP (io/file TMP_DIR (str (juid) ".txt")))
(eval '(do (spit TMP_FP "heeloo" :encoding "utf-8")))
;; force to use file
;;(eval '(do (czlab.xlib.IO/setStreamLimit 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest testutil-ioutils

  (is (.exists TMP_DIR))

  (is (= "ab"
         (let [baos (baos<>)
               inp (ByteArrayInputStream. (bytesify "abcde"))]
           (stringify (copyBytes inp baos 2)))))

  (is (= "abcde"
         (let [baos (baos<> 100)
               inp (ByteArrayInputStream. (bytesify "abcde"))]
           (stringify (copyBytes inp baos 100)))))

  (is (= "abcde"
         (let [baos (ByteArrayOutputStream.)
               inp (ByteArrayInputStream. (bytesify "abcde"))]
           (stringify (copy inp baos)))))

  (is (= "abcde"
         (let [inp (ByteArrayInputStream. (bytesify "abcde"))]
           (stringify (toBytes inp)))))

  (is (= "hello"
         (stringify (charsToBytes (.toCharArray "hello")))))

  (is (= "hello"
         (stringify (toChars (.getBytes "hello" "utf-8")))))

  (is (== Long/MAX_VALUE
          (readNumber (writeNumber (Long/MAX_VALUE)) Long)))

  (is (== Integer/MAX_VALUE
          (readNumber (writeNumber (Integer/MAX_VALUE)) Integer)))

  (is (do->true (closeQ (baos<>))))

  (is (some? (bytesToHex (.getBytes "abc"))))
  (is (string? (hexify (bytesToHex (.getBytes "abc")))))

  (is (= "hello"
         (stringify (gunzip (gzip (bytesify "hello"))))))

  (is (= "helloworld" (fromGZB64 (toGZB64 "helloworld"))))

  (is (== 5 (readableBytes (streamify (bytesify "hello")))))

  (is (let [f (tempFile)
            _ (spit f "a")
            e? (.exists f)] (deleteQ f) e?))

  (is (let [[f os] (openTempFile)
            o? (some? os)] (closeQ os) (deleteQ f) o?))

  (is (= "hello"
         (let [f (copyStream (streamify (bytesify "hello")))
               s (slurp f)] (deleteQ f) s)))

  (is (inst? XData (xdata<> nil)))
  (is (let [x (xdata<file>)
            f? (some? (.fileRef x))] f?))

  (is (with-open [inp (resStream "czlab/xlib/mime.properties")]
        (binding [*MEMBUF-LIMIT* (* 2 1024)]
          (let [z (.available inp)
                x (readBytes inp)
                f (.fileRef x)
                n (if (some? f)
                    (.length f) 0)]
            (.dispose x)
            (== z n)))))

  (is (with-open [inp (resStream "czlab/xlib/mime.properties")
                  rdr (InputStreamReader. inp)]
        (binding [*MEMBUF-LIMIT* (* 2 1024)]
          (let [z (.available inp)
                x (readChars rdr)
                f (.fileRef x)
                n (if (some? f)
                    (.length f) 0)]
            (.dispose x)
            (== z n)))))

  (is (== 5 (alength (convBytes "hello"))))

  (is (= "hello"
         (let [f (tempFile)
               _ (spitUtf8 f "hello")
               s (slurpUtf8 f)] (deleteQ f) s)))

  (is (let [f (tempFile)
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
        (and (= d dd)
             d? dw? x? ok? r? f?)))

  (is (let [f (tempFile)
            _ (spitUtf8 f "hello")
            t (.lastModified f)
            p (.getCanonicalPath f)
            f2 (str p ".t")]
        (safeWait 1000)
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
               p (.getCanonicalPath f)
               _ (writeFile p "hello")
               s (slurpUtf8 f)]
           (deleteQ f)
           s)))

  (is (== 5
         (let [f (tempFile)
               _ (spitUtf8 f "hello")
               p (.getCanonicalPath f)
               b (slurpBytes p)]
           (deleteQ f)
           (alength b))))

  (is (= "hello"
         (let [f (tempFile)
               _ (spitUtf8 f "hello")
               s (readAsStr f)]
           (deleteQ f)
           s)))

  (is (= "hello"
         (let [n (juid)
               _ (saveFile *TEMPFILE-REPO* n (xdata<> "hello") true)
               x (getFile *TEMPFILE-REPO* n)
               s (readAsStr (.fileRef x))]
           (deleteQ (.fileRef x)) s)))

  (is (let [n (juid)
            _ (mkdirs *TEMPFILE-REPO* n)
            d (io/file *TEMPFILE-REPO* n)
            e? (fileOK? d)]
        (deleteQ d) e?))

  (is (let [n0 (juid)
            n1 (juid)
            n2 (juid)
            f1 (str n1 ".txt")
            f2 (str n2 ".txt")
            root (io/file *TEMPFILE-REPO* n0)
            d1 (io/file root n1)
            d2 (io/file root n2)
            _ (mkdirs root)
            _ (mkdirs d1)
            _ (mkdirs d2)
            _ (spitUtf8 (io/file root f1) "a")
            _ (spitUtf8 (io/file root f2) "a")
            _ (spitUtf8 (io/file root d1 f1) "a")
            _ (spitUtf8 (io/file root d2 f2) "a")
            dz (count (listDirs root))
            fz (count (listFiles root ".txt"))
            tz (count (listAnyFiles root ".txt"))]
        (deleteQ (io/file root d1 f1))
        (deleteQ (io/file root d2 f2))
        (deleteQ (io/file root f1))
        (deleteQ (io/file root f2))
        (deleteQ (io/file root d1))
        (deleteQ (io/file root d2))
        (deleteQ root)
        (and (== 2 dz) (== 2 fz) (== 4 tz))))

  (is (let [n0 (juid) n1 (juid) n2 (juid)
            n3 (juid) n4 (juid) n5 (juid)
            root (io/file *TEMPFILE-REPO* n0)
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

  (is (let [n (juid)
            f (io/file *TEMPFILE-REPO* (str n ".txt"))
            _ (spitUtf8 f "hello")
            b (basename f)]
        (deleteQ f)
        (= n f)))

  (is (string? "That's all folks!")))

;;(clojure.test/run-tests 'czlabtest.xlib.ioutils)

