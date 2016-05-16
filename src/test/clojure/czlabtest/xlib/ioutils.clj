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

  (:require [czlab.xlib.core :as CU]
            [czlab.xlib.io :as IO])

  (:use [clojure.test])

  (:import  [org.apache.commons.io FileUtils]
            [java.io FileReader
             File InputStream
             OutputStream FileOutputStream]
            [czlab.xlib XData XStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private TMP_DIR (File. (System/getProperty "java.io.tmpdir")))
(def ^:private TMP_FP (File. ^File TMP_DIR (str (CU/juid) ".txt")))
(eval '(do (FileUtils/writeStringToFile ^File TMP_FP "heeloo" "utf-8")))
;; force to use file
;;(eval '(do (czlab.xlib.IO/setStreamLimit 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest testutil-ioutils

  (is (true? (.exists (IO/tempFile))))

  (is (true? (let [ v (IO/openTempFile)
                    rc (and (.exists ^File (first v))
                            (instance? OutputStream (nth v 1))) ]
               (when rc (.close ^OutputStream (nth v 1)))
               rc)))

  (is (instance? InputStream (IO/streamify (byte-array 10))))

  (is (instance? OutputStream (IO/byteOS)))

  (is (= "616263" (IO/HexifyString (CU/bytesify "abc"))))

  (is (= "heeloo world!" (CU/stringify (IO/gunzip (IO/gzip (CU/bytesify "heeloo world!"))))))

  (is (true? (do (IO/resetStream! (IO/streamify (CU/bytesify "hello"))) true)))

  (is (true? (let [ xs (IO/openFile (.getCanonicalPath ^File TMP_FP))
                    rc (instance? XStream xs) ]
               (.close ^XStream xs) rc)))

  (is (true? (let [ xs (IO/openFile ^File TMP_FP)
                   rc (instance? XStream xs)]
               (.close ^XStream xs) rc)))

  (is (= "heeloo world" (CU/stringify (IO/fromGZB64 (IO/toGZB64 (CU/bytesify "heeloo world"))))))

  (is (>= (with-open [ ^InputStream inp (IO/openFile TMP_FP) ] (IO/available inp)) 6))

  (is (true? (let [ ^File fp (with-open [ ^InputStream inp (IO/openFile TMP_FP) ]
                       (IO/copyStream inp))]
             (.exists fp))))

  (is (true? (let [ v (IO/tempFile)]
                  (with-open [^InputStream inp (IO/openFile TMP_FP) ]
                    (with-open [ os (FileOutputStream. ^File v)]
                      (IO/copyBytes inp os 4)))
                  (>= (.length ^File v) 4))))

  (is (true? (.isDiskFile (IO/newXData true))))
  (is (false? (.isDiskFile (IO/newXData))))

  (is (true? (let [ x (with-open [ ^InputStream inp (IO/openFile TMP_FP)] (IO/readBytes inp true))]
                (and (instance? XData x)
                     (.isDiskFile ^XData x)
                     (> (.size ^XData x) 0))) ))

  (is (true? (let [ x (with-open [ ^InputStream inp (IO/openFile TMP_FP)] (IO/readBytes inp))]
                (and (instance? XData x)
                     (not (.isDiskFile ^XData x))
                     (> (.size ^XData x) 0))) ))

  (is (true? (let [ x (with-open [ rdr (FileReader. ^File TMP_FP)] (IO/readChars rdr true))]
                (and (instance? XData x)
                     (.isDiskFile ^XData x)
                     (> (.size ^XData x) 0))) ))

  (is (true? (let [ x (with-open [ rdr (FileReader. ^File TMP_FP)] (IO/readChars rdr))]
                (and (instance? XData x)
                     (not (.isDiskFile ^XData x))
                     (> (.size ^XData x) 0))) ))

  (is (= "heeloo" (String. (IO/morphChars (CU/bytesify "heeloo")))))


)

;;(clojure.test/run-tests 'czlabtest.xlib.ioutils)

