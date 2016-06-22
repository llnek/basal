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

(ns czlabtest.xlib.fileutils

  (:require [czlab.xlib.files :as FU]
            [czlab.xlib.core :as CU]
            [clojure.java.io :as io])

  (:use [clojure.test])

  (:import  [czlab.xlib XData]
            [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private TMP_DIR (File. (System/getProperty "java.io.tmpdir")))
(def ^:private TMP_FP (File. ^File TMP_DIR (str (CU/juid) ".txt")))
(eval '(do (spit TMP_FP "heeloo" :encoding "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-fileutils

(is (true? (FU/fileReadWrite? TMP_FP)))
(is (true? (FU/fileRead? TMP_FP)))

(is (true? (FU/dirReadWrite? TMP_DIR)))
(is (true? (FU/dirRead? TMP_DIR)))

(is (false? (FU/canExec? TMP_FP)))
(is (true? (FU/canExec? TMP_DIR)))

(is (= "/tmp/a/b" (FU/parentPath "/tmp/a/b/c")))
(is (nil?  (FU/parentPath nil)))

(is (= "heeloo" (let [ fp (str (CU/juid) ".txt") ]
                    (FU/saveFile ^File TMP_DIR fp (FU/getFile ^File TMP_DIR (.getName ^File TMP_FP)))
                    (slurp (File. ^File TMP_DIR fp) :encoding "utf-8")) ))


)

;;(clojure.test/run-tests 'testcljc.util.fileutils)



