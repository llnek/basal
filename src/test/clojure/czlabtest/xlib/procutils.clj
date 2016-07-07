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

(ns czlabtest.xlib.procutils

  (:require
    [czlab.xlib.process :as PU]
    [clojure.java.io :as io]
    [czlab.xlib.core :as CU])

  (:use [clojure.test])
  (:import  [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private CUR_MS (str (System/currentTimeMillis)))
(def ^:private CUR_FP (io/file (System/getProperty "java.io.tmpdir") CUR_MS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-procutils

  (is (true? (do
               (PU/async! #(spit CUR_FP "123" :encoding "utf-8"))
               (PU/safeWait 2500)
               (and (.exists ^File CUR_FP)
                    (>= (.length ^File CUR_FP) 3)))))

  (is (true? (do
               (PU/delayExec #(spit CUR_FP "123456" :encoding "utf-8") 1500)
               (PU/safeWait 2500)
               (and (.exists ^File CUR_FP)
                    (>= (.length ^File CUR_FP) 6)))))

  (is (true? (do
               (PU/syncBlockExec
                 (String. "lock")
                 (fn [a & xs]
                   (spit CUR_FP (apply str a xs) :encoding "utf-8"))
                 "123" "456" "789")
               (and (.exists ^File CUR_FP)
                    (>= (.length ^File CUR_FP) 9)))))

  (is (> (.length (PU/processPid)) 0))

    ;;
)

;;(clojure.test/run-tests 'czlabtest.xlib.procutils)

