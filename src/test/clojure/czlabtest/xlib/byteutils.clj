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

(ns czlabtest.xlib.byteutils

  (:require [czlab.xlib.io :as BU])
  (:use [clojure.test])
  (:import  [java.nio.charset Charset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private CS_UTF8 "utf-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-byteutils

  (is (= "heeloo" (String. ^chars
                           (BU/toChars (BU/charsToBytes (.toCharArray "heeloo") CS_UTF8) CS_UTF8))))

  (is (= 4 (alength ^bytes (BU/writeNumber (Integer/MAX_VALUE)))))
  (is (= 8 (alength ^bytes (BU/writeNumber (Long/MAX_VALUE)))))

  (is (= (Integer/MAX_VALUE) (BU/readNumber (BU/writeNumber (Integer/MAX_VALUE)) Integer)))
  (is (= (Long/MAX_VALUE) (BU/readNumber (BU/writeNumber (Long/MAX_VALUE)) Long)))

)

;;(clojure.test/run-tests 'czlabtest.xlib.byteutils)

