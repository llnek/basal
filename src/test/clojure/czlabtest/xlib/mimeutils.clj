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

(ns czlabtest.xlib.mimeutils

  (:require [czlab.xlib.core :as CU]
            [czlab.xlib.io :as IO]
            [czlab.xlib.mime :as MU])
  (:use [clojure.test])
  (:import  [java.io File InputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(eval '(MU/setupCache (CU/resUrl "czlab/xlib/mime.properties")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-mimeutils

  (is (= "utf-16" (MU/getCharset "text/plain; charset=utf-16")))

  (is (true? (MU/isSigned? "saljas application/x-pkcs7-mime laslasdf lksalfkla multipart/signed signed-data ")))
  (is (true? (MU/isEncrypted? "saljas laslasdf lksalfkla application/x-pkcs7-mime  enveloped-data ")))
  (is (true? (MU/isCompressed? "saljas laslasdf lksalfkla application/pkcs7-mime compressed-data")))
  (is (true? (MU/isMDN? "saljas laslasdf lksalfkla multipart/report   disposition-notification    ")))

  (is (instance? InputStream (MU/maybeStream (IO/streamify (CU/bytesify "hello")))))
  (is (instance? InputStream (MU/maybeStream (CU/bytesify "hello"))))
  (is (instance? InputStream (MU/maybeStream "hello")))
  (is (not (instance? InputStream (MU/maybeStream 3))))

  (is (>= (.indexOf (MU/guessMimeType (File. "/tmp/abc.jpeg")) "image/") 0))
  (is (> (.indexOf (MU/guessContentType (File. "/tmp/abc.pdf")) "/pdf") 0))


)

;;(clojure.test/run-tests 'czlabtest.xlib.mimeutils)

