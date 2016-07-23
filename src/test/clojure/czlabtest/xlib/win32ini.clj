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

(ns czlabtest.xlib.win32ini

  (:require
    [czlab.xlib.core :as CU]
    [czlab.xlib.ini :as WI])
  (:use [clojure.test])
  (:import
    [czlab.xlib Win32Conf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^Win32Conf ^:private INIFILE (WI/w32ini<> (CU/resUrl "czlab/xlib/sample.ini")))

;;(println "->>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
;;(.dbgShow INIFILE)
;;(println "-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-wi32ini

  (is (= (count (.headings INIFILE)) 2))

  (is (map? (.heading INIFILE "operating systems")))
  (is (map? (.heading INIFILE "boot loader")))

  (is (true? (.endsWith (.strValue INIFILE "boot loader" "default") "WINDOWS")))

  (is (true? (= (.longValue INIFILE "boot loader" "timeout") 30)))


)


;;(clojure.test/run-tests 'czlabtest.xlib.win32ini)

