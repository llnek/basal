;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.xlib.win32ini

  (:use [czlab.xlib.core]
        [czlab.xlib.ini]
        [clojure.test])

  (:import [czlab.xlib Win32Conf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  ^Win32Conf INIFILE (w32ini<> (resUrl "czlab/xlib/sample.ini")))

;;(println "->>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
;;(.dbgShow INIFILE)
;;(println "-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-wi32ini

  (testing
    "related to: win32 ini file"

    (is (= (count (.headings INIFILE)) 2))

    (is (map? (.heading INIFILE "operating systems")))
    (is (map? (.heading INIFILE "boot loader")))

    (is (.endsWith
          (.strValue INIFILE
                     "boot loader" "default") "WINDOWS"))

    (is (= (.longValue INIFILE
                       "boot loader" "timeout") 30)))

  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlab.test.xlib.win32ini)

