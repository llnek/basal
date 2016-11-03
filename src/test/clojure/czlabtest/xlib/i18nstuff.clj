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

(ns czlabtest.xlib.i18nstuff

  (:use [czlab.xlib.resources]
        [czlab.xlib.core]
        [clojure.test])

  (:import [java.util ResourceBundle]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-i18nstuff

  (is (= "hello joe, how is your dawg"
         (-> (loadResource (resUrl "czlab/xlib/Resources_en.properties"))
             (rstr "test"  "joe" "dawg" ))))

  (is (= ["hello joe, how is your dawg" "hello joe, how is your dawg"]
         (-> (loadResource (resUrl "czlab/xlib/Resources_en.properties"))
             (rstr* ["test"  "joe" "dawg"] ["test2"  "joe" "dawg"] ))))

  (is (inst? ResourceBundle
             (getResource "czlab/xlib/Resources")))

  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlabtest.xlib.i18nstuff)

