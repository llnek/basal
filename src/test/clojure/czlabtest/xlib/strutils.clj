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

(ns czlabtest.xlib.strutils

  (:require [czlab.xlib.str :as SU])
  (:use [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-strutils

  (is (false? (SU/has? "hallowed are the ori" \z)))
  (is (true? (SU/has? "hallowed are the ori" \w)))

  (is (= "heeloo" (SU/nsb "heeloo")))
  (is (= "" (SU/nsb nil)))

  (is (= "heeloo" (SU/nsn "heeloo")))
  (is (= "(null)" (SU/nsn nil)))

  (is (false? (SU/same? "aaa" "axa")))
  (is (true? (SU/same? "aaa" "aaa")))

  (is (true? (SU/hgl? "haha")))
  (is (false? (SU/hgl? "")))

  (is (= "haha" (SU/strim "            haha                          ")))
  (is (= "" (SU/strim nil)))

  (is (= "joe;blogg" (let [ x (StringBuilder.) ]
                  (SU/addDelim! x ";" "joe")
                  (SU/addDelim! x ";" "blogg")
                  (.toString x))))

  (is (= 4 (count (SU/splunk "hello, how are you" 5))))

  (is (true? (SU/hasicAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))
  (is (false? (SU/hasAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))

  (is (true? (SU/swicAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Hall" ])))
  (is (true? (SU/swAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ha" ])))
  (is (false? (SU/swAny? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

  (is (true? (SU/eqicAny? "heeloo" [ "sdfsdg" "jffflf" "HeeLoo" ])))
  (is (true? (SU/eqAny? "heeloo" [ "sdfsdg" "jffflf" "heeloo" ])))
  (is (false? (SU/eqAny? "heeloo" [ "sdfsdg" "jffflf" ])))

  (is (= 10 (.length (SU/makeString \x 10))))
  (is (= "ori" (SU/rights "Hallowed are the ori" 3)))
  (is (= "Hal" (SU/lefts "Hallowed are the ori" 3)))


)


;;(clojure.test/run-tests 'czlabtest.xlib.strutils)

