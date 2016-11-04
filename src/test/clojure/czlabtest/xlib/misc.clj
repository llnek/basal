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

(ns czlabtest.xlib.misc

  (:use [czlab.xlib.resources]
        [czlab.xlib.countries]
        [czlab.xlib.format]
        [czlab.xlib.guids]
        [czlab.xlib.core]
        [czlab.xlib.io]
        [clojure.test])

  (:import [java.util ResourceBundle]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-misc

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;country codes

  (is (= (findCountry "AU") (findCountry "au")))
  (is (= "Australia" (findCountry "AU")))
  (is (= "AU" (findCountryCode "Australia")))
  (is (false? (isUSA? "aa")))
  (is (and (isUSA? "US") (= (isUSA? "US") (isUSA? "us"))))
  (is (> (count (listCodes)) 0))

  (is (= (findState "CA") (findState "ca")))
  (is (= "California" (findState "ca")))
  (is (= "CA" (findStateCode "California")))
  (is (> (count (listStates)) 0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;guids

  (is (not= (wwid<>) (wwid<>)))
  (is (not= (uuid<>) (uuid<>)))

  (is (> (.length (wwid<>)) 0))
  (is (> (.length (uuid<>)) 0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;formats

  (is (string? (writeEdnString
                 {:a 1 :b {:c {:e "hello"} :d 4}})))

  (is (let [s (writeEdnString
                {:a 1 :b {:c {:e "hello"} :d 4}})
            t (tempFile)
            _ (spit t s)
            e (readEdn t)]
        (deleteQ t)
        (and (string? s)
             (= "hello" (get-in e [:b :c :e])))))

  (is (string? (writeJsonStr
                 {:a 1 :b {:c {:e "hello"} :d 4}})))

  (is (let [s (writeJsonStr
                {:a 1 :b {:c {:e "hello"} :d 4}})
            t (tempFile)
            _ (spit t s)
            e (readJson t)]
        (deleteQ t)
        (and (string? s)
             (= "hello" (get-in e [:b :c :e])))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;resource bundles

  (is (= "hello joe, how is your dawg"
         (-> (loadResource (resUrl "czlab/xlib/Resources_en.properties"))
             (rstr "test"  "joe" "dawg" ))))

  (is (= ["hello joe, how is your dawg" "hello joe, how is your dawg"]
         (-> (loadResource (resUrl "czlab/xlib/Resources_en.properties"))
             (rstr* ["test"  "joe" "dawg"] ["test2"  "joe" "dawg"] ))))

  (is (inst? ResourceBundle
             (getResource "czlab/xlib/Resources")))



  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlabtest.xlib.misc)

