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

(ns czlabtest.xlib.codes

  (:require [czlab.xlib.countries :as CC])
  (:use [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-codes

  (is (= (CC/findCountry "AU") (CC/findCountry "au")))
  (is (= "Australia" (CC/findCountry "AU")))
  (is (= "AU" (CC/findCountryCode "Australia")))
  (is (false? (CC/isUSA? "aa")))
  (is (and (CC/isUSA? "US") (= (CC/isUSA? "US") (CC/isUSA? "us"))))
  (is (> (count (CC/listCodes)) 0))

  (is (= (CC/findState "CA") (CC/findState "ca")))
  (is (= "California" (CC/findState "ca")))
  (is (= "CA" (CC/findStateCode "California")))
  (is (> (count (CC/listStates)) 0))

)

;;(clojure.test/run-tests 'czlabtest.xlib.codes)

