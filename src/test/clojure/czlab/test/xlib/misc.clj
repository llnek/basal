;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.xlib.misc

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

  (testing
    "related to: country codes"
    (is (= (findCountry "AU") (findCountry "au")))
    (is (= "Australia" (findCountry "AU")))
    (is (= "AU" (findCountryCode "Australia")))
    (is (false? (isUSA? "aa")))
    (is (and (isUSA? "US") (= (isUSA? "US") (isUSA? "us"))))
    (is (> (count (listCodes)) 0))

    (is (= (findState "CA") (findState "ca")))
    (is (= "California" (findState "ca")))
    (is (= "CA" (findStateCode "California")))
    (is (> (count (listStates)) 0)))

  (testing
    "related to: guids"
    (is (not= (wwid<>) (wwid<>)))
    (is (not= (uuid<>) (uuid<>)))

    (is (> (.length (wwid<>)) 0))
    (is (> (.length (uuid<>)) 0)))

  (testing
    "related to: formats"
    (is (string? (writeEdnStr
                   {:a 1 :b {:c {:e "hello"} :d 4}})))

    (is (let [s (writeEdnStr
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
               (= "hello" (get-in e [:b :c :e]))))))

  (testing
    "related to: resource bundles"
    (is (= "hello joe, how is your dawg"
           (-> (loadResource (resUrl "czlab/xlib/etc/Resources_en.properties"))
               (rstr "test"  "joe" "dawg" ))))

    (is (= ["hello joe, how is your dawg" "hello joe, how is your dawg"]
           (-> (loadResource (resUrl "czlab/xlib/etc/Resources_en.properties"))
               (rstr* ["test"  "joe" "dawg"] ["test2"  "joe" "dawg"] ))))

    (is (inst? ResourceBundle
               (getResource "czlab/xlib/etc/Resources"))))


  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlab.test.xlib.misc)

