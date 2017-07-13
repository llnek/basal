;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.misc

  (:require [czlab.basal.resources :as r]
            [czlab.basal.countries :as u]
            [czlab.basal.format :as f]
            [czlab.basal.guids :as g]
            [czlab.basal.ebus :as e]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i])

  (:use [clojure.test])

  (:import [java.util ResourceBundle]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private EBUS (e/eventBus<> true))
(defn- sub "" [subto topic msg]
  (println (str "!!!!!!!!!!!!!!subto: " subto ", topic: " topic )))

(let [[x y z] (e/ev-sub+ EBUS "/a/b/c /a/** /a/*/c" sub)]
  (e/ev-pub EBUS "/a/b/c" {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-misc

  (testing
    "related to: country codes"
    (is (= (u/findCountry "AU") (u/findCountry "au")))
    (is (= "Australia" (u/findCountry "AU")))
    (is (= "AU" (u/findCountryCode "Australia")))
    (is (false? (u/isUSA? "aa")))
    (is (and (u/isUSA? "US") (= (u/isUSA? "US") (u/isUSA? "us"))))
    (is (> (count (u/listCodes)) 0))

    (is (= (u/findState "CA") (u/findState "ca")))
    (is (= "California" (u/findState "ca")))
    (is (= "CA" (u/findStateCode "California")))
    (is (> (count (u/listStates)) 0)))

  (testing
    "related to: guids"
    (is (not= (g/wwid<>) (g/wwid<>)))
    (is (not= (c/uuid<>) (c/uuid<>)))

    (is (> (.length (g/wwid<>)) 0))
    (is (> (.length (c/uuid<>)) 0)))

  (testing
    "related to: formats"
    (is (string? (f/writeEdnStr
                   {:a 1 :b {:c {:e "hello"} :d 4}})))

    (is (let [s (f/writeEdnStr
                  {:a 1 :b {:c {:e "hello"} :d 4}})
              t (i/tempFile)
              _ (spit t s)
              e (f/readEdn t)]
          (i/deleteQ t)
          (and (string? s)
               (= "hello" (get-in e [:b :c :e])))))

    (is (string? (f/writeJsonStr
                   {:a 1 :b {:c {:e "hello"} :d 4}})))

    (is (let [s (f/writeJsonStr
                  {:a 1 :b {:c {:e "hello"} :d 4}})
              t (i/tempFile)
              _ (spit t s)
              e (f/readJson t)]
          (i/deleteQ t)
          (and (string? s)
               (= "hello" (get-in e [:b :c :e]))))))

  (testing
    "related to: resource bundles"
    (is (= "hello joe, how is your dawg"
           (-> (r/loadResource (c/resUrl "czlab/basal/etc/Resources_en.properties"))
               (r/rstr "test"  "joe" "dawg" ))))

    (is (= ["hello joe, how is your dawg" "hello joe, how is your dawg"]
           (-> (r/loadResource (c/resUrl "czlab/basal/etc/Resources_en.properties"))
               (r/rstr* ["test"  "joe" "dawg"] ["test2"  "joe" "dawg"] ))))

    (is (c/ist? ResourceBundle
               (r/getResource "czlab/basal/etc/Resources"))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


