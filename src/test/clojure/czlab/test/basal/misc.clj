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
(def ^:private BC (atom {}))
(defn- incv "" [v] (if (number? v) (inc v) 1))
(defn- sub-func "" [subto topic msg]
  (swap! BC update-in [subto] incv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-misc

  (testing
    "related to: eventbus"
    (is (let [bus (e/eventBus<> true)]
          (reset! BC {})
          (e/ev-sub+ bus "/a/b/c /a/** /a/*/c" sub-func)
          (e/ev-pub bus "/a/b/c" {})
          (and (= 1 (get @BC "/a/b/c"))
               (= 1 (get @BC "/a/**"))
               (= 1 (get @BC "/a/*/c")))))
    (is (let [bus (e/eventBus<>)]
          (reset! BC {})
          (e/ev-sub+ bus "/a/b/c" sub-func)
          (e/ev-pub bus "/a/b/c" {})
          (and (= 1 (get @BC "/a/b/c")))))
    (is (let [bus (e/eventBus<> true)]
          (reset! BC {})
          (e/ev-sub+ bus "/a/b/c /**" sub-func)
          (e/ev-pub bus "/x/b/c" {})
          (and (= 1 (get @BC "/**"))
               (= 1 (count @BC)))))
    (is (let [bus (e/eventBus<> true)
              _ (reset! BC {})
              [x y] (e/ev-sub+ bus "/a/b/c /**" sub-func)]
          (e/ev-unsub bus y)
          (e/ev-pub bus "/x/b/c" {})
          (= 0 (count @BC))))
    (is (let [bus (e/eventBus<> true)
              _ (reset! BC {})
              _ (e/ev-sub+ bus "/a/b/c" sub-func)
              _ (e/ev-sub* bus "/a/*/c" sub-func)]
          (e/ev-pub bus "/a/b/c" {})
          (e/ev-pub bus "/a/b/c" {})
          (and (= 2 (get @BC "/a/b/c"))
               (= 1 (get @BC "/a/*/c")))))
    (is (let [bus (e/eventBus<> true)
              _ (reset! BC {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" {})
          (e/ev-pause bus x)
          (e/ev-pub bus "/a/b/c" {})
          (e/ev-resume bus x)
          (e/ev-pub bus "/a/b/c" {})
          (= 2 (get @BC "/a/b/c"))))
    (is (let [bus (e/eventBus<> true)
              _ (reset! BC {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-removeAll bus)
          (e/ev-pub bus "/a/b/c" {})
          (= 0 (count @BC))))

    (is (let [bus (e/eventBus<>)]
          (reset! BC {})
          (e/ev-sub+ bus "/a/b/c /a" sub-func)
          (e/ev-pub bus "/a" {})
          (and (= 1 (get @BC "/a"))
               (= 1 (count @BC)))))
    (is (let [bus (e/eventBus<>)
              _ (reset! BC {})
              [x y] (e/ev-sub+ bus "/a/b/c /a" sub-func)]
          (e/ev-unsub bus y)
          (e/ev-pub bus "/a" {})
          (= 0 (count @BC))))
    (is (let [bus (e/eventBus<>)
              _ (reset! BC {})
              _ (e/ev-sub* bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" {})
          (e/ev-pub bus "/a/b/c" {})
          (= 1 (get @BC "/a/b/c"))))
    (is (let [bus (e/eventBus<>)
              _ (reset! BC {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" {})
          (e/ev-pause bus x)
          (e/ev-pub bus "/a/b/c" {})
          (e/ev-resume bus x)
          (e/ev-pub bus "/a/b/c" {})
          (= 2 (get @BC "/a/b/c"))))
    (is (let [bus (e/eventBus<>)
              _ (reset! BC {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-removeAll bus)
          (e/ev-pub bus "/a/b/c" {})
          (= 0 (count @BC)))))

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


