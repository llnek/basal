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
(def ^:private SBA {:subjectBased? true})
(defn- incv "" [v] (if (number? v) (inc v) 1))
(defn- sub-func "" [subto topic msg]
  ;;msg is an atom
  (swap! msg update-in [subto] incv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-misc

  (testing
    "related to: eventbus"
    (is (let [bus (e/eventBus<> SBA)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c /a/** /a/*/c" sub-func)
          (e/ev-pub bus "/a/b/c" msg)
          (and (= 1 (get @msg "/a/b/c"))
               (= 1 (get @msg "/a/**"))
               (= 1 (get @msg "/a/*/c")))))
    (is (let [bus (e/eventBus<>)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c" sub-func)
          (e/ev-pub bus "/a/b/c" msg)
          (and (= 1 (get @msg "/a/b/c")))))
    (is (let [bus (e/eventBus<> SBA)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c /**" sub-func)
          (e/ev-pub bus "/x/b/c" msg)
          (and (= 1 (get @msg "/**"))
               (= 1 (count @msg)))))
    (is (let [bus (e/eventBus<> SBA)
              msg (atom {})
              [x y] (e/ev-sub+ bus "/a/b/c /**" sub-func)]
          (e/ev-unsub bus y)
          (e/ev-pub bus "/x/b/c" msg)
          (= 0 (count @msg))))
    (is (let [bus (e/eventBus<> SBA)
              msg (atom {})
              _ (e/ev-sub+ bus "/a/b/c" sub-func)
              _ (e/ev-sub* bus "/a/*/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-pub bus "/a/b/c" msg)
          (and (= 2 (get @msg "/a/b/c"))
               (= 1 (get @msg "/a/*/c")))))
    (is (let [bus (e/eventBus<> SBA)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-pause bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-resume bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (= 2 (get @msg "/a/b/c"))))
    (is (let [bus (e/eventBus<> SBA)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-finz bus)
          (e/ev-pub bus "/a/b/c" msg)
          (= 0 (count @msg))))

    (is (let [bus (e/eventBus<>)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c /a" sub-func)
          (e/ev-pub bus "/a" msg)
          (and (= 1 (get @msg "/a"))
               (= 1 (count @msg)))))
    (is (let [bus (e/eventBus<>)
              msg (atom {})
              [x y] (e/ev-sub+ bus "/a/b/c /a" sub-func)]
          (e/ev-unsub bus y)
          (e/ev-pub bus "/a" msg)
          (= 0 (count @msg))))
    (is (let [bus (e/eventBus<>)
              msg (atom {})
              _ (e/ev-sub* bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-pub bus "/a/b/c" msg)
          (= 1 (get @msg "/a/b/c"))))
    (is (let [bus (e/eventBus<>)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-pause bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-resume bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (= 2 (get @msg "/a/b/c"))))
    (is (let [bus (e/eventBus<>)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-finz bus)
          (e/ev-pub bus "/a/b/c" msg)
          (= 0 (count @msg)))))

  (testing
    "related to: gobus"

    (is (let [bus (e/goBus<> SBA)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c /a/** /a/*/c" sub-func)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (and (= 1 (get @msg "/a/b/c"))
               (= 1 (get @msg "/a/**"))
               (= 1 (get @msg "/a/*/c")))))
    (is (let [bus (e/goBus<>)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c" sub-func)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (and (= 1 (get @msg "/a/b/c")))))
    (is (let [bus (e/goBus<> SBA)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c /**" sub-func)
          (e/ev-pub bus "/x/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (and (= 1 (get @msg "/**"))
               (= 1 (count @msg)))))
    (is (let [bus (e/goBus<> SBA)
              msg (atom {})
              [x y] (e/ev-sub+ bus "/a/b/c /**" sub-func)]
          (e/ev-unsub bus y)
          (e/ev-pub bus "/x/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (= 0 (count @msg))))
    (is (let [bus (e/goBus<> SBA)
              msg (atom {})
              _ (e/ev-sub+ bus "/a/b/c" sub-func)
              _ (e/ev-sub* bus "/a/*/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 3500)
          (e/ev-finz bus)
          (and (= 2 (get @msg "/a/b/c"))
               (= 1 (get @msg "/a/*/c")))))
    (is (let [bus (e/goBus<> SBA)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-pause bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-resume bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (= 2 (get @msg "/a/b/c"))))
    (is (let [bus (e/goBus<> SBA)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-finz bus)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (= 0 (count @msg))))
    (is (let [bus (e/goBus<>)
              msg (atom {})]
          (e/ev-sub+ bus "/a/b/c /a" sub-func)
          (e/ev-pub bus "/a" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (and (= 1 (get @msg "/a"))
               (= 1 (count @msg)))))
    (is (let [bus (e/goBus<>)
              msg (atom {})
              [x y] (e/ev-sub+ bus "/a/b/c /a" sub-func)]
          (e/ev-unsub bus y)
          (e/ev-pub bus "/a" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (= 0 (count @msg))))
    (is (let [bus (e/goBus<>)
              msg (atom {})
              _ (e/ev-sub* bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 3500)
          (e/ev-finz bus)
          (= 1 (get @msg "/a/b/c"))))
    (is (let [bus (e/goBus<>)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-pause bus x)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-resume bus x)
          (c/pause 1000)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (= 2 (get @msg "/a/b/c"))))
    (is (let [bus (e/goBus<>)
              msg (atom {})
              x (e/ev-sub+ bus "/a/b/c" sub-func)]
          (e/ev-finz bus)
          (e/ev-pub bus "/a/b/c" msg)
          (c/pause 1000)
          (e/ev-finz bus)
          (= 0 (count @msg)))))

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


