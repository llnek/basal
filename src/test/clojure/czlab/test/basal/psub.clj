;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns
  ^{:doc ""
    :author "Kenneth Leung"}

  czlab.test.basal.psub

  (:require [clojure
             [test :as ct]
             [string :as cs]]
            [czlab.basal
             [evbus :as e]
             [rvbus :as r]
             [util :as u]
             [xpis :as po]
             [core
              :refer [ensure?? ensure-thrown??] :as c]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- incv "" [v] (if (number? v) (inc v) 1))
(defn- sub-func "" [expected topic msg]
  ;;msg is an atom
  (swap! msg update-in [expected] incv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-psub

  (ensure?? "rv/ev-sub"
            (let [bus (r/event-bus<>)
                  msg (atom {})]
              (r/ev-sub bus "a.b.c" sub-func)
              (r/ev-sub bus "a.**" sub-func)
              (r/ev-sub bus "a.*.c" sub-func)
              (r/ev-pub bus "a.b.c" msg)
              (and (= 1 (get @msg "a.b.c"))
                   (= 1 (get @msg "a.**"))
                   (= 1 (get @msg "a.*.c")))))

  (ensure?? "ev/ev-sub"
            (let [bus (e/event-bus<>)
                  msg (atom {})]
              (e/ev-sub bus 333 sub-func)
              (e/ev-pub bus 333 msg)
              (and (= 1 (get @msg 333)))))

  (ensure?? "rv/rv-sub"
            (let [bus (r/event-bus<>)
                  msg (atom {})]
              (r/ev-sub bus "a.b.c" sub-func)
              (r/ev-sub bus "**" sub-func)
              (r/ev-pub bus "x.b.c" msg)
              (and (= 1 (get @msg "**"))
                   (= 1 (count @msg)))))

  (ensure?? "rv/ev-unsub"
            (let [bus (r/event-bus<>)
                  msg (atom {})
                  y (r/ev-sub bus "**" sub-func)
                  x (r/ev-sub bus "a.b.c" sub-func)]
              (r/ev-unsub bus y)
              (r/ev-pub bus "x.b.c" msg)
              (= 0 (count @msg))))

  (ensure?? "rv/ev-sub"
            (let [bus (r/event-bus<>)
                  msg (atom {})]
              (r/ev-sub bus "a.*.c" sub-func)
              (r/ev-pub bus "a.b.c" msg)
              (r/ev-pub bus "a.b.c" msg)
              (= 2 (get @msg "a.*.c"))))

  (ensure?? "r/ev-finz"
            (let [bus (r/event-bus<>)
                  msg (atom {})
                  x (r/ev-sub bus "a.b.c" sub-func)]
              (po/finz bus)
              (r/ev-pub bus "a.b.c" msg)
              (= 0 (count @msg))))

  (ensure?? "ev/ev-sub"
            (let [bus (e/event-bus<>)
                  msg (atom {})]
              (e/ev-sub bus "a.b.c" sub-func)
              (e/ev-sub bus "a" sub-func)
              (e/ev-pub bus "a" msg)
              (and (= 1 (get @msg "a"))
                   (= 1 (count @msg)))))

  (ensure?? "ev/ev-unsub"
            (let [bus (e/event-bus<>)
                  msg (atom {})
                  y (e/ev-sub bus "a" sub-func)
                  x (e/ev-sub bus "a.b.c" sub-func)]
              (e/ev-unsub bus y)
              (e/ev-pub bus "a" msg)
              (= 0 (count @msg))))

  (ensure?? "ev/ev-sub"
            (let [bus (e/event-bus<>)
                  msg (atom {})]
              (e/ev-sub bus "a.b.c" sub-func)
              (e/ev-pub bus "a.b.c" msg)
              (e/ev-pub bus "a.b.c" msg)
              (= 2 (get @msg "a.b.c"))))

  (ensure?? "ev/ev-finz"
            (let [bus (e/event-bus<>)
                  msg (atom {})
                  x (e/ev-sub bus "a.b.c" sub-func)]
              (po/finz bus)
              (e/ev-pub bus "a.b.c" msg)
              (= 0 (count @msg))))

  (ensure?? "go/rv/ev-sub"
            (let [bus (r/event-bus<> {:async? true})
                  msg (atom {})]
              (r/ev-sub bus "a.b.c" sub-func)
              (r/ev-sub bus "a.**" sub-func)
              (r/ev-sub bus "a.*.c" sub-func)
              (r/ev-pub bus "a.b.c" msg)
              (u/pause 500)
              (po/finz bus)
              (and (= 1 (get @msg "a.b.c"))
                   (= 1 (get @msg "a.**"))
                   (= 1 (get @msg "a.*.c")))))

  (ensure?? "go/ev/ev-sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/ev-sub bus "a.b.c" sub-func)
              (e/ev-pub bus "a.b.c" msg)
              (u/pause 500)
              (po/finz bus)
              (= 1 (get @msg "a.b.c"))))

  (ensure?? "go/rv/ev-sub"
            (let [bus (r/event-bus<> {:async? true})
                  msg (atom {})]
              (r/ev-sub bus "a.b.c" sub-func)
              (r/ev-sub bus "**" sub-func)
              (r/ev-pub bus "x.b.c" msg)
              (u/pause 500)
              (po/finz bus)
              (and (= 1 (get @msg "**"))
                   (= 1 (count @msg)))))

  (ensure?? "go/rv/ev-sub"
            (let [bus (r/event-bus<> {:async? true})
                  msg (atom {})
                  y (r/ev-sub bus "**" sub-func)
                  x (r/ev-sub bus "a.b.c" sub-func)]
              (r/ev-unsub bus y)
              (r/ev-pub bus "x.b.c" msg)
              (u/pause 500)
              (po/finz bus)
              (= 0 (count @msg))))

  (ensure?? "go/rv/ev-sub"
            (let [bus (r/event-bus<> {:async? true})
                  msg (atom {})]
              (r/ev-sub bus "a.*.c" sub-func)
              (r/ev-pub bus "a.b.c" msg)
              (r/ev-pub bus "a.b.c" msg)
              (u/pause 500)
              (po/finz bus)
              (= 2 (get @msg "a.*.c"))))

  (ensure?? "go/rv/ev-sub"
            (let [bus (r/event-bus<> {:async? true})
                  msg (atom {})]
              (r/ev-sub bus "a.b.c" sub-func)
              (po/finz bus)
              (r/ev-pub bus "a.b.c" msg)
              (u/pause 500)
              (po/finz bus)
              (= 0 (count @msg))))

  (ensure?? "go/ev/ev-sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/ev-sub bus "a.b.c" sub-func)
              (e/ev-sub bus "a" sub-func)
              (e/ev-pub bus "a" msg)
              (u/pause 500)
              (po/finz bus)
              (and (= 1 (get @msg "a"))
                   (= 1 (count @msg)))))

  (ensure?? "go/ev/ev-unsub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})
                  y (e/ev-sub bus "a" sub-func)
                  x (e/ev-sub bus "a.b.c" sub-func)]
              (e/ev-unsub bus y)
              (e/ev-pub bus "a" msg)
              (u/pause 500)
              (po/finz bus)
              (= 0 (count @msg))))

  (ensure?? "go/ev/ev-sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/ev-sub bus "abc" sub-func)
              (e/ev-pub bus "abc" msg)
              (e/ev-pub bus "abc" msg)
              (u/pause 500)
              (po/finz bus)
              (= 2 (get @msg "abc"))))

  (ensure?? "go/ev/ev-sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/ev-sub bus "abc" sub-func)
              (po/finz bus)
              (e/ev-pub bus "abc" msg)
              (u/pause 500)
              (po/finz bus)
              (= 0 (count @msg))))

  (ensure?? "test-end" (= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-psub basal-test-psub
  (ct/is (c/clj-test?? test-psub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


