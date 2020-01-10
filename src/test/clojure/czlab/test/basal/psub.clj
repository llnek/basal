;; Copyright ©  2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.psub

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.evbus :as e]
            [czlab.basal.util :as u]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- incv
  [v] (if (number? v) (inc v) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sub-func
  [expected topic msg]
  (swap! msg update-in [expected] incv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-psub

  (ensure?? "rv/sub"
            (let [bus (e/event-bus<+>)
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/sub bus "a.**" sub-func)
              (e/sub bus "a.*.c" sub-func)
              (e/pub bus "a.b.c" msg)
              (and (== 1 (get @msg "a.b.c"))
                   (== 1 (get @msg "a.**"))
                   (== 1 (get @msg "a.*.c")))))

  (ensure?? "ev/sub"
            (let [bus (e/event-bus<>)
                  msg (atom {})]
              (e/sub bus 333 sub-func)
              (e/pub bus 333 msg)
              (and (== 1 (get @msg 333)))))

  (ensure?? "rv/sub"
            (let [bus (e/event-bus<+>)
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/sub bus "**" sub-func)
              (e/pub bus "x.b.c" msg)
              (and (== 1 (get @msg "**"))
                   (== 1 (count @msg)))))

  (ensure?? "rv/unsub"
            (let [bus (e/event-bus<+>)
                  msg (atom {})
                  [y _] (e/sub bus "**" sub-func)
                  [x _] (e/sub bus "a.b.c" sub-func)]
              (e/unsub bus y)
              (e/pub bus "x.b.c" msg)
              (zero? (count @msg))))

  (ensure?? "rv/sub"
            (let [bus (e/event-bus<+>)
                  msg (atom {})]
              (e/sub bus "a.*.c" sub-func)
              (e/pub bus "a.b.c" msg)
              (e/pub bus "a.b.c" msg)
              (== 2 (get @msg "a.*.c"))))

  (ensure?? "rv/finz"
            (let [bus (e/event-bus<+>)
                  msg (atom {})
                  _ (e/sub bus "a.b.c" sub-func)]
              (c/finz bus)
              (e/pub bus "a.b.c" msg)
              (zero? (count @msg))))

  (ensure?? "ev/sub"
            (let [bus (e/event-bus<>)
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/sub bus "a" sub-func)
              (e/pub bus "a" msg)
              (and (== 1 (get @msg "a"))
                   (== 1 (count @msg)))))

  (ensure?? "ev/unsub"
            (let [bus (e/event-bus<>)
                  msg (atom {})
                  [y _] (e/sub bus "a" sub-func)
                  x (e/sub bus "a.b.c" sub-func)]
              (e/unsub bus y)
              (e/pub bus "a" msg)
              (zero? (count @msg))))

  (ensure?? "ev/sub"
            (let [bus (e/event-bus<>)
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/pub bus "a.b.c" msg)
              (e/pub bus "a.b.c" msg)
              (== 2 (get @msg "a.b.c"))))

  (ensure?? "ev/finz"
            (let [bus (e/event-bus<>)
                  msg (atom {})
                  x (e/sub bus "a.b.c" sub-func)]
              (c/finz bus)
              (e/pub bus "a.b.c" msg)
              (zero? (count @msg))))

  (ensure?? "go/rv/sub"
            (let [bus (e/event-bus<+> {:async? true})
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/sub bus "a.**" sub-func)
              (e/sub bus "a.*.c" sub-func)
              (e/pub bus "a.b.c" msg)
              (u/pause 500)
              (c/finz bus)
              (and (== 1 (get @msg "a.b.c"))
                   (== 1 (get @msg "a.**"))
                   (== 1 (get @msg "a.*.c")))))

  (ensure?? "go/ev/sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/pub bus "a.b.c" msg)
              (u/pause 500)
              (c/finz bus)
              (== 1 (get @msg "a.b.c"))))

  (ensure?? "go/rv/sub"
            (let [bus (e/event-bus<+> {:async? true})
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/sub bus "**" sub-func)
              (e/pub bus "x.b.c" msg)
              (u/pause 500)
              (c/finz bus)
              (and (== 1 (get @msg "**"))
                   (== 1 (count @msg)))))

  (ensure?? "go/rv/sub"
            (let [bus (e/event-bus<+> {:async? true})
                  msg (atom {})
                  [y _] (e/sub bus "**" sub-func)
                  x (e/sub bus "a.b.c" sub-func)]
              (e/unsub bus y)
              (e/pub bus "x.b.c" msg)
              (u/pause 500)
              (c/finz bus)
              (zero? (count @msg))))

  (ensure?? "go/rv/sub"
            (let [bus (e/event-bus<+> {:async? true})
                  msg (atom {})]
              (e/sub bus "a.*.c" sub-func)
              (e/pub bus "a.b.c" msg)
              (e/pub bus "a.b.c" msg)
              (u/pause 500)
              (c/finz bus)
              (== 2 (get @msg "a.*.c"))))

  (ensure?? "go/rv/sub"
            (let [bus (e/event-bus<+> {:async? true})
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (c/finz bus)
              (e/pub bus "a.b.c" msg)
              (u/pause 500)
              (c/finz bus)
              (zero? (count @msg))))

  (ensure?? "go/ev/sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/sub bus "a.b.c" sub-func)
              (e/sub bus "a" sub-func)
              (e/pub bus "a" msg)
              (u/pause 500)
              (c/finz bus)
              (and (== 1 (get @msg "a"))
                   (== 1 (count @msg)))))

  (ensure?? "go/ev/unsub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})
                  [y _] (e/sub bus "a" sub-func)
                  x (e/sub bus "a.b.c" sub-func)]
              (e/unsub bus y)
              (e/pub bus "a" msg)
              (u/pause 500)
              (c/finz bus)
              (zero? (count @msg))))

  (ensure?? "go/ev/sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/sub bus "abc" sub-func)
              (e/pub bus "abc" msg)
              (e/pub bus "abc" msg)
              (u/pause 500)
              (c/finz bus)
              (== 2 (get @msg "abc"))))

  (ensure?? "go/ev/sub"
            (let [bus (e/event-bus<> {:async? true})
                  msg (atom {})]
              (e/sub bus "abc" sub-func)
              (c/finz bus)
              (e/pub bus "abc" msg)
              (u/pause 500)
              (c/finz bus)
              (zero? (count @msg))))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-psub basal-test-psub
  (ct/is (c/clj-test?? test-psub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


