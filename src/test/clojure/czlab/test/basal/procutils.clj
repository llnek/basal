;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.procutils

  (:require [czlab.basal.scheduler :as s]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [czlab.basal.process :as p])

  (:use [clojure.test])

  (:import [java.io File]
           [czlab.jasal
            Schedulable
            RunnableWithId]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^{:private true :tag File} CUR_FP (io/file (c/sysTmpDir) (str (c/now<>))))
(def ^:private SCD (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setup []
  (let [s (s/scheduler<>)]
    (reset! SCD s)
    (.activate s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tearDown []
  (.deactivate ^Schedulable @SCD)
  (reset! SCD nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(use-fixtures :once setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-procutils

  (testing
    "related to: process scheduling"

    (is (c/do->true (setup)))

    (is (== 1
            (let [x (atom 0)]
              (.run ^Schedulable
                    @SCD
                    (c/run-able<> (swap! x inc)))
              (c/pause 500)
              @x)))

    (is (== 1
            (let
              [x (atom 0)]
              (.postpone ^Schedulable
                 @SCD
                 (c/run-able<> (swap! x inc)) 500)
              (c/pause 1000)
              @x)))

    (is (== 1
            (let
              [^Schedulable s @SCD
               x (atom 0)
               r (c/run-able+id<>
                   "117" (swap! x inc))]
              (.hold s r)
              (c/pause 500)
              (.wakeup s r)
              (c/pause 500)
              @x)))

    (is (do
          (p/async!
            #(spit CUR_FP "123"))
          (c/pause 500)
          (and (.exists CUR_FP)
               (>= (.length CUR_FP) 3))))

    (is (do
          (p/delayExec
            #(spit CUR_FP "123456") 500)
          (c/pause 1000)
          (and (.exists CUR_FP)
               (>= (.length CUR_FP) 6))))

    (is (do
          (p/syncBlockExec
            (String. "lock")
            (fn [a & xs]
              (spit CUR_FP
                    (apply str a xs)))
            "123" "456" "789")
          (and (.exists CUR_FP)
               (>= (.length CUR_FP) 9))))

    (is (> (.length (p/processPid)) 0))

    (is (c/do->true (tearDown))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


