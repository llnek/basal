;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.xlib.procutils

  (:require [clojure.java.io :as io])

  (:use [czlab.xlib.scheduler]
        [czlab.xlib.core]
        [czlab.xlib.process]
        [clojure.test])

  (:import [java.io File]
           [czlab.xlib
            Schedulable
            RunnableWithId]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private CUR_MS (str (now<>)))
(def ^:private
  CUR_FP (io/file (sysTmpDir) CUR_MS))
(def ^:private SCD (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setup []
  (let [s (scheduler<>)]
    (reset! SCD s)
    (.activate s {})))

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
(deftest czlabtestxlib-procutils

  (testing
    "related to: process scheduling"

    (is (do->true (setup)))

    (is (== 1
            (let [x (atom 0)]
              (.run ^Schedulable
                    @SCD
                    (runnable<>
                      #(swap! x inc)))
              (safeWait 500)
              @x)))

    (is (== 1
            (let
              [x (atom 0)]
              (.postpone
                ^Schedulable
                @SCD
                (runnable<>
                      #(swap! x inc))
                500)
              (safeWait 1000)
              @x)))

    (is (== 1
            (let
              [x (atom 0)
               r (runnable<>
                   #(swap! x inc) "117")]
              (.hold ^Schedulable @SCD r)
              (safeWait 500)
              (.wakeup ^Schedulable @SCD r)
              (safeWait 500)
              @x)))

    (is (do
          (async!
            #(spit CUR_FP "123"))
          (safeWait 500)
          (and (.exists ^File CUR_FP)
               (>= (.length ^File CUR_FP) 3))))

    (is (do
          (delayExec
            #(spit CUR_FP "123456") 500)
          (safeWait 1000)
          (and (.exists ^File CUR_FP)
               (>= (.length ^File CUR_FP) 6))))

    (is (do
          (syncBlockExec
            (String. "lock")
            (fn [a & xs]
              (spit CUR_FP
                    (apply str a xs)))
            "123" "456" "789")
          (and (.exists ^File CUR_FP)
               (>= (.length ^File CUR_FP) 9))))

    (is (> (.length (processPid)) 0))

    (is (do->true (tearDown))))

  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlab.test.xlib.procutils)

