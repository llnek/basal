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

(ns czlabtest.xlib.procutils

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

  (is (do->true (tearDown)))
  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlabtest.xlib.procutils)

