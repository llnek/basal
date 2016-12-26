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

(ns ^{:doc "A scheduler with pooled threads."
      :author "Kenneth Leung" }

  czlab.xlib.scheduler

  (:require [czlab.xlib.logging :as log])

  (:use [czlab.xlib.core]
        [czlab.xlib.str])

  (:import [java.util.concurrent ConcurrentHashMap]
           [czlab.xlib
            Schedulable
            TCore
            Disposable
            Activable
            Identifiable
            RunnableWithId]
          [java.util Map Properties Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xrefPID
  ""
  [r]
  (if
    (instance? Identifiable r)
    (.id ^Identifiable r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- preRun
  ""
  [^Map hQ w]
  (if-some [pid (xrefPID w)] (.remove hQ pid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addTimer "" [^Timer timer
                    ^TimerTask task delayMillis]
  (.schedule timer task ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkSCD
  ""
  ^Schedulable
  [^String named]

  (let [timer (atom (Timer. named true))
        holdQ (ConcurrentHashMap.)
        cpu (atom nil)]
    (with-meta
      (reify Schedulable

        (alarm [_ w arg delayMillis]
          (if (spos? delayMillis)
            (let
              [tt (tmtask<>
                    #(.interrupt w arg))]
              (addTimer @timer tt delayMillis)
              tt)))

        (purge [_]
          (.purge ^Timer @timer))

        (dequeue [_ w] )

        (run [_ w]
          (when-some [r (cast? Runnable w)]
            (preRun holdQ r)
            (-> ^TCore
                @cpu
                (.execute r))))

        (postpone [me w delayMillis]
          (cond
            (szero? delayMillis)
            (do->nil (.run me w))
            (sneg? delayMillis)
            (do->nil (.hold me w))
            :else
            (let [tt (tmtask<>
                       #(.wakeup me w))]
              (addTimer @timer tt delayMillis)
              tt)))

        (hold [this w]
          (.hold this (xrefPID w) w))

        (hold [_ pid w]
          (if (some? pid)
            (.put holdQ pid w)))

        (wakeup [this w]
          (.wakeAndRun this (xrefPID w) w))

        (wakeAndRun [this pid w]
          (if (some? pid)
            (.remove holdQ pid))
          (.run this w))

        (reschedule [this w]
          (.run this w))

        (dispose [this]
          (let [^TCore c @cpu]
            (.deactivate this)
            (if (some? c) (.dispose c))))

        (activate [_ options]
          (let [b (not (false? (:trace options)))
                t (or (:threads options) 0)
                c (TCore. named ^long t b)]
            (reset! cpu c)
            (.start c nil)))

        (deactivate [_]
          (let [^TCore c @cpu]
            (doto ^Timer @timer
              (.cancel)
              (.purge))
            (.clear holdQ)
            (if (some? c) (.stop c)))))

      {:typeid ::Scheduler })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scheduler<>
  "Make a Scheduler"
  {:tag Schedulable }

  ([] (scheduler<> (juid)))
  ([^String named] (mkSCD named)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


