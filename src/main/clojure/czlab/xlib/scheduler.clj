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

(ns ^{:doc "Implementation of a scheduler with pooled threads."
      :author "Kenneth Leung" }

  czlab.xlib.scheduler

  (:require
    [czlab.xlib.core :refer [cast? juid]]
    [czlab.xlib.logging :as log]
    [czlab.xlib.str :refer [hgl?]])

  (:import
    [java.util.concurrent ConcurrentHashMap]
    [czlab.xlib
     Schedulable
     TCore
     Disposable
     Activable
     Identifiable
     Named
     RunnableWithId]
    [java.util Map Properties Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xrefPID

  ""
  ^Object
  [r]

  (when
    (instance? Identifiable r)
    (.id ^Identifiable r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- preRun

  ""
  [^Map hQ w]

  (when-some
    [pid (xrefPID w)]
    (.remove hQ pid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addTimer

  ""
  [^Timer timer ^TimerTask task delayMillis]

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
      (reify

        Schedulable

        (dequeue [_ w] )

        (run [this w]
          (when-some [^Runnable
                      r (cast? Runnable w)]
            (preRun holdQ r)
            (-> ^TCore
                @cpu
                (.execute r))))

        (postpone [me w delayMillis]
          (cond
            (< delayMillis 0) (.hold me w)
            (== delayMillis 0) (.run me w)
            :else
            (addTimer @timer
                      (proxy
                        [TimerTask][]
                        (run [] (.wakeup me w)))
                      delayMillis)))

        (hold [this w]
          (.hold this (xrefPID w) w))

        (hold [_ pid w]
          (when (some? pid)
            (.put holdQ pid w)))

        (wakeup [this w]
          (.wakeAndRun this (xrefPID w) w))

        (wakeAndRun [this pid w]
          (when (some? pid)
            (.remove holdQ pid)
            (.run this w)))

        (reschedule [this w]
          (when (some? w)
            (.run this w)))

        (dispose [_]
          (let [^TCore c @cpu]
            (.cancel ^Timer @timer)
            (.clear holdQ)
            (when (some? c) (.dispose c))))

        (activate [_ options]
          (let [^long t (->> (Runtime/getRuntime)
                             (.availableProcessors)
                             (or (:threads options) ))
                b (not (false? (:trace options)))
                c (TCore. named t b)]
            (reset! cpu c)
            (.start c)))

        (deactivate [_]
          (.cancel ^Timer @timer)
          (.clear holdQ)
          (.stop ^TCore @cpu)))

      {:typeid ::Scheduler })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mkScheduler

  "Make a Scheduler"

  (^Schedulable [] (mkScheduler (juid)))
  (^Schedulable [^String named] (mkSCD named)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

