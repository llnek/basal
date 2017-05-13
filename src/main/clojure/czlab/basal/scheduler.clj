;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "A scheduler with pooled threads."
      :author "Kenneth Leung"}

  czlab.basal.scheduler

  (:require [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:import [java.util.concurrent ConcurrentHashMap]
           [czlab.jasal
            Schedulable
            TCore
            Disposable
            Activable
            Idable
            RunnableWithId]
          [java.util Map Properties Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xrefPID "" [r] (if (c/ist? Idable r) (.id ^Idable r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- preRun "" [^Map hQ w] (some->> (xrefPID w) (.remove hQ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addTimer "" [^Timer timer
                    ^TimerTask task delayMillis]
  (.schedule timer task ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkSCD
  "" ^Schedulable [named]

  (let [timer (atom (Timer. named true))
        holdQ (ConcurrentHashMap.)
        cpu (atom nil)]
    (reify Schedulable

      (alarm [_ w arg delayMillis]
        (when (c/spos? delayMillis)
          (let [tt (c/tmtask<>
                     #(.interrupt w arg))]
            (addTimer @timer tt delayMillis) tt)))

      (purge [_] (.purge ^Timer @timer))

      (dequeue [_ w] )

      (run [_ w]
        (when-some [r (c/cast? Runnable w)]
          (preRun holdQ r)
          (.execute ^TCore @cpu r)))

      (postpone [me w delayMillis]
        (cond
          (c/szero? delayMillis)
          (c/do->nil (.run me w))
          (c/sneg? delayMillis)
          (c/do->nil (.hold me w))
          :else
          (let [tt (c/tmtask<>
                     #(.wakeup me w))]
            (addTimer @timer tt delayMillis)
            tt)))

      (hold [this w]
        (.hold this (xrefPID w) w))

      (hold [_ pid w]
        (if pid (.put holdQ pid w)))

      (wakeup [this w]
        (.wakeAndRun this (xrefPID w) w))

      (wakeAndRun [this pid w]
        (some->> pid
                 (.remove holdQ))
        (.run this w))

      (reschedule [this w] (.run this w))

      (dispose [this]
        (.deactivate this)
        (some-> ^TCore @cpu .dispose))

      (activate [_] (.activate _ nil))
      (activate [_ options]
        (let [b (c/!false? (:trace options))
              t (or (:threads options) 0)
              c (TCore. named ^long t b)]
          (reset! cpu c)
          (.start  c)))

      (deactivate [_]
        (doto ^Timer @timer .cancel .purge)
        (.clear holdQ)
        (some-> ^TCore @cpu .stop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scheduler<>
  "Make a Scheduler"
  {:tag Schedulable}

  ([] (scheduler<> (c/jid<>)))
  ([named] (mkSCD (s/sname named))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


