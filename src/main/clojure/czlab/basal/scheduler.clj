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

  (:require [czlab.basal.logging :as log])

  (:use [czlab.basal.core]
        [czlab.basal.str])

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
(defn- xrefPID "" [r] (if (ist? Idable r) (.id ^Idable r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- preRun "" [^Map hQ w] (some->> (xrefPID w) (. hQ remove)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addTimer "" [^Timer timer
                    ^TimerTask task delayMillis]
  (. timer schedule task ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkSCD
  "" ^Schedulable [named]

  (let [timer (atom (Timer. named true))
        holdQ (ConcurrentHashMap.)
        cpu (atom nil)]
    (with-meta
      (reify Schedulable

        (alarm [_ w arg delayMillis]
          (when (spos? delayMillis)
            (let [tt (tmtask<>
                       #(. w interrupt arg))]
              (addTimer @timer tt delayMillis) tt)))

        (purge [_] (. ^Timer @timer purge))

        (dequeue [_ w] )

        (run [_ w]
          (when-some [r (cast? Runnable w)]
            (preRun holdQ r)
            (. ^TCore @cpu execute r)))

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
          (. this hold (xrefPID w) w))

        (hold [_ pid w]
          (if pid (.put holdQ pid w)))

        (wakeup [this w]
          (. this wakeAndRun (xrefPID w) w))

        (wakeAndRun [this pid w]
          (some->> pid
                   (. holdQ remove))
          (.run this w))

        (reschedule [this w] (.run this w))

        (dispose [this]
          (.deactivate this)
          (some-> ^TCore @cpu .dispose))

        (activate [_] (.activate _ nil))
        (activate [_ options]
          (let [b (!false? (:trace options))
                t (or (:threads options) 0)
                c (TCore. named ^long t b)]
            (reset! cpu c)
            (. c start nil)))

        (deactivate [_]
          (doto ^Timer @timer .cancel .purge)
          (. holdQ clear)
          (some-> ^TCore @cpu .stop)))

      {:typeid ::Scheduler })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scheduler<>
  "Make a Scheduler"
  {:tag Schedulable}

  ([] (scheduler<> (jid<>)))
  ([named] (mkSCD (sname named))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


