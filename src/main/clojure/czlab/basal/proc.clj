;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Useful os process & runtime functions."
      :author "Kenneth Leung"}

  czlab.basal.proc

  (:require [czlab.basal.util :as u]
            [czlab.basal.meta :as m]
            [czlab.basal.str :as s]
            [czlab.basal.log :as l]
            [czlab.basal.core :as c])

  (:import [java.util
            Map
            Timer
            Timer
            TimerTask
            Properties]
           [java.lang
            Thread
            Runnable]
           [czlab.jasal
            TCore
            Idable
            Activable
            Disposable
            Schedulable
            Interruptable
            RunnableWithId]
           [java.util.concurrent
            ConcurrentHashMap]
           [java.lang.management
            RuntimeMXBean
            ManagementFactory
            OperatingSystemMXBean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn thread<>
  "Run code in a separate thread."
  {:tag Thread}
  ([func start?] (thread<> func start? nil))
  ([func start? {:keys [cldr daemon] :as options}]
   {:pre [(fn? func)]}
   (c/do-with [t (Thread. (u/run<> (func)))]
     (let [c (or cldr (u/get-cldr))]
       (some->> (c/cast? ClassLoader c)
                (.setContextClassLoader t))
       (.setDaemon t (true? daemon))
       (if start? (.start t))
       (l/debug "thread#%s%s%s"
                (.getName t)
                ", daemon = " (.isDaemon t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn async!
  "Run function async."
  ([func] (async! func nil))
  ([func options] (thread<> func true options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jvm-info "Get info on the jvm." []
  (let [os (ManagementFactory/getOperatingSystemMXBean)
        rt (ManagementFactory/getRuntimeMXBean)]
    {:processors (.getAvailableProcessors os)
     :spec-version (.getSpecVersion rt)
     :vm-version (.getVmVersion rt)
     :spec-vendor (.getSpecVendor rt)
     :vm-vendor (.getVmVendor rt)
     :spec-name (.getSpecName rt)
     :vm-name (.getVmName rt)
     :name (.getName rt)
     :arch (.getArch os)
     :os-name (.getName os)
     :os-version (.getVersion os)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn process-pid
  "Get process pid." ^String []
  (let [b (ManagementFactory/getRuntimeMXBean)]
   (u/try!! "" (c/_1 (-> b .getName str (.split "@"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn delay-exec
  "Run function after some delay."
  [func delayMillis]
  {:pre [(fn? func)
         (c/spos? delayMillis)]}
  (.schedule (Timer. true) (u/tmtask<> func) ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exit-hook
  "Add a shutdown hook."
  [func]
  {:pre [(fn? func)]}
  (->> (thread<> func false {:daemon true})
       (.addShutdownHook (Runtime/getRuntime))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;scheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xref-pid [r] (if-some [r' (c/cast? Idable r)] (.id r')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prerun [hQ w] (some->> (xref-pid w) (.remove ^Map hQ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add-timer [timer task delayMillis]
  (.schedule ^Timer
             timer ^TimerTask task ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn alarm
  [sch w arg delayMillis]
  {:pre [(c/spos? delayMillis)
         (c/is? Interruptable w)]}
  (c/do-with
    [tt (u/tmtask<>
          #(.interrupt ^Interruptable w arg))]
    (add-timer (:timer @sch) tt delayMillis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn purge
  "Kill all pending timer tasks." [sch] (.purge ^Timer (:timer @sch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dequeue "" [sch w] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run "Execute task." [sch w]
  (when-some
    [w' (c/cast? Runnable w)]
    (prerun (:hq @sch) w')
    (.execute ^TCore (:cpu @sch) w')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hold "Pause a task."
  ([sch w] (hold sch (xref-pid w) w))
  ([sch pid w] (if pid (.put ^Map (:hq @sch) pid w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wakeup
  "Restart a task."
  ([sch w] (wakeup sch (xref-pid w) w))
  ([sch pid w]
   (some->> pid
            (.remove ^Map (:hq @sch))) (run sch w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn postpone "" [sch w delayMillis]
  {:pre [(number? delayMillis)]}
  (cond (zero? delayMillis)
        (c/do#nil (run sch w))
        (neg? delayMillis)
        (c/do#nil (hold sch w))
        :else
        (c/do-with [tt (u/tmtask<>
                         #(wakeup sch w))]
          (add-timer (:timer @sch) tt delayMillis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reschedule "" [sch w] (run sch w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn activate
  "Start the scheduler." [sch] (.start ^TCore (:cpu @sch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deactivate
  "Stop the scheduler." [sch]
  (let [{:keys [^Timer timer hq cpu]} @sch]
    (.stop ^TCore cpu)
    (.clear ^Map hq)
    (doto timer .cancel .purge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dispose
  "Stop and tear down." [sch]
  (deactivate sch) (.dispose ^TCore (:cpu @sch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scheduler<>
  "Make a Scheduler."
  ([] (scheduler<> (u/jid<>) nil))
  ([named options]
   (let [id (s/sname named)
         {:keys [threads trace?]} options]
     (atom {:cpu (TCore. id
                         ^long
                         (c/num?? threads 0)
                         (c/!false? trace?))
            :id id
            :timer (Timer. id true)
            :hq (ConcurrentHashMap.)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


