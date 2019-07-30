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
           [java.util.concurrent
            ThreadFactory
            LinkedBlockingQueue
            ThreadPoolExecutor
            TimeUnit
            ConcurrentHashMap
            RejectedExecutionHandler]
           [java.lang.management
            RuntimeMXBean
            ManagementFactory
            OperatingSystemMXBean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore<> ""
  ([id] (tcore<> id (* 2 (.availableProcessors (Runtime/getRuntime)))))
  ([id tds] (tcore<> id tds true))
  ([id tds trace?] (tcore<> id tds 60000 trace?))
  ([id tds keepAliveMillis trace?]
   (let [^ThreadPoolExecutor
         p (proxy [ThreadPoolExecutor RejectedExecutionHandler]
             [(int (max 1 tds))
              (int (max 1 tds))
              ^long keepAliveMillis
              TimeUnit/MILLISECONDS
              (LinkedBlockingQueue.)]
             (rejectedExecution [r x]
               ;;TODO: deal with too much work for the core...
               (if trace?
                 (l/error "tcore rejecting work!"))))]
     (->> (reify ThreadFactory
            (newThread [_ r]
              (c/do-with [t (Thread. r)]
                (.setName t (str id "#" (u/seqint2)))
                (.setContextClassLoader t (u/get-cldr)))))
          (.setThreadFactory p))
     (.setRejectedExecutionHandler p ^RejectedExecutionHandler p)
     (if trace?
       (l/debug
         (str "tcore#" id " ctor: threads = " (.getCorePoolSize p))))
     (atom {:id id :core p :paused? false :trace? trace?}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore-start "" [t]
  (c/assoc!! t :paused? false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore-stop "" [t]
  (c/assoc!! t :paused? true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore-dispose "" [t]
  (let [{:keys [id trace? core]} @t]
    (tcore-stop t)
    (.shutdown ^ThreadPoolExecutor core)
    (if trace?
      (l/debug
        "tcore#" id " disposed and shut down"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore-exec "" [t r]
  (let [{:keys [paused? core]} @t]
    (if-not paused?
      (.execute ^ThreadPoolExecutor core ^Runnable r)
      (l/warn
        "Ignoring the runnable, core is not running"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore-info "" [t]
  (let [{:keys [id core]} @t]
    (str "tcore#" id " with threads = "
         (.getCorePoolSize ^ThreadPoolExecutor core))))

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
(defn- prerun [hQ w] (some->> w (.remove ^Map hQ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add-timer [timer task delayMillis]
  (.schedule ^Timer
             timer ^TimerTask task ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn alarm
  [sch delayMillis f & args]
  {:pre [(fn? f)(c/spos? delayMillis)]}
  (c/do-with
    [tt (u/tmtask<> #(apply f args))]
    (add-timer (:timer @sch) tt delayMillis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn purge
  "Drop all pending timer tasks."
  [sch] (.purge ^Timer (:timer @sch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defn dequeue "" [sch w] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run "Execute task." [sch w]
  (when-some
    [w' (c/cast? Runnable w)]
    (prerun (:hq @sch) w')
    (tcore-exec (:cpu @sch) w')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hold "Pause a task."
  [sch w] (if w (.put ^Map (:hq @sch) w w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wakeup "Restart a task." [sch w] (run sch w))

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
  "Start the scheduler." [sch] (tcore-start (:cpu @sch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deactivate
  "Stop the scheduler." [sch]
  (let [{:keys [^Timer timer hq cpu]} @sch]
    (tcore-stop cpu)
    (.clear ^Map hq)
    (doto timer .cancel .purge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dispose
  "Stop and tear down." [sch]
  (deactivate sch)
  (tcore-dispose (:cpu @sch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scheduler<>
  "Make a Scheduler."
  ([] (scheduler<> (u/jid<>) nil))
  ([named options]
   (let [id (s/sname named)
         {:keys [threads trace?]} options]
     (atom {:cpu (tcore<> id
                          ^long
                          (c/num?? threads 0)
                          (c/!false? trace?))
            :id id
            :timer (Timer. id true)
            :hq (ConcurrentHashMap.)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


