;; Copyright © 2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.proc

  "Useful os process & runtime functions."

  (:require [czlab.basal.util :as u]
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
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tcore<>

  ^{:arglists '([id]
                [id tds]
                [id tds trace?]
                [id tds keepAliveMillis trace?])
    :doc "A BlockingQueue Thread Pool Executor."}

  ([id] (tcore<> id 0))

  ([id tds] (tcore<> id tds true))

  ([id tds trace?] (tcore<> id tds 60000 trace?))

  ([id tds keepAliveMillis trace?]
   (let [tds (if (pos? (c/num?? tds 0)) tds (u/pthreads))
         ^ThreadPoolExecutor
         core (proxy [ThreadPoolExecutor]
                [tds
                 tds
                 ^long keepAliveMillis
                 TimeUnit/MILLISECONDS
                 (LinkedBlockingQueue.)])
         paused? (c/mu-int* 1)
         rex (reify
               RejectedExecutionHandler
               (rejectedExecution [_ r x]
                 (if trace? (c/error "TCore rejecting work!"))))]
     (.setRejectedExecutionHandler core rex)
     (.setThreadFactory core
                        (reify ThreadFactory
                          (newThread [_ r]
                            (c/do-with [t (Thread. r)]
                              (.setName t (str id "#" (u/seqint2)))
                              (.setContextClassLoader t (u/get-cldr))))))
     (if trace?
       (c/debug
         "TCore#%s - ctor: threads = %s" id (.getCorePoolSize core)))
     (reify
       Object
       (toString [_]
         (c/fmt "TCore#%s - threads = %d." id (.getCorePoolSize core)))
       c/Startable
       (start [me _] (locking me (c/mu-int paused? 0)) me)
       (start [me] (.start me nil))
       (stop [me] (locking me (c/mu-int paused? 1)) me)
       c/Testable
       (is-valid? [_] (zero? (c/mu-int paused?)))
       c/Enqueable
       (put [me r]
         (if (pos? (c/mu-int paused?))
           (c/warn "TCore[%s] is not running!" core)
           (if (c/is? Runnable r)
             (.execute core ^Runnable r)
             (c/warn "Unsupported %s" r))) me)
       c/Finzable
       (finz [me]
         (.stop me)
         (c/try! (.shutdown core))
         (if trace?
           (c/debug
             "TCore#%s - disposed and shut down." id)) me)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn thread<>

  ^{:arglists '([func start?]
                [func start? options])
    :doc "Run a function in a separate thread."}

  {:tag Thread}

  ([func start?]
   (thread<> func start? nil))

  ([func start? {:as options
                 :keys [cldr daemon?]}]
   {:pre [(fn? func)]}

   (c/do-with [t (Thread. (u/run<> (func)))]
     (let [c (or cldr (u/get-cldr))]
       (some->> (c/cast? ClassLoader c)
                (.setContextClassLoader t))
       (.setDaemon t (true? daemon?))
       (if start? (.start t))
       (c/debug "thread#%s%s%s"
                (.getName t)
                ", daemon = " (.isDaemon t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn async!

  ^{:arglists '([func]
                [func options])
    :doc "Run function async."}

  ([func]
   (async! func nil))

  ([func options]
   (thread<> func true options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord JvmInfo [])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jvm-info

  ^{:arglists '([])
    :doc "Get info on the JVM."}

  []

  (let [os (ManagementFactory/getOperatingSystemMXBean)
        rt (ManagementFactory/getRuntimeMXBean)]
    (c/object<> JvmInfo
                :spec-version (.getSpecVersion rt)
                :vm-version (.getVmVersion rt)
                :spec-vendor (.getSpecVendor rt)
                :vm-vendor (.getVmVendor rt)
                :spec-name (.getSpecName rt)
                :vm-name (.getVmName rt)
                :name (.getName rt)
                :arch (.getArch os)
                :os-name (.getName os)
                :os-version (.getVersion os)
                :processors (.getAvailableProcessors os))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn process-pid

  ^{:arglists '([])
    :tag String
    :doc "Get the process pid."}

  []

  (let [b (ManagementFactory/getRuntimeMXBean)]
   (u/try!! "" (c/_1 (-> b .getName str (c/split "@"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn delay-exec

  ^{:arglists '([func delayMillis])
    :doc "Run function after some delay."}

  [func delayMillis]
  {:pre [(fn? func)
         (c/spos? delayMillis)]}

  (.schedule (Timer. true) (u/tmtask<> func) ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exit-hook

  ^{:arglists '([func])
    :doc "Add a shutdown hook."}

  [func]
  {:pre [(fn? func)]}

  (->> (thread<> func false {:daemon? true})
       (.addShutdownHook (Runtime/getRuntime))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;scheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prerun
  [hQ w] (some->> w (.remove ^Map hQ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add-timer
  [timer task delayMillis]
  (.schedule ^Timer
             timer ^TimerTask task ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Scheduler
  "Schedules tasks."
  (alarm [_ delayMillis f arglist] "Runs the function after some delay.")
  (run* [_ f arglist] "Run this function.")
  (run [_ w] "Run this task.")
  (postpone [_ w delayMillis] "Delay this task."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scheduler<>

  ^{:arglists '([]
                [named options])
    :doc "Create a task scheduler."}

  ([]
   (scheduler<> (u/jid<>) nil))

  ([named options]
   (let [{:keys [threads trace?]} options
         id (c/sname named)
         timer (Timer. id true)
         cpu (tcore<> id
                      ^long
                      (c/num?? threads 0)
                      (c/!false? trace?))]
     (reify Scheduler
       (alarm [_ delayMillis f args]
         (c/do-with
           [tt (u/tmtask<> (cond (c/sas? c/Interruptable f)
                                 #(c/interrupt f args)
                                 (and (fn? f)
                                      (sequential? args))
                                 #(apply f args)
                                 :else (c/raise! "alarm call failed")))]
           (add-timer timer tt delayMillis)))
       (run* [me f args]
         (c/pre (fn? f) (sequential? args))
         (c/put cpu
                #(try (apply f args)
                      (catch Throwable _ (c/exception _)))) me)
       (run [me w]
         (c/pre (c/is? Runnable w))
         (.run* me (if-not (fn? w)
                     #(.run ^Runnable w) w) []))
       (postpone [me w delayMillis]
         (c/pre (number? delayMillis))
         (cond (zero? delayMillis)
               (.run me w)
               (pos? delayMillis)
               (.alarm me delayMillis #(.run me w) [])) me)
       c/Testable
       (is-valid? [_] (c/is-valid? cpu))
       c/Activable
       (activate [me]
         (c/start cpu) me)
       (deactivate [me]
         (c/stop cpu)
         (doto timer .cancel .purge) me)
       c/Finzable
       (finz [me]
         (.deactivate me)
         (c/finz cpu) me)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

