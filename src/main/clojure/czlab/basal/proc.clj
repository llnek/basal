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
;; Copyright © 2013-2024, Kenneth Leung. All rights reserved.

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

  "A BlockingQueue Thread Pool Executor."
  {:arglists '([id]
               [id tds]
               [id tds trace?]
               [id tds keepAliveMillis trace?])}

  ([id]
   (tcore<> id 0))

  ([id tds]
   (tcore<> id tds true))

  ([id tds trace?]
   (tcore<> id tds 60000 trace?))

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

  "Run a function in a separate thread."
  {:tag Thread
   :arglists '([func start?]
               [func start? options])}

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

  "Run function async."
  {:arglists '([func]
               [func options])}

  ([func]
   (async! func nil))

  ([func options]
   (thread<> func true options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord JvmInfo [])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jvm-info

  "Get info on the JVM."
  {:arglists '([])}
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

  "Get the process pid."
  {:tag String
   :arglists '([])}
  []

  (let [b (ManagementFactory/getRuntimeMXBean)]
   (u/try!! "" (c/_1 (-> b .getName str (c/split "@"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn delay-exec

  "Run function after some delay."
  {:arglists '([func delayMillis])}
  [func delayMillis]
  {:pre [(fn? func)
         (c/spos? delayMillis)]}

  (.schedule (Timer. true) (u/tmtask<> func) ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exit-hook

  "Add a shutdown hook."
  {:arglists '([func])}
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

  "Create a task scheduler."
  {:arglists '([]
               [named options])}

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

