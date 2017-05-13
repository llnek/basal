;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Useful os process & runtime functions."
      :author "Kenneth Leung"}

  czlab.basal.process

  (:require [czlab.basal.meta :as m :refer [getCldr]]
            [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:import [czlab.jasal CU CallableWithArgs]
           [java.util.concurrent Callable]
           [clojure.lang APersistentMap]
           [java.lang.management
            RuntimeMXBean
            ManagementFactory
            OperatingSystemMXBean ]
           [java.util TimerTask Timer]
           [java.lang Thread Runnable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn thread<>
  "Run function in a separate thread" {:tag Thread}

  ([func start? {:keys [cl daemon
                        classLoader] :as arg}]
   {:pre [(fn? func)]}
   (let [t (Thread. (c/run-able<> (func)))
         c (or cl
               classLoader (getCldr))]
     (some->> (c/cast? ClassLoader c)
              (.setContextClassLoader t))
     (.setDaemon t (true? daemon))
     (if start? (.start t))
     (log/debug "thread#%s%s%s"
                (.getName t)
                ", daemon = " (.isDaemon t))
     t))

  ([func start?] (thread<> func start? nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn syncBlockExec
  "Run function synchronized"
  [lockObj func & args]
  {:pre [(fn? func)]}
  (CU/syncExec
    lockObj
    (reify CallableWithArgs
      (run [_ p1 xs]
        (apply func p1 xs)))
    (first args)
    (object-array (drop 1 args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn async!
  "Run function async"
  ([func] (async! func nil))
  ([func arg] (thread<> func true arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jvmInfo
  "Get info on the jvm" ^APersistentMap []

  (let
    [os (ManagementFactory/getOperatingSystemMXBean)
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
;;
(defn processPid
  "Get process pid" ^String []

  (c/if-some+
    [ss (-> (ManagementFactory/getRuntimeMXBean)
            .getName
            str
            (.split "@"))]
    (first ss)
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn delayExec
  "Run function after some delay"
  [func delayMillis]
  {:pre [(fn? func)
         (number? delayMillis)]}
  (.schedule (Timer. true) (c/tmtask<> func) ^long delayMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn exitHook
  "Add a shutdown hook"
  [func]
  {:pre [(fn? func)]}
  (->> (thread<> func false {:daemon true})
       (.addShutdownHook (Runtime/getRuntime))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


