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

(ns ^{:doc "Useful os process & runtime functions."
      :author "Kenneth Leung" }

  czlab.xlib.process

  (:require
    [czlab.xlib.meta :refer [getCldr]]
    [czlab.xlib.logging :as log])

  (:use [czlab.xlib.core]
        [czlab.xlib.str])

  (:import
    [czlab.xlib CU CallableWithArgs]
    [clojure.lang APersistentMap]
    [java.lang.management
     RuntimeMXBean
     ManagementFactory
     OperatingSystemMXBean ]
    [java.util.concurrent Callable]
    [java.util TimerTask Timer]
    [java.lang Thread Runnable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn thread<>

  "Run this function in a separate thread"
  {:tag Thread}

  ([func start?] (thread<> func start? nil))
  ([func start? arg]
   {:pre [(fn? func)(or (nil? arg)(map? arg))]}
   (let [t (Thread. (runnable<> func))
         c (or (:cl arg)
               (:classLoader arg))]
     (some->> (cast? ClassLoader c)
              (.setContextClassLoader t ))
     (.setDaemon t (true? (:daemon arg)))
     (if start? (.start t))
     (log/debug "thread<>: thread#%s%s%s"
                (.getName t)
                ", daemon = " (.isDaemon t))
     t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn syncBlockExec

  "Run this function synchronously"
  [^Object lock func & args]
  {:pre [(fn? func)]}

  (CU/syncExec
    lock
    (reify CallableWithArgs
      (run [_ p1 xs]
        (apply func p1 xs)))
    (first args)
    (object-array (drop 1 args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn async!

  "Run this function asynchronously"

  ([func] (async! func nil))
  ([func arg]
   {:pre [(fn? func) (or (nil? arg)(map? arg))]}
   (thread<> func true arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn safeWait

  "Block current thread for some millisecs"
  [millisecs]

  (trye! nil
         (if (> millisecs 0) (Thread/sleep millisecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jvmInfo

  "Get info on the jvm"
  ^APersistentMap
  []

  (let [os (ManagementFactory/getOperatingSystemMXBean)
        rt (ManagementFactory/getRuntimeMXBean)]
    {:spec-version (.getSpecVersion rt)
     :vm-version (.getVmVersion rt)
     :spec-vendor (.getSpecVendor rt)
     :vm-vendor (.getVmVendor rt)
     :spec-name (.getSpecName rt)
     :vm-name (.getVmName rt)
     :name (.getName rt)
     :arch (.getArch os)
     :processors (.getAvailableProcessors os)
     :os-name (.getName os)
     :os-version (.getVersion os) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn processPid

  "Get the current process pid"
  ^String
  []

  (let [ss (-> (ManagementFactory/getRuntimeMXBean)
               (.getName)
               str
               (.split "@"))]
    (if (empty ss) "" (first ss))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn delayExec

  "Run this function after some delay"
  [func delayMillis]
  {:pre [(fn? func)]}

  (-> (Timer. true)
      (.schedule (proxy
                   [TimerTask][]
                   (run [] (func)))
                 (long delayMillis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn exitHook

  "Add this as a shutdown hook"
  [func]
  {:pre [(fn? func)]}

  (->> (thread<> func false {:daemon true})
       (.addShutdownHook (Runtime/getRuntime))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


