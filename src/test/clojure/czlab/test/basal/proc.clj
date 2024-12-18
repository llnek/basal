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

(ns czlab.test.basal.proc

  (:require [clojure.java.io :as io]
            [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.proc :as p]
            [czlab.basal.io :as i]
            [czlab.basal.util :as u]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^{:private true
       :tag java.io.File} CUR-FP (i/tmpfile (u/jid<>)))
(def ^:private SCD (atom nil))
(def ^:private TDATA (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-proc

  (ensure?? "init"
            (c/let->true
              [s (p/scheduler<>)] (reset! SCD s) (c/activate s)))

  (ensure?? "thread<>"
            (let [_ (reset! TDATA nil)
                  t (p/thread<> #(reset! TDATA 117)
                                true
                                {:name "t9"})]
              (Thread/sleep 1500)
              (== 117 @TDATA)))

  (ensure?? "vthread<>"
            (let [_ (reset! TDATA nil)
                  t (p/vthread<> #(reset! TDATA 117)
                                 true
                                 {:name "t9"})]
              (Thread/sleep 1500)
              (== 117 @TDATA)))

  (ensure?? "run*" (== 1
                     (let [x (atom 0)]
                       (p/run* @SCD swap! [x inc])
                       (u/pause 500)
                       @x)))

  (ensure?? "run" (== 1
                     (let [x (atom 0)]
                       (p/run @SCD
                              (u/run<> (swap! x inc)))
                       (u/pause 500)
                       @x)))

  (ensure?? "postpone"
            (== 1 (let [x (atom 0)]
                   (p/postpone @SCD
                               (u/run<> (swap! x inc)) 500)
                   (u/pause 800)
                   @x)))

  (ensure?? "async!"
            (do (p/async! #(spit CUR-FP "123"))
                (u/pause 500)
                (and (.exists CUR-FP)
                     (>= (.length CUR-FP) 3))))

  (ensure?? "delay-exec"
            (do (p/delay-exec
                  #(spit CUR-FP "123456") 500)
                (u/pause 800)
                (and (.exists CUR-FP)
                     (>= (.length CUR-FP) 6))))

  (ensure?? "locking"
            (do (locking
                  (String. "lock")
                  ((fn [a & xs]
                     (spit CUR-FP
                           (apply str a xs)))
                   "123" "456" "789"))
                (and (.exists CUR-FP)
                     (>= (.length CUR-FP) 9))))

  (ensure?? "process-pid" (> (.length (p/process-pid)) 0))

  (ensure?? "finz" (c/do->true (c/deactivate @SCD)))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-proc basal-test-proc
  (ct/is (c/clj-test?? test-proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


