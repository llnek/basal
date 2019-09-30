;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns
  ^{:doc ""
    :author "Kenneth Leung"}

  czlab.test.basal.proc

  (:require [clojure.java.io :as io]
            [clojure
             [test :as ct]
             [string :as cs]]
            [czlab.basal
             [proc :as p]
             [io :as i]
             [util :as u]
             [xpis :as po]
             [core
              :refer [ensure?? ensure-thrown??] :as c]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^{:private true
       :tag java.io.File} CUR-FP (i/tmpfile (u/jid<>)))
(def ^:private SCD (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-proc

  (ensure?? "init"
            (c/let#true
              [s (p/scheduler<>)] (reset! SCD s) (po/activate s)))

  (ensure?? "run*" (= 1
                     (let [x (atom 0)]
                       (p/run* @SCD swap! [x inc])
                       (u/pause 500)
                       @x)))

  (ensure?? "run" (= 1
                     (let [x (atom 0)]
                       (p/run @SCD
                              (u/run<> (swap! x inc)))
                       (u/pause 500)
                       @x)))

  (ensure?? "postpone"
            (= 1 (let [x (atom 0)]
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

  (ensure?? "finz" (c/do#true (po/deactivate @SCD)))

  (ensure?? "test-end" (= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-proc basal-test-proc
  (ct/is (c/clj-test?? test-proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


