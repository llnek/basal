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

(ns ^{:doc "Console interactions."
      :author "Kenneth Leung" }

  czlab.xlib.cmdline

  (:require
    [czlab.xlib.core :refer [isWindows? in? do->nil]]
    [czlab.xlib.str :refer [stror strim has?]]
    [czlab.xlib.logging :as log]
    [clojure.string :as cs])

  (:import
    [java.io
     InputStreamReader
     OutputStreamWriter
     BufferedOutputStream]
    [java.io Reader Writer]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- readData

  "Read user input"
  ^String
  [^Writer cout ^Reader cin]

  ;; windows has '\r\n' linux has '\n'
  (let
    [bf (StringBuilder.)
     ms (loop [c (.read cin)]
          (let [m (cond
                    (or (== c -1) (== c 4))
                    #{:quit :break}
                    (== c (int \newline))
                    #{:break}
                    (or (== c (int \backspace))
                        (== c (int \return))
                        (== c 27))
                    nil
                    :else
                    (do->nil (.append bf (char c))))]
            (if (in? m :break) m (recur (.read cin)))))]
    (if (in? ms :quit) nil (strim bf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- onAnswer

  "Process the answer, returning the next question"
  [^Writer cout
   cmdQ
   props
   answer]

  (let [dft (strim (:default cmdQ))
        res (:result cmdQ)
        must (:must cmdQ)
        nxt (:next cmdQ)]
    (if (nil? answer)
      (do->nil (.write cout "\n"))
      ;;else
      (let [rc (stror answer dft)]
        (cond
          ;;if required to answer, repeat the question
          (and (empty? rc) must)
          (:id cmdQ)

          (keyword? res)
          (do (swap! props assoc res rc) nxt)

          (fn? res)
          (let [[_ p] (res rc @props)]
            (reset! props p)
            n)

          :else :end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popQQ

  "Pop the question"
  [^Writer cout
   ^Reader cin
   cmdQ
   props]

  (let [chs (strim (:choices cmdQ))
        dft (strim (:default cmdQ))
        q (strim (:question cmdQ))
        must (:must cmdQ)]
    (.write cout (str q (if must "*" "") "? "))
    ;; choices ?
    (when-not (empty? chs)
      (if (has? chs \n)
        (.write cout (str (if (.startsWith chs "\n") "[" "[\n")
                          chs
                          (if (.endsWith chs "\n") "]" "\n]" )))
        (.write cout (str "[" chs "]"))))
    ;; defaults ?
    (when-not (empty? dft)
      (.write cout (str "(" dft ")")))
    (doto cout (.write " ")(.flush))
    ;; get the input from user
    ;; return the next question, :end ends it
    (->> (readData cout cin)
         (onAnswer cout cmdQ props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popQ

  "Pop the question"
  [cout cin cmdQ props]

  (if (some? cmdQ) (popQQ cout cin cmdQ props) :end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cycleQ

  "Cycle through the questions"
  [cout cin cmdQNs start props]

  (loop [rc (popQ cout
                  cin
                  (cmdQNs start) props)]
    (cond
      (= :end rc) @props
      (nil? rc) {}
      :else (recur (popQ cout
                         cin
                         (cmdQNs rc) props)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cliConverse

  "Prompt a sequence of questions via console"
  [cmdQs question1]

  {:pre [(map? cmdQs)]}

  (let [cout (->> (BufferedOutputStream. (System/out))
                  (OutputStreamWriter.))
        kp (if (isWindows?) "<ctrl-c>" "<ctrl-d>")
        cin (InputStreamReader. (System/in))
        func (partial cycleQ cout cin)]
    (.write cout (str ">>> Press "
                      kp
                      "<Enter> to cancel...\n"))
    (->
      (reduce
        #(assoc %1 %2 (assoc (get cmdQs %2) :id %2))
        {}
        (keys cmdQs))
      (func question1 (atom {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(comment
(def QM
  {:q1 {:question "hello ken"
        :choices "q|b|c"
        :default "c"
        :required true
        :result :a1
        :next :q2}

   :q2 {:question "hello paul"
        :result :a2
        :next :q3}

   :q3 {:question "hello joe"
        :choices "2"
        :result (fn [answer result]
                  [:end (assoc result :zzz answer)])}})
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


