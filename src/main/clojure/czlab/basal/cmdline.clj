;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Console interactions."
      :author "Kenneth Leung"}

  czlab.basal.cmdline

  (:require [czlab.basal.log :as log]
            [clojure.string :as cs]
            [czlab.basal.core :as bc]
            [czlab.basal.str :as bs])

  (:import [czlab.basal.core GenericMutable]
           [java.io
            InputStreamReader
            OutputStreamWriter
            BufferedOutputStream]
           [java.io Reader Writer]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- isOption? "" [option]
  (and (string? option)
       (not= "--" option)
       (bs/swAny? option ["--" "-" "/"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeOption "" [option key?]
  (if (isOption? option)
    (bc/if-some+
      [s (cs/replace option
                     #"^(-|/)+" "")]
      (if key? (keyword s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parseOptions
  "Parse command line, returning options and args"
  ([cargs] (parseOptions cargs true))
  ([cargs key?]
   (loop [options (transient {})
          [p1 p2 & more
           :as args] cargs]
     (if-some [o1 (maybeOption p1 key?)]
       (if (or (nil? p2)
               (isOption? p2))
         (recur (assoc! options o1 true)
                (if (nil? p2)
                  more
                  (cons p2 more)))
         (recur (assoc! options o1 p2) more))
       (vector (bc/pcoll! options)
               (if (= "--" p1)
                 (if p2 (cons p2 more)) args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private rdr "" [r]
  `(.read ~(with-meta r {:tag 'Reader})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- readData
  "Read user input: windows has \\r\\n linux has \\n"
  ^String
  [in]
  (let [[ms bf]
        (loop [c (rdr in)
               bf (bs/strbf<>)]
          (let
            [m (cond
                 (or (== c -1) (== c 4)) #{:quit :break}
                 (== c (int \newline)) #{:break}
                 (or (== c (int \backspace))
                     (== c (int \return)) (== c 27)) nil
                 :else (bc/do->nil (.append bf (char c))))]
            (if (bc/in? m :break)
              [m bf]
              (recur (rdr in) bf))))]
    (if-not
      (bc/in? ms :quit) (bs/strim bf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- onAnswer
  "Process the answer, returning the next question"
  [^Writer cout
   {:keys [default id
           result must? next] :as cmdQ}
   props answer]

  (if (nil? answer)
    (bc/do->nil (.write cout "\n"))
    (let [rc (bs/stror answer default)]
      (cond
        ;;if required to answer, repeat the question
        (and (bs/nichts? rc) must?)
        id
        (keyword? result)
        (do (bc/setf! props
                      result rc) next)
        (fn? result)
        (let [[n p]
              (result rc @props)]
          (doto props bc/wipe! (bc/copy* p))
          (if (nil? n) ::caio!! n))

        :else :caio!!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popQQ
  "Pop the question"
  [^Writer cout
   ^Reader cin
   {:keys [question
           choices
           default] :as cmdQ} props]

  (.write cout
          (str question
               (if (:must? cmdQ) "*") "? "))
  ;; choices ?
  (if (bs/hgl? choices)
    (.write cout (str "[" choices "]")))
  ;; defaults ?
  (if (bs/hgl? default)
    (.write cout (str "(" default ")")))
  (doto cout (.write " ") .flush)
  ;; get the input from user
  ;; return the next question, :end ends it
  (->> (readData cin)
       (onAnswer cout cmdQ props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popQ
  "Pop the question"
  [cout cin cmdQ props]
  (if cmdQ (popQQ cout cin cmdQ props) :caio!!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cycleQ
  "Cycle through the questions"
  [cout cin cmdQNs start props]

  (loop [rc (popQ cout
                  cin
                  (cmdQNs start) props)]
    (cond
      (= :caio!! rc) @props
      (nil? rc) {}
      :else (recur (popQ cout
                         cin
                         (cmdQNs rc) props)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn termio
  "Prompt a sequence of questions via console"
  [cmdQs question1]
  {:pre [(map? cmdQs)]}

  (let [cout (-> System/out
                 BufferedOutputStream. OutputStreamWriter.)
        cin (InputStreamReader. (System/in))
        func (partial cycleQ cout cin)]
    (.write cout (str ">>> Press "
                      "<ctrl-c> or <ctrl-d>"
                      "<Enter> to cancel...\n"))
    (-> (reduce
          #(update-in %1 [%2] assoc :id %2)
          cmdQs
          (keys cmdQs))
        (func question1 (GenericMutable. {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
#_
(def ExampleQuestions
  {:q1 {:question "hello ken"
        :choices "q|b|c"
        :default "c"
        :must? true
        :result :a1
        :next :q2}
   :q2 {:question "hello paul"
        :result :a2
        :next :q3}
   :q3 {:question "hello joe"
        :choices "2"
        :result (fn [answer result]
                  [nil (assoc result :zzz answer)])}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

