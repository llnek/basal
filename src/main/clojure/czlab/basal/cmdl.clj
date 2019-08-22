;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Console interactions."
      :author "Kenneth Leung"}

  czlab.basal.cmdl

  (:require [clojure.string :as cs]
            [czlab.basal.str :as s]
            [czlab.basal.core :as c])

  (:import [java.io
            Reader
            Writer
            InputStreamReader
            OutputStreamWriter
            BufferedOutputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private rdr [r] `(.read ~(with-meta r {:tag 'Reader})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-option? [option]
  (and (string? option)
       (not= "--" option)
       (s/sw-any? option ["--" "-" "/"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-option [option key?]
  (if (is-option? option)
    (c/if-some+
      [s (cs/replace option
                     #"^(-|/)+" "")] (if key? (keyword s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-options
  "Parse command line, returning options and args."
  ([cargs]
   (parse-options cargs true))
  ([cargs key?]
   (loop [options (c/tmap*)
          [p1 p2 & more :as args] cargs]
     (if-some [o1 (maybe-option p1 key?)]
       (let [b? (or (nil? p2)
                    (is-option? p2))]
         (recur (assoc! options
                        o1 (if b? true p2))
                (if b?
                  (if (nil? p2)
                    more (cons p2 more)) more)))
       (vector (c/ps! options)
               (if (= "--" p1)
                 (if p2 (cons p2 more)) args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- read-data
  "Read user input: windows has \\r\\n linux has \\n."
  ^String [in]
  (let [[ms bf]
        (loop [c (rdr in)
               bf (s/sbf<>)]
          (let [m (cond
                    (or (== c -1) (== c 4)) #{:quit :break}
                    (== c (int \newline)) #{:break}
                    (or (== c (int \return))
                        (== c 27)
                        (== c (int \backspace))) nil
                    :else (c/do#nil (s/sbf+ bf (char c))))]
            (if (c/in? m :break)
              [m bf]
              (recur (rdr in) bf))))]
    (if-not
      (c/in? ms :quit) (s/strim (str bf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-answer
  "Process the answer, returning the next question."
  [^Writer cout {:keys [result id
                        must?
                        default next] :as cmdQ} props answer]
  (if (nil? answer)
    (c/do#nil (.write cout "\n"))
    (let [rc (s/stror answer default)]
      (or (cond (and must?
                     (s/nichts? rc))
                id ;no answer, loop try again
                (keyword? result)
                (do (swap! props
                           #(assoc % result rc)) next)
                (fn? result)
                (let [[nxt p]
                      (result rc @props)]
                  (reset! props p)
                  (or nxt ::caio!!))) ::caio!!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-QQ
  "Pop the question."
  [^Writer cout ^Reader cin {:keys [question must?
                                    choices default] :as cmdQ} props]
  (.write cout (str question (if must? "*") "? "))
  ;; choices ?
  (if (s/hgl? choices)
    (.write cout (str "[" choices "]")))
  ;; defaults ?
  (if (s/hgl? default)
    (.write cout (str "(" default ")")))
  (doto cout (.write " ") .flush)
  ;; get the input from user
  ;; return the next question, :end ends it
  (->> (read-data cin)
       (on-answer cout cmdQ props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-Q
  "Pop the question"
  [cout cin cmdQ props]
  (if cmdQ (pop-QQ cout cin cmdQ props) ::caio!!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cycle-Q
  "Cycle through the questions."
  [cout cin cmdQNs start props]
  (loop [rc (pop-Q cout cin (cmdQNs start) props)]
    (cond (nil? rc) {}
          (= ::caio!! rc) @props
          :else (recur (pop-Q cout cin (cmdQNs rc) props)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn termio
  "Prompt a sequence of questions via console."
  [cmdQs question1]
  {:pre [(map? cmdQs)]}
  (let [cout (-> System/out
                 BufferedOutputStream.
                 OutputStreamWriter.)
        func (->> System/in
                  InputStreamReader.
                  (partial cycle-Q cout))]
    (.write cout (str ">>> Press "
                      "<ctrl-c> or <ctrl-d>"
                      "<Enter> to cancel...\n"))
    (-> (reduce #(update-in %1
                            [%2]
                            assoc :id %2)
                cmdQs (keys cmdQs))
        (func question1 (atom {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

