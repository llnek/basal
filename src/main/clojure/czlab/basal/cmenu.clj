;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.cmenu

  "Text menu interactions for consoles."

  (:require [clojure.string :as cs]
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
(c/defmacro- rdr

  "Read from the reader."
  [r]

  `(.read ~(with-meta r {:tag 'Reader})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-option?

  "If this is an option?"
  [option]

  (and (string? option)
       (not (.equals "--" option))
       (or (cs/starts-with? option "--")
           (cs/starts-with? option "-"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-option

  "Maybe get the actual option?"
  [option key?]

  (if (is-option? option)
    (c/if-some+
      [s (cs/replace option
                     #"^(-|/)+" "")] (if key? (keyword s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-options

  "Parse command line, returning options and args.
  e.g.  --a b -c d -e f g
        =>
        [{:a \"b\" :c \"d\" :e \"f\"} '(\"g\")]"

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
       (vector (c/persist! options)
               (if (.equals "--" p1)
                 (if p2 (cons p2 more)) args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- read-data

  "Read user input: windows has \\r\\n linux has \\n."
  ^String [in]

  (let [[ms bf]
        (loop [c (rdr in)
               bf (c/sbf<>)]
          (let [m (cond (or (== c 4)
                            (== c -1))
                        #{:quit :break}
                        (== c (int \newline))
                        #{:break}
                        (or (== c (int \return))
                            (== c 27)
                            (== c (int \backspace)))
                        nil
                        :else
                        (c/do#nil (c/sbf+ bf (char c))))]
            (if (c/in? m :break)
              [m bf]
              (recur (rdr in) bf))))]
    (if-not
      (c/in? ms :quit) (c/strim (str bf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-answer

  "Process the answer, returning the next question."
  [^Writer cout answer props {:keys [result id
                                     must? default next]}]

  (if (nil? answer)
    (c/do#nil (.write cout "\n"))
    (let [rc (c/stror answer default)]
      (cond (and must?
                 (c/nichts? rc))
            id ;no answer, loop try again

            (keyword? result)
            (do (swap! props assoc result rc) next)

            (fn? result)
            (let [[nxt p]
                  (result rc @props)]
              (reset! props p)
              (or nxt ::caio!!))

            :else ::caio!!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popQ

  "Pop the question."
  [^Writer cout ^Reader cin props {:as Q
                                   :keys [must? choices
                                          default question]}]

  (if (nil? Q)
    ::caio!!
    (do (.write cout (str question (if must? "*") "? "))
        (if (c/hgl? choices) (.write cout (str "[" choices "]")))
        (if (c/hgl? default) (.write cout (str "(" default ")")))
        (doto cout (.write " ") .flush)
        ;; get the input from user, return the next question
        (-> (read-data cin) (on-answer cout props Q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cycleQ

  "Cycle through the questions."
  [cout cin cmdQs start props]

  (loop [rc (popQ cout cin props (cmdQs start))]
    (cond (nil? rc) {}
          (= ::caio!! rc) @props
          :else (recur (popQ cout cin props (cmdQs rc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cmenu

  "A console menu, prompting a sequence of questions via console."
  [cmdQs q1]
  {:pre [(map? cmdQs)]}

  (let [cout (-> System/out
                 BufferedOutputStream. OutputStreamWriter.)
        func (->> System/in InputStreamReader. (partial cycleQ cout))]
    (.write cout (str ">>> Press "
                      "<ctrl-c> or <ctrl-d>"
                      "<Enter> to cancel...\n"))
    (-> #(update-in %1 [%2] assoc :id %2)
        (reduce cmdQs (keys cmdQs)) (func q1 (atom {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_
(def sample
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

