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

(ns ^{:doc "String utilities."
      :author "Kenneth Leung" }

  czlab.xlib.str

  (:require
    [czlab.xlib.logging :as log]
    [clojure.string :as cs])

  (:import
    [java.util Arrays Collection Iterator StringTokenizer]
    [org.apache.commons.lang3 StringUtils]
    [java.io
     File
     CharArrayWriter
     OutputStream
     OutputStreamWriter
     Reader
     Writer]
    [java.lang StringBuilder]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro lcase "Lowercase string" [s] `(if-some [s# ~s] (cs/lower-case s#) ""))
(defmacro ucase "Uppercase string" [s] `(if-some [s# ~s] (cs/upper-case s#) ""))
(defmacro stror "If not s then s2" [s s2] `(let [s# ~s] (if (empty? s#) ~s2 s#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn triml

  "Get rid of unwanted chars from left"
  [^String src ^String s]

  (StringUtils/stripStart src s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn trimr

  "Get rid of unwanted chars from right"
  [^String src ^String s]

  (StringUtils/stripEnd src s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn splitTokens

  "String tokenizer"

  [^String s ^String sep incSep?]

  (let [t (StringTokenizer. s sep incSep?)]
    (loop [rc (transient [])]
      (if-not (.hasMoreTokens t)
        (persistent! rc)
        (recur (conj! rc (.nextToken t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasNoCase?

  "true if this sub-string is inside this bigger string"

  [^String bigs ^String s]

  (>= (.indexOf (lcase bigs) (lcase s)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn embeds?

  "true if this sub-string is inside this bigger string"

  [^String bigs ^String s]

  (>= (.indexOf bigs s) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn has?

  "true if this character is inside this string"

  [^String bigs ^Character ch]

  (>= (.indexOf bigs (int (.charValue ch))) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sname

  "Safely get the name of this object"

  ^String
  [n]

  `(when-some [n# ~n] (name n#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nsb

  "Empty string if obj is null, or obj.toString"

  ^String
  [^Object obj]

  (if (keyword? obj)
    (name obj)
    (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toKW

  "Concatenate all args and return it as a keyword"

  [& args]

  (keyword (cs/join "/" args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nsn

  "(null) if obj is null, or obj.toString"

  ^String
  [^Object obj]

  (if (nil? obj) "(null)" (.toString obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn same?

  "true if these 2 strings are the same"

  [^String a ^String b]

  (cond
    (and (nil? a) (nil? b))
    true

    (or (nil? a) (nil? b))
    false

    (not= (.length a) (.length b))
    false

    :else
    (Arrays/equals (.toCharArray a) (.toCharArray b)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro hgl?

  "true if this string is not empty"

  [s]

  `(not (empty? ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro nichts?

  "true if this string is empty"

  [s]

  `(empty? ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strim

  "Safely trim this string - handles null"

  ^String
  [s]

  (if (nil? s) "" (cs/trim (nsb s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strimAny

  "Strip source string of these unwanted chars"

  [^String src ^String unwantedChars & [whitespace?]]

  (let [ s (-> (if whitespace? (strim src) src)
               (StringUtils/strip unwantedChars))]
    (if whitespace? (strim s) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addDelim!

  "Append to a string-builder, optionally
   inserting a delimiter if the buffer is not empty"

  ^StringBuilder
  [^StringBuilder buf ^String delim ^String item]

  (when (some? item)
    (when (and (> (.length buf) 0)
               (some? delim))
      (.append buf delim))
    (.append buf item))
  buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn splunk

  "Split a large string into chunks, each chunk having a specific length"

  [^String largeString chunkLength]

  (if (nil? largeString)
    []
    (loop [ret (transient [])
           src largeString ]
      (if (<= (.length src) chunkLength)
        (persistent! (if (> (.length src) 0)
                       (conj! ret src)
                       ret))
        (recur (conj! ret (.substring src 0 chunkLength))
               (.substring src chunkLength))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasicAny?

  "Match against a list of possible args. (ignoring case)"

  [^String src substrs]

  (if (or (empty? substrs)
          (nil? src))
    false
    (let [lc (lcase src)]
      (true? (some #(>= (.indexOf lc (lcase %)) 0) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasAny? "Returns true if src contains one of these substrings"

  [^String src substrs]

  (if (or (empty? substrs)
          (nil? src))
    false
    (true? (some #(>= (.indexOf src ^String %) 0) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ewicAny? "Tests endsWith() no-case, looping through
                the list of possible suffixes"

  [^String src suxs]

  (if (or (empty? suxs)
          (nil? src))
    false
    (let [lc (lcase src)]
      (true? (some #(.endsWith lc (lcase %)) suxs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ewAny? "Tests endsWith(), looping through
              the list of possible suffixes"

  [^String src suxs]

  (if (or (empty? suxs)
          (nil? src))
    false
    (true? (some #(.endsWith src ^String %) suxs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swicAny? "Tests startWith() no-case, looping through
                the list of possible prefixes"

  [^String src pfxs]

  (if (or (empty? pfxs)
          (nil? src))
    false
    (let [lc (lcase src)]
      (true? (some #(.startsWith lc (lcase %)) pfxs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swAny? "Tests startWith(), looping through
              the list of possible prefixes"

  [^String src pfxs]

  (if (or (empty? pfxs)
          (nil? src))
    false
    (true? (some #(.startsWith src ^String %) pfxs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eqicAny? "Tests String.equals() against a
                list of possible args. (ignore-case)"

  [^String src strs]

  (if (or (empty? strs)
          (nil? src))
    false
    (let [lc (lcase src)]
      (true? (some #(.equals lc (lcase %)) strs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eqAny? "Tests String.equals() against a list of possible args"

  [^String src strs]

  (if (or (empty? strs)
          (nil? src))
    false
    (true? (some #(.equals src %) strs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn makeString "Make a string of certain length"

  ^String
  [^Character ch cnt]

  (let [buf (StringBuilder.) ]
    (dotimes [n cnt]
      (.append buf ch))
    (.toString buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rights "Gets the rightmost len characters of a String"

  ^String
  [^String src len]

  (if (or (<= len 0)
          (nil? src))
    ""
    (StringUtils/right src (int len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn lefts "Gets the leftmost len characters of a String"

  ^String
  [^String src len]

  (if (or (<= len 0)
          (nil? src))
    ""
    (StringUtils/left src (int len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn sformat "Format a string using Java String format syntax"

  [^String fmt & args]

  (String/format fmt (into-array Object args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


