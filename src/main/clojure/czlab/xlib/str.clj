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

(ns ^{:doc "String helpers."
      :author "Kenneth Leung" }

  czlab.xlib.str

  (:require
    [czlab.xlib.logging :as log]
    [clojure.string :as cs])

  (:import
    [java.util Arrays Collection Iterator StringTokenizer]
    [java.net URLEncoder URLDecoder]
    [clojure.lang
     Keyword
     APersistentVector]
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
(defmacro hgl?

  "true if string *has good length* - not empty or nil"
  [s]

  `(not (empty? ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro stror

  "If not s then s2"
  {:tag String}
  [s s2]

  `(let [s# ~s] (if (empty? s#) ~s2 s#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro lcase

  "Lowercase string, handling nil"
  {:tag String}
  [s]

  `(if-some [s# ~s] (cs/lower-case s#) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ucase

  "Uppercase string, handling nil"
  {:tag String}
  [s]

  `(if-some [s# ~s] (cs/upper-case s#) ""))

;;#^"[Ljava.lang.Class;"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn triml

  "Get rid of unwanted chars from left"
  ^String
  [^String src ^String unwantedChars]

  (if (and (hgl? unwantedChars)
           (hgl? src))
    (loop [len (.length src)
           pos 0]
      (if (and (< pos len)
               (>= (->> (int (.charAt src pos))
                        (.indexOf unwantedChars)) 0))
        (recur len (inc pos))
        (.substring src pos)))
    src))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn trimr

  "Get rid of unwanted chars from right"
  ^String
  [^String src ^String unwantedChars]

  (if (and (hgl? unwantedChars)
           (hgl? src))
    (loop [pos (.length src)]
      (if (and (> pos 0)
               (>= (->> (int (.charAt src (dec pos)))
                        (.indexOf unwantedChars)) 0))
        (recur (dec pos))
        (.substring src 0 pos)))
    src))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn splitTokens

  "String tokenizer"
  {:tag APersistentVector}

  ([^String s ^String sep]
   (splitTokens s sep false))

  ([^String s ^String sep incSep?]
   (let [t (StringTokenizer. s
                             sep
                             (boolean incSep?))]
     (loop [rc (transient [])]
       (if-not (.hasMoreTokens t)
         (persistent! rc)
         (recur (conj! rc (.nextToken t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro embeds?

  "true if sub-str is inside the big str"
  [^String bigs ^String s]

  `(>= (.indexOf (str ~bigs) (str ~s)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro hasNoCase?

  "true if sub-str is inside the big str - ignore case"
  [bigs s]

  `(embeds? (lcase ~bigs) (lcase ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro has?

  "true if the char is inside the big str"
  [^String bigs ^Character ch]

  (let [c (int (.charValue ch))]
    `(>= (.indexOf  ~bigs ~c) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn indexAny

  "if any one char is inside the str, return the position"
  [^String bigs ^String chStr]

  (if (and (hgl? chStr)
           (hgl? bigs))
    (let [rc (some #(let [x (.indexOf bigs (int %))]
                      (if (< x 0) nil x))
                   (.toCharArray chStr))]
      (if (nil? rc) -1 (int rc)))
    -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn countStr

  "Count the times the sub-str appears in the big str"
  [^String bigs ^String s]

  (if (and (hgl? bigs)
           (hgl? s))
    (loop [len (.length s)
           total 0
           start 0]
      (let [pos (.indexOf bigs s start)]
        (if (< pos 0)
          total
          (recur len (inc total) (+ pos len)))))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn countChar

  "Count the times this char appears in the big str"
  [^String bigs ^Character ch]

  (reduce
    #(if (= ch %2) (inc %1) %1)
    0
    (.toCharArray bigs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sname

  "Safely get the name of this object"
  {:tag String}
  [n]

  `(when-some [n# ~n] (name n#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nsb

  "Empty string if obj is null, or obj.toString"
  ^String
  [obj]

  (if (keyword? obj) (name obj) (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toKW

  "Concatenate all args and return it as a keyword"
  ^Keyword
  [& args]

  (if-not (empty? args)
    (keyword (cs/join "/" args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nsn

  "(null) if obj is null, or obj.toString"
  ^String
  [obj]

  (if (nil? obj) "(null)" (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn matchChar?

  "true if this char is inside this set of chars"
  [ch setOfChars]

  (if (set? setOfChars) (contains? setOfChars ch) false))

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

    (not= (.length a)
          (.length b))
    false

    :else
    (Arrays/equals (.toCharArray a)
                   (.toCharArray b)) ))

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

  (cs/trim (str s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strimAny

  "Strip source string of these unwanted chars"
  {:tag String}

  ([^String src ^String unwantedChars]
   (strimAny src unwantedChars false))

  ([^String src ^String unwantedChars whitespace?]
   (let [s (-> (if whitespace? (strim src) src)
               (triml unwantedChars)
               (trimr unwantedChars))]
     (if whitespace? (strim s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addDelim!

  "Append to a string-builder, optionally
   inserting a delimiter if the buffer is not empty"
  ^StringBuilder
  [^StringBuilder buf ^String delim ^String item]

  (when (some? item)
    (when (and (> (.length buf) 0)
               (hgl? delim))
      (.append buf delim))
    (.append buf item))
  buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn splunk

  "Split a large string into chunks, each chunk having a specific length"
  ^APersistentVector
  [^String largeString chunkLength]

  (if (and (hgl? largeString)
           (some? chunkLength)
           (pos? chunkLength))
    (into [] (map #(cs/join "" %1)
                  (partition-all chunkLength largeString)))
    []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasicAny?

  "true if bigs contains any one of these strs - ignore case"
  [^String bigs substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? bigs))
    false
    (let [lc (lcase bigs)]
      (true?
        (some #(>= (.indexOf lc (lcase %)) 0)
              substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasAny?

  "true if bigs contains any one of these strs"
  [^String bigs substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? bigs))
    false
    (true?
      (some #(>= (.indexOf bigs ^String %) 0) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ewicAny?

  "true if bigs endsWith any one of the strs - ignore-case"
  [^String bigs substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? bigs))
    false
    (let [lc (lcase bigs)]
      (true?
        (some #(.endsWith lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ewAny?

  "true if bigs endsWith any one of the strs"
  [^String bigs substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? bigs))
    false
    (true?
      (some #(.endsWith bigs ^String %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swicAny?

  "true if bigs startWith any one of the strs - ignore case"
  [^String bigs substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? bigs))
    false
    (let [lc (lcase bigs)]
      (true?
        (some #(.startsWith lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swAny?

  "true if bigs startWith any one of the strs"
  [^String bigs substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? bigs))
    false
    (true?
      (some #(.startsWith bigs ^String %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro eqic?

  "String.equalIgnoreCase()"
  [^String src ^String other]

  `(.equalsIgnoreCase ~src ~other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eqicAny?

  "String.equalIgnoreCase() on any one of the strs"
  [^String src substrs]
  {:pre [(coll? substrs)]}

  (if (or (empty? substrs)
          (nil? src))
    false
    (let [lc (lcase src)]
      (true?
        (some #(= lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eqAny?

  "true if String.equals() on any one of the strs"
  [^String src substrs]
  {:pre [(coll? substrs)]}

  (if (empty? substrs)
    false
    (true?
      (some #(= src %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro strbf<>

  "StringBuilder.new"
  {:tag StringBuilder}

  ([] `(StringBuilder.))
  ([s] `(StringBuilder. ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn str<>

  "Make a string of certain length"
  ^String
  [cnt ^Character ch]

  (let [buf (strbf<>)]
    (dotimes [n cnt]
      (.append buf ch))
    (.toString buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rights

  "Get the rightmost len characters of a String"
  ^String
  [^String src len]

  (if (or (<= len 0)
          (empty? src))
    ""
    (if (< (.length src) len)
      src
      (.substring src (- (.length src) len)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn lefts

  "Get the leftmost len characters of a String"
  ^String
  [^String src len]

  (if (or (<= len 0)
          (empty? src))
    ""
    (if (< (.length src) len)
      src
      (.substring src 0 len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sformat

  ""
  [^String fmt & args]

  (String/format fmt (into-array Object args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn urlEncode

  "HTML encode"
  {:tag String}

  ([^String s] (urlEncode s "utf-8"))
  ([^String s enc]
   (if (hgl? s)
     (URLEncoder/encode s (stror enc "utf-8"))
     s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn urlDecode

  "HTML decode"
  {:tag String}

  ([^String s] (urlDecode s "utf8"))
  ([^String s enc]
   (if (hgl? s)
     (-> s;;(cs/replace s "+" (urlEncode "+" enc))
         (URLDecoder/decode (stror enc "utf-8")))
     s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


