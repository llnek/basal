;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "String helpers."
      :author "Kenneth Leung"}

  czlab.basal.str

  (:require [czlab.basal.log :as log]
            [clojure.string :as cs]
            [czlab.basal.core :as bc])

  (:import [java.util Arrays Collection Iterator StringTokenizer]
           [java.net URLEncoder URLDecoder]
           [clojure.lang Keyword APersistentVector]
           [java.io
            Reader
            Writer
            File
            CharArrayWriter
            OutputStream
            OutputStreamWriter]
           [java.lang StringBuilder]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nichts?
  "Is string empty?"
  [s]
  (or (nil? s)
      (not (string? s))
      (.isEmpty ^String s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro hgl? "If string has length?" [s] `(not (nichts? ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn stror
  "If not s then s2"
  ^String
  [^String s ^String s2] (if (nichts? s) s2 s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn stror*
  "If not s then s2...etc"
  ^String
  [& args] (loop [[a & more] args]
             (if (or (hgl? a)
                     (empty? more))
               a
               (recur more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro lcase
  "Lowercase string safely"
  [s] `(str (some-> ~s clojure.string/lower-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ucase
  "Uppercase string safely"
  [s] `(str (some-> ~s clojure.string/upper-case)))

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
               (>= (->> (int (.charAt  src pos))
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

  ([s sep] (splitTokens s sep false))
  ([^String s ^String sep incSep?]
   (let [t (StringTokenizer.
             s sep (bc/bool! incSep?))]
     (loop [rc (transient [])]
       (if-not (.hasMoreTokens t)
         (bc/pcoll! rc)
         (recur (conj! rc (.nextToken t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn has?
  "If the char is inside the big str"
  [^String bigs arg]

  (let
    [rc
     (cond
       (or (nil? bigs)
           (nil? arg))
       false
       (string? arg)
       (>= (.indexOf bigs ^String arg) 0)
       (instance? Character arg)
       (int (.charValue ^Character arg))
       (integer? arg) (int arg))]
    (if (number? rc)
      (>= (.indexOf bigs (int rc)) 0) rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro embeds?
  "If sub-str is inside the big str"
  [bigs s] `(czlab.basal.str/has? ~bigs ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro hasNoCase?
  "If sub-str is inside the big str - ignore case"
  [bigs s]
  `(czlab.basal.str/has?
     (czlab.basal.str/lcase ~bigs) (czlab.basal.str/lcase ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn indexAny
  "If any one char is inside the big str, return the position"
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
           total 0 start 0]
      (let [pos (.indexOf bigs
                          s start)]
        (if (< pos 0)
          total
          (recur len
                 (inc total)
                 (+ pos len))))) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn countChar
  "Count the times this char appears in the big str"
  [bigs ch]
  (reduce #(if (= ch %2) (inc %1) %1) 0 (bc/charsit bigs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sname
  "Safely get the name of this object"
  [n]
  `(str (some-> ~n name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nsb
  "Empty string if obj is null, or obj.toString"
  ^String
  [obj]
  (if (keyword? obj) (name obj) (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strKW
  "Stringify a keyword - no leading colon"
  ^String [k] (cs/replace (str k) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toKW
  "Concatenate all args and return it as a keyword"
  ^Keyword
  [& args] (if-not (empty? args)
             (keyword (apply str args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nsn
  "(null) if obj is null, or obj.toString"
  ^String [obj] (if (nil? obj) "(null)" (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn matchChar?
  "If this char is inside this set of chars"
  [ch setOfChars]
  (if (set? setOfChars) (contains? setOfChars ch) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eq?
  "If these 2 strings are the same"
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
                   (.toCharArray b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro strim "Safely trim this string"
  [s] `(str (some-> ~s clojure.string/trim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strimAny
  "Strip source string of these unwanted chars"
  {:tag String}

  ([^String src ^String unwantedChars whitespace?]
   (let [s (-> (if whitespace? (strim src) src)
               (triml unwantedChars)
               (trimr unwantedChars))]
     (if whitespace? (strim s) s)))
  ([src unwantedChars] (strimAny src unwantedChars false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addDelim!
  "Append to a string-builder, optionally
   inserting a delimiter if the buffer is not empty"
  ^StringBuilder
  [^StringBuilder buf ^String delim ^String item]

  (bc/do-with [buf buf]
    (when item
      (if (and (> (.length buf) 0)
               (hgl? delim)) (.append buf delim))
      (.append buf item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn splunk
  "Split a large string into chunks,
  each chunk having a specific length"
  ^APersistentVector
  [^String largeString chunkLength]

  (if (and (hgl? largeString)
           (bc/snneg? chunkLength))
    (into [] (map #(cs/join "" %1)
                  (partition-all chunkLength largeString)))
    []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasicAny?
  "If bigs contains any one of these strs - ignore case"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? bigs))
    false
    (let [lc (lcase bigs)]
      (true?
        (some #(>= (.indexOf lc (lcase %)) 0) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasAny?
  "If bigs contains any one of these strs"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? bigs))
    false
    (true? (some #(>= (.indexOf bigs ^String %) 0) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ewicAny?
  "If bigs endsWith any one of the strs, no-case"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? bigs))
    false
    (let [lc (lcase bigs)]
      (true?
        (some #(.endsWith lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ewAny?
  "If bigs endsWith any one of the strs"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? bigs))
    false
    (true?
      (some #(.endsWith bigs ^String %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swicAny?
  "If bigs startWith any one of the strs - no case"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? bigs))
    false
    (let [lc (lcase bigs)]
      (true?
        (some #(.startsWith lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn swAny?
  "If bigs startWith any one of the strs"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? bigs))
    false
    (true?
      (some #(.startsWith bigs ^String %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro eqic?
  "String.equalIgnoreCase()"
  [src other]
  (let [^String ss src
        ^String oo other]
    `(.equalsIgnoreCase ~ss ~oo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eqicAny?
  "String.equalIgnoreCase() on any one of the strs"
  [^String src substrs]
  {:pre [(sequential? substrs)]}

  (if (or (empty? substrs)
          (nichts? src))
    false
    (let [lc (lcase src)]
      (true? (some #(= lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eqAny?
  "If String.equals() on any one of the strs"
  [^String src substrs]
  {:pre [(sequential? substrs)]}

  (if (empty? substrs)
    false
    (true? (some #(= src %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn wrapped?
  "If src string starts with head and ends with tail"
  [^String src ^String head ^String tail]

  (if (and (hgl? src)
           (hgl? head) (hgl? tail))
    (and (.startsWith src head)
         (.endsWith src tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro strbf<>
  "StringBuilder.new"
  ([] `(strbf<> nil))
  ([s] `(StringBuilder. (str ~s))))

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
  {:pre [(number? len)]}

  (if (or (<= len 0)
          (nichts? src))
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
  {:pre [(number? len)]}

  (if (or (<= len 0)
          (nichts? src))
    ""
    (if (< (.length src) len)
      src
      (.substring src 0 len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn drophead
  "Drop leftmost len characters of a String"
  ^String
  [^String src len]
  {:pre [(number? len)]}

  (if (or (<= len 0)
          (nichts? src))
    ""
    (if (< (.length src) len)
      ""
      (.substring src len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn droptail
  "Drop rightmost len characters of a String"
  ^String
  [^String src len]
  {:pre [(number? len)]}

  (if (or (<= len 0)
          (nichts? src))
    ""
    (let [n (.length src)]
      (if (< n len)
        ""
        (.substring src 0 (- n len))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sformat
  ""
  [fmt & args]
  (String/format ^String fmt (into-array Object args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn urlEncode
  "HTML encode"
  {:tag String}

  ([s] (urlEncode s "utf-8"))
  ([^String s enc]
   (if (hgl? s)
     (URLEncoder/encode
       s (bc/strCharset enc)) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn urlDecode
  "HTML decode"
  {:tag String}

  ([s] (urlDecode s "utf8"))
  ([^String s enc]
   (if (hgl? s)
     (-> s;;(cs/replace s "+" (urlEncode "+" enc))
         (URLDecoder/decode (bc/strCharset enc)))
     s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


