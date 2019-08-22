;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "String helpers."
      :author "Kenneth Leung"}

  czlab.basal.str

  (:refer-clojure :exclude [shuffle])

  (:require [clojure.string :as cs]
            [czlab.basal.core :as c])

  (:import [java.lang
            StringBuilder]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sreduce<>
  "Reduce with a string-builder, returning a string."
  [f c] `(str (reduce ~f (StringBuilder.) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt
  "Like format." ^String [f & args] (apply format f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf<>
  "StringBuilder.new"
  {:tag StringBuilder}
  ([]
   (sbf<> nil))
  ([s]
   (StringBuilder. (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf-join
  "Append to a string-builder, optionally
   inserting a delimiter if the buffer is not empty."
  ^StringBuilder
  [^StringBuilder buf sep item]
  (when item
    (if (and (c/!nil? sep)
             (pos? (.length buf)))
      (.append buf sep))
    (.append buf item))
  buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbfz
  "Length of the string-buffer." [b] (.length ^StringBuilder b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf+
  "StringBuilder concat."
  [buf & args]
  (doseq [x args]
    (.append ^StringBuilder buf x)) buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nichts?
  "Is string empty?"
  [s]
  (or (nil? s)
      (not (string? s))
      (.isEmpty ^String s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hgl?
  "If string has length?" [s] (and (string? s)
                                   (not (.isEmpty ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn stror
  "If not s then s2"
  ^String
  [s s2] (if (nichts? s) s2 s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXXstror*
  "If not s then s2...etc"
  ^String
  [& args] (loop [[a & more] args]
             (if (or (hgl? a) (empty? more)) a (recur more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro stror*
  "If not s then s2...etc"
  [& args]
  (let [[a & xs] args]
    (if (empty? xs)
      `(stror nil ~a)
      `(stror ~a (stror* ~@xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lcase
  "Lowercase string safely."
  ^String [s] (str (some-> s clojure.string/lower-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ucase
  "Uppercase string safely."
  ^String [s] (str (some-> s clojure.string/upper-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#^"[Ljava.lang.Class;"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triml
  "Get rid of unwanted chars from left."
  ^String
  [^String src ^String unwantedChars]
  (if (and (hgl? src)
           (hgl? unwantedChars))
    (loop [len (.length src) pos 0]
      (if-not (and (< pos len)
                   (cs/index-of unwantedChars
                                (.charAt src pos)))
        (subs src pos)
        (recur len (+ 1 pos))))
    src))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trimr
  "Get rid of unwanted chars from right."
  ^String
  [^String src ^String unwantedChars]
  (if (and (hgl? src)
           (hgl? unwantedChars))
    (loop [pos (.length src)]
      (if-not (and (pos? pos)
                   (cs/index-of unwantedChars
                                (.charAt src (- pos 1))))
        (subs src 0 pos)
        (recur (- pos 1))))
    src))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has?
  "If the char is inside the big str?"
  [^String bigs arg]
  (let [rc (cond (or (nil? arg)
                     (nil? bigs)) false
                 (integer? arg) (int arg)
                 (string? arg) (number? (cs/index-of bigs arg))
                 (c/is? Character arg) (int (.charValue ^Character arg)))]
    (if (number? rc)
      (>= (.indexOf bigs (int rc)) 0) rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro embeds?
  "If sub-str is inside the big str?"
  [bigs s] `(czlab.basal.str/has? ~bigs ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro has-no-case?
  "If sub-str is inside the big str - ignore case?"
  [bigs s]
  `(czlab.basal.str/has?
     (czlab.basal.str/lcase ~bigs) (czlab.basal.str/lcase ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn index-any
  "If any one char is inside the big str, return the position."
  [^String bigs ^String chStr]
  (if (and (hgl? bigs)
           (hgl? chStr))
    (let [rc (some #(cs/index-of bigs %)
                   (.toCharArray chStr))] (if (nil? rc) -1 (int rc))) -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-str
  "Count the times the sub-str appears in the big str."
  [^String bigs ^String s]
  (if (and (hgl? s)
           (hgl? bigs))
    (loop [len (.length s)
           total 0 start 0]
      (let [pos (cs/index-of bigs s start)]
        (if (nil? pos)
          total
          (recur len
                 (+ 1 total)
                 (long (+ pos len)))))) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-char
  "Count the times this char appears in the big str."
  [bigs ch]
  (reduce #(if (= ch %2)
             (+ 1 %1) %1)
          0
          (.toCharArray ^String bigs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sname
  "Safely get the name of this object" [n] `(str (some-> ~n name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nsb
  "Empty string if obj is null, or obj.toString"
  ^String [obj] (if (keyword? obj) (name obj) (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kw->str
  "Stringify a keyword - no leading colon."
  ^String [k] (cs/replace (str k) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->kw
  "Concatenate all args and return it as a keyword"
  [& args] (if-not (empty? args)
             (keyword (apply str args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nsn
  "(null) if obj is null, or obj.toString"
  ^String [obj] (if (nil? obj) "(null)" (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match-char?
  "If this char is inside this set of chars?"
  [ch setOfChars]
  (if (set? setOfChars) (contains? setOfChars ch) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro strim
  "Safely trim this string." [s] `(str (some-> ~s clojure.string/trim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strim-any
  "Strip source string of these unwanted chars."
  {:tag String}

  ([src unwantedChars]
   (strim-any src unwantedChars false))

  ([^String src ^String unwantedChars whitespace?]
   (let [s (-> (if whitespace? (strim src) src)
               (triml unwantedChars)
               (trimr unwantedChars))]
     (if whitespace? (strim s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splunk
  "Split a large string into chunks,
  each chunk having a specific length."
  [^String largeString chunkLength]
  (if (and (hgl? largeString)
           (c/snneg? chunkLength))
    (mapv #(cs/join "" %1)
          (partition-all chunkLength largeString)) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasic-any?
  "If bigs contains any one of these strs - ignore case?"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (empty? substrs) (nichts? bigs))
    (let [lc (lcase bigs)]
      (true? (some #(number? (cs/index-of lc (lcase %))) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-any?
  "If bigs contains any one of these strs?"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(number? (cs/index-of bigs %)) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ewic-any?
  "If bigs endsWith any one of the strs, no-case?"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (nichts? bigs)
              (empty? substrs))
    (let [lc (lcase bigs)]
      (true? (some #(cs/ends-with? lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ew-any?
  "If bigs endsWith any one of the strs?"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (nichts? bigs)
          (empty? substrs))
    (true? (some #(cs/ends-with? bigs %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn swic-any?
  "If bigs startWith any one of the strs - no case?"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (nichts? bigs)
              (empty? substrs))
    (let [lc (lcase bigs)]
      (true? (some #(cs/starts-with? lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sw-any?
  "If bigs startWith any one of the strs?"
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(cs/starts-with? bigs %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eqic?
  "String.equalIgnoreCase()?"
  [src other]
  (let [^String ss src
        ^String oo other] `(.equalsIgnoreCase ~ss ~oo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eqic-any?
  "String.equalIgnoreCase() on any one of the strs?"
  [^String src substrs]
  {:pre [(sequential? substrs)]}
  (if-not (or (nichts? src)
              (empty? substrs))
    (let [lc (lcase src)]
      (true? (some #(= lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eq-any?
  "If String.equals() on any one of the strs?"
  [^String src substrs]
  {:pre [(sequential? substrs)]}
  (if-not (empty? substrs)
    (true? (some #(= src %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrapped?
  "If src string starts with head and ends with tail?"
  [^String src ^String head ^String tail]
  (if (and (hgl? src)
           (hgl? head) (hgl? tail))
    (and (cs/starts-with? src head) (cs/ends-with? src tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rights
  "Get the rightmost len characters of a String."
  ^String
  [^String src len]
  {:pre [(number? len)]}
  (if (or (<= len 0)
          (nichts? src))
    ""
    (if (< (.length src) len)
      src
      (subs src (- (.length src) len)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lefts
  "Get the leftmost len characters of a String."
  ^String
  [^String src len]
  {:pre [(number? len)]}
  (if (or (<= len 0)
          (nichts? src))
    ""
    (if (< (.length src) len)
      src
      (subs src 0 len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop-head
  "Drop leftmost len characters of a String."
  ^String
  [^String src len]
  {:pre [(number? len)]}
  (cond
    (nichts? src)
    ""
    (<= len 0)
    src
    :else
    (if (< (.length src) len)
      ""
      (subs src len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop-tail
  "Drop rightmost len characters of a String."
  ^String
  [^String src len]
  {:pre [(number? len)]}
  (cond
    (nichts? src)
    ""
    (<= len 0)
    src
    :else
    (let [n (.length src)]
      (if (< n len)
        ""
        (subs src 0 (- n len))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split
  "String.split."
  ([^String src ^String regex]
   (.split src regex))
  ([^String src ^String regex limit]
   (.split src regex (int limit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-str
  "String tokenizer."
  ([s sep]
   (split-str s sep false))
  ([s sep incSep?]
   (let [t (new java.util.StringTokenizer
                ^String s ^String sep (boolean incSep?))]
     (loop [rc (c/tvec*)]
       (if-not (.hasMoreTokens t)
         (c/ps! rc)
         (recur (conj! rc (.nextToken t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn shuffle
  "Shuffle characters in string." [s]
  (let [lst (java.util.ArrayList.)]
    (doseq [c (seq s)] (.add lst c))
    (java.util.Collections/shuffle lst)
    (loop [i 0
           SZ (.size lst)
           out (char-array (.size lst))]
      (if (>= i SZ)
        (String. out)
        (do (aset-char out
                       i
                       (.get lst i))
            (recur (+ 1 i) SZ out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn esc-xml
  "Escape XML special chars."
  [s]
  (cs/escape s {\& "&amp;"
                \> "&gt;"
                \< "&lt;"
                \" "&quot;"
                \' "&apos;"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


