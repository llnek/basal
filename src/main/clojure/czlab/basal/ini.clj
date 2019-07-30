;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Access to a windows style .ini file."
      :author "Kenneth Leung"}

  czlab.basal.ini

  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [czlab.basal.util :as u]
            [czlab.basal.str :as s]
            [czlab.basal.core :as c])

  (:use [flatland.ordered.map])

  (:import [java.net
            URL]
           [java.io
            File
            LineNumberReader]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private throw-bad-key
  [k] `(u/throw-BadData "No such item %s" ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private throw-bad-map
  [s] `(u/throw-BadData "No such heading %s" ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- throw-bad-ini [rdr]
  (u/throw-BadData "Bad ini line: %s"
                   (.getLineNumber ^LineNumberReader rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-section
  "Look for a section storing the
  actual name in meta."
  [rdr ncmap line]
  (c/if-some+ [s (s/strim-any line "[]" true)]
    (c/do-with [k (keyword (s/lcase s))]
      (if-not (contains? @ncmap k)
        (c/assoc!! ncmap
                   k
                   (with-meta (ordered-map) {:name s}))))
    ;else
    (throw-bad-ini rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-line
  "Parse a line (name=value) under a section."
  [rdr ncmap section line]
  (if-some [_ (get @ncmap section)]
    (let [pos (cs/index-of line \=)
          nm (if (c/spos? pos)
               (s/strim (.substring ^String
                                    line 0 pos)) "")
          k (if (s/hgl? nm)
              (keyword (s/lcase nm)) (throw-bad-ini rdr))]
      (swap! ncmap
             #(update-in %
                         [section]
                         assoc
                         k
                         [nm (s/strim (.substring ^String
                                                  line (+ 1 pos)))]))
      section)
    ;else
    (throw-bad-ini rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- eval-one-line
  "Parses a line in the file."
  [rdr ncmap cursec ^String line]
  (let [ln (s/strim line)]
    (cond (or (s/nichts? ln)
              (cs/starts-with? ln "#"))
          ;comment
          cursec
          (.matches ln "^\\[.*\\]$")
          (maybe-section rdr ncmap ln)
          :else
          (maybe-line rdr ncmap cursec ln))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getkv ^String [sects s k err]
  (let [sn (keyword (s/lcase s))
        kn (keyword (s/lcase k))
        mp (get @sects sn)]
    (cond (nil? mp) (if err (throw-bad-map s))
          (nil? k) (if err (throw-bad-key k))
          (c/!in? mp kn) (if err (throw-bad-key k))
          :else (str (last (get mp kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn headings
  "List all section names." [ini]
  (c/preduce<set> #(conj! %1 (:name (meta %2))) (vals @ini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn heading "" [ini sect]
  (let [sn (keyword (s/lcase sect))]
    (reduce #(assoc %1
                    (first %2) (last %2))
            (ordered-map)
            (or (vals (get @ini sn)) []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn str-value
  ""
  ([ini sect prop] (str (getkv ini sect prop true)))
  ([ini sect prop dft]
   (if-some [rc (getkv ini sect prop false)] rc dft)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn long-value
  ""
  ([ini sect prop dft]
   (if-some [rc (getkv ini
                       sect prop false)] (c/s->long rc) dft))
  ([ini sect prop]
   (c/s->long (getkv ini sect prop true) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn int-value
  ""
  ([ini sect prop dft]
   (if-some [rc (getkv ini
                       sect prop false)] (c/s->int rc dft) dft))
  ([ini sect prop] (c/s->int (getkv ini sect prop true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn double-value
  ""
  ([ini sect prop dft]
   (if-some [rc (getkv ini
                       sect prop false)] (c/s->double rc dft) dft))
  ([ini sect prop]
   (c/s->double (getkv ini sect prop true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bool-value
  ""
  ([ini sect prop dft]
   (if-some [rc (getkv ini
                       sect prop false)] (c/s->bool rc dft) dft))
  ([ini sect prop] (c/s->bool (getkv ini sect prop true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dbg-show
  "Dump ini as string." [ini]
  (let [buf (s/sbf<>)]
    (doseq [v (vals @ini)]
      (s/sbf+ buf "[" (:name (meta v)) "]\n")
      (doseq [[x y] (vals v)]
        (s/sbf+ buf x "=" y "\n"))
      (s/sbf+ buf "\n"))
    (println (str buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- parse-file
  ([fUrl] (parse-file fUrl "utf-8"))
  ([fUrl enc]
   (c/wo* [inp (-> (io/input-stream fUrl)
                   (io/reader :encoding
                              (s/stror enc "utf8"))
                   LineNumberReader. )]
     (loop [total (atom (sorted-map))
            rdr inp
            line (.readLine rdr)
            curSec nil]
       (if (nil? line)
         total
         (recur total
                rdr
                (.readLine rdr)
                (eval-one-line rdr
                               total curSec line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn win-ini<>
  "Parse a ini conf file."
  [f]
  (cond (c/is? File f) (win-ini<> (io/as-url f))
        (string? f) (if (s/hgl? f)
                      (win-ini<> (io/file f)))
        (c/is? URL f) (parse-file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


