;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns
  ^{:doc "Access to a windows style .ini file."
    :author "Kenneth Leung"}

  czlab.basal.ini

  (:require [czlab.basal
             [util :as u]
             [core :as c]]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [flatland.ordered.map])

  (:import [java.net
            URL]
           [java.io
            File
            LineNumberReader]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/defmacro- throw-bad-key
  [k] `(u/throw-BadData "No such item %s." ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/defmacro- throw-bad-map
  [s] `(u/throw-BadData "No such heading %s" ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- throw-bad-ini
  [rdr] (u/throw-BadData "Bad ini line: %d."
                         (.getLineNumber ^LineNumberReader rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-section
  "Look for a section storing the
  actual name in meta."
  [rdr line ncmap]
  (c/if-some+ [s (c/strim-any line "[]" true)]
    (let [k (keyword (c/lcase s))]
      [(if-not (c/in? ncmap k)
         (assoc ncmap
                k (c/wm* (ordered-map)
                         (hash-map :name s))) ncmap) k])
    (throw-bad-ini rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-line
  "Parse a line (name=value) under a section."
  [rdr line ncmap section]
  (if-some [_ (get ncmap section)]
    (let [pos (cs/index-of line \=)
          nm (if (c/spos? pos)
               (c/strim (subs line 0 pos)) "")
          k (if (c/hgl? nm)
              (keyword (c/lcase nm)) (throw-bad-ini rdr))]
      [(update-in ncmap
                 [section]
                 assoc
                 k
                 [nm (c/strim (subs line (+ 1 pos)))]) section])
    (throw-bad-ini rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- eval-one-line
  "Parses a line in the file."
  [rdr line ncmap curSec]
  (let [ln (c/strim line)]
    (cond (or (c/nichts? ln)
              (cs/starts-with? ln "#"))
          ;skip comments
          [ncmap curSec]
          (c/matches? ln "^\\[.*\\]$")
          (maybe-section rdr ln ncmap)
          :else
          (maybe-line rdr ln ncmap curSec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getkv
  ^String [sects s k err]
  (let [sn (keyword (c/lcase s))
        kn (keyword (c/lcase k))
        mp (get sects sn)]
    (cond (nil? mp) (if err (throw-bad-map s))
          (nil? k) (if err (throw-bad-key k))
          (c/!in? mp kn) (if err (throw-bad-key k))
          :else (str (last (get mp kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol WinINI
  "Access to a windows ini file."
  (heading [_ sect] "Get the section for this heading.")
  (headings [_] "Get all headings.")
  (dbg-str [_] "Internal.")
  (str-value [_ sect prop]
             [_ sect prop dft] "Get the section then the field.")
  (long-value [_ sect prop]
              [_ sect prop dft] "Get the section then the field.")
  (int-value [_ sect prop]
             [_ sect prop dft] "Get the section then the field.")
  (bool-value [_ sect prop]
              [_ sect prop dft] "Get the section then the field.")
  (double-value [_ sect prop]
                [_ sect prop dft] "Get the section then the field."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- wrap->ini
  [M]
  (reify WinINI
    (headings [_]
      (c/preduce<set> #(conj! %1 (:name (meta %2))) (vals M)))
    (heading [_ sect]
      (let [sn (keyword (c/lcase sect))]
        (reduce #(assoc %1
                        (c/_1 %2) (c/_E %2))
                (ordered-map)
                (or (vals (get M sn)) []))))
    (str-value [_ sect prop]
      (str (getkv M sect prop true)))
    (str-value [_ sect prop dft]
      (if-some [rc (getkv M sect prop false)] rc dft))
    (long-value [_ sect prop dft]
      (if-some [rc (getkv M
                          sect prop false)] (c/s->long rc) dft))
    (long-value [_ sect prop]
      (c/s->long (getkv M sect prop true) 0))
    (int-value [_ sect prop dft]
      (if-some [rc (getkv M
                          sect prop false)] (c/s->int rc dft) dft))
    (int-value [_ sect prop]
      (c/s->int (getkv M sect prop true)))
    (double-value [_ sect prop dft]
      (if-some [rc (getkv M
                          sect prop false)] (c/s->double rc dft) dft))
    (double-value [_ sect prop]
      (c/s->double (getkv M sect prop true)))
    (bool-value [_ sect prop dft]
      (if-some [rc (getkv M
                          sect prop false)] (c/s->bool rc dft) dft))
    (bool-value [_ sect prop]
      (c/s->bool (getkv M sect prop true)))
    (dbg-str [_]
      (c/do-with-str
        [buf (c/sbf<>)]
        (doseq [v (vals M)]
          (c/sbf+ buf "[" (:name (meta v)) "]\n")
          (doseq [[x y] (vals v)]
            (c/sbf+ buf x "=" y "\n"))
          (c/sbf+ buf "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- parse-file
  ([fUrl]
   (parse-file fUrl "utf-8"))
  ([fUrl enc]
   (c/wo* [inp (-> (io/input-stream fUrl)
                   (io/reader :encoding
                              (c/stror enc "utf8"))
                   LineNumberReader. )]
     (loop [rdr inp line (.readLine rdr)
            [total curSec] [(sorted-map) nil]]
       (if (nil? line)
         (wrap->ini total)
         (recur rdr
                (.readLine rdr)
                (eval-one-line rdr line total curSec)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn win-ini<>
  "Parse a windows ini file."
  [in]
  (cond (c/is? File in)
        (win-ini<> (io/as-url in))
        (string? in)
        (if (c/hgl? in)
          (win-ini<> (io/file in)))
        (c/is? URL in)
        (parse-file in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


