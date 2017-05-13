;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Access to a win32 .ini file."
      :author "Kenneth Leung"}

  czlab.basal.ini

  (:require [czlab.basal.io :refer [fileRead?]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [flatland.ordered.map]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.jasal Win32Conf]
           [java.net URL]
           [java.io
            File
            PrintStream
            IOException
            InputStreamReader
            LineNumberReader]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwBadIni
  "" [^LineNumberReader rdr]
  (throwBadData "Bad ini line: %s" (.getLineNumber rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private throwBadKey
  "" [k] `(throwBadData "No such item %s" ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private throwBadMap
  "" [s] `(throwBadData "No such heading %s" ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeSection
  "Look for a section storing the
  actual name in meta" [rdr ncmap line]

  (if-some+ [s (strimAny line "[]" true)]
    (let [k (keyword (lcase s))]
      (if-not (contains? @ncmap k)
        (->> (assoc @ncmap
                    k
                    (with-meta (ordered-map) {:name s}))
             (reset! ncmap)))
      k)
    (throwBadIni rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeLine
  "Parse a line (name=value) under a section"
  [rdr ncmap section ^String line]

  (if-some [kvs (get @ncmap section)]
    (let [pos (.indexOf line (int \=))
          nm (if (> pos 0)
               (strim (.substring line 0 pos)) "")]
      (if (nichts? nm) (throwBadIni rdr))
      (let [k (keyword (lcase nm))]
        (->> (assoc kvs
                    k
                    [nm  (strim (.substring line
                                            (inc pos)))])
             (swap! ncmap assoc section)))
      section)
    (throwBadIni rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evalOneLine
  "Parses a line in the file"
  [rdr ncmap curSec ^String line]

  (let [ln (strim line)]
    (cond
      (or (nichts? ln)
          (.startsWith ln "#"))
      curSec

      (.matches ln "^\\[.*\\]$")
      (maybeSection rdr ncmap ln)

      :else
      (maybeLine rdr ncmap curSec ln))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getKV
  "" ^String [sects s k err]

  (let [sn (keyword (lcase s))
        kn (keyword (lcase k))
        mp (get sects sn)]
    (cond
      (nil? mp) (if err (throwBadMap s))
      (nil? k) (if err (throwBadKey k))
      (notin? mp kn) (if err (throwBadKey k))
      :else (str (last (get mp kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- makeWinini "" [sects]

  (reify Win32Conf

    (headings [_]
      (preduce<set>
        #(conj! %1 (:name (meta %2)))
        (vals sects)))

    (heading [_ sect]
      (let [sn (keyword (lcase sect))]
        (reduce #(assoc %1
                        (first %2) (last %2))
                (ordered-map)
                (or (vals (get sects sn)) []))))

    (strValue [this sect prop]
      (str (getKV sects sect prop true)))

    (strValue [this sect prop dft]
      (if-some [rc (getKV sects
                          sect prop false)]
        rc dft))

    (longValue [this sect prop dft]
      (if-some [rc (getKV sects
                          sect prop false)]
        (convLong rc) dft))

    (longValue [this sect prop]
      (convLong (getKV sects sect prop true) 0))

    (intValue [this sect prop dft]
      (if-some [rc (getKV sects
                          sect prop false)]
        (convInt rc dft) dft))

    (intValue [this sect prop]
      (convInt (getKV sects sect prop true)))

    (doubleValue [this sect prop dft]
      (if-some [rc (getKV sects sect prop false)]
        (convDouble rc dft)
        dft))

    (doubleValue [this sect prop]
      (convDouble (getKV sects sect prop true)))

    (boolValue [this sect prop dft]
      (if-some [rc (getKV sects
                          sect prop false)]
        (convBool rc dft) dft))

    (boolValue [this sect prop]
      (convBool (getKV sects sect prop true)))

    (dbgShow [_]
      (let [buf (strbf<>)]
        (doseq [v (vals sects)]
          (.append buf (str "[" (:name (meta v)) "]\n"))
          (doseq [[x y] (vals v)]
            (.append buf (str x "=" y "\n")))
          (.append buf "\n"))
        (println (.toString buf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- parseFile ""

  ([fUrl] (parseFile fUrl "utf-8"))
  ([fUrl enc]
   (with-open [inp (-> (.openStream ^URL fUrl)
                       (io/reader :encoding
                                  (stror enc "utf8"))
                       LineNumberReader. )]
     (loop [total (atom (sorted-map))
            rdr inp
            line (.readLine rdr)
            curSec nil]
       (if (nil? line)
         (makeWinini @total)
         (recur total
                rdr
                (.readLine rdr)
                (evalOneLine rdr
                             total curSec line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn w32ini<>
  "Parse a ini conf file"
  ^Win32Conf
  [f]
  (cond
    (string? f) (if (hgl? f) (w32ini<> (io/file f)))
    (ist? File f) (if (fileRead? f) (w32ini<> (io/as-url f)))
    (ist? URL f) (some-> f parseFile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


