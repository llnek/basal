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

  (:require [czlab.basal.io :as i :refer [fileRead?]]
            [czlab.basal.log :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:use [flatland.ordered.map])

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
  (c/throwBadData "Bad ini line: %s" (.getLineNumber rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private throwBadKey
  "" [k] `(c/throwBadData "No such item %s" ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private throwBadMap
  "" [s] `(c/throwBadData "No such heading %s" ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeSection
  "Look for a section storing the
  actual name in meta" [rdr ncmap line]

  (c/if-some+ [s (s/strimAny line "[]" true)]
    (c/do-with [k (keyword (s/lcase s))]
      (if-not (contains? @ncmap k)
        (->> (assoc @ncmap
                    k
                    (with-meta (ordered-map) {:name s}))
             (reset! ncmap))))
    (throwBadIni rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeLine
  "Parse a line (name=value) under a section"
  [rdr ncmap section ^String line]

  (if-some [kvs (get @ncmap section)]
    (let [pos (.indexOf line (int \=))
          nm (if (> pos 0)
               (s/strim (.substring line 0 pos)) "")]
      (if (s/nichts? nm) (throwBadIni rdr))
      (let [k (keyword (s/lcase nm))]
        (->> (assoc kvs
                    k
                    [nm  (s/strim (.substring line
                                              (inc pos)))])
             (swap! ncmap assoc section)))
      section)
    (throwBadIni rdr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evalOneLine
  "Parses a line in the file"
  [rdr ncmap curSec ^String line]

  (let [ln (s/strim line)]
    (cond
      (or (s/nichts? ln)
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

  (let [sn (keyword (s/lcase s))
        kn (keyword (s/lcase k))
        mp (get sects sn)]
    (cond
      (nil? mp) (if err (throwBadMap s))
      (nil? k) (if err (throwBadKey k))
      (c/notin? mp kn) (if err (throwBadKey k))
      :else (str (last (get mp kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- makeWinini "" [sects]

  (reify Win32Conf

    (headings [_]
      (c/preduce<set>
        #(conj! %1 (:name (meta %2)))
        (vals sects)))

    (heading [_ sect]
      (let [sn (keyword (s/lcase sect))]
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
        (c/convLong rc) dft))

    (longValue [this sect prop]
      (c/convLong (getKV sects sect prop true) 0))

    (intValue [this sect prop dft]
      (if-some [rc (getKV sects
                          sect prop false)]
        (c/convInt rc dft) dft))

    (intValue [this sect prop]
      (c/convInt (getKV sects sect prop true)))

    (doubleValue [this sect prop dft]
      (if-some [rc (getKV sects sect prop false)]
        (c/convDouble rc dft)
        dft))

    (doubleValue [this sect prop]
      (c/convDouble (getKV sects sect prop true)))

    (boolValue [this sect prop dft]
      (if-some [rc (getKV sects
                          sect prop false)]
        (c/convBool rc dft) dft))

    (boolValue [this sect prop]
      (c/convBool (getKV sects sect prop true)))

    (dbgShow [_]
      (let [buf (s/strbf<>)]
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
                                  (s/stror enc "utf8"))
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
    (string? f) (if (s/hgl? f)
                  (w32ini<> (io/file f)))
    (c/ist? File f) (if (i/fileRead? f)
                      (w32ini<> (io/as-url f)))
    (c/ist? URL f) (some-> f parseFile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


