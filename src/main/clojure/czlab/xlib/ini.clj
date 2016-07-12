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

(ns ^{:doc "Access to a win32 .ini file."
      :author "Kenneth Leung" }

  czlab.xlib.ini

  (:require
    [czlab.xlib.files :refer [fileRead?]]
    [czlab.xlib.core
     :refer [throwBadData
             throwIOE
             convBool
             convInt
             convLong
             convDouble]]
    [czlab.xlib.logging :as log]
    [clojure.java.io :as io]
    [clojure.string :as cs]
    [czlab.xlib.str :refer [strim strimAny lcase]])

  (:use [flatland.ordered.map])

  (:import
    [czlab.xlib IWin32Conf]
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
(defmulti parseInifile "Parse a INI config file"  ^IWin32Conf class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwBadIni

  ""
  [^LineNumberReader rdr]

  (throwBadData (format "Bad ini line: %s" (.getLineNumber rdr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwBadKey

  ""
  [k]

  (throwBadData (format "No such property %s" k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwBadMap

  ""
  [s]

  (throwBadData (format "No such section %s" s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeSection

  "Look for a section, store the actual section name in metadata"
  [^LineNumberReader rdr
   ncmap
   ^String line]

  (let [s (strimAny line "[]" true) ]
    (when (empty? s)
      (throwBadIni rdr))
    (let [k (keyword (lcase s))]
      (when-not (contains? @ncmap k)
        (->> (assoc @ncmap
                    k
                    (with-meta (sorted-map) {:name s}))
             (reset! ncmap)))
      k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeLine

  "Parse a line (name=value) under a section"
  [^LineNumberReader rdr
   ncmap
   section
   ^String line]

  (let [kvs (get @ncmap section) ]
    (when (nil? kvs)
      (throwBadIni rdr))
    (let [pos (.indexOf line (int \=))
          nm (if (> pos 0)
               (strim (.substring line 0 pos))
               "" ) ]
      (when (empty? nm)
        (throwBadIni rdr))
      (let [k (keyword (lcase nm))]
        (->> (assoc kvs k [nm  (strim (.substring line (inc pos)))])
             (swap! ncmap assoc section)))
      section)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evalOneLine

  "Parse a line in the file"
  [^LineNumberReader rdr
   ncmap
   curSec
   ^String line]

  (let [ln (strim line) ]
    (cond
      (or (empty? ln)
          (.startsWith ln "#"))
      curSec

      (.matches ln "^\\[.*\\]$")
      (maybeSection rdr ncmap ln)

      :else
      (maybeLine rdr ncmap curSec ln))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getKV

  ""
  ^String
  [sects s k err]

  (let [sn (keyword (lcase s))
        kn (keyword (lcase k))
        mp (get sects sn)]
    (cond
      (nil? mp) (when err (throwBadMap s))
      (nil? k) (when err (throwBadKey k))
      (not (contains? mp kn)) (when err (throwBadKey k))
      :else (str (last (get mp kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- makeWinini

  ""
  [sects]

  (reify

    IWin32Conf

    (headings [_]
      (persistent!
        (reduce
          #(conj! %1 (:name (meta %2)))
          (transient #{})
          (vals sects))))

    (heading [_ sect]
      (let [sn (keyword (lcase sect))]
        (reduce #(assoc %1
                        (first %2) (last %2))
                (sorted-map)
                (or (vals (get sects sn)) []))))

    (strValue [this section prop]
      (str (getKV sects section prop true)))

    (strValue [this section prop dft]
      (if-some [rc (getKV sects section prop false) ]
        rc
        dft))

    (longValue [this section prop dft]
      (if-some [rc (getKV sects section prop false) ]
        (convLong rc)
        dft))

    (longValue [this section prop]
      (convLong (getKV sects section prop true) 0))

    (intValue [this section prop dft]
      (if-some [rc (getKV sects section prop false) ]
        (convInt rc dft)
        dft))

    (intValue [this section prop]
      (convInt (getKV sects section prop true) ))

    (doubleValue [this section prop dft]
      (if-some [rc (getKV sects section prop false) ]
        (convDouble rc dft)
        dft))

    (doubleValue [this section prop]
      (convDouble (getKV sects section prop true) ))

    (boolValue [this section prop dft]
      (if-some [rc (getKV sects section prop false) ]
        (convBool rc dft)
        dft))

    (boolValue [this section prop]
      (convBool (getKV sects section prop true) ))

    (dbgShow [_]
      (let [buf (StringBuilder.)]
        (doseq [v (vals sects)]
          (.append buf (str "[" (:name (meta v)) "]\n"))
          (doseq [[x y] (vals v)]
            (.append buf (str x "=" y "\n")))
          (.append buf "\n"))
        (println (.toString buf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod parseInifile

  String

  ^IWin32Conf
  [fpath]

  (when (some? fpath)
    (parseInifile (io/file fpath))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod parseInifile

  File

  ^IWin32Conf
  [file]

  (when (fileRead? file)
    (parseInifile (io/as-url file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- parseFile

  ""
  ^IWin32Conf
  [^URL fUrl]

  (with-open [inp (.openStream fUrl) ]
    (loop [rdr (->> (io/reader inp :encoding "utf-8")
                    (LineNumberReader. ))
           total (atom (sorted-map))
           line (.readLine rdr)
           curSec nil]
      (if (nil? line)
        (makeWinini @total)
        (recur rdr
               total
               (.readLine rdr)
               (evalOneLine rdr total curSec line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod parseInifile

  URL

  ^IWin32Conf
  [^URL fileUrl]

  (when (some? fileUrl)
    (parseFile fileUrl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


