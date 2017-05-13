;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Ways to generate an unique id."
      :author "Kenneth Leung"}

  czlab.basal.guids

  (:require [czlab.basal.io :refer [readNumber]]
            [clojure.string :as cs]
            [czlab.basal.logging :as log])

  (:use [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.lang StringBuilder]
           [czlab.jasal CU]
           [java.net InetAddress]
           [java.util UUID]
           [java.lang Math]
           [java.security SecureRandom]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pre-shuffle the chars in string
(def ^{:private true
       :tag String}
  _ss (CU/shuffle
        (let [s "abcdefghijklmnopqrstuvwxyz"]
          (str s "0123456789" (cs/upper-case s)))))
(def ^:private _chars (charsit _ss))
(def ^:private _uuid-len (.length _ss))
(def ^:private ^String int-mask "00000")
(def ^:private ^String long-mask "0000000000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- fmt
  "" ^String [^String pad ^String mask]

  (let [mlen (.length mask)
        plen (.length pad)]
    (if (>= mlen plen)
      (.substring mask 0 plen)
      (str (.replace (strbf<> pad)
                     (- plen mlen) plen mask)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private fmtInt
  "" [nm] `(fmt int-mask (Integer/toHexString ~nm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private fmtLong
  "" [nm] `(fmt long-mask (Long/toHexString ~nm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- splitTime "" []
  (let [s (fmtLong (now<>))
        n (.length s)]
    [(lefts s (/ n 2))
     (rights s (max 0 (- n (/ n 2))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeSetIP "" []
  (let [neta (InetAddress/getLocalHost)
        b (.getAddress neta)]
    (cond
      (.isLoopbackAddress neta)
      (. (rand<>) nextLong)
      (== 4 (alength b))
      (long (readNumber b Integer))
      :else (readNumber b Long))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^long _IP (Math/abs ^long (maybeSetIP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn myOwnNewUUid
  "rfc4122, v4 format"
  {:tag String :no-doc true} []

  ;;at i==19 set the high bits of clock
  ;;sequence as per rfc4122, sec. 4.1.5
  (let [rc (char-array _uuid-len)
        rnd (rand<>)]
    (dotimes [n (alength rc)]
      (aset-char rc
                 n
                 (case n
                   (8 13 18 23) \-
                   (14) \4
                   (let [d (Double. (* (.nextDouble rnd) 16))
                         r (bit-or 0 (.intValue d))
                         pos (if (= n 19)
                               (bit-or (bit-and r 0x3) 0x8)
                               (bit-and r 0xf))]
                     (aget ^chars _chars pos)))))
    (String. rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn wwid<>
  "uid based on time/ip" ^String []

  (let [seed (.nextInt (rand<>)
                       (Integer/MAX_VALUE))
        ts (splitTime)]
    (str (nth ts 0)
         (fmtLong _IP)
         (fmtInt seed)
         (fmtInt (seqint))
         (nth ts 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


