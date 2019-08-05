;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Ways to generate an unique id."
      :author "Kenneth Leung"}

  czlab.basal.guids

  (:require [czlab.basal.core :as c]
            [czlab.basal.str :as s]
            [clojure.java.io :as io]
            [czlab.basal.util :as u])

  (:import [java.util
            UUID]
           [java.lang
            Math]
           [java.net
            InetAddress]
           [java.io
            DataInputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pre-shuffle the chars in string
(def ^{:private true :tag String} _ss
  (s/shuffle (str "abcdefghijklmnopqrstuvwxyz"
                      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(def ^:private _chars (.toCharArray _ss))
(def ^:private _uuid-len (count _ss))
(def ^:private ^String int-mask "00000")
(def ^:private ^String long-mask "0000000000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private fmt-int
  [nm] `(fmt int-mask (Integer/toHexString ~nm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private fmt-long
  [nm] `(fmt long-mask (Long/toHexString ~nm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- fmt ^String [pad mask]
  (let [plen (count pad)
        mlen (count mask)]
    (if (>= mlen plen)
      (.substring ^String mask 0 plen)
      (str (.replace (s/sbf<> pad)
                     (int (- plen mlen)) (int plen) ^String mask)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- split-time []
  (let [s (fmt-long (System/currentTimeMillis))
        n (count s)]
    [(s/lefts s (/ n 2))
     (s/rights s (max 0 (- n (/ n 2))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- maybe-set-iP []
  (let [neta (InetAddress/getLocalHost)
        b (.getAddress neta)]
    (cond (.isLoopbackAddress neta)
          (.nextLong (u/rand<>))
          :else
          (c/wo* [dis (DataInputStream.
                        (io/input-stream b))]
            (if (= 4 (alength b))
              (long (.readInt dis)) (.readLong dis))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private ^long _IP (Math/abs ^long (maybe-set-iP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn uuid<>
  "rfc4122, v4 format"
  ^String []
  ;;at i==19 set the high bits of clock
  ;;sequence as per rfc4122, sec. 4.1.5
  (let [rc (char-array _uuid-len)
        rnd (u/rand<>)]
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
(defn wwid<>
  "uid based on time/ip"
  ^String []
  (let [seed (.nextInt (u/rand<>)
                       (Integer/MAX_VALUE))
        [hi lo] (split-time)]
    (str hi (fmt-long _IP) (fmt-int seed) (fmt-int (u/seqint)) lo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


