;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "NLS (national language support) helpers."
      :author "Kenneth Leung"}

  czlab.basal.resources

  (:require [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.io File FileInputStream]
           [java.util
            Locale
            ResourceBundle
            PropertyResourceBundle]
           [java.net URL]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti loadResource
  "Load properties file with localized strings" {:tag ResourceBundle} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadResource File [aFile] (loadResource (io/as-url aFile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadResource
  URL
  [^URL url]
  (with-open
    [inp (.openStream url)]
    (PropertyResourceBundle. inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadResource
  String
  [^String path]
  (with-open [inp (some-> getCldr
                          (.getResource path) .openStream)]
    (PropertyResourceBundle. inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getResource
  "A resource bundle"
  {:tag ResourceBundle}

  ([baseName] (getResource baseName (Locale/getDefault) nil))
  ([baseName locale] (getResource baseName locale nil))
  ([^String baseName
    ^Locale locale
    ^ClassLoader cl]
   (if (and locale (hgl? baseName))
     (ResourceBundle/getBundle baseName
                               locale (getCldr cl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rstr
  "The string value for this key,
   pms may contain values for positional substitutions"
  ^String
  [^ResourceBundle bundle ^String pkey & pms]

  (if (and bundle (hgl? pkey))
    (let [kv (str (.getString bundle pkey))
          pc (count pms)]
      ;;(log/debug "RStr key = %s, value = %s" pkey kv)
      (loop [src kv pos 0]
        (if (>= pos pc)
         src
         (recur (. src
                   replaceFirst
                   "\\{\\}" (str (nth pms pos)))
                (inc pos)))))
    pkey))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rstr*
  "Handle a bunch of resource keys
  (rstr bundle [\"k1\" p1 p2] [\"k2\" p3 p4] )"
  ^String
  [^ResourceBundle bundle & pms]
  (map #(apply rstr bundle (first %) (drop 1 %)) pms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


