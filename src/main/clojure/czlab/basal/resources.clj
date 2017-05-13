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

  (:require [czlab.basal.meta :as m :refer [getCldr]]
            [czlab.basal.log :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

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
(defn loadResource
  "Load file with localized strings"
  ^ResourceBundle
  [arg]
  (cond
    (c/ist? File arg)
    (loadResource (io/as-url arg))
    (c/ist? URL arg)
    (with-open
      [inp (.openStream ^URL arg)]
      (PropertyResourceBundle. inp))
    (string? arg)
    (with-open
      [inp (some-> (m/getCldr)
                   (.getResource ^String arg) .openStream)]
      (PropertyResourceBundle. inp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getResource
  "A resource bundle" {:tag ResourceBundle}

  ([baseName] (getResource baseName (Locale/getDefault) nil))
  ([baseName locale] (getResource baseName locale nil))
  ([^String baseName
    ^Locale locale
    ^ClassLoader cl]
   (if (and locale (s/hgl? baseName))
     (ResourceBundle/getBundle baseName
                               locale (m/getCldr cl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rstr
  "The string value for this key,
  pms may contain values
  for positional substitutions"
  ^String
  [^ResourceBundle bundle ^String pkey & pms]

  (if (and bundle (s/hgl? pkey))
    (let [kv (str (.getString bundle pkey))
          pc (count pms)]
      ;;(log/debug "RStr key = %s, value = %s" pkey kv)
      (loop [src kv pos 0]
        (if (>= pos pc)
         src
         (recur (.replaceFirst src
                               "\\{\\}"
                               (str (nth pms pos))) (inc pos)))))
    pkey))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rstr*
  "Handle a bunch of resource keys
  (rstr bundle [\"k1\" p1 p2] [\"k2\" p3 p4] )"
  ^String
  [^ResourceBundle bundle & pms]
  (mapv #(apply rstr bundle (first %) (drop 1 %)) pms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


