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

(ns ^{:doc "Locale resources."
      :author "Kenneth Leung" }

  czlab.xlib.resources

  (:require
    [czlab.xlib.meta :refer [getCldr]]
    [czlab.xlib.logging :as log]
    [clojure.string :as cs]
    [clojure.java.io :as io])

  (:import
    [org.apache.commons.lang3 StringUtils]
    [java.io File FileInputStream]
    [java.util
     Locale
     ResourceBundle
     PropertyResourceBundle ]
    [java.net URL]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti^ResourceBundle loadResource
  "Load properties file with localized strings" class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadResource File

  [^File aFile]

  (loadResource (io/as-url aFile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadResource URL

  [^URL url]

  (with-open [inp (.openStream url) ] (PropertyResourceBundle. inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadResource String

  [^String path]

  (with-open
    [inp (some-> (getCldr)
                 (.getResource path)
                 (.openStream)) ]
    (PropertyResourceBundle. inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getResource

  "A resource bundle"

  (^ResourceBundle
    [^String baseName]
    (getResource baseName (Locale/getDefault) nil))

  (^ResourceBundle
    [^String baseName
     ^Locale locale]
    (getResource baseName locale nil))

  (^ResourceBundle
    [^String baseName
     ^Locale locale
     ^ClassLoader cl]
    (if (or (nil? baseName)
            (nil? locale))
      nil
      (ResourceBundle/getBundle baseName
                                locale (getCldr cl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rstr

  "The string value for this key,
   pms may contain values for positional substitutions"

  ^String
  [^ResourceBundle bundle ^String pkey & pms]

  (if (empty? pkey)
    ""
    (let [kv (str (.getString bundle pkey))
          pc (count pms) ]
      ;;(log/debug "RStr key = %s, value = %s" pkey kv)
      (loop [src kv pos 0 ]
        (if (>= pos pc)
         src
         (recur (StringUtils/replace src
                            "{}"
                            (str (nth pms pos)) 1)
                (inc pos)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rstr* ""

  [^ResourceBundle bundle & pms]

  (map #(apply rstr bundle (first %) (drop 1 %)) pms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


