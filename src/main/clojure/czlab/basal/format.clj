;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Helpers for file formats JSON, EDN."
      :author "Kenneth Leung"}

  czlab.basal.format

  (:require [czlab.basal.indent :refer [indent-dispatch]]
            [clojure.pprint
             :refer [pprint with-pprint-dispatch]]
            [czlab.basal.io :refer [readAsStr]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.data.json :as js])

  (:use [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.net URL]
           [java.io File StringWriter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeEdnStr
  "Format to edn" ^String [obj]

  (str (do-with [w (StringWriter.)]
               (if obj
                 (with-pprint-dispatch
                   indent-dispatch (pprint obj w))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readEdn
  "Parse EDN formatted text"
  [arg]

  (cond
    (ist? File arg) (readEdn (io/as-url arg))
    (ist? URL arg) (readEdn (readAsStr arg))
    :else (some-> arg str edn/read-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn writeJsonStr
  "Format to json" ^String [data] (some-> data js/write-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readJsonStrKW
  "Parses json. keys mapped to keywords"
  [data] (if (hgl? data)
           (js/read-str data :key-fn keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readJsonStr
  "Parses json" {:tag String}

  ([data] (if (hgl? data) (js/read-str data)))
  ([data keyfn]
   (if (hgl? data) (js/read-str data :key-fn keyfn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readJson
  "Parses json" [arg]

  (cond
    (ist? File arg) (readJson (io/as-url arg))
    (ist? URL arg) (readJson (readAsStr arg))
    :else (some-> arg str readJsonStrKW )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


