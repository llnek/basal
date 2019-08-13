;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Logging api."
      :author "Kenneth Leung"}

  czlab.basal.log

  (:require [clojure.tools.logging :as l]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this codeblock allows us to turn logging of czlab libs on/off during
;;compile time.
(defmacro ^:private hack []
  (let [x (System/getProperty "czlabloggerflag")]
    `(def ~(with-meta '*czlab-logger-flag* {:dynamic true}) ~x)))
;(hack)
(def ^:dynamic *czlab-logger-flag*
  (not (false? (System/getProperty "czlabloggerflag"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trace "" [& args]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :trace ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro debug "" [& args]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :debug ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro info "" [& args]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :info ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro warn "" [& args]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :warn ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro exception "" [e]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :error ~e "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error "" [& args]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :error ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fatal "" [& args]
  `(if czlab.basal.log/*czlab-logger-flag*
     (clojure.tools.logging/logf :fatal ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


