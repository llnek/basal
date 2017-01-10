;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Logging api."
      :author "Kenneth Leung"}

  czlab.xlib.logging

  (:require [clojure.tools.logging :as log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defmacro trace "" [& args]
  `(if (log/enabled? :trace) (log/logf :trace ~@args)))

(defmacro debug "" [& args]
  `(if (log/enabled? :debug) (log/logf :debug ~@args)))

(defmacro info "" [& args]
  `(if (log/enabled? :info) (log/logf :info ~@args)))

(defmacro warn "" [& args]
  `(if (log/enabled? :warn) (log/logf :warn ~@args)))

(defmacro exception "" [e]
  `(if (log/enabled? :error) (log/logf :error ~e "")))

(defmacro error "" [& args]
  `(if (log/enabled? :error) (log/logf :error ~@args)))

(defmacro fatal "" [& args]
  `(if (log/enabled? :fatal) (log/logf :fatal ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


