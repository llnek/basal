;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.basal.cljrt

  (:require [czlab.basal.util :as u]
            [clojure.string :as cs]
            [czlab.basal.core :as c]
            [czlab.basal.xpis :as po])

  (:import [clojure.lang IFn RT Var Symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Cljrt
  "Clojure environment that can load and run a function."
  (call* [_ v arglist] "Invoke a function dynamically.")
  (var* [_ name] "Load the named var.")
  (require* [_ namespacelist] "Load list of namespaces."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cljrt<>
  "A clojure runtime."

  ([cl]
   (cljrt<> cl "?"))
  ([]
   (cljrt<> nil))
  ([cl name]
   (let [^IFn _require (RT/var "clojure.core" "require")
         cl (or cl (u/get-cldr))
         ^IFn _resolve (RT/var "clojure.core" "resolve")]
     (reify
       po/Idable
       (id [_] (c/stror name "?"))
       Cljrt
       (require* [_ nsps]
         (doseq [n nsps]
           (.invoke _require (Symbol/create n))))
       (call* [me v args]
         (if (or (string? v)
                 (keyword? v))
           (.call* me (.var* me v) args)
           (if-some [f (c/cast? IFn v)]
             (let [[a b c d e g] args]
               (case (count args)
                 0 (.invoke f)
                 1 (.invoke f a)
                 2 (.invoke f a b)
                 3 (.invoke f a b c)
                 4 (.invoke f a b c d)
                 5 (.invoke f a b c d e)
                 6 (.invoke f a b c d e g)
                 (u/throw-BadArg  "too many args to invoke"))))))
       (var* [me fname]
         (let [fname (c/kw->str fname)
               v (or (.invoke _resolve
                              (Symbol/create fname))
                     (let [[a b]
                           (cs/split fname #"/")]
                       (.invoke _require
                                (Symbol/create a))
                       (RT/var a b)))]
           (if (nil? v)
             (c/raise! "Var %s not found!" fname)) v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


