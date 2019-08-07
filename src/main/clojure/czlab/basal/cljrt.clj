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
            [czlab.basal.str :as s]
            [czlab.basal.core :as c])

  (:import [clojure.lang
            IFn
            RT
            Var
            Symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(defprotocol CljrtAPI
  (call* [_ v args])
  (var* [_ name])
  (require* [_ namespaces]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord CljrtObj []
  java.io.Closeable
  (close [_] )
  CljrtAPI
  (require* [me nsps]
    (let [{:keys [_require]} me]
      (doseq [n nsps]
        (.invoke ^IFn _require (Symbol/create n)))))
  (call* [me v args]
    (if (or (string? v)
            (keyword? v))
      (call* me (var* me v) args)
      ;else
      (let [n (count args)
            ^IFn func v
            [a b c d e f] args]
        (case n
          0 (.invoke func)
          1 (.invoke func a)
          2 (.invoke func a b)
          3 (.invoke func a b c)
          4 (.invoke func a b c d)
          5 (.invoke func a b c d e)
          6 (.invoke func a b c d e f)
          (u/throw-BadArg  "too many arguments to invoke")))))
  (var* [me fname]
    (try (let [{:keys [_resolve _require]} me
               fname (s/kw->str fname)
               v (or (.invoke ^IFn
                              _resolve
                              (Symbol/create fname))
                     (let [[a b] (cs/split fname #"/")]
                       (.invoke ^IFn
                                _require
                                (Symbol/create a))
                       (RT/var a b)))]
           (if (nil? v)
             (c/trap! Exception "not found")) v)
         (catch Exception _
           (c/trap! RuntimeException
                    (str "can't load var: " fname) _)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cljrt<> ""
  ([cl] (cljrt<> cl "?"))
  ([] (cljrt<> nil))
  ([cl name]
   (assoc (CljrtObj.)
          :_require (RT/var "clojure.core" "require")
          :_resolve (RT/var "clojure.core" "resolve")
          :_refer (RT/var "clojure.core" "refer")
          :loader (u/get-cldr cl)
          :id (s/stror name "?"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


