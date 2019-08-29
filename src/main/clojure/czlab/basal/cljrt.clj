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
            [czlab.basal.core :as c]
            [czlab.basal.proto :as po])

  (:import [clojure.lang
            IFn
            RT
            Var
            Symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(defprotocol Cljrt
  "Clojure environment that can load and run a function."
  (call* [_ v arglist] "Invoke a function dynamically.")
  (var* [_ name] "Load the named var.")
  (require* [_ namespacelist] "Load list of namespaces."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cljrt<>
  "A clojure runtime."
  {:tag java.io.Closeable}
  ([cl] (cljrt<> cl "?"))
  ([] (cljrt<> nil))
  ([cl name]
   (let [^IFn _require (RT/var "clojure.core" "require")
         ^IFn _resolve (RT/var "clojure.core" "resolve")]
     (reify
       po/Idable
       (id [_] (s/stror name "?"))
       java.io.Closeable
       (close [_] )
       Cljrt
       (require* [_ nsps]
         (doseq [n nsps]
           (.invoke _require (Symbol/create n))))
       (call* [me v args]
         (if (or (string? v)
                 (keyword? v))
           (.call* me (.var* me v) args)
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
         (try (let [fname (s/kw->str fname)
                    v (or (.invoke _resolve
                                   (Symbol/create fname))
                          (let [[a b] (cs/split fname #"/")]
                            (.invoke _require
                                     (Symbol/create a))
                            (RT/var a b)))]
                (if (nil? v)
                  (c/raise! "Var not found!")) v)
              (catch Throwable _
                (c/trap! RuntimeException
                         (s/fmt "Can't load var: %s." fname) _))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


