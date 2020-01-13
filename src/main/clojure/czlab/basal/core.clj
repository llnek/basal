;; Copyright © 2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.core

  "Useful additions to clojure core & string."

  (:refer-clojure :exclude [send])

  (:require [clojure.set :as ct]
            [clojure.string :as cs]
            [io.aviso.ansi :as ansi]
            [clojure.tools.logging :as l])

  (:import [java.util Date]
           [java.lang System StringBuilder]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;; #^"[Ljava.lang.Object;"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^{:doc "Log flag(toggle) used internally."}
  LOG-FLAG (not (.equals "false" (System/getProperty "czlabloggerflag"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private t-bad "FAILED")
(def ^:private t-ok "passed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmacro-

  "Same as defmacro but private."
  {:arglists '([name & more])}
  [name & more]

  (list* `defmacro (with-meta name
                              (assoc (meta name)
                                     :private true)) more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defonce-

  "Same as defonce but private."
  {:arglists '([name & more])}
  [name & more]

  (list* `defonce (with-meta name
                             (assoc (meta name)
                                    :private true)) more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-

  "Same as def but private."
  {:arglists '([name & more])}
  [name & more]

  (list* `def (with-meta name
                         (assoc (meta name)
                                :private true)) more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trace

  "Logging at TRACE level."
  {:arglists '([& xs])}
  [& xs]

  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :trace ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro debug

  "Logging at DEBUG level."
  {:arglists '([& xs])}
  [& xs]

  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :debug ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro info

  "Logging at INFO level."
  {:arglists '([& xs])}
  [& xs]

  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :info ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro warn

  "Logging at WARN level."
  {:arglists '([& xs])}
  [& xs]

  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :warn ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error

  "Logging at ERROR level."
  {:arglists '([& xs])}
  [& xs]

  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :error ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fatal

  "Logging at FATAL level."
  {:arglists '([& xs])}
  [& xs]

  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :fatal ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exception

  "Log an exception at ERROR level."
  {:arglists '([e])}
  [e]

  (when czlab.basal.core/LOG-FLAG
    (clojure.tools.logging/logf :error e "")
    (clojure.tools.logging/logf :error "%s" "exception thrown.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def BOOLS #{"true" "yes" "on" "ok" "active" "1"})
(def ^String HEX-CHAR-STR "0123456789ABCDEF")
(def ^String hex-char-str "0123456789abcdef")
(def ^{:tag "[C"} HEX-CHARS (.toCharArray HEX-CHAR-STR))
(def ^{:tag "[C"} hex-chars (.toCharArray hex-char-str))
(def ^String USASCII "ISO-8859-1")
(def ^String UTF16 "UTF-16")
(def ^String UTF8 "UTF-8")

(def OneK 1024)
(def FourK (* 4 OneK))
(def KiloBytes OneK)
(def BUF-SZ (* 4 KiloBytes))
(def MegaBytes (* KiloBytes KiloBytes))
(def GigaBytes (* KiloBytes KiloBytes KiloBytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro funcit??

  "Maybe run a function with args."
  {:arglists '([f & args])}
  [f & args]

  `(let [f# ~f] (if (fn? f#) (f# ~@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pre

  "Like :pre, assert conditions."
  {:arglists '([conds])}
  [& conds]

  `(assert (and ~@conds) "precond failed."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atom?

  "If obj is an atom?"
  {:arglists '([x])}
  [x]

  `(instance? clojure.lang.Atom ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n#-even?

  "Count of collection even?"
  {:arglists '([coll])}
  [coll]

  `(even? (count ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !sas?

  "Same as not-satisfies?"
  {:arglists '([proto obj])}
  [proto obj]

  `(not (satisfies? ~proto ~obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sas?

  "Same as satisfies?"
  {:arglists '([proto obj])}
  [proto obj]

  `(satisfies? ~proto ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !is?

  "Same as not-instance?"
  {:arglists '([clazz obj])}
  [clazz obj]

  `(not (instance? ~clazz ~obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro is?

  "Same as instance?"
  {:arglists '([clazz obj])}
  [clazz obj]

  `(instance? ~clazz ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro map->

  "Same as into {}."
  {:arglists '([& more])}
  [& more]

  `(into {} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vec->

  "Same as into []."
  {:arglists '([& more])}
  [& more]

  `(into [] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro set->

  "Same as into #{}."
  {:arglists '([& more])}
  [& more]

  `(into #{} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+1

  "Prepend an item to collection(s)."
  {:arglists '([a & more])}
  [a & more]

  `(concat [~a] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+

  "Same as concat."
  {:arglists '([& more])}
  [& more]

  `(concat ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _2

  "Same as second."
  {:arglists '([coll])}
  [coll]

  `(second ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _1

  "Same as first."
  {:arglists '([coll])}
  [coll]

  `(first ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _3

  "Get the 3rd item."
  {:arglists '([coll])}
  [coll]

  `(nth ~coll 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _E

  "Same as last."
  {:arglists '([coll])}
  [coll]

  `(last ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _NE

  "Get the last item via nth."
  {:arglists '([coll])}
  [coll]

  `(let [x# ~coll] (nth x# (- (count x#) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !zero?

  "Same as not-zero?"
  {:arglists '([n])}
  [n]

  `(not (zero? ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n#

  "Same as count."
  {:arglists '([coll])}
  [coll]

  `(count ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro car

  "Same as first."
  {:arglists '([coll])}
  [coll]

  `(first ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cdr

  "Same as rest."
  {:arglists '([coll])}
  [coll]

  `(rest ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro one?

  "If count() is 1?"
  {:arglists '([coll])}
  [coll]

  `(== 1 (count ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro two?

  "If count() is 2?"
  {:arglists '([coll])}
  [coll]

  `(== 2 (count ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro one+?

  "If count() is more than 1?"
  {:arglists '([coll])}
  [coll]

  `(> (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro or??

  "(or?? [= a] b c) => (or (= b a) (= c a))."
  {:arglists '([bindings & args])}
  [bindings & args]

  (let [op (gensym)
        arg (gensym)
        [p1 p2] bindings]
    `(let [~op ~p1
           ~arg ~p2]
       (or ~@(map (fn [n]
                    `(~op ~n ~arg)) args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dissoc!!

  "Mutable dissoc (atom)."
  {:arglists '([a & args])}
  [a & args]

  (let [X (gensym)]
    `(let [~X ~a] (swap! ~X dissoc ~@args) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assoc!!

  "Mutable assoc (atom)."
  {:arglists '([a & args])}
  [a & args]

  (let [X (gensym)]
    `(let [~X ~a] (swap! ~X assoc ~@args) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_*

  "Wrap code into a fn(...)."
  {:arglists '([& forms])}
  [& forms]

  `(fn [& ~'____xs] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_3

  "Wrap code into a fn(a1,a2,a3)."
  {:arglists '([& forms])}
  [& forms]

  `(fn [~'____1 ~'____2 ~'____3] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_2

  "Wrap code into a fn(a1,a2)."
  {:arglists '([& forms])}
  [& forms]

  `(fn [~'____1 ~'____2] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_1

  "Wrap code into a fn(a1)."
  {:arglists '([& forms])}
  [& forms]

  `(fn [~'____1] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_0

  "Wrap code into a fn()."
  {:arglists '([& forms])}
  [& forms]

  `(fn [] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tset*

  "A transient set."
  {:arglists '([]
               [x])}

  ([]
   `(tset* nil))

  ([x]
   `(transient (or ~x #{}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tvec*

  "A transient vector."
  {:arglists '([]
               [x])}

  ([]
   `(tvec* nil))

  ([x]
   `(transient (or ~x []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tmap*

  "A transient map."
  {:arglists '([]
               [x])}

  ([]
   `(tmap* nil))

  ([x]
   `(transient (or ~x {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro persist!

  "Same as persistent!."
  {:arglists '([coll])}
  [coll]

  `(persistent! ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atomic

  "Atomize fields as map."
  {:arglists '([& args])}
  [& args]

  `(atom (array-map ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mapfv

  "Apply a binary-op to the value over the forms.
  e.g. (+ 3 1 (+ 2 2)) => [4 7]."
  {:arglists '([op value & forms])}
  [op value & forms]

  `(vector ~@(map (fn [f] `(~op ~f ~value)) forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro last-index

  "count less 1."
  {:arglists '([coll])}
  [coll]

  `(- (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nexth

  "The nth item after i."
  {:arglists '([coll i])}
  [coll i]

  `(nth ~coll (+ 1 ~i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro chop

  "Same as partition."
  {:arglists '([& args])}
  [& args]

  `(partition ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro inc*

  "One plus, x+1."
  {:arglists '([x])}
  [x]

  `(+ 1 ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dec*

  "One less, x-1."
  {:arglists '([x])}
  [x]

  `(- ~x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#false

  "Do returns false."
  {:arglists '([& forms])}
  [& forms]

  `(do ~@forms false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#true

  "Do returns true."
  {:arglists '([& forms])}
  [& forms]

  `(do ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#nil

  "Do returns nil."
  {:arglists '([& forms])}
  [& forms]

  `(do ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#false

  "Let returns false."
  {:arglists '([& forms])}
  [& forms]

  `(let ~@forms false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#true

  "Let returns true."
  {:arglists '([& forms])}
  [& forms]

  `(let ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#nil

  "Let returns nil."
  {:arglists '([& forms])}
  [& forms]

  `(let ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defenum

  "Enum definition.
  e.g. (defenum ^:private xyz 1 a b c) will generate
  (def ^:private xyz-a 1)
  (def ^:private xyz-b 2)
  (def ^:private xyz-c 3)."
  {:arglists '([ename start & args])}
  [ename start & args]

  (let [mm (meta ename)]
    (assert (number? start)
            "Enum expecting a number.")
    `(do ~@(loop [v start [m & ms] args out []]
             (if (nil? m)
               out
               (let [m' (meta m)
                     z (str (name ename)
                            "-" (name m))]
                 (recur (+ 1 v)
                        ms
                        (conj out
                              `(def ~(with-meta (symbol z)
                                                (merge m' mm)) ~v)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro condp??

  "condp without throwing exception."
  {:arglists '([pred expr & clauses])}
  [pred expr & clauses]

  (let [c (count clauses)
        z (count (filter #(and (keyword? %)
                               (= :>> %)) clauses))
        d (- c z)
        xs (if-not (even? d)
             clauses
             (concat clauses ['nil]))] `(condp ~pred ~expr ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro case??

  "case without throwing exception."
  {:arglists '([expr & clauses])}
  [expr & clauses]

  (let [c (count clauses)
        xs (if-not (even? c)
             clauses
             (concat clauses ['nil]))] `(case ~expr ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-xxx??

  {:arglists '([F bindings then]
               [F bindings then else])}

  ([F bindings then]
   `(if-xxx?? ~F ~bindings ~then nil))

  ([F bindings then else & oldform]
   (let [_ (assert (== 2 (count bindings))
                   "Too many (> 2) in bindings.")
         X (gensym)
         f1 (first bindings)]
     (assert (symbol? f1))
     `(let [~X ~(last bindings) ~f1 ~X] (if (~F ~X) ~then ~else)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-inst

  "If expr is instance of class, execute `then`, otherwise `else`."
  {:arglists '([clazz expr then]
               [clazz expr then else])}

  ([clazz expr then]
   `(if-inst ~clazz ~expr ~then nil))

  ([clazz expr then else & oldform]
   `(if (instance? ~clazz ~expr) ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-inst

  "If expr is instance of class, execute `body`."
  {:arglists '([clazz expr & body])}
  [clazz expr & body]

  `(if (instance? ~clazz ~expr) (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-proto

  "If expr satisfies the protocol, execute `then`, otherwise `else`."
  {:arglists '([proto expr then]
               [proto expr then else])}

  ([proto expr then]
   `(if-proto ~proto ~expr ~then nil))

  ([proto expr then else & oldform]
   `(if (satisfies? ~proto ~expr) ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-proto

  "If expr satisfies the protocol, execute `body`."
  {:arglists '([proto expr & body])}
  [proto expr & body]

  `(if (satisfies? ~proto ~expr) (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-nil

  "If expr evals to nil, execute `then`, otherwise `else`."
  {:arglists '([expr then]
               [expr then else])}

  ([expr then]
   `(if-nil ~expr ~then nil))

  ([expr then else & oldform]
   `(if (nil? ~expr) ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-nil

  "If expr evals to nil, execute `body`."
  {:arglists '([expr & body])}
  [expr & body]

  `(if (nil? ~expr) (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-throwable?

  "If object is a Throwable?"
  {:arglists '([x])}
  [x]

  (instance? Throwable x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-throw

  "bindings => binding-form test
  If test is a Throwable, evaluates 'then'
  with binding-form bound to the value of test."
  {:arglists '([bindings then]
               [bindings then else])}

  ([bindings then]
   `(if-throw ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx??
      czlab.basal.core/is-throwable? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-throw

  "bindings => binding-form test
  If test is a Throwable, evaluates the body."
  {:arglists '([bindings & body])}
  [bindings & body]

  `(if-throw ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-var

  "bindings => binding-form test
  If test is a var, evaluates 'then'
  with binding-form bound to the value of test."
  {:arglists '([bindings then]
               [bindings then else])}

  ([bindings then]
   `(if-var ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? var? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-var

  "bindings => binding-form test
  If test is a var, evaluates the body."
  {:arglists '([bindings & body])}
  [bindings & body]

  `(if-var ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-number

  "bindings => binding-form test
  If test is a number, evaluates 'then'
  with binding-form bound to the value of test."
  {:arglists '([bindings then]
               [bindings then else])}

  ([bindings then]
   `(if-number ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? number? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-number

  "bindings => binding-form test
  If test is a number, evaluates the body."
  {:arglists '([bindings & body])}
  [bindings & body]

  `(if-number ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-string

  "bindings => binding-form test
  If test is a string, evaluates 'then'
  with binding-form bound to the value of test."
  {:arglists '([bindings then]
               [bindings then else])}

  ([bindings then]
   `(if-string ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? string? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-string

  "bindings => binding-form test
  If test is a string, evaluates the body."
  {:arglists '([bindings & body])}
  [bindings & body]

  `(if-string ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nloop

  "Loop over code n times."
  {:arglists '([n & forms])}
  [n & forms]

  (let [x (gensym)] `(dotimes [~x ~n] ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each*

  "Evals function for each element, indexed."
  {:arglists '([func coll])}
  [func coll]

  (let [C (gensym) I (gensym) T (gensym)]
    `(let [~C ~coll ~T (count ~C)]
       (dotimes [~I ~T] (~func (nth ~C ~I) ~I)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each

  "Evals function for each element."
  {:arglists '([func coll])}
  [func coll]

  (let [I (gensym)] `(doseq [~I ~coll] (~func ~I))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mget

  "Java map.get()."
  {:arglists '([m k])}
  [m k]

  `(.get ~(with-meta m {:tag 'java.util.Map}) ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mdel!

  "Java map.remove()."
  {:arglists '([m k])}
  [m k]

  (.remove ^java.util.Map m k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mput!

  "Java map.put()."
  {:arglists '([m k v])}
  [m k v]

  `(.put ~(with-meta m {:tag 'java.util.Map}) ~k ~v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deftest

  "A bunch of test-cases grouped together."
  {:arglists '([name & body])}
  [name & body]

  `(def ~name (fn [] (filter #(not (nil? %)) [~@body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??

  "Assert test was ok."
  {:arglists '([msg form])}
  [msg form]

  `(czlab.basal.core/ensure-test ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure-thrown??

  "Assert an error was thrown."
  {:arglists '([msg expected form])}
  [msg expected form]

  `(try ~form
        (czlab.basal.core/ensure-thrown ~msg)
        (catch Throwable e#
          (czlab.basal.core/ensure-thrown ~expected e# ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prn!!

  "Println with format."
  {:arglists '([fmt & args])}
  [fmt & args]

  `(println (format ~fmt ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prn!

  "Print with format."
  {:arglists '([fmt & args])}
  [fmt & args]

  `(print (format ~fmt ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
  (defmacro- make-fn
  "Hack to wrap a macro as fn
  so one can use *apply* on it."
  [m] `(fn [& xs#] (eval (cons '~m xs#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro object<>

  "Create a new record instance.
  e.g. (object<> ClazzA)
  (object<> ClazzA {:a 1})
  (object<> ClazzA :a 1 :b 2)"
  {:arglists '([z]
               [z m]
               [z ab & more])}

  ([z]
   `(new ~z))

  ([z m]
   `(merge (new ~z) ~m))

  ([z a b & more]
   `(assoc (new ~z) ~a ~b ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vargs

  "Coerce into java array of type z."
  {:arglists '([z arglist])}
  [z arglist]

  `(into-array ~z ~arglist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<map>

  "Reduce with a transient map accumulator, returning a map."
  {:arglists '([f coll])}
  [f coll]

  `(persistent! (reduce ~f (transient {}) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<set>

  "Reduce with a transient set accumulator, returning a set."
  {:arglists '([f coll])}
  [f coll]

  `(persistent! (reduce ~f (transient #{}) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<vec>

  "Reduce with a transient vec accumulator, returning a vec."
  {:arglists '([f coll])}
  [f coll]

  `(persistent! (reduce ~f (transient []) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rset!

  "Reset a atom."
  {:arglists '([a]
               [a value])}

  ([a]
   `(rset! ~a nil))

  ([a value]
   `(reset! ~a ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro exp!

  "Create an exception of type E."
  {:arglists '([E & args])}
  [E & args]

  `(new ~E ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trap!

  "Throw exception of type E."
  {:arglists '([E & args])}

  [E & args]

  `(throw (exp! ~E ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro raise!

  "Throw java Exception with message."
  {:arglists '([fmt & args])}
  [fmt & args]

  `(throw (new java.lang.Exception (str (format ~fmt ~@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-assert-exp

  "Generate a function which when called, will
  assert the condition and if false, throw the desired exception."
  {:arglists '([name E])}
  [name E]

  `(defn ~name [~'kond ~'arg & ~'xs]
     (if-not ~'kond
       (cond
         (string? ~'arg)
         (czlab.basal.core/trap! ~E (str (apply format ~'arg ~'xs)))
         (instance? Throwable ~'arg)
         (czlab.basal.core/trap! ~E
                                 ~(with-meta 'arg {:tag 'Throwable}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-throw-exp

  "Generate a function which when called,
  will throw the desired exception."
  {:arglists '([name E])}
  [name E]

  `(defn ~name [~'arg & ~'xs]
     (cond
       (string? ~'arg)
       (czlab.basal.core/trap! ~E (str (apply format ~'arg ~'xs)))
       (instance? Throwable ~'arg)
       (czlab.basal.core/trap! ~E
                               ~(with-meta 'arg {:tag 'Throwable})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assert-on!

  "Like assert, but throws a specific exception."
  {:arglists '([cond E & args])}
  [cond E & args]

  `(if-not ~cond (trap! ~E ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with

  "bindings => [symbol init-expr]
  Evals the body in a context in which the symbol is always the
  returned value."
  {:arglists '([bindings & forms])}
  [bindings & forms]

  (let [f (first bindings)
        sz (count bindings)]
    (assert (or (== sz 1)
                (== sz 2))
            "(not 1 or 2) in bindings.")
    (assert (symbol? f))
    (if (== sz 1)
      `(let [~f ~f] ~@forms ~f)
      `(let [~f ~(last bindings)] ~@forms ~f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-str

  "bindings => [symbol init-expr]
  Eval the body in a context in which the symbol is always the
  returned value to-string'ed.
  e.g. (do-with-str [a (f)] ... (str a))."
  {:arglists '([bindings & forms])}
  [bindings & forms]

  `(str (czlab.basal.core/do-with ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-atom

  "bindings => [symbol init-expr]
  Eval the body in a context in which the symbol is always the
  returned value deref'ed.
  e.g. (do-with-atom [a (atom 0)] ... (deref a))."
  {:arglists '([bindings & forms])}
  [bindings & forms]

  `(deref (czlab.basal.core/do-with ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bool!

  "Same as boolean."
  {:arglists '([x])}
  [x]

  `(boolean ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trye!

  "Eat exception and return a value."
  {:arglists '([value & forms])}
  [value & forms]

  `(try ~@forms (catch Throwable ~'_ ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!

  "Eat exception and return nil."
  {:arglists '([& forms])}
  [& forms]

  `(czlab.basal.core/trye! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+

  "bindings => [binding-form test].
  When test is not empty, evaluates body
  with binding-form bound to the value of test."
  {:arglists '([bindings then]
               [bindings then else])}

  ([bindings then]
   `(if-some+ ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? empty? ~bindings ~else ~then)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+

  "bindings => [binding-form test].
  When test is not empty, evaluates body
  with binding-form bound to the value of test."
  {:arglists '([bindings & body])}
  [bindings & body]

  `(czlab.basal.core/if-some+ ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-fn

  "bindings => [binding-form test].
  When test is a fn?, evaluates body
  with binding-form bound to the value of test."
  {:arglists '([bindings then]
               [bindings then else])}

  ([bindings then]
   `(if-fn ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? fn? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-fn

  "bindings => [binding-form test].
  When test is a fn?, evaluates body
  with binding-form bound to the value of test."
  {:arglists '([bindings & body])}
  [bindings & body]

  `(czlab.basal.core/if-fn ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doto->>

  "Combine doto and ->>."
  {:arglists '([x & forms])}
  [x & forms]

  (let [X (gensym)]
    `(let [~X ~x]
       ~@(map (fn [f] (if (seq? f)
                        `(~@f ~X) `(~f ~X))) forms) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doto->

  "Combine doto and ->."
  {:arglists '([x & forms])}
  [x & forms]

  (let [X (gensym)]
    `(let [~X ~x]
       ~@(map (fn [f]
                (if (seq? f)
                  (let [z (first f)
                        r (rest f)]
                    `(~z ~X ~@r)) `(~f ~X))) forms) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eq?

  "Use java's .equals method."
  {:arglists '([a b])}
  [a b]

  `(.equals ~a ~b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !eq?

  "Use java's .equals method."
  {:arglists '([a b])}
  [a b]

  `(not (.equals ~a ~b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !=?

  "Same as not-identical?"
  {:arglists '([& more])}
  [& more]

  `(not (identical? ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro =?

  "Same as identical?"
  {:arglists '([& more])}
  [& more]

  `(identical? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !in?

  "Same as not-contains?"
  {:arglists '([& more])}
  [& more]

  `(not (contains? ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro in?

  "Same as contains?"
  {:arglists '([& more])}
  [& more]

  `(contains? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;had to use this trick to prevent reflection warning
(defmacro cast?

  "Cast object of this type else nil."
  {:arglists '([someType obj])}
  [someType obj]

  (let [X (gensym)]
    `(let [~X ~obj]
       (if (instance? ~someType ~X)
         ^{:tag ~someType} (identity (.cast ~someType ~X))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cexp?

  "Try casting to Throwable."
  {:arglists '([e])}
  [e]

  `(cast? Throwable ~e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !nil?

  "Same as not-nil."
  {:arglists '([x])}
  [x]

  `(not (nil? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rnil

  "Skip nil(s) in collection."
  {:arglists '([coll])}
  [coll]

  `(remove nil? ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rnilv

  "Same as rnil but returns a vec."
  {:arglists '([coll])}
  [coll]

  `(into [] (remove nil? ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro szero?

  "Safe zero?"
  {:arglists '([e])}
  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(zero? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sneg?

  "Safe neg?"
  {:arglists '([e])}
  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(neg? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro spos?

  "Safe pos?"
  {:arglists '([e])}
  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(pos? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro snneg?

  "Safe not neg?"
  {:arglists '([e])}
  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)
                       (or (zero? ~E)(pos? ~E))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !false?

  "Same as not-false."
  {:arglists '([x])}
  [x]

  `(not (false? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !true?

  "Same as not true."
  {:arglists '([x])}
  [x]

  `(not (true? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assert-not

  "Assert a false condition."
  {:arglists '([cond])}
  [cond]

  `(assert (not ~cond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro marray

  "n-size java array of type Z."
  {:arglists '([Z n])}
  [Z n]

  `(make-array ~Z ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zarray

  "0-size java array of type Z."
  {:arglists '([Z])}
  [Z]

  `(make-array ~Z 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl**

  "Make a virtual table:
  op1 f1 op2 f2, with parent vtbl."
  {:arglists '([parent & args])}
  [parent & args]

  (do (assert (even? (count args)))
      `(czlab.basal.core/object<>
         ~'czlab.basal.core.VTable :____proto ~parent ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl*

  "Make a virtual table: op1 f1 op2 f2."
  {:arglists '([& args])}
  [& args]

  `(vtbl** nil ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro wo*

  "Same as with-open."
  {:arglists '([bindings & forms])}
  [bindings & forms]

  `(with-open ~bindings ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro wm*

  "Same as with-meta."
  {:arglists '([obj m])}
  [obj m]

  `(with-meta ~obj ~m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ->str

  "Same as java's .toString."
  {:arglists '([obj])}
  [obj]

  `(some-> ~obj .toString))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run-bind

  ;"Run the bind operator. *Internal*"
  {:arglists '([binder steps expr])}
  [binder steps expr]

  (let [more (drop 2 steps)
        [a1 mv] (take 2 steps)]
    `(~binder ~mv
              (fn [~a1]
                ~(if (not-empty more)
                   `(run-bind ~binder ~more ~expr) `(do ~expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmonad

  "Define a named monad by defining the monad operations.
  The definitions are written like bindings
  to the monad operations bind and
  unit (required) and zero and plus (optional)."
  {:arglists '([name ops]
               [name docs ops])}

  ([name ops]
   `(defmonad ~name "" ~ops))

  ([name docs ops]
   (let [_ (assert (not-empty ops) "no monad ops!")]
     `(def ~(with-meta name {:doc docs})
        (merge {:bind nil
                :unit nil
                :zero nil
                :plus nil}
               (into {} (map #(vec %) (partition 2 ~ops))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro domonad

  "Monad comprehension. Takes the name of a monad,
  a vector of steps given as binding-form,
  and a result value specified by body."
  {:arglists '([monad steps]
               [monad steps body])}

  ([monad steps]
   `(domonad ~monad ~steps nil))

  ([monad steps body]
   (let [E (gensym) B (gensym) U (gensym) Z (gensym)]
     `(let [{~B :bind ~U :unit ~Z :zero} ~monad
            ;if no body try to run the zero func,
            ;else run the unit on it.
            ~E #(if (and (nil? %)
                         (some? ~Z)) ~Z (~U %))]
        (czlab.basal.core/run-bind ~B ~steps (~E ~body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end-macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-identity

  "Monad describing plain computations. This monad does nothing
  at all. It is useful for testing, for combination with monad
  transformers, and for code that is parameterized with a monad."

  [:bind (fn [mv mf] (mf mv)) :unit identity])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-maybe

  "Monad describing computations with possible failures. Failure is
  represented by nil, any other value is considered valid. As soon as
  a step returns nil, the whole computation will yield nil as well."

  [:unit identity :bind (fn [mv mf] (if-not (nil? mv) (mf mv)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-list

  "Monad describing multi-valued computations, i.e. computations
  that can yield multiple values. Any object implementing the seq
  protocol can be used as a monadic value."

  [:bind (fn [mv mf] (flatten (map mf mv)))
   :unit (fn_1 (vector ____1))
   :zero []
   :plus (fn_* (flatten ____xs))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-state

  "Monad describing stateful computations. The monadic values have the
  structure (fn [old-state] [result new-state])."

  [:unit (fn [v] (fn [s] [v s]))
   :bind (fn [mv mf]
           (fn [s]
             (let [[v s'] (mv s)] ((mf v) s'))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-continuation

  "Monad describing computations in continuation-passing style. The monadic
  values are functions that are called with a single argument representing
  the continuation of the computation, to which they pass their result."

  [:unit (fn [v] (fn [cont] (cont v)))
   :bind (fn [mv mf]
           (fn [cont]
             (mv (fn [v] ((mf v) cont)))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-cont

  "Execute the computation in the
  cont monad and return its result."
  {:arglists '([cont])}
  [cont]

  (cont identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end-monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kvs->map

  "Turn a list of key values into map."
  {:arglists '([arglist])}
  [arglist]
  {:pre [(even? (count arglist))]}

  (apply array-map arglist))
  ;(into {} (map #(vec %) (partition 2 arglist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-str

  "Repeat string n times."
  {:tag String
   :arglists '([n s])}
  [n s]

  (cs/join "" (repeat n s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num??

  "If n is not a number, return other."
  {:arglists '([n other])}
  [n other]
  {:pre [(number? other)]}

  (if (number? n) n other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn !==

  "Same as (not (== num1 num2))."
  {:arglists '([x]
               [x y]
               [x y & more])}

  ([x] false)

  ([x y]
   (not (== x y)))

  ([x y & more]
   (not (apply == x y more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn flip

  "Invert number if not zero."
  {:arglists '([x])}
  [x]
  {:pre [(number? x)]}

  (if (zero? x) 0 (/ 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn percent

  "Calculate the percentage."
  {:arglists '([numerator denominator])}
  [numerator denominator]

  (* 100.0 (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-seq

  "Split a collection into 2 parts."
  {:arglists '([coll cnt])}
  [coll cnt]

  (if-not (< cnt (count coll))
    (list (concat [] coll) ())
    (list (take cnt coll) (drop cnt coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-asc*

  "Returns a ascending compare function."
  {:arglists '([]
               [f])}

  ([]
   (compare-asc* identity))

  ([f]
   (fn_2 (cond (< (f ____1) (f ____2)) -1
               (> (f ____1) (f ____2)) 1 :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-des*

  "Returns a descending compare function."
  {:arglists '([]
               [f])}

  ([]
   (compare-des* identity))

  ([f]
   (fn_2 (cond (< (f ____1) (f ____2)) 1
               (> (f ____1) (f ____2)) -1 :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xxx-by

  "Used by min-by & max-by. *Internal*"
  {:arglists '([cb coll])}
  [cb coll]

  (if (not-empty coll)
    (reduce cb (_1 coll) (rest coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn min-by

  "Find item with minimum value as defined by the function."
  {:arglists '([f coll])}
  [f coll]

  (xxx-by #(if (< (f %1) (f %2)) %1 %2) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn max-by

  "Find item with maximum value as defined by the function."
  {:arglists '([f coll])}
  [f coll]

  (xxx-by #(if (< (f %1) (f %2)) %2 %1) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-test

  "Assert a test condition, returning a message."
  {:arglists '([cond msg])}
  [cond msg]

  (str (try (if cond t-ok t-bad)
            (catch Throwable _ t-bad)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-thrown

  "Assert an exception is thrown during test."
  {:arglists '([msg]
               [expected error msg])}

  ([msg]
   (ensure-thrown nil nil msg))

  ([expected error msg]
   (str (if (nil? error)
          t-bad
          (if (or (= expected :any)
                  (is? expected error)) t-ok t-bad)) ": " msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rip-fn-name

  "Extract (mangled) name of the function."
  {:tag String
   :arglists '([func])}
  [func]

  (let [s (str func)
        h (cs/index-of s "$")
        s (if h (subs s (+ 1 h)) s)
        p (cs/last-index-of s "@")] (if p (subs s 0 p) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-int

  "Operate like a mutable int, get and set."
  {:arglists '([]
               [arr]
               [arr v]
               [arr op nv])}

  ;setter
  ([^ints arr v]
   (aset arr 0 (int v)) (int v))
  ;ctor
  ([]
   (int-array 1 0))
  ;getter
  ([^ints arr]
   (int (aget arr 0)))
  ;apply (op old-val new-value)
  ([^ints arr op nv]
   (let [v (int (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-int*

  "Create & init mu-i."
  {:arglists '([i])}
  [i]
  {:pre [(number? i)]}

  (doto (mu-int) (mu-int i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-long

  "Operate like a mutable long, get and set."
  {:arglists '([]
               [arr]
               [arr v]
               [arr op nv])}

  ;setter
  ([^longs arr v]
   (aset arr 0 (long v)) (long v))
  ;ctor
  ([]
   (long-array 1 0))
  ;getter
  ([^longs arr]
   (long (aget arr 0)))
  ;apply (op old-val new-val)
  ([^longs arr op nv]
   (let [v (long (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-long*

  "Create & init a mu-long."
  {:arglists '([n])}
  [n]
  {:pre [(number? n)]}

  (doto (mu-long) (mu-long n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nth??

  "Get the nth element, 1-indexed."
  {:arglists '([coll pos])}
  [coll pos]
  {:pre [(> pos 0)]}

  (first (drop (- pos 1) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vargs*

  "Coerce into java array."
  {:arglists '([clazz & args])}
  [clazz & args]

  (vargs clazz args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(defn- interject
  "Run the function on the current field value,
   replacing the key with the returned value.
   function(pojo oldvalue) -> newvalue."
  [pojo field func]
  {:pre [(map? pojo) (fn? func)]}
  (assoc pojo field (func pojo field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scoped-keyword

  "Scope name as a fully-qualified keyword."
  {:arglists '([t])}
  [t]
  {:pre [(string? t)
         (nil? (cs/index-of t "/"))
         (nil? (cs/index-of t ":"))]}

  (keyword (str *ns*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-scoped-keyword?

  "If a scoped keyword?"
  {:arglists '([kw])}
  [kw]

  (and (keyword? kw)
       (cs/includes? (str kw) "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-sign

  "Randomly choose a sign."
  {:arglists '([])}
  []

  (if (even? (rand-int Integer/MAX_VALUE)) 1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-bool

  "Randomly choose a boolean value."
  {:arglists '([])}
  []

  (even? (rand-int Integer/MAX_VALUE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num-sign

  "Find the sign of a number."
  {:arglists '([n])}
  [n]
  {:pre [(number? n)]}

  (cond (> n 0) 1 (< n 0) -1 :else 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->long

  "String as a long value."
  {:tag Long
   :arglists '([s]
               [s dv])}

  ([s]
   (s->long s nil))

  ([s dv]
   (try (Long/parseLong ^String s)
        (catch Throwable _ (if (number? dv) (long dv) (throw _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->int

  "String as an int value."
  {:tag Integer
   :arglists '([s]
               [s dv])}

  ([s]
   (s->int s nil))

  ([s dv]
   (try (Integer/parseInt ^String s)
        (catch Throwable _ (if (number? dv) (int dv) (throw _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->double

  "String as a double value."
  {:tag Double
   :arglists '([s][s dv])}

  ([s]
   (s->double s nil))

  ([s dv]
   (try (Double/parseDouble ^String s)
        (catch Throwable _  (if (number? dv) (double dv) (throw _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->bool

  "String as a boolean value."
  {:arglists '([s])}
  [s]

  (contains? BOOLS (cs/lower-case (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-isa

  "Is child of parent?"
  {:arglists '([reason parent child])}
  [reason parent child]

  (do#true (-> (cond
                 (and (class? parent) (class? child)) (isa? child parent)
                 (or (nil? parent) (nil? child)) false
                 (not (class? parent)) (test-isa reason (class parent) child)
                 (not (class? child)) (test-isa reason parent (class child)))
               (assert (str child " not-isa " parent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-some

  "Object is not null?"
  {:arglists '([reason obj])}
  [reason obj]

  (do#true (assert (some? obj) (str reason " is null."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-cond

  "A true condition?"
  {:arglists '([reason cond])}
  [reason cond]

  (do#true (assert cond (str reason))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-hgl

  "String is not empty?"
  {:arglists '([reason s])}
  [reason s]

  (do#true
    (assert (and (string? s)
                 (not-empty s)) (str reason " is empty."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- test-xxx
  [a b]
  (condp instance? b
    Double  :double
    Long  :long
    Float  :double
    Integer  :long
    (raise! "Allow numbers only!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-pos0
  "Check number is not negative?" test-xxx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-pos
  "Check number is positive?" test-xxx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos0

  :double [reason v]

  (do#true (assert (snneg? v) (str reason " must be >= 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos0

  :long [reason v]

  (do#true (assert (snneg? v) (str reason " must be >= 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos

  :double [reason v]

  (do#true (assert (spos? v) (str reason " must be > 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos

  :long [reason v]

  (do#true (assert (spos? v) (str reason " must be > 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-seq+

  "Check sequence is not empty?"
  {:arglists '([reason v])}
  [reason v]

  (do#true (assert (pos? (count v)) (str reason " must be non empty."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sort-join

  "Sort and concatenate strings."
  {:tag String
   :arglists '([ss]
               [sep ss])}

  ([ss]
   (sort-join "" ss))

  ([sep ss]
   (if (nil? ss) "" (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strip-ns-path

  "Remove the leading colon."
  {:tag String
   :arglists '([path])}
  [path]

  (cs/replace (str path) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol VTableAPI

  "Act like a virtual table."

  (vt-set-proto [_ par] "Hook up parent prototype object.")
  (vt-has? [_ kee] "True if key exists in the hierarchy.")
  (vt-run?? [_ kee arglist]
            [_ kee arglist skip?]
            "Find key.  If function run it else return value.")
  (vt-find?? [_ kee]
             [_ kee skip?]
             "Find this key, if skip? then search from parent."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord VTable []
  VTableAPI
  (vt-set-proto [me par]
    (assoc me :____proto par))
  (vt-find?? [me kee]
    (vt-find?? me kee false))
  (vt-find?? [me kee skip?]
    (let [p (:____proto me)]
      (cond skip?
            (if p (vt-find?? p kee))
            (in? me kee)
            (get me kee)
            :else (if p (vt-find?? p kee)))))
  (vt-has? [me kee]
    (or (in? me kee)
        (if-some [p (:____proto me)] (vt-has? p kee))))
  (vt-run?? [me kee arglist]
    (vt-run?? me kee arglist false))
  (vt-run?? [me kee arglist skip?]
    (let [f (vt-find?? me kee skip?)]
      (if (fn? f) (apply f me arglist) f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn identity-n

  "Like identity but returns positional param.
  e.g. (def get-2nd (identity-n 2)) (get-2nd a b c) => b."
  {:arglists '([n]
               [n index-zero?])}

  ([n]
   (identity-n n false))

  ([n index-zero?]
   {:pre [(number? n)]}
   (fn_* (let [z (count ____xs)
               p (if index-zero? n (- n 1))]
           (assert (and (< p z)
                        (>= p 0))
                   "Index out of bound.") (nth ____xs p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- merge-2

  [a b]

  (if (nil? a)
    b
    (if (nil? b)
      a
      (loop [[z & M] (seq b) tmp (tmap* a)]
        (let [[k vb] z
              va (get a k)]
          (if (nil? z)
            (persist! tmp)
            (recur M
                   (assoc! tmp
                           k
                           (cond (not (in? a k))
                                 vb
                                 (and (map? vb)
                                      (map? va))
                                 (merge-2 va vb)
                                 (and (set? vb)
                                      (set? va))
                                 (ct/union va vb)
                                 :else vb)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn merge+

  "Merge (deep) of clojure data."
  {:arglists '([& more])}
  [& more]

  (cond (empty? more) nil
        (one? more) (_1 more)
        :else (loop [prev nil
                     [a & xs] more]
                (if (empty? xs)
                  (merge-2 prev a)
                  (recur (merge-2 prev a) xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;string stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sreduce<>

  "Reduce with a string-builder."
  {:arglists '([f coll])}
  [f coll]

  `(str (reduce ~f (StringBuilder.) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt

  "Same as string format."
  {:tag String
   :arglists '([f & args])}
  [f & args]

  (apply format f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn char-array??

  "Splits a string into string-char,
  e.g. \"abc\" = [\"a\" \"b\" \"c\"]."
  {:arglists '([s])}
  [s]
  {:pre [(or (nil? s)
             (string? s))]}

  (remove empty? (cs/split s #"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf+

  "StringBuilder concat."
  {:tag StringBuilder
   :arglists '([buf & args])}
  [buf & args]

  (doseq [x args]
    (.append ^StringBuilder buf x)) buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf<>

  "Same as StringBuilder.new"
  {:tag StringBuilder
   :arglists '([]
               [& args])}

  ([]
   (sbf<> ""))

  ([& args]
   (let [s (StringBuilder.)] (apply sbf+ s args) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf-join

  "Append to a string-builder, optionally
  inserting a delimiter if the buffer is not empty."
  {:tag StringBuilder
   :arglists '([buf sep item])}
  [buf sep item]

  (do-with [^StringBuilder buf]
    (when item
      (if (and (!nil? sep)
               (pos? (.length buf)))
        (.append buf sep))
      (.append buf item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbfz

  "Length of the string-buffer."
  {:arglists '([b])}
  [b]

  (.length ^StringBuilder b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nichts?

  "Is string empty?"
  {:arglists '([s])}
  [s]

  (or (nil? s)
      (not (string? s))
      (.isEmpty ^String s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hgl?

  "If string has length?"
  {:arglists '([s])}
  [s]

  (and (string? s) (not (.isEmpty ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn stror

  "If not s then s2"
  {:tag String
   :arglists '([s s2])}
  [s s2]

  (if (nichts? s) s2 s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(defn stror*

  "If not s then s2...etc"
  ^String
  [& args] (loop [[a & more] args]
             (if (or (hgl? a) (empty? more)) a (recur more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro stror*

  "If not s then s2...etc"
  {:arglists '([& args])}
  [& args]

  (let [[a & xs] args]
    (if (empty? xs)
      `(stror nil ~a)
      `(stror ~a (stror* ~@xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lcase

  "Lowercase string safely."
  {:tag String
   :arglists '([s])}
  [s]

  (str (some-> s clojure.string/lower-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ucase

  "Uppercase string safely."
  {:tag String
   :arglists '([s])}
  [s]

  (str (some-> s clojure.string/upper-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#^"[Ljava.lang.Class;"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triml

  "Get rid of unwanted chars from left."
  {:tag String
   :arglists '([src unwantedChars])}
  [^String src ^String unwantedChars]

  (if-not (and (hgl? src)
               (hgl? unwantedChars))
    src
    (loop [len (.length src) pos 0]
      (if-not (and (< pos len)
                   (cs/index-of unwantedChars
                                (.charAt src pos)))
        (subs src pos)
        (recur len (+ 1 pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trimr

  "Get rid of unwanted chars from right."
  {:tag String
   :arglists '([src unwantedChars])}
  [^String src ^String unwantedChars]

  (if-not (and (hgl? src)
               (hgl? unwantedChars))
    src
    (loop [pos (.length src)]
      (if-not (and (pos? pos)
                   (cs/index-of unwantedChars
                                (.charAt src (- pos 1))))
        (subs src 0 pos)
        (recur (- pos 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn includes?

  "If the char is inside the big str?"
  {:arglists '([bigs arg])}
  [^String bigs arg]

  (let [rc (cond (or (nil? arg)
                     (nil? bigs)) false
                 (integer? arg) (int arg)
                 (string? arg) (number? (cs/index-of bigs arg))
                 (is? Character arg) (int (.charValue ^Character arg)))]
    (if-not (number? rc)
      rc
      (>= (.indexOf bigs (int rc)) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro embeds?

  "If sub-str is inside the big str?"
  {:arglists '([bigs s])}
  [bigs s]

  `(czlab.basal.core/includes? ~bigs ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro has-no-case?

  "If sub-str is inside the big str - ignore case?"
  {:arglists '([bigs s])}
  [bigs s]

  `(czlab.basal.core/includes?
     (czlab.basal.core/lcase ~bigs) (czlab.basal.core/lcase ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn index-any

  "If any one char is inside the big str, return the position."
  {:arglists '([bigs chStr])}
  [^String bigs ^String chStr]

  (if-not (and (hgl? bigs)
               (hgl? chStr))
    -1
    (let [rc (some #(cs/index-of bigs %)
                   (.toCharArray chStr))] (if (nil? rc) -1 (int rc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-str

  "Count the times the sub-str appears in the big str."
  {:arglists '([bigs s])}
  [^String bigs ^String s]

  (if-not (and (hgl? s)
               (hgl? bigs))
    0
    (loop [len (.length s)
           total 0 start 0]
      (let [pos (cs/index-of bigs s start)]
        (if (nil? pos)
          total
          (recur len
                 (+ 1 total)
                 (long (+ pos len))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-char

  "Count the times this char appears in the big str."
  {:arglists '([bigs ch])}
  [bigs ch]

  (reduce #(if (= ch %2)
             (+ 1 %1) %1)
          0
          (.toCharArray ^String bigs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sname

  "Safely get the name of this object."
  {:arglists '([n])}
  [n]

  `(str (some-> ~n name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nsb

  "Empty string if obj is null, or obj.toString."
  {:tag String
   :arglists '([obj])}
  [obj]

  (if (keyword? obj) (name obj) (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kw->str

  "Stringify a keyword - no leading colon."
  {:tag String
   :arglists '([k])}
  [k]

  (cs/replace (str k) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->kw

  "Concatenate all args and return it as a keyword."
  {:arglists '([& args])}
  [& args]

  (if-not (empty? args)
    (keyword (apply str args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nsn

  "(null) if obj is null, or obj.toString."
  {:tag String
   :arglists '([obj])}
  [obj]

  (if (nil? obj) "(null)" (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match-char?

  "If this char is inside this set of chars?"
  {:arglists '([ch setOfChars])}
  [ch setOfChars]

  (and (set? setOfChars)
       (contains? setOfChars ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro strim

  "Safely trim this string."
  {:arglists '([s])}
  [s]

  `(str (some-> ~s clojure.string/trim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strim-any

  "Strip source string of these unwanted chars."
  {:tag String
   :arglists '([src unwantedChars]
               [src unwantedChars whitespace?])}

  ([src unwantedChars]
   (strim-any src unwantedChars false))

  ([^String src ^String unwantedChars whitespace?]
   (let [s (-> (if whitespace? (strim src) src)
               (triml unwantedChars)
               (trimr unwantedChars))]
     (if whitespace? (strim s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splunk

  "Split a large string into chunks,
  each chunk having a specific length."
  {:arglists '([largeString chunkLength])}
  [^String largeString chunkLength]

  (if-not (and (hgl? largeString)
               (snneg? chunkLength))
    []
    (mapv #(cs/join "" %1)
          (partition-all chunkLength largeString))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasic-any?

  "If bigs contains any one of these strs - ignore case?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (empty? substrs) (nichts? bigs))
    (let [lc (lcase bigs)]
      (true? (some #(number? (cs/index-of lc (lcase %))) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-any?

  "If bigs contains any one of these strs?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(number? (cs/index-of bigs %)) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasic-all?

  "If bigs contains all of these strs - ignore case?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (empty? substrs) (nichts? bigs))
    (let [lc (lcase bigs)]
      (every? #(number? (cs/index-of lc (lcase %))) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-all?

  "If bigs contains all of these strs?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (every? #(number? (cs/index-of bigs %)) substrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ewic-any?

  "If bigs endsWith any one of the strs, no-case?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (let [lc (lcase bigs)]
      (true? (some #(cs/ends-with? lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ew-any?

  "If bigs endsWith any one of the strs?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(cs/ends-with? bigs %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn swic-any?

  "If bigs startWith any one of the strs - no case?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (let [lc (lcase bigs)]
      (true? (some #(cs/starts-with? lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sw-any?

  "If bigs startWith any one of the strs?"
  {:arglists '([bigs substrs])}
  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(cs/starts-with? bigs %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eqic?

  "Same as String.equalIgnoreCase()?"
  {:arglists '([src other])}
  [src other]

  (let [^String ss src
        ^String oo other] `(.equalsIgnoreCase ~ss ~oo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eqic-any?

  "String.equalIgnoreCase() on any one of the strs?"
  {:arglists '([src substrs])}
  [^String src substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? src)
              (empty? substrs))
    (let [lc (lcase src)]
      (true? (some #(= lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eq-any?

  "If String.equals() on any one of the strs?"
  {:arglists '([src substrs])}
  [^String src substrs]
  {:pre [(sequential? substrs)]}

  (if-not (empty? substrs)
    (true? (some #(.equals src %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrapped?

  "If src string starts with head and ends with tail?"
  {:arglists '([src head tail])}
  [^String src ^String head ^String tail]

  (if (and (hgl? src)
           (hgl? head) (hgl? tail))
    (and (cs/starts-with? src head) (cs/ends-with? src tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rights

  "Get the rightmost len characters of a String."
  {:tag String
   :arglists '([src len])}
  [^String src len]
  {:pre [(number? len)]}

  (if (or (<= len 0)
          (nichts? src))
    ""
    (if (< (.length src) len)
      src
      (subs src (- (.length src) len)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lefts

  "Get the leftmost len characters of a String."
  {:tag String
   :arglists '([src len])}
  [^String src len]
  {:pre [(number? len)]}

  (if (or (<= len 0)
          (nichts? src))
    ""
    (if (< (.length src) len)
      src
      (subs src 0 len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop-head

  "Drop leftmost len characters of a String."
  {:tag String
   :arglists '([src len])}
  [^String src len]
  {:pre [(number? len)]}

  (cond (nichts? src)
        ""
        (<= len 0)
        src
        :else
        (if (< (.length src) len) "" (subs src len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop-tail

  "Drop rightmost len characters of a String."
  {:tag String
   :arglists '([src len])}
  [^String src len]
  {:pre [(number? len)]}

  (cond (nichts? src)
        ""
        (<= len 0)
        src
        :else
        (let [n (.length src)]
          (if (< n len) "" (subs src 0 (- n len))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn matches?

  "Same as String.matches."
  {:arglists '([src regex])}
  [src regex]
  {:pre [(string? regex)]}

  (if (hgl? src) (.matches ^String src ^String regex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split

  "Same as String.split."
  {:arglists '([src regex]
               [src regex limit])}

  ([src regex]
   (split src regex nil))

  ([src regex limit]
   {:pre [(string? regex)]}
   (if (hgl? src)
     (remove empty?
             (if-not (number? limit)
               (.split ^String src ^String regex)
               (.split ^String src ^String regex (int limit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-str

  "Same as String-tokenizer."
  {:arglists '([s sep]
               [s sep incSep?])}

  ([s sep]
   (split-str s sep false))

  ([s sep incSep?]
   (let [t (new java.util.StringTokenizer
                ^String s ^String sep (boolean incSep?))]
     (loop [rc (tvec*)]
       (if-not (.hasMoreTokens t)
         (persist! rc)
         (recur (conj! rc (.nextToken t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn esc-xml

  "Escape XML special chars."
  {:tag String
   :arglists '([s])}
  [s]

  (cs/escape s {\& "&amp;"
                \> "&gt;"
                \< "&lt;"
                \" "&quot;"
                \' "&apos;"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run-test
  [test]
  (let [res (test)]
    [res (every? #(cs/starts-with? % "p") res)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pretty-millis

  "Pretty print milliseconds value."
  {:tag String
   :arglists '([millis])}
  [millis]
  {:pre [(number? millis)]}

  (let [secs (int (/ millis 1000.0))
        millis (int (- millis
                       (* secs 1000.0)))
        mins (int (/ secs 60.0))
        secs (int (- secs
                     (* mins 60.0)))
        hours (int (/ mins 60.0))
        mins (int (- mins
                     (* hours 60.0)))]
    (cond
      (pos? hours) (fmt "%d hours %2d minutes %2d.%03d secs" hours mins secs millis)
      (pos? mins) (fmt "%2d minutes %2d.%03d secs" mins secs millis)
      (pos? secs) (fmt "%2d.%03d seconds" secs millis)
      :else (fmt "0.%03d seconds" millis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prn-test
  [results title diff]
  (let [lsep (repeat-str 78 "+")
        esep (repeat-str 78 "=")
        sum (count results)
        f (filter #(cs/starts-with? % "F") results)
        p (filter #(cs/starts-with? % "p") results)
        ok (count p)
        perc (percent ok sum)]

    (-> lsep ansi/bold-white prn!!)
    (-> "test-case: "
        ansi/bold-white prn!)
    (-> (stror title
               (rip-fn-name test))
        cs/upper-case
        ansi/bold-yellow prn!!)
    (-> (str (Date.))
        ansi/bold-white prn!!)
    (-> lsep ansi/bold-white prn!!)
    (doseq [r p]
      (-> r ansi/bold-green prn!!))
    (doseq [r f]
      (-> r ansi/bold-red prn!!))
    (-> esep ansi/bold-white prn!!)
    (-> (str "Passed: " ok
             "/" sum
             " [" (int perc) "%%]")
        ansi/bold-white prn!!)
    (-> (str "Failed: " (- sum ok))
        ansi/bold-white prn!!)
    (-> (str "cpu-time: "
             (pretty-millis diff))
        ansi/bold-white prn!!)
    (-> "" ansi/white prn!!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clj-test??

  "Run test as a clojure test case."
  {:arglists '([test]
               [test title print?])}

  ([test]
   (clj-test?? test nil true))

  ([test title print?]
   {:pre [(fn? test)]}
   (let [mark (System/currentTimeMillis)
         [res ok?] (run-test test)
         diff (- (System/currentTimeMillis) mark)]
     (if print?
       (prn-test res title diff)) ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Activable
  "Something that can be activated."
  (deactivate [_] "")
  (activate [_] [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Cancelable
  "Something that can be canceled."
  (cancel [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Testable
  "Something that can be tested for validity."
  (is-valid? [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Catchable
  "Something that can catch an Exception."
  (catche [_ e] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Configurable
  "Something that can be configured with key-values."
  (get-conf [_]
            [_ k] "")
  (set-conf [_ arg]
            [_ k v] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Debuggable
  "Something that can be debugged."
  (dbg-str [_] "")
  (dbg-show [_ ^PrintStream out] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Dispatchable
  "Something that can be dispatched, like an observer."
  (add-handler [_ handler] "")
  (remove-handler [_ handler] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Disposable
  "Something that can be disposed."
  (dispose [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Enqueable
  "Something that can be enqueued."
  (put [_ something] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Gettable
  "Something that gives access to key-values."
  (getv [_ key] "")
  (has? [_ key] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Hierarchical
  "Something that has a parent."
  (parent [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Idable
  "Something that can be identified."
  (id [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Initable
  "Something that can be initialized."
  (init [_]
        [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Finzable
  "Something that can be finalized - opposite to initialize."
  (finz [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Interruptable
  "Something that can be interrupted."
  (interrupt [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Schedulable
  "Something that can be scheduled."
  (wakeup [_ arg] "")
  (schedule [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Morphable
  "Something that can be morphed."
  (morph [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Typeable
  "Something that can be identified as a type."
  (typeid [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Openable
  "Something that can be opened."
  (open [_]
        [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Receivable
  "Something that can receive a message."
  (receive [_ msg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Resetable
  "Something that can be reset."
  (reset [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Restartable
  "Something that can be restarted."
  (restart [_]
           [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Sendable
  "Something that can send a message."
  (send [_ msg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Settable
  "Something that supports settable key-values."
  (unsetv [_ key] "")
  (setv [_ key arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Startable
  "Something that can be started."
  (stop [_] "")
  (start [_]
         [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Connectable
  "Something that can be connected."
  (disconnect [_] "")
  (connect [_]
           [_ arg] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Suspendable
  "Something that can be suspended."
  (resume [_] "")
  (suspend [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Triggerable
  "Something that can be triggered."
  (fire [_]
        [_ arg] "")
  (set-trigger [_ t] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Versioned
  "Something that has a version."
  (version [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EOF



