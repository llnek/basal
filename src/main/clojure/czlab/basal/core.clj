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
(def ^{:doc "Log flag used internally."}
  LOG-FLAG (not (.equals "false" (System/getProperty "czlabloggerflag"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private t-bad "FAILED")
(def ^:private t-ok "passed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmacro-

  ^{:arglists '([name & more])
    :doc "Same as defmacro but private."}

  [name & more]
  (list* `defmacro (with-meta name
                              (assoc (meta name)
                                     :private true)) more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defonce-

  ^{:arglists '([name & more])
    :doc "Same as defonce but private."}

  [name & more]
  (list* `defonce (with-meta name
                             (assoc (meta name)
                                    :private true)) more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-

  ^{:arglists '([name & more])
    :doc "Same as def but private."}
  [name & more]

  (list* `def (with-meta name
                         (assoc (meta name)
                                :private true)) more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trace

  ^{:arglists '([& xs])
    :doc "Logging at TRACE level."}

  [& xs]
  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :trace ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro debug

  ^{:arglists '([& xs])
    :doc "Logging at DEBUG level."}

  [& xs]
  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :debug ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro info

  ^{:arglists '([& xs])
    :doc "Logging at INFO level."}

  [& xs]
  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :info ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro warn

  ^{:arglists '([& xs])
    :doc "Logging at WARN level."}

  [& xs]
  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :warn ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro error

  ^{:arglists '([& xs])
    :doc "Logging at ERROR level."}

  [& xs]
  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :error ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fatal

  ^{:arglists '([& xs])
    :doc "Logging at FATAL level."}

  [& xs]
  (and czlab.basal.core/LOG-FLAG
       `(clojure.tools.logging/logf :fatal ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exception

  ^{:arglists '([e])
    :doc "Log an exception at ERROR level."}

  [e]

  (when czlab.basal.core/LOG-FLAG
    (clojure.tools.logging/logf :error e "")
    (clojure.tools.logging/logf :error "%s" "exception thrown.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def BOOLS #{"true" "yes" "on" "ok" "active" "1"})
(def ^String HEX-CHAR-STR "0123456789ABCDEF")
(def ^String hex-char-str "0123456789abcdef")
(def HEX-CHARS (.toCharArray HEX-CHAR-STR))
(def hex-chars (.toCharArray hex-char-str))
(def ^String USASCII "ISO-8859-1")
(def ^String UTF16 "UTF-16")
(def ^String UTF8 "UTF-8")
(def ^String SLASH "/")

(def KiloBytes 1024)
(def BUF-SZ (* 4 KiloBytes))
(def MegaBytes (* KiloBytes KiloBytes))
(def GigaBytes (* KiloBytes KiloBytes KiloBytes))

(def OneK 1024)
(def FourK (* 4 OneK))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro funcit??

  ^{:arglists '([f & args])
    :doc "Maybe run a function."}

  [f & args]
  `(let [f# ~f] (if (fn? f#) (f# ~@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pre

  ^{:arglists '([conds])
    :doc "Like :pre, assert conditions."}

  [& conds]
  `(assert (and ~@conds) "precond failed."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atom?

  ^{:arglists '([x])
    :doc "If obj is an atom?"}

  [x]
  `(instance? clojure.lang.Atom ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n#-even?

  ^{:arglists '([coll])
    :doc "Count of collection even?"}

  [coll] `(even? (count ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !sas?

  ^{:arglists '([proto obj])
    :doc "Same as not-satisfies?"}

  [proto obj] `(not (satisfies? ~proto ~obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sas?

  ^{:arglists '([proto obj])
    :doc "Same as satisfies?"}

  [proto obj] `(satisfies? ~proto ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !is?

  ^{:arglists '([clazz obj])
    :doc "Same as not-instance?"}

  [clazz obj] `(not (instance? ~clazz ~obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro is?

  ^{:arglists '([clazz obj])
    :doc "Same as instance?"}

  [clazz obj] `(instance? ~clazz ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro map->

  ^{:arglists '([& more])
    :doc "Same as into {}."}

  [& more] `(into {} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vec->

  ^{:arglists '([& more])
    :doc "Same as into []."}

  [& more] `(into [] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro set->

  ^{:arglists '([& more])
    :doc "Same as into #{}."}

  [& more] `(into #{} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+1

  ^{:arglists '([a & more])
    :doc "Prepend an item to collection(s)."}

  [a & more] `(concat [~a] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+

  ^{:arglists '([& more])
    :doc "Same as concat."}

  [& more] `(concat ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _2

  ^{:arglists '([coll])
    :doc "Same as second."}

  [coll] `(second ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _1

  ^{:arglists '([coll])
    :doc "Same as first."}

  [coll] `(first ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _3

  ^{:arglists '([coll])
    :doc "Get the 3rd item."}

  [coll] `(nth ~coll 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _E

  ^{:arglists '([coll])
    :doc "Same as last."}

  [coll] `(last ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _NE

  ^{:arglists '([coll])
    :doc "Get the last item via nth."}

  [coll] `(let [x# ~coll] (nth x# (- (count x#) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !zero?

  ^{:arglists '([n])
    :doc "Same as not-zero?"}

  [n] `(not (zero? ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n#

  ^{:arglists '([coll])
    :doc "Same as count."}

  [coll] `(count ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro car

  ^{:arglists '([coll])
    :doc "Same as first."}

  [coll] `(first ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cdr

  ^{:arglists '([coll])
    :doc "Same as rest."}

  [coll] `(rest ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro one?

  ^{:arglists '([coll])
    :doc "If count() is 1?"}

  [coll] `(== 1 (count ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro two?

  ^{:arglists '([coll])
    :doc "If count() is 2?"}

  [coll] `(== 2 (count ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro one+?

  ^{:arglists '([coll])
    :doc "If count() is more than 1?"}

  [coll] `(> (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro or??

  ^{:arglists '([bindings & args])
    :doc "(or?? [= a] b c) => (or (= b a) (= c a))."}

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

  ^{:arglists '([a & args])
    :doc "Mutable dissoc (atom)."}

  [a & args]

  (let [X (gensym)]
    `(let [~X ~a] (swap! ~X dissoc ~@args) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assoc!!

  ^{:arglists '([a & args])
    :doc "Mutable assoc (atom)."}

  [a & args]

  (let [X (gensym)]
    `(let [~X ~a] (swap! ~X assoc ~@args) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_*

  ^{:arglists '([& forms])
    :doc "Wrap code into a fn(...)."}

  [& forms]

  `(fn [& ~'____xs] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_3

  ^{:arglists '([& forms])
    :doc "Wrap code into a fn(a1,a2,a3)."}
  [& forms]

  `(fn [~'____1 ~'____2 ~'____3] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_2

  ^{:arglists '([& forms])
    :doc "Wrap code into a fn(a1,a2)."}

  [& forms]

  `(fn [~'____1 ~'____2] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_1

  ^{:arglists '([& forms])
    :doc "Wrap code into a fn(a1)."}

  [& forms] `(fn [~'____1] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_0

  ^{:arglists '([& forms])
    :doc "Wrap code into a fn()."}

  [& forms] `(fn [] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tset*

  ^{:arglists '([][x])
    :doc "A transient set."}

  ([] `(tset* nil))
  ([x] `(transient (or ~x #{}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tvec*

  ^{:arglists '([][x])
    :doc "A transient vector."}

  ([] `(tvec* nil))
  ([x] `(transient (or ~x []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tmap*

  ^{:arglists '([][x])
    :doc "A transient map."}

  ([] `(tmap* nil))
  ([x] `(transient (or ~x {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro persist!

  ^{:arglists '([coll])
    :doc "Same as persistent!."}

  [coll] `(persistent! ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atomic

  ^{:arglists '([& args])
    :doc "Atomize fields as map."}

  [& args] `(atom (array-map ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mapfv

  ^{:arglists '([op value & forms])
    :doc "Apply a binary-op to the value over the forms.
         e.g. (+ 3 1 (+ 2 2)) => [4 7]."}

  [op value & forms]

  `(vector ~@(map (fn [f] `(~op ~f ~value)) forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro last-index

  ^{:arglists '([coll])
    :doc "count less 1."}

  [coll] `(- (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nexth

  ^{:arglists '([coll i])
    :doc "The nth item after i."}

  [coll i] `(nth ~coll (+ 1 ~i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro chop

  ^{:arglists '([& args])
    :doc "Same as partition."}

  [& args] `(partition ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro inc*

  ^{:arglists '([x])
    :doc "One plus, x+1."}

  [x] `(+ 1 ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dec*

  ^{:arglists '([x])
    :doc "One less, x-1."}

  [x] `(- ~x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#false

  ^{:arglists '([& forms])
    :doc "Do returns false."}

  [& forms] `(do ~@forms false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#true

  ^{:arglists '([& forms])
    :doc "Do returns true."}

  [& forms] `(do ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#nil

  ^{:arglists '([& forms])
    :doc "Do returns nil."}

  [& forms] `(do ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#false

  ^{:arglists '([& forms])
    :doc "Let returns false."}

  [& forms] `(let ~@forms false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#true

  ^{:arglists '([& forms])
    :doc "Let returns true."}

  [& forms] `(let ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#nil

  ^{:arglists '([& forms])
    :doc "Let returns nil."}

  [& forms] `(let ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defenum

  ^{:arglists '([ename & args])
    :doc "Enum definition.
         e.g. (defenum ^:private xyz a 1 b c) will generate
         (def ^:private xyz-a 1)
         (def ^:private xyz-b 2)
         (def ^:private xyz-c 3)."}

  [ename & args]

  (let [[e1 n] (take 2 args)
        mm (meta ename)
        more (concat [e1] (drop 2 args))]
    (assert (number? n)
            "Enum expecting a number.")
    `(do ~@(loop [v n [m & ms] more out []]
             (if (nil? m)
               out
               (let [z (str (name ename)
                            "-" (name m))]
                 (recur (+ 1 v)
                        ms
                        (conj out
                              `(def ~(with-meta (symbol z) mm) ~v)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro condp??

  ^{:arglists '([pred expr & clauses])
    :doc "condp without throwing exception."}

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

  ^{:arglists '([expr & clauses])
    :doc "case without throwing exception."}

  [expr & clauses]

  (let [c (count clauses)
        xs (if-not (even? c)
             clauses
             (concat clauses ['nil]))] `(case ~expr ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-xxx??

  ^{:arglists '([F bindings then]
                [F bindings then else]) :no-doc true}

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

  ^{:arglists '([clazz expr then]
                [clazz expr then else])
    :doc "If expr is instance of class, execute `then`, otherwise `else`."}

  ([clazz expr then]
   `(if-inst ~clazz ~expr ~then nil))

  ([clazz expr then else & oldform]
   `(if (instance? ~clazz ~expr) ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-inst

  ^{:arglists '([clazz expr & body])
    :doc "If expr is instance of class, execute `body`."}

  [clazz expr & body]
  `(if (instance? ~clazz ~expr) (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-proto

  ^{:arglists '([proto expr then]
                [proto expr then else])
    :doc "If expr satisfies the protocol, execute `then`, otherwise `else`."}

  ([proto expr then]
   `(if-proto ~proto ~expr ~then nil))

  ([proto expr then else & oldform]
   `(if (satisfies? ~proto ~expr) ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-proto

  ^{:arglists '([proto expr & body])
    :doc "If expr satisfies the protocol, execute `body`."}

  [proto expr & body]
  `(if (satisfies? ~proto ~expr) (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-nil

  ^{:arglists '([expr then]
                [expr then else])
    :doc "If expr evals to nil, execute `then`, otherwise `else`."}

  ([expr then]
   `(if-nil ~expr ~then nil))

  ([expr then else & oldform]
   `(if (nil? ~expr) ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-nil

  ^{:arglists '([expr & body])
    :doc "If expr evals to nil, execute `body`."}

  [expr & body]
  `(if (nil? ~expr) (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-throwable?

  ^{:arglists '([x])
    :doc "If object is a Throwable?"}

  [x] (instance? Throwable x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-throw

  ^{:arglists '([bindings then]
                [bindings then else])
    :doc "bindings => binding-form test
         If test is a Throwable, evaluates 'then'
         with binding-form bound to the value of test."}

  ([bindings then]
   `(if-throw ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx??
      czlab.basal.core/is-throwable? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-throw

  ^{:arglists '([bindings & body])
    :doc "bindings => binding-form test
         If test is a Throwable, evaluates the body."}

  [bindings & body]

  `(if-throw ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-var

  ^{:arglists '([bindings then]
                [bindings then else])
    :doc "bindings => binding-form test
         If test is a var, evaluates 'then'
         with binding-form bound to the value of test."}

  ([bindings then]
   `(if-var ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? var? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-var

  ^{:arglists '([bindings & body])
    :doc "bindings => binding-form test
         If test is a var, evaluates the body."}

  [bindings & body]

  `(if-var ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-number

  ^{:arglists '([bindings then]
                [bindings then else])
    :doc "bindings => binding-form test
         If test is a number, evaluates 'then'
         with binding-form bound to the value of test."}

  ([bindings then]
   `(if-number ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? number? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-number

  ^{:arglists '([bindings & body])
    :doc "bindings => binding-form test
         If test is a number, evaluates the body."}

  [bindings & body]

  `(if-number ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-string

  ^{:arglists '([bindings then]
                [bindings then else])
    :doc "bindings => binding-form test
         If test is a string, evaluates 'then'
         with binding-form bound to the value of test."}

  ([bindings then]
   `(if-string ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? string? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-string

  ^{:arglists '([bindings & body])
    :doc "bindings => binding-form test
         If test is a string, evaluates the body."}

  [bindings & body]

  `(if-string ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nloop

  ^{:arglists '([n & forms])
    :doc "Loop over code n times."}

  [n & forms] (let [x (gensym)]
                `(dotimes [~x ~n] ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each*

  ^{:arglists '([func coll])
    :doc "Evals function for each element, indexed."}

  [func coll]

  (let [C (gensym) I (gensym) T (gensym)]
    `(let [~C ~coll ~T (count ~C)]
       (dotimes [~I ~T] (~func (nth ~C ~I) ~I)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each

  ^{:arglists '([func coll])
    :doc "Evals function for each element."}

  [func coll] (let [I (gensym)]
                `(doseq [~I ~coll] (~func ~I))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mget

  ^{:arglists '([m k])
    :doc "Java map.get()."}

  [m k]

  `(.get ~(with-meta m {:tag 'java.util.Map}) ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mdel!

  ^{:arglists '([m k])
    :doc "Java map.remove()."}

  [m k]

  (.remove ^java.util.Map m k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mput!

  ^{:arglists '([m k v])
    :doc "Java map.put()."}

  [m k v]

  `(.put ~(with-meta m {:tag 'java.util.Map}) ~k ~v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deftest

  ^{:arglists '([name & body])
    :doc "A bunch of test-cases grouped together."}

  [name & body]

  `(def ~name (fn [] (filter #(not (nil? %)) [~@body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??

  ^{:arglists '([msg form])
    :doc "Assert test was ok."}

  [msg form]
  `(czlab.basal.core/ensure-test ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure-thrown??

  ^{:arglists '([msg expected form])
    :doc "Assert an error was thrown."}

  [msg expected form]

  `(try ~form
        (czlab.basal.core/ensure-thrown ~msg)
        (catch Throwable e#
          (czlab.basal.core/ensure-thrown ~expected e# ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prn!!

  ^{:arglists '([fmt & args])
    :doc "Println with format."}

  [fmt & args] `(println (format ~fmt ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prn!

  ^{:arglists '([fmt & args])
    :doc "Print with format."}

  [fmt & args] `(print (format ~fmt ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
  (defmacro- make-fn
  "Hack to wrap a macro as fn
  so one can use *apply* on it."
  [m] `(fn [& xs#] (eval (cons '~m xs#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro object<>

  ^{:arglists '([z][z m][z ab & more])
    :doc "Create a new record instance.
         e.g. (object<> ClazzA)
              (object<> ClazzA {:a 1})
              (object<> ClazzA :a 1 :b 2)"}

  ([z] `(new ~z))
  ([z m] `(merge (new ~z) ~m))
  ([z a b & more] `(assoc (new ~z) ~a ~b ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vargs

  ^{:arglists '([z arglist])
    :doc "Coerce into java array of type z."}

  [z arglist] `(into-array ~z ~arglist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<map>

  ^{:arglists '([f coll])
    :doc "Reduce with a transient map accumulator, returning a map."}

  [f coll] `(persistent! (reduce ~f (transient {}) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<set>

  ^{:arglists '([f coll])
    :doc "Reduce with a transient set accumulator, returning a set."}

  [f coll] `(persistent! (reduce ~f (transient #{}) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<vec>

  ^{:arglists '([f coll])
    :doc "Reduce with a transient vec accumulator, returning a vec."}

  [f coll] `(persistent! (reduce ~f (transient []) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rset!

  ^{:arglists '([a][a value])
    :doc "Reset a atom."}

  ([a] `(rset! ~a nil))
  ([a value] `(reset! ~a ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro exp!

  ^{:arglists '([E & args])
    :doc "Create an exception of type E."}

  [E & args] `(new ~E ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trap!

  ^{:arglists '([E & args])
    :doc "Throw exception of type E."}

  [E & args] `(throw (exp! ~E ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro raise!

  ^{:arglists '([fmt & args])
    :doc "Throw java Exception with message."}

  [fmt & args]
  `(throw (new java.lang.Exception (str (format ~fmt ~@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-assert-exp

  ^{:arglists '([name E])
    :doc "Generate a function which when called, will
         assert the condition and if false, throw the desired exception."}

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

  ^{:arglists '([name E])
    :doc "Generate a function which when called,
         will throw the desired exception."}

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

  ^{:arglists '([cond E & args])
    :doc "Like assert, but throws a specific exception."}

  [cond E & args] `(if-not ~cond (trap! ~E ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with

  ^{:arglists '([bindings & forms])
    :doc "bindings => [symbol init-expr]
         Evals the body in a context in which the symbol is always the
         returned value."}

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

  ^{:arglists '([bindings & forms])
    :doc "bindings => [symbol init-expr]
         Eval the body in a context in which the symbol is always the
         returned value to-string'ed.
         e.g. (do-with-str [a (f)] ... (str a))."}

  [bindings & forms]

  `(str (czlab.basal.core/do-with ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-atom

  ^{:arglists '([bindings & forms])
    :doc "bindings => [symbol init-expr]
         Eval the body in a context in which the symbol is always the
         returned value deref'ed.
         e.g. (do-with-atom [a (atom 0)] ... (deref a))."}

  [bindings & forms]

  `(deref (czlab.basal.core/do-with ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bool!

  ^{:arglists '([x])
    :doc "Same as boolean."} [x] `(boolean ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trye!

  ^{:arglists '([value & forms])
    :doc "Eat exception and return a value."}

  [value & forms]
  `(try ~@forms (catch Throwable ~'_ ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!

  ^{:arglists '([& forms])
    :doc "Eat exception and return nil."}

  [& forms] `(czlab.basal.core/trye! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+

  ^{:arglists '([bindings then]
                [bindings then else])
    :doc "bindings => [binding-form test].
         When test is not empty, evaluates body
         with binding-form bound to the value of test."}

  ([bindings then]
   `(if-some+ ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? empty? ~bindings ~else ~then)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+

  ^{:arglists '([bindings & body])
    :doc "bindings => [binding-form test].
         When test is not empty, evaluates body
         with binding-form bound to the value of test."}

  [bindings & body]
  `(czlab.basal.core/if-some+ ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-fn

  ^{:arglists '([bindings then]
                [bindings then else])
    :doc "bindings => [binding-form test].
         When test is a fn?, evaluates body
         with binding-form bound to the value of test."}

  ([bindings then]
   `(if-fn ~bindings ~then nil))

  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? fn? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-fn

  ^{:arglists '([bindings & body])
    :doc "bindings => [binding-form test].
         When test is a fn?, evaluates body
         with binding-form bound to the value of test."}

  [bindings & body]
  `(czlab.basal.core/if-fn ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doto->>

  ^{:arglists '([x & forms])
    :doc "Combine doto and ->>."}

  [x & forms]

  (let [X (gensym)]
    `(let [~X ~x]
       ~@(map (fn [f] (if (seq? f)
                        `(~@f ~X) `(~f ~X))) forms) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doto->

  ^{:arglists '([x & forms])
    :doc "Combine doto and ->."}

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

  ^{:arglists '([a b])
    :doc "Use java's .equals method."} [a b] `(.equals ~a ~b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !eq?

  ^{:arglists '([a b])
    :doc "Use java's .equals method."} [a b] `(not (.equals ~a ~b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !=?

  ^{:arglists '([& more])
    :doc "Same as not-identical?"}

  [& more] `(not (identical? ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro =?

  ^{:arglists '([& more])
    :doc "Same as identical?"}

  [& more] `(identical? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !in?

  ^{:arglists '([& more])
    :doc "Same as not-contains?"} [& more] `(not (contains? ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro in?

  ^{:arglists '([& more])
    :doc "Same as contains?"} [& more] `(contains? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;had to use this trick to prevent reflection warning
(defmacro cast?

  ^{:arglists '([someType obj])
    :doc "Cast object of this type else nil."}

  [someType obj]

  (let [X (gensym)]
    `(let [~X ~obj]
       (if (instance? ~someType ~X)
         ^{:tag ~someType} (identity (.cast ~someType ~X))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cexp?

  ^{:arglists '([e])
    :doc "Try casting to Throwable."} [e] `(cast? Throwable ~e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !nil?

  ^{:arglists '([x])
    :doc "Same as not-nil."} [x] `(not (nil? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rnil

  ^{:arglists '([coll])
    :doc "Skip nil(s) in collection."} [coll] `(remove nil? ~coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rnilv

  ^{:arglists '([coll])
    :doc "Same as rnil but returns a vec."}

  [coll]
  `(into [] (remove nil? ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro szero?

  ^{:arglists '([e])
    :doc "Safe zero?"}

  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(zero? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sneg?

  ^{:arglists '([e])
    :doc "Safe neg?"}

  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(neg? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro spos?

  ^{:arglists '([e])
    :doc "Safe pos?"}

  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(pos? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro snneg?

  ^{:arglists '([e])
    :doc "Safe not neg?"}

  [e]

  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)
                       (or (zero? ~E)(pos? ~E))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !false?

  ^{:arglists '([x])
    :doc "Same as not-false."} [x] `(not (false? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !true?

  ^{:arglists '([x])
    :doc "Same as not true."} [x] `(not (true? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assert-not

  ^{:arglists '([cond])
    :doc "Assert a false condition."} [cond] `(assert (not ~cond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro marray

  ^{:arglists '([Z n])
    :doc "n-size java array of type Z."} [Z n] `(make-array ~Z ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zarray

  ^{:arglists '([Z])
    :doc "0-size java array of type Z."}[Z] `(make-array ~Z 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl**

  ^{:arglists '([parent & args])
    :doc "Make a virtual table:
         op1 f1 op2 f2, with parent vtbl."}

  [parent & args]

  (assert (even? (count args)))
  `(czlab.basal.core/object<>
     ~'czlab.basal.core.VTable :____proto ~parent ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl*

  ^{:arglists '([& args])
    :doc "Make a virtual table: op1 f1 op2 f2."}

  [& args] `(vtbl** nil ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro wo*

  ^{:arglists '([bindings & forms])
    :doc "Same as with-open."}

  [bindings & forms] `(with-open ~bindings ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro wm*

  ^{:arglists '([obj m])
    :doc "Same as with-meta."} [obj m] `(with-meta ~obj ~m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ->str

  ^{:arglists '([obj])
    :doc "Same as java's .toString."} [obj] `(some-> ~obj .toString))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run-bind

  ^{:arglists '([binder steps expr])
    :no-doc true
    :Xdoc "Run the bind operator. *Internal*"}

  [binder steps expr]

  (let [more (drop 2 steps)
        [a1 mv] (take 2 steps)]
    `(~binder ~mv
              (fn [~a1]
                ~(if (not-empty more)
                   `(run-bind ~binder ~more ~expr) `(do ~expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmonad

  ^{:arglists '([name ops][name docs ops])
    :doc "Define a named monad by defining the monad operations.
         The definitions are written like bindings
         to the monad operations bind and
         unit (required) and zero and plus (optional)."}

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

  ^{:arglists '([monad steps][monad steps body])
    :doc "Monad comprehension. Takes the name of a monad,
         a vector of steps given as binding-form,
         and a result value specified by body."}

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

  ^{:arglists '([cont])
    :doc "Execute the computation in the
         cont monad and return its result."} [cont] (cont identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end-monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kvs->map

  ^{:arglists '([arglist])
    :doc "Turn a list of key values into map."}

  [arglist]
  {:pre [(even? (count arglist))]} (apply array-map arglist))
  ;(into {} (map #(vec %) (partition 2 arglist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-str

  ^{:arglists '([n s])
    :tag String
    :doc "Repeat string n times."} [n s] (cs/join "" (repeat n s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num??

  ^{:arglists '([n other])
    :doc "If n is not a number, return other."}

  [n other]
  {:pre [(number? other)]} (if (number? n) n other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn !==

  ^{:arglists '([x][x y][x y & more])
    :doc "Same as (not (== num1 num2))."}

  ([x] false)
  ([x y] (not (== x y)))
  ([x y & more]
   (not (apply == x y more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn flip

  ^{:arglists '([x])
    :doc "Invert number if not zero."}

  [x] {:pre [(number? x)]} (if (zero? x) 0 (/ 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn percent

  ^{:arglists '([numerator denominator])
    :doc "Calculate the percentage."}

  [numerator denominator]
  (* 100.0 (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-seq

  ^{:arglists '([coll cnt])
    :doc "Split a collection into 2 parts."}

  [coll cnt]

  (if-not (< cnt (count coll))
    (list (concat [] coll) ())
    (list (take cnt coll) (drop cnt coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-asc*

  ^{:arglists '([][f])
    :doc "Returns a ascending compare function."}

  ([]
   (compare-asc* identity))

  ([f]
   (fn_2 (cond (< (f ____1) (f ____2)) -1
               (> (f ____1) (f ____2)) 1 :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-des*

  ^{:arglists '([][f])
    :doc "Returns a descending compare function."}

  ([]
   (compare-des* identity))

  ([f]
   (fn_2 (cond (< (f ____1) (f ____2)) 1
               (> (f ____1) (f ____2)) -1 :else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xxx-by

  ^{:arglists '([cb coll])
    :no-doc true
    :Xdoc "Used by min-by & max-by. *Internal*"}

  [cb coll]

  (if (not-empty coll)
    (reduce cb (_1 coll) (rest coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn min-by

  ^{:arglists '([f coll])
    :doc "Find item with minimum value as defined by the function."}

  [f coll]
  (xxx-by #(if (< (f %1) (f %2)) %1 %2) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn max-by

  ^{:arglists '([f coll])
    :doc "Find item with maximum value as defined by the function."}

  [f coll]
  (xxx-by #(if (< (f %1) (f %2)) %2 %1) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-test

  ^{:arglists '([cond msg])
    :doc "Assert a test condition, returning a message."}

  [cond msg]
  (str (try (if cond t-ok t-bad)
            (catch Throwable _ t-bad)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-thrown

  ^{:arglists '([msg][expected error msg])
    :doc "Assert an exception is thrown during test."}

  ([msg]
   (ensure-thrown nil nil msg))

  ([expected error msg]
   (str (if (nil? error)
          t-bad
          (if (or (= expected :any)
                  (is? expected error)) t-ok t-bad)) ": " msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rip-fn-name

  ^{:arglists '([func])
    :tag String
    :doc "Extract (mangled) name of the function."}

  [func]

  (let [s (str func)
        h (cs/index-of s "$")
        s (if h (subs s (+ 1 h)) s)
        p (cs/last-index-of s "@")] (if p (subs s 0 p) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-int

  ^{:arglists '([][arr][arr v][arr op nv])
    :doc "Operate like a mutable int, get and set."}

  ;setter
  ([^ints arr v] (aset arr 0 (int v)) (int v))
  ;ctor
  ([] (int-array 1 0))
  ;getter
  ([^ints arr] (int (aget arr 0)))
  ;apply (op old-val new-value)
  ([^ints arr op nv]
   (let [v (int (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-int*

  ^{:arglists '([i])
    :doc "Create & init mu-i."}

  [i]
  {:pre [(number? i)]} (doto (mu-int) (mu-int i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-long

  ^{:arglists '([][arr][arr v][arr op nv])
    :doc "Operate like a mutable long, get and set."}

  ;setter
  ([^longs arr v] (aset arr 0 (long v)) (long v))
  ;ctor
  ([] (long-array 1 0))
  ;getter
  ([^longs arr] (long (aget arr 0)))
  ;apply (op old-val new-val)
  ([^longs arr op nv]
   (let [v (long (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mu-long*

  ^{:arglists '([n])
    :doc "Create & init a mu-long."}

  [n]
  {:pre [(number? n)]} (doto (mu-long) (mu-long n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nth??

  ^{:arglists '([coll pos])
    :doc "Get the nth element, 1-indexed."}

  [coll pos] {:pre [(> pos 0)]} (first (drop (- pos 1) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vargs*

  ^{:arglists '([clazz & args])
    :doc "Coerce into java array."} [clazz & args] (vargs clazz args))

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

  ^{:arglists '([t])
    :doc "Scope name as a fully-qualified keyword."}

  [t]
  {:pre [(string? t)
         (nil? (cs/index-of t "/"))
         (nil? (cs/index-of t ":"))]} (keyword (str *ns*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-scoped-keyword?

  ^{:arglists '([kw])
    :doc "If a scoped keyword?"}

  [kw] (and (keyword? kw)
            (cs/includes? (str kw) "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-sign

  ^{:arglists '([])
    :doc "Randomly choose a sign."}

  [] (if (even? (rand-int Integer/MAX_VALUE)) 1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-bool

  ^{:arglists '([])
    :doc "Randomly choose a boolean value."}

  [] (even? (rand-int Integer/MAX_VALUE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num-sign

  ^{:arglists '([n])
    :doc "Find the sign of a number."}

  [n]
  {:pre [(number? n)]}
  (cond (> n 0) 1 (< n 0) -1 :else 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->long

  ^{:arglists '([s][s dv])
    :tag Long
    :doc "String as a long value."}

  ([s]
   (s->long s nil))

  ([s dv]
   (try (Long/parseLong ^String s)
        (catch Throwable _ (if (number? dv) (long dv) (throw _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->int

  ^{:arglists '([s][s dv])
    :doc "String as an int value."}

  {:tag Integer}

  ([s]
   (s->int s nil))

  ([s dv]
   (try (Integer/parseInt ^String s)
        (catch Throwable _ (if (number? dv) (int dv) (throw _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->double

  ^{:arglists '([s][s dv])
    :doc "String as a double value."}

  {:tag Double}

  ([s]
   (s->double s nil))

  ([s dv]
   (try (Double/parseDouble ^String s)
        (catch Throwable _  (if (number? dv) (double dv) (throw _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->bool

  ^{:arglists '([s])
    :doc "String as a boolean value."}

  [s] (contains? BOOLS (cs/lower-case (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-isa

  ^{:arglists '([reason parent child])
    :doc "Is child of parent?"}

  [reason parent child]

  (do#true (-> (cond
                 (and (class? parent) (class? child)) (isa? child parent)
                 (or (nil? parent) (nil? child)) false
                 (not (class? parent)) (test-isa reason (class parent) child)
                 (not (class? child)) (test-isa reason parent (class child)))
               (assert (str child " not-isa " parent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-some

  ^{:arglists '([reason obj])
    :doc "Object is not null?"}

  [reason obj]
  (do#true (assert (some? obj) (str reason " is null."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-cond

  ^{:arglists '([reason cond])
    :doc "A true condition?"}

  [reason cond]
  (do#true (assert cond (str reason))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-hgl

  ^{:arglists '([reason s])
    :doc "String is not empty?"}

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

  ^{:arglists '([reason v])
    :doc "Check sequence is not empty?"}

  [reason v]

  (do#true (assert (pos? (count v)) (str reason " must be non empty."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sort-join

  ^{:arglists '([ss][sep ss])
    :doc "Sort and concatenate strings."}

  {:tag String}

  ([ss]
   (sort-join "" ss))

  ([sep ss]
   (if (nil? ss) "" (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strip-ns-path

  ^{:arglists '([path])
    :tag String
    :doc "Remove the leading colon."}

  [path] (cs/replace (str path) #"^:" ""))

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

  ^{:arglists '([n][n index-zero?])
    :doc "Like identity but returns positional param.
         e.g. (def get-2nd (identity-n 2)) (get-2nd a b c) => b."}

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

  ^{:arglists '([& more])
    :doc "Merge (deep) of clojure data."}

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

  ^{:arglists '([f coll])
    :doc "Reduce with a string-builder."}

  [f coll] `(str (reduce ~f (StringBuilder.) ~coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt

  ^{:arglists '([f & args])
    :tag String
    :doc "Same as string format."}

  [f & args] (apply format f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn char-array??

  ^{:arglists '([s])
    :doc "Splits a string into string-char,
         e.g. \"abc\" = [\"a\" \"b\" \"c\"]."}

  [s]
  {:pre [(or (nil? s)
             (string? s))]}

  (remove empty? (cs/split s #"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf+

  ^{:arglists '([buf & args])
    :tag StringBuilder
    :doc "StringBuilder concat."}

  [buf & args]

  (doseq [x args]
    (.append ^StringBuilder buf x)) buf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf<>

  ^{:arglists '([][& args])
    :doc "Same as StringBuilder.new"}

  {:tag StringBuilder}

  ([]
   (sbf<> ""))

  ([& args]
   (let [s (StringBuilder.)] (apply sbf+ s args) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbf-join

  ^{:arglists '([buf sep item])
    :tag StringBuilder
    :doc "Append to a string-builder, optionally
         inserting a delimiter if the buffer is not empty."}

  [buf sep item]

  (do-with [^StringBuilder buf]
    (when item
      (if (and (!nil? sep)
               (pos? (.length buf)))
        (.append buf sep))
      (.append buf item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sbfz

  ^{:arglists '([b])
    :doc "Length of the string-buffer."}

  [b] (.length ^StringBuilder b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nichts?

  ^{:arglists '([s])
    :doc "Is string empty?"}

  [s]

  (or (nil? s)
      (not (string? s))
      (.isEmpty ^String s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hgl?

  ^{:arglists '([s])
    :doc "If string has length?"}

  [s] (and (string? s)
           (not (.isEmpty ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn stror

  ^{:arglists '([s s2])
    :tag String
    :doc "If not s then s2"}

  [s s2] (if (nichts? s) s2 s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(defn stror*

  "If not s then s2...etc"
  ^String
  [& args] (loop [[a & more] args]
             (if (or (hgl? a) (empty? more)) a (recur more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro stror*

  ^{:arglists '([& args])
    :doc "If not s then s2...etc"}

  [& args]

  (let [[a & xs] args]
    (if (empty? xs)
      `(stror nil ~a)
      `(stror ~a (stror* ~@xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lcase

  ^{:arglists '([s])
    :tag String
    :doc "Lowercase string safely."}

  [s] (str (some-> s clojure.string/lower-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ucase

  ^{:arglists '([s])
    :tag String
    :doc "Uppercase string safely."}

  [s] (str (some-> s clojure.string/upper-case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#^"[Ljava.lang.Class;"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triml

  ^{:arglists '([src unwantedChars])
    :tag String
    :doc "Get rid of unwanted chars from left."}

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

  ^{:arglists '([src unwantedChars])
    :tag String
    :doc "Get rid of unwanted chars from right."}

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

  ^{:arglists '([bigs arg])
    :doc "If the char is inside the big str?"}

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

  ^{:arglists '([bigs s])
    :doc "If sub-str is inside the big str?"}

  [bigs s] `(czlab.basal.core/includes? ~bigs ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro has-no-case?

  ^{:arglists '([bigs s])
    :doc "If sub-str is inside the big str - ignore case?"}

  [bigs s]

  `(czlab.basal.core/includes?
     (czlab.basal.core/lcase ~bigs) (czlab.basal.core/lcase ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn index-any

  ^{:arglists '([bigs chStr])
    :doc "If any one char is inside the big str, return the position."}

  [^String bigs ^String chStr]

  (if-not (and (hgl? bigs)
               (hgl? chStr))
    -1
    (let [rc (some #(cs/index-of bigs %)
                   (.toCharArray chStr))] (if (nil? rc) -1 (int rc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-str

  ^{:arglists '([bigs s])
    :doc "Count the times the sub-str appears in the big str."}

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

  ^{:arglists '([bigs ch])
    :doc "Count the times this char appears in the big str."}

  [bigs ch]

  (reduce #(if (= ch %2)
             (+ 1 %1) %1)
          0
          (.toCharArray ^String bigs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sname

  ^{:arglists '([n])
    :doc "Safely get the name of this object."}

  [n] `(str (some-> ~n name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nsb

  ^{:arglists '([obj])
    :tag String
    :doc "Empty string if obj is null, or obj.toString"}

  [obj] (if (keyword? obj) (name obj) (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn kw->str

  ^{:arglists '([k])
    :tag String
    :doc "Stringify a keyword - no leading colon."}

  [k] (cs/replace (str k) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->kw

  ^{:arglists '([& args])
    :doc "Concatenate all args and return it as a keyword."}

  [& args] (if-not (empty? args)
             (keyword (apply str args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nsn

  ^{:arglists '([obj])
    :tag String
    :doc "(null) if obj is null, or obj.toString"}

  [obj] (if (nil? obj) "(null)" (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match-char?

  ^{:arglists '([ch setOfChars])
    :doc "If this char is inside this set of chars?"}

  [ch setOfChars]

  (and (set? setOfChars)
       (contains? setOfChars ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro strim

  ^{:arglists '([s])
    :doc "Safely trim this string."}

  [s] `(str (some-> ~s clojure.string/trim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strim-any

  ^{:arglists '([src unwantedChars]
                [src unwantedChars whitespace?])
    :doc "Strip source string of these unwanted chars."}

  {:tag String}

  ([src unwantedChars]
   (strim-any src unwantedChars false))

  ([^String src ^String unwantedChars whitespace?]
   (let [s (-> (if whitespace? (strim src) src)
               (triml unwantedChars)
               (trimr unwantedChars))]
     (if whitespace? (strim s) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splunk

  ^{:arglists '([largeString chunkLength])
    :doc "Split a large string into chunks,
         each chunk having a specific length."}

  [^String largeString chunkLength]

  (if-not (and (hgl? largeString)
               (snneg? chunkLength))
    []
    (mapv #(cs/join "" %1)
          (partition-all chunkLength largeString))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasic-any?

  ^{:arglists '([bigs substrs])
    :doc "If bigs contains any one of these strs - ignore case?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (empty? substrs) (nichts? bigs))
    (let [lc (lcase bigs)]
      (true? (some #(number? (cs/index-of lc (lcase %))) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-any?

  ^{:arglists '([bigs substrs])
    :doc "If bigs contains any one of these strs?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(number? (cs/index-of bigs %)) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasic-all?

  ^{:arglists '([bigs substrs])
    :doc "If bigs contains all of these strs - ignore case?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (empty? substrs) (nichts? bigs))
    (let [lc (lcase bigs)]
      (every? #(number? (cs/index-of lc (lcase %))) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-all?

  ^{:arglists '([bigs substrs])
    :doc "If bigs contains all of these strs?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (every? #(number? (cs/index-of bigs %)) substrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ewic-any?

  ^{:arglists '([bigs substrs])
    :doc "If bigs endsWith any one of the strs, no-case?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (let [lc (lcase bigs)]
      (true? (some #(cs/ends-with? lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ew-any?

  ^{:arglists '([bigs substrs])
    :doc "If bigs endsWith any one of the strs?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(cs/ends-with? bigs %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn swic-any?

  ^{:arglists '([bigs substrs])
    :doc "If bigs startWith any one of the strs - no case?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (let [lc (lcase bigs)]
      (true? (some #(cs/starts-with? lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sw-any?

  ^{:arglists '([bigs substrs])
    :doc "If bigs startWith any one of the strs?"}

  [^String bigs substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? bigs)
              (empty? substrs))
    (true? (some #(cs/starts-with? bigs %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eqic?

  ^{:arglists '([src other])
    :doc "Same as String.equalIgnoreCase()?"}

  [src other]

  (let [^String ss src
        ^String oo other] `(.equalsIgnoreCase ~ss ~oo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eqic-any?

  ^{:arglists '([src substrs])
    :doc "String.equalIgnoreCase() on any one of the strs?"}

  [^String src substrs]
  {:pre [(sequential? substrs)]}

  (if-not (or (nichts? src)
              (empty? substrs))
    (let [lc (lcase src)]
      (true? (some #(= lc (lcase %)) substrs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eq-any?

  ^{:arglists '([src substrs])
    :doc "If String.equals() on any one of the strs?"}

  [^String src substrs]
  {:pre [(sequential? substrs)]}

  (if-not (empty? substrs)
    (true? (some #(.equals src %) substrs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrapped?

  ^{:arglists '([src head tail])
    :doc "If src string starts with head and ends with tail?"}

  [^String src ^String head ^String tail]

  (if (and (hgl? src)
           (hgl? head) (hgl? tail))
    (and (cs/starts-with? src head) (cs/ends-with? src tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rights

  ^{:arglists '([src len])
    :tag String
    :doc "Get the rightmost len characters of a String."}

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

  ^{:arglists '([src len])
    :tag String
    :doc "Get the leftmost len characters of a String."}

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

  ^{:arglists '([src len])
    :tag String
    :doc "Drop leftmost len characters of a String."}

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

  ^{:arglists '([src len])
    :tag String
    :doc "Drop rightmost len characters of a String."}

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

  ^{:arglists '([src regex])
    :doc "Same as String.matches."}

  [src regex]
  {:pre [(string? regex)]}
  (if (hgl? src) (.matches ^String src ^String regex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split

  ^{:arglists '([src regex]
                [src regex limit])
    :doc "Same as String.split."}

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

  ^{:arglists '([s sep]
                [s sep incSep?])
    :doc "Same as String-tokenizer."}

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

  ^{:arglists '([s])
    :tag String
    :doc "Escape XML special chars."}

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
(defn- prn-test
  [results title]
  (let [mark (System/currentTimeMillis)
        lsep (repeat-str 78 "+")
        esep (repeat-str 78 "=")
        sum (count results)
        f (filter #(cs/starts-with? % "F") results)
        p (filter #(cs/starts-with? % "p") results)
        ok (count p)
        perc (percent ok sum)
        diff (- (System/currentTimeMillis) mark)]

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
    (-> (str "cpu-time: " diff "ms")
        ansi/bold-white prn!!)
    (-> "" ansi/white prn!!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clj-test??

  ^{:arglists '([test]
                [test title print?])
    :doc "Run test as a clojure test case."}

  ([test]
   (clj-test?? test nil true))

  ([test title print?]
   {:pre [(fn? test)]}
   (let [[res ok?]
         (run-test test)]
     (if print?
       (prn-test res title)) ok?)))

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


