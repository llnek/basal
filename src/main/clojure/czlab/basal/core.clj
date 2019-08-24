;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "General helpers."
      :author "Kenneth Leung"}

  czlab.basal.core

  (:require [clojure.set :as ct]
            [clojure.string :as cs])

  (:import [java.util Date]
           [java.lang System]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;; #^"[Ljava.lang.Object;"

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
(defmacro atom?
  "If obj is an atom?" [x] `(instance? clojure.lang.Atom ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro is?
  "Alias instance?" [& more] `(instance? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro map->
  "Alias into {}." [& more] `(into {} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vec->
  "Alias into []." [& more] `(into [] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro set->
  "Alias into #{}." [& more] `(into #{} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+1
  "Alias concat - prepend an item." [a & more] `(concat [~a] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+
  "Alias concat." [& more] `(concat ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _2 "Alias second." [x] `(second ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _1 "Alias first." [x] `(first ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _3 "Alias 3rd." [x] `(nth ~x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _E "Alias last." [x] `(last ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !zero? "If not-zero?" [n] `(not (zero? ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n# "Alias count." [c] `(count ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro car "Alias first." [x] `(first ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cdr "Alias rest." [x] `(rest ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro one? "" [c] `(= 1 (count ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro two? "" [c] `(= 2 (count ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dissoc!!
  "Mutable dissoc (atom)."
  [a & args]
  (let [X (gensym)]
    `(let [~X ~a] (swap! ~X #(dissoc % ~@args)) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assoc!!
  "Mutable assoc (atom)."
  [a & args]
  (let [X (gensym)]
    `(let [~X ~a] (swap! ~X #(assoc % ~@args)) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_*
  "Wrap code into a fn(...)."
  [& forms] `(fn [& ~'____xs] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_3
  "Wrap code into a fn(a1,a2,a3)."
  [& forms] `(fn [~'____1 ~'____2 ~'____3] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_2
  "Wrap code into a fn(a1,a2)."
  [& forms] `(fn [~'____1 ~'____2] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_1
  "Wrap code into a fn(a1)." [& forms] `(fn [~'____1] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_0
  "Wrap code into a fn()." [& forms] `(fn [] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tset*
  "A transient set."
  ([] `(tset* nil))
  ([x] `(transient (or ~x #{}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tvec*
  "A transient vector."
  ([] `(tvec* nil))
  ([x] `(transient (or ~x []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tmap*
  "A transient map."
  ([] `(tmap* nil))
  ([x] `(transient (or ~x {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ps!
  "Alias persistent!." [c] `(persistent! ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atomic
  "Atomize fields as map." [& args] `(atom (hash-map ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mapfv
  "Apply a binary-op to the value over the forms."
  [op v & forms]
  `(vector ~@(map (fn [f] `(~op ~f ~v)) forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro last-index "Length - 1." [c] `(- (count ~c) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nexth
  "The next item after i." [c i] `(nth ~c (+ 1 ~i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro chop
  "Alias partition." [& args] `(partition ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro o+ "Alias for x+1." [X] `(+ 1 ~X))
(defmacro o- "Alias for x-1." [X] `(- ~X 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do#false "Do returns false." [& forms] `(do ~@forms false))
(defmacro do#true "Do returns true." [& forms] `(do ~@forms true))
(defmacro do#nil "Do returns nil." [& forms] `(do ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let#false "Let returns false." [& forms] `(let ~@forms false))
(defmacro let#true "Let returns true." [& forms] `(let ~@forms true))
(defmacro let#nil "Let returns nil." [& forms] `(let ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defenum
  "Enum definition. e.g. (defenum xyz a 1 b c) will
  generate
  (def xyz-a 1)
  (def xyz-b 2)
  (def xyz-c 3)."
  [name_ & args]
  (let [[e1 n] (take 2 args)
        more (cc+1 e1 (drop 2 args))]
    (assert (number? n) "Enum expecting a number.")
    `(do ~@(loop [v n [m & ms] more out []]
            (if (nil? m)
              out
              (recur (+ 1 v)
                     ms
                     (conj out
                           `(def ~(symbol (str (name name_)
                                               "-" (name m))) ~v))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro condp??
  "Make *simple* condp like cond." [pred expr & clauses]
  (let [c (count clauses)
        xs (if-not (even? c)
             clauses
             (concat clauses ['nil]))] `(condp ~pred ~expr ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro case??
  "Make *simple* condp like cond." [expr & clauses]
  (let [c (count clauses)
        xs (if-not (even? c)
             clauses
             (concat clauses ['nil]))] `(case ~expr ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-xxx??
  "*internal*"
  ([F bindings then] `(if-xxx?? ~F ~bindings ~then nil))
  ([F bindings then else & oldform]
   (let [_ (assert (= 2 (count bindings))
                   "Too many (> 2) in bindings.")
         X (gensym)
         f1 (first bindings)]
     (assert (symbol? f1))
     `(let [~X ~(last bindings) ~f1 ~X] (if (~F ~X) ~then ~else)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-number
  "bindings => binding-form test
  If test is a number, evaluates 'then'
  with binding-form bound to the value of test."
  ([bindings then]
   `(if-number ~bindings ~then nil))
  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? number? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-string
  "bindings => binding-form test
  If test is a string, evaluates 'then'
  with binding-form bound to the value of test."
  ([bindings then] `(if-string ~bindings ~then nil))
  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? string? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nloop
  "Loop over code n times."
  [n & forms] `(dotimes [~'_ ~n] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each*
  "Evals function for each element, indexed."
  [func coll]
  (let [C (gensym) I (gensym) T (gensym)]
    `(let [~C ~coll ~T (count ~C)]
       (dotimes [~I ~T] (~func (nth ~C ~I) ~I)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each
  "Evals function for each element."
  [func coll] (let [I (gensym)]
                `(doseq [~I ~coll] (~func ~I))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deftest
  "A test group." [name & body]
  `(def ~name (fn [] (filter #(not (nil? %)) [~@body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??
  "Assert test ok." [msg form] `(czlab.basal.core/ensure-test ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure-thrown??
  "Assert error was thrown."
  [msg expected form]
  `(try ~form
        (czlab.basal.core/ensure-thrown ~expected nil ~msg)
        (catch Throwable e#
          (czlab.basal.core/ensure-thrown ~expected e# ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prn!!
  "Println with format."
  [fmt & args] `(println (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro prn!
  "Print with format."
  [fmt & args] `(print (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private make-fn
  "Hack to wrap a macro as fn
  so you can use *apply* on it."
  [m] `(fn [& xs#] (eval (cons '~m xs#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro object<>
  "Create a new record instance."
  ([z] `(new ~z))
  ([z m] `(merge (new ~z) ~m))
  ([z a b & more] `(assoc (new ~z) ~a ~b ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vargs
  "Coerce into java vargs." [z c] `(into-array ~z ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<map>
  "Reduce with a transient map accumulator, returning a map."
  [f c] `(persistent! (reduce ~f (transient {}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<set>
  "Reduce with a transient set accumulator, returning a set."
  [f c] `(persistent! (reduce ~f (transient #{}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro preduce<vec>
  "Reduce with a transient vec accumulator, returning a vec."
  [f c] `(persistent! (reduce ~f (transient []) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rset!
  "Reset a atom."
  ([a] `(rset! ~a nil))
  ([a v] `(reset! ~a ~v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro exp!
  "Create an exception." [E & args] `(new ~E ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trap!
  "Throw exception." [E & args] `(throw (exp! ~E ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro raise!
  "Throw exception." [fmt & args]
  `(czlab.basal.core/trap! ~'Exception (format ~fmt ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-throw-exp
  "Generate a function which when called will
  throw the desired exception."
  ([name E] `(decl-throw-exp ~name ~E ""))
  ([name E doc]
  `(defn ~name ~doc [~'arg & ~'xs]
     (cond
       (string? ~'arg)
       (czlab.basal.core/trap! ~E (str (apply format ~'arg ~'xs)))
       (instance? Throwable ~'arg)
       (czlab.basal.core/trap! ~E
                               ~(with-meta 'arg {:tag 'Throwable}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with
  "binding=> symbol init-expr
  Evals the body in a context in which the symbol is always the
  returned value."
  [bindings & forms]
  (let [f (first bindings)
        sz (count bindings)]
    (assert (or (= sz 1)
                (= sz 2))
            "(not 1 or 2) in bindings.")
    (if (= sz 1)
      (assert (symbol? f)))
    (if (= sz 1)
      `(let [~f ~f] ~@forms ~f)
      `(let [~f ~(last bindings)] ~@forms ~f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-atom
  "binding=> symbol init-expr
  Eval the body in a context in which the symbol is always the
  returned value deref'ed.
  e.g. (do-with-atom [a (atom 0)] ... (deref a))."
  [bindings & forms]
  `(deref (czlab.basal.core/do-with ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bool! "Alias for boolean" [x] `(boolean ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trye!
  "Eat exception and return a value."
  [value & forms]
  `(try ~@forms (catch Throwable ~'_ ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!
  "Eat exception and return nil."
  [& forms] `(czlab.basal.core/trye! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+
  "bindings => binding-form test. When test is not empty, evaluates body
  with binding-form bound to the value of test."
  ([bindings then]
   `(if-some+ ~bindings ~then nil))
  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? empty? ~bindings ~else ~then)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+
  "bindings => binding-form test. When test is not empty, evaluates body
  with binding-form bound to the value of test."
  [bindings & body]
  `(czlab.basal.core/if-some+ ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-fn?
  "bindings => binding-form test. When test is a fn?, evaluates body
  with binding-form bound to the value of test."
  ([bindings then]
   `(if-fn? ~bindings ~then nil))
  ([bindings then else & oldform]
   `(czlab.basal.core/if-xxx?? fn? ~bindings ~then ~else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-fn?
  "bindings => binding-form test. When test is a fn?, evaluates body
  with binding-form bound to the value of test."
  [bindings & body]
  `(czlab.basal.core/if-fn? ~bindings (do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro doto->>
  "Combine doto and ->>." [x & forms]
  (let [X (gensym)]
    `(let [~X ~x]
       ~@(map (fn [f] (if (seq? f)
                        `(~@f ~X) `(~f ~X))) forms) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro
  doto->
  "Combine doto and ->." [x & forms]
  (let [X (gensym)]
    `(let [~X ~x]
       ~@(map (fn [f]
                (if (seq? f)
                  (let [z (first f)
                        r (rest f)]
                    `(~z ~X ~@r)) `(~f ~X))) forms) ~X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !=? "Alias for not identical?" [& more] `(not (identical? ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro =? "Alias for identical?" [& more] `(identical? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !in? "Alias for not contains?" [& more] `(not (contains? ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro in? "Alias for contains?" [& more] `(contains? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_
(defmacro XXcast?
  [someType obj]
  `^{:tag ~someType}
  ((fn []
    (let [x# ~obj]
      (if (instance? ~someType x#) (.cast ~someType x#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;had to use this trick to prevent reflection warning
(defmacro cast?
  "Cast object of this type else nil."
  [someType obj]
  (let [X (gensym)]
    `(let [~X ~obj]
       (if (instance? ~someType ~X)
         ^{:tag ~someType} (identity (.cast ~someType ~X))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cexp? "Try casting to Throwable." [e] `(cast? Throwable ~e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !nil? "is x not nil." [x] `(not (nil? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rnil "Get rid of nil(s) in seq." [seq] `(remove nil? ~seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro rnilv
  "Get rid of nil(s), returning a vec." [seq] `(into [] (remove nil? ~seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro szero?
  "Safe zero?" [e]
  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(zero? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sneg?
  "Safe neg?" [e]
  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(neg? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro spos?
  "Safe pos?" [e]
  (let [E (gensym)]
    `(let [~E ~e] (and (number? ~E)(pos? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro snneg?
  "Safe not neg?" [e]
  (let [E (gensym)]
    `(let [~E ~e] (or (szero? ~E)(spos? ~E)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !false? "Explicit not false." [x] `(not (false? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !true? "Explicit not true." [x] `(not (true? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assert-not
  "Verify a false condition?" [cnd] `(assert (not ~cnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro marray
  "n-size java array." [type n] `(make-array ~type ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zarray
  "0-size java array." [type] `(make-array ~type 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl**
  "Make a virtual table - op1 f1 op2 f2, with parent vtbl."
  [par & args]
  (assert (even? (count args)))
  `(czlab.basal.core/object<>
     ~'czlab.basal.core.VTable :____proto ~par ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl*
  "Make a virtual table - op1 f1 op2 f2." [& args] `(vtbl** nil ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro wo*
  "Alias with-open." [bindings & forms] `(with-open ~bindings ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run-bind
  "Run the bind operator. *internal*"
  [binder steps expr]
  (let [[a1 mv] (take 2 steps) more (drop 2 steps)]
    `(~binder ~mv
              (fn [~a1]
                ~(if (not-empty more)
                   `(run-bind ~binder ~more ~expr) `(do ~expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmonad
  "Define a named monad by defining the monad operations. The definitions
   are written like bindings to the monad operations bind and
   unit (required) and zero and plus (optional)."
  ([name ops] `(defmonad ~name "" ~ops))
  ([name docs ops]
   (let [_ (assert (not-empty ops) "no monad ops!")]
     `(def ~name (merge {:bind nil
                         :unit nil
                         :zero nil
                         :plus nil}
                        (into {} (map #(vec %) (partition 2 ~ops))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro domonad
  "Monad comprehension. Takes the name of a monad, a vector of steps
   given as binding-form, and a result value
   specified by body."
  ([monad steps] `(domonad ~monad ~steps nil))
  ([monad steps body]
   (let [E (gensym) B (gensym) U (gensym) Z (gensym)]
     `(let [{~B :bind ~U :unit ~Z :zero} ~monad
            ~E #(if (and (nil? %)
                         (some? ~Z)) ~Z (~U %))]
        (czlab.basal.core/run-bind ~B ~steps (~E ~body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end-macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-identity
  "Monad describing plain computations. This monad does in fact nothing
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
  "Execute the computation cont
  in the cont monad and return its result." [cont] (cont identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end-monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-str
  "Repeat string n times." [n s] (cs/join "" (repeat n s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num??
  "If n is not a number, return other."
  [n other] (if (number? n) n other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn flip
  "Invert number if not zero."
  [x] {:pre [(number? x)]} (if (zero? x) 0 (/ 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn percent
  "Return the percentage."
  [numerator denominator]
  (* 100.0 (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-seq
  "Split a collection into 2 parts."
  [coll cnt]
  (if-not (< cnt (count coll))
    (list (concat [] coll) () )
    (list (take cnt coll) (drop cnt coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-asc*
  "A generic compare function."
  [f] (fn_2 (cond (< (f ____1) (f ____2)) -1
                  (> (f ____1) (f ____2)) 1 :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-des*
  "A generic compare function."
  [f] (fn_2 (cond (< (f ____1) (f ____2)) 1
                  (> (f ____1) (f ____2)) -1 :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xxx-by
  "Used by min-by & max-by. *internal*"
  [cb coll]
  (if (not-empty coll)
    (reduce cb (_1 coll) (rest coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn min-by
  "Find item with minimum value as defined by the function."
  [f coll]
  (xxx-by #(if (< (f %1) (f %2)) %1 %2) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn max-by
  "Find item with maximum value as defined by the function."
  [f coll]
  (xxx-by #(if (< (f %1) (f %2)) %2 %1) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private t-bad "FAILED")
(def ^:private t-ok "PASSED")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-test
  "Assert a test condition, returning a message."
  [cnd msg]
  (str (try (if cnd t-ok t-bad)
            (catch Throwable _ t-bad)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-thrown
  "Assert an exception is thrown during test."
  [expected error msg]
  (str (if (nil? error)
         t-bad
         (cond (or (= expected :any)
                   (instance? expected error))
               t-ok
               :else
               t-bad)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runtest
  "Run a test group, returning the summary."
  ([test] (runtest test nil))
  ([test title]
   {:pre [(fn? test)]}
   (let [mark (System/currentTimeMillis)
         f #(cs/starts-with? % "P")
         lsep (repeat-str 78 "+")
         esep (repeat-str 78 "=")
         res (test)
         sum (count res)
         ok (count (filter f res))
         perc (int (* 100 (/ ok sum)))
         diff (- (System/currentTimeMillis) mark)
         result
         (cs/join "\n"
                  [lsep
                   (or title "test")
                   (Date.)
                   lsep
                   (cs/join "\n" res)
                   esep
                   (cs/join "" ["Passed: " ok "/" sum " [" perc "%]"])
                   (str "Failed: " (- sum ok))
                   (cs/join "" ["cpu-time: " diff "ms"])])]
     [(number? (cs/index-of result
                            "100%"
                            (cs/last-index-of result esep))) result])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn int-var "Like a mutable int."
  ;setter
  ([^ints arr v] (aset arr 0 (int v)) (int v))
  ;ctor
  ([] (int-array 1 0))
  ;getter
  ([^ints arr] (aget arr 0))
  ;apply (op old-val new-value)
  ([^ints arr op nv]
   (let [v (int (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn int-var*
  "Create & init int-var." [i]
  {:pre [(number? i)]} (doto (int-var) (int-var i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn long-var "like a mutable long."
  ;setter
  ([^longs arr v] (aset arr 0 (long v)) (long v))
  ;ctor
  ([] (long-array 1 0))
  ;getter
  ([^longs arr] (aget arr 0))
  ;apply (op old-val new-val)
  ([^longs arr op nv]
   (let [v (long (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn long-var*
  "Create & init a long-var." [n]
  {:pre [(number? n)]} (doto (long-var) (long-var n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nth??
  "Get the nth element, 1-indexed."
  [coll pos] {:pre [(> pos 0)]} (first (drop (- pos 1) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vargs*
  "Coerce into java array." [clazz & args] (vargs clazz args))

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
  [t]
  {:pre [(string? t)
         (nil? (cs/index-of t "/"))
         (nil? (cs/index-of t ":"))]} (keyword (str *ns*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-scoped-keyword?
  "If a scoped keyword?"
  [kw] (and (keyword? kw)
            (cs/includes? (str kw) "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-sign
  "Randomly choose a sign."
  [] (if (even? (rand-int Integer/MAX_VALUE)) 1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-bool
  "Randomly choose a boolean value."
  [] (even? (rand-int Integer/MAX_VALUE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num-sign
  "Find the sign of a number." [n]
  {:pre [(number? n)]}
  (cond (> n 0) 1 (< n 0) -1 :else 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->long
  "String as a long value."
  {:tag Long}
  ([s] (s->long s 0))
  ([s dv]
   {:pre [(or (nil? s)(string? s))(number? dv)]}
   (trye! (long dv) (Long/parseLong ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->int
  "String as an int value."
  {:tag Integer}
  ([s] (s->int s 0))
  ([s dv]
   {:pre [(or (nil? s)(string? s))(number? dv)]}
   (trye! (int dv) (Integer/parseInt ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->double
  "String as a double value."
  {:tag Double}
  ([s] (s->double s 0.0))
  ([s dv]
   {:pre [(or (nil? s)(string? s))(number? dv)]}
   (trye! (double dv) (Double/parseDouble ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->bool
  "String as a boolean value."
  [s] (contains? BOOLS (cs/lower-case (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-isa "Is child of parent?"  [reason par chi]
  (do#true (-> (cond
                 (and (class? par)(class? chi)) (isa? chi par)
                 (or (nil? par)(nil? chi)) false
                 (not (class? par)) (test-isa reason (class par) chi)
                 (not (class? chi)) (test-isa reason par (class chi)))
               (assert (str chi " not-isa " par)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-some
  "Object is not null?" [reason obj]
  (do#true (assert (some? obj) (str reason " is null."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-cond
  "A true condition?" [reason cnd]
  (do#true (assert cnd (str reason))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-hgl
  "String is not empty?"
  [reason s]
  (do#true
    (assert (and (string? s)
                 (not-empty s)) (str reason " is empty."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-pos0
  "Check number is not negative?"
  (fn [a b]
    (condp instance? b
      Double  :double
      Long  :long
      Float  :double
      Integer  :long
      (raise! "Allow numbers only!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-pos
  "Check number is positive?"
  (fn [a b]
    (condp instance? b
      Double  :double
      Long  :long
      Float  :double
      Integer  :long
      (raise! "Allow numbers only!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos0 :double [reason v]
  (do#true (assert (snneg? v) (str reason " must be >= 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos0 :long [reason v]
  (do#true (assert (snneg? v) (str reason " must be >= 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos :double [reason v]
  (do#true (assert (spos? v) (str reason " must be > 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos :long [reason v]
  (do#true (assert (spos? v) (str reason " must be > 0."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-seq+
  "Check sequence is not empty?"
  [reason v]
  (do#true (assert (pos? (count v)) (str reason " must be non empty."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sort-join
  "Sort and concatenate strings."
  ([ss] (sort-join "" ss))
  ([sep ss] (if (nil? ss) "" (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strip-ns-path
  "Remove the leading colon."
  ^String [path] (cs/replace (str path) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol VTableAPI "Act like a virtual table."
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
    (if (in? me kee)
      true
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
  ([n] (identity-n n false))
  ([n index-zero?]
   {:pre [(number? n)]}
     (fn_* (let [z (count ____xs)
                 p (if index-zero? n (- n 1))]
             (assert (and (< p z)
                          (>= p 0))
                     "Index out of bound.") (nth ____xs p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- merge-2 [a b]
  (cond (nil? a) b
        (nil? b) a
        :else
        (loop [tmp (tmap* a)
               [z & M] (seq b)]
          (let [[k vb] z
                va (get a k)]
            (if (nil? z)
              (ps! tmp)
              (recur (assoc! tmp
                             k
                             (cond (not (in? a k)) vb
                                   (and (map? vb)
                                        (map? va)) (merge-2 va vb)
                                   (and (set? vb)
                                        (set? va)) (ct/union va vb) :else vb)) M))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn merge+ [& more]
  "Merge (deep) of clojure data."
  (cond (empty? more) nil
        (one? more) (_1 more)
        :else (loop [prev nil
                     [a & xs] more]
                (if (empty? xs)
                  (merge-2 prev a)
                  (recur (merge-2 prev a) xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

