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

  ;need this for running tests
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
  "Alias into {} ..." [& more] `(into {} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vec->
  "Alias into [] ..." [& more] `(into [] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+1
  "Alias concat" [a & more] `(concat [~a] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+
  "Alias concat" [& more] `(concat ~@more))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Alias partition" [& args] `(partition ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro o+ "Alias for x+1" [X] `(+ 1 ~X))
(defmacro o- "Alias for x-1" [X] `(- ~X 1))

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
  (def xyz-c 3)"
  [name_ & args]
  (let [[e1 n] (take 2 args)
        more (cc+1 e1 (drop 2 args))]
    (assert (number? n) "enum expecting a number")
    `(do ~@(loop [v n [m & ms] more out []]
            (if (nil? m)
              out
              (recur (+ 1 v)
                     ms
                     (conj out
                           `(def ~(symbol (str (name name_)
                                               "-" (name m))) ~v))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-number
  "bindings => binding-form test
  If test is a number, evaluates 'then'
  with binding-form bound to the value of test."
  ([bindings then] `(if-number ~bindings ~then nil))
  ([bindings then else & oldform]
   (do (assert (= 2 (count bindings))
               "too many (> 2) in bindings")
       (let [X (gensym)]
         `(let [~X ~(last bindings)
                ~(first bindings) ~X] (if (number? ~X) ~then ~else))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-string
  "bindings => binding-form test
  If test is a string, evaluates 'then'
  with binding-form bound to the value of test."
  ([bindings then] `(if-string ~bindings ~then nil))
  ([bindings then else & oldform]
   (do (assert (= 2 (count bindings))
               "too many (> 2) in bindings")
       (let [X (gensym)]
         `(let [~X ~(last bindings)
                ~(first bindings) ~X] (if (string? ~X) ~then ~else))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nloop
  "Loop over code n times."
  [n & forms] `(dotimes [~'_ ~n] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each*
  "Evals function for each element, indexed."
  [func coll]
  (let [C (gensym)
        I (gensym)
        T (gensym)]
    `(let [~C ~coll
           ~T (count ~C)]
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
  "A test group."
  [name & body]
  `(def ~name (fn [] (filter #(not (nil? %)) [~@body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??
  "Assert test ok."
  [msg form] `(czlab.basal.core/ensure-test ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure-thrown??
  "Assert error was thrown."
  [msg expected form]
  `(try ~form
        (czlab.basal.core/ensure-test-thrown ~expected nil ~msg)
        (catch Throwable e#
          (czlab.basal.core/ensure-test-thrown ~expected e# ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro var-doc!
  "Add docstring to var." [v s] `(alter-meta! (var ~v) assoc :doc ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro var-private!
  "Make var private." [v] `(alter-meta! (var ~v) assoc :private true))

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
(defmacro decl-object
  "Define a simple type."
  [name & more] `(defrecord ~name [] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atomic<>
  "Create a new entity."
  ([classname] `(atomic<> ~classname {}))
  ([classname seed] `(atomic<> ~classname ~seed false))
  ([classname seed volatile??]
   (let [S (gensym)]
     `(let [~S ~seed]
        (assert (map? ~S))
        (new ~classname (if-not ~volatile?? (atom ~S) (volatile! ~S)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro object<>
  "Create a new object"
  ([classname] `(object<> ~classname {}))
  ([classname seed]
   (let [S (gensym) Z (gensym) R (gensym)]
     `(let [~S ~seed
            ~Z (assert (map? ~S))
            ~R (merge (new ~classname) ~S)] (assert (record? ~R)) ~R))))

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
  "Create an exception instance."
  [e & args]
  (if (empty? args) `(new ~e) `(new ~e ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trap!
  "Throw an exception." [e & args] `(throw (exp! ~e ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro raise!
  "Throw an exception." [& args]
  `(throw (exp! ~'Exception (str ~@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-throw-exp
  "Generate a function which when called will
  throw the desired exception."
  ([name etype] `(decl-throw-exp ~name ~etype ""))
  ([name etype doc]
  `(defn ~name ~doc [~'arg & ~'xs]
     (cond
       (string? ~'arg)
       (trap! ~etype (str (apply format ~'arg ~'xs)))
       (instance? Throwable ~'arg)
       (trap! ~etype
              ~(with-meta 'arg {:tag 'Throwable}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (meta nil) is fine, so no need to worry
(defmacro get-type-id
  "typeId from metadata." [m] `(:typeid (meta ~m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-atom
  "binding=> symbol init-expr
  Eval the body in a context in which the symbol is always the
  returned value deref'ed.
  e.g. (do-with-atom [a (atom 0)] ... (deref a))"
  [bindings & forms]
  (let [f (first bindings)
        sz (count bindings)]
    (assert (or (= sz 1)
                (= sz 2))
            "(not 1 or 2) in bindings")
    (if (= sz 1)
      `(let [~f ~f] ~@forms (deref ~f))
      `(let [~f ~(last bindings)] ~@forms (deref ~f)))))

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
            "(not 1 or 2) in bindings")
    (if (= sz 1)
      `(let [~f ~f] ~@forms ~f)
      `(let [~f ~(last bindings)] ~@forms ~f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bool! "Alias for boolean" [x] `(boolean ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trye!
  "Eat exception and return a value."
  [value & forms]
  `(try ~@forms (catch Throwable e# ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!
  "Eat exception and return nil."
  [& forms] `(try ~@forms (catch Throwable e# nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+
  "bindings => binding-form test. When test is not empty, evaluates body
  with binding-form bound to the value of test."
  ([bindings then]
   `(if-some+ ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (first bindings)
         tst (last bindings)]
     (assert (= 2 (count bindings))
             "not = 2 in bindings")
     (let [T (gensym)]
       `(let [~T ~tst]
          (if (empty? ~T)
            ~else (let [~form ~T] ~then)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+
  "bindings => binding-form test. When test is not empty, evaluates body
  with binding-form bound to the value of test."
  [bindings & body]
  (let [form (first bindings)
        tst (last bindings)]
    (assert (= 2 (count bindings))
            "not = 2 in bindings")
    (let [T (gensym)]
      `(let [~T ~tst]
         (if-not (empty? ~T)
           (let [~form ~T] ~@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-fn?
  "bindings => binding-form test. When test is a fn?, evaluates body
  with binding-form bound to the value of test."
  ([bindings then]
   `(if-fn? ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (first bindings)
         tst (last bindings)]
     (assert (= 2 (count bindings))
             "not = 2 in bindings")
     (let [T (gensym)]
       `(let [~T ~tst]
          (if-not (fn? ~T)
            ~else (let [~form ~T] ~then)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-fn?
  "bindings => binding-form test. When test is a fn?, evaluates body
  with binding-form bound to the value of test."
  [bindings & body]
  (let [form (first bindings)
        tst (last bindings)]
    (assert (= 2 (count bindings))
            "not = 2 in bindings")
    (let [T (gensym)]
      `(let [~T ~tst]
         (if (fn? ~T)
           (let [~form ~T] ~@body))))))

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
  "Combine doto and ->" [x & forms]
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
(defmacro cexp? "Try casting to Throwable" [e] `(cast? Throwable ~e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro !nil? "is x not nil" [x] `(not (nil? ~x)))

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
(defmacro ^:private _defvtable_
  [name & args]
  (assert (even? (count args)))
  `(def ~name
     (-> {}
        ~@(reduce (fn [acc [k v]]
                    (conj acc `(assoc ~k ~v))) [] (partition 2 args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl**
  "Make a virtual table - op1 f1 op2 f2, with parent vtbl"
  [par & args]
  (assert (even? (count args))) `(hash-map :____proto ~par ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl*
  "Make a virtual table - op1 f1 op2 f2" [& args] `(vtbl** nil ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro wo*
  "Alias with-open" [bindings & forms] `(with-open ~bindings ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run-bind
  "Run the bind operator."
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
   (let []
     (assert (not-empty ops) "no monad ops!")
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
   (let [E (gensym)
         B (gensym)
         U (gensym)
         Z (gensym)]
     `(let [{~B :bind ~U :unit ~Z :zero} ~monad
            ~E #(if (and (nil? %)
                         (some? ~Z)) ~Z (~U %))] (run-bind ~B ~steps (~E ~body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end-macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-identity
  "Monad describing plain computations. This monad does in fact nothing
  at all. It is useful for testing, for combination with monad
  transformers, and for code that is parameterized with a monad."
  (vector :bind (fn [mv mf] (mf mv)) :unit identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-maybe
  "Monad describing computations with possible failures. Failure is
  represented by nil, any other value is considered valid. As soon as
  a step returns nil, the whole computation will yield nil as well."
  (vector :unit identity
          :bind (fn [mv mf] (if-not (nil? mv) (mf mv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-list
  "Monad describing multi-valued computations, i.e. computations
  that can yield multiple values. Any object implementing the seq
  protocol can be used as a monadic value."
  (vector :bind (fn [mv mf] (flatten (map mf mv)))
          :unit (fn_1 (vector ____1))
          :zero []
          :plus (fn_* (flatten ____xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-state
  "Monad describing stateful computations. The monadic values have the
  structure (fn [old-state] [result new-state])."
  (vector :unit (fn [v] (fn [s] [v s]))
          :bind (fn [mv mf]
                  (fn [s]
                    (let [[v s'] (mv s)] ((mf v) s'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-continuation
  "Monad describing computations in continuation-passing style. The monadic
  values are functions that are called with a single argument representing
  the continuation of the computation, to which they pass their result."
  (vector :unit (fn [v] (fn [cont] (cont v)))
          :bind (fn [mv mf]
                  (fn [cont]
                    (mv (fn [v] ((mf v) cont)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-cont
  "Execute the computation cont
  in the cont monad and return its result."
  [cont]
  (cont identity))

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
  (if (< cnt (count coll))
    (list (take cnt coll) (drop cnt coll))
    (list (concat [] coll) () )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-asc*
  "A generic compare function."
  [f]
  (fn_2 (cond (< (f ____1) (f ____2)) -1
              (> (f ____1) (f ____2)) 1 :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-des*
  "A generic compare function."
  [f]
  (fn_2 (cond (< (f ____1) (f ____2)) 1
              (> (f ____1) (f ____2)) -1 :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xxx-by
  "Used by min-by & max-by - internal."
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
(defn ensure-test-thrown
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
  ([^ints arr v] (aset arr 0 (int v)) (int v))
  ([] (int-array 1 0))
  ([^ints arr] (aget arr 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn long-var "like a mutable long."
  ([^longs arr v] (aset arr 0 (long v)) (long v))
  ([] (long-array 1 0))
  ([^longs arr] (aget arr 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nth??
  "Get the nth element, 1-indexed."
  [coll pos] {:pre [(> pos 0)]} (first (drop (- pos 1) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vargs*
  "Coerce into java array."
  [clazz & args] (vargs clazz args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- interject
  "Run the function on the current field value,
   replacing the key with the returned value.
   function(pojo oldvalue) -> newvalue."
  [pojo field func]
  {:pre [(map? pojo) (fn? func)]}
  (assoc pojo field (func pojo field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scoped-keyword
  "Scope name as a fully-qualified keyword."
  [t]
  {:pre [(string? t)
         (nil? (cs/index-of t "/"))
         (nil? (cs/index-of t ":"))]}
  (keyword (str *ns*) t))

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
  [] (if (even? (rand-int Integer/MAX_VALUE)) true false))

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
   {:pre [(string? s)(number? dv)]}
   (trye! (long dv) (Long/parseLong ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->int
  "String as an int value."
  {:tag Integer}
  ([s] (s->int s 0))
  ([s dv]
   {:pre [(string? s)(number? dv)]}
   (trye! (int dv) (Integer/parseInt ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->double
  "String as a double value."
  {:tag Double}
  ([s] (s->double s 0.0))
  ([s dv]
   {:pre [(string? s)(number? dv)]}
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
  (do#true (assert (some? obj) (str reason " is null"))))

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
                 (not-empty s)) (str reason " is empty"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-pos0
  "Check number is not negative?"
  (fn [a b]
    (condp instance? b
      Double  :double
      Long  :long
      Float  :double
      Integer  :long
      (trap! Exception "allow numbers only"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-pos
  "Check number is positive?"
  (fn [a b]
    (condp instance? b
      Double  :double
      Long  :long
      Float  :double
      Integer  :long
      (trap! Exception "allow numbers only"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos0 :double [reason v]
  (do#true (assert (snneg? v) (str reason " must be >= 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos0 :long [reason v]
  (do#true (assert (snneg? v) (str reason " must be >= 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos :double [reason v]
  (do#true (assert (spos? v) (str reason " must be > 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod test-pos :long [reason v]
  (do#true (assert (spos? v) (str reason " must be > 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-seq+
  "Check sequence is not empty?"
  [reason v]
  (do#true (assert (> (count v) 0) (str reason " must be non empty"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sort-join
  "Sort a list of strings and then concatenate them"
  ([ss] (sort-join "" ss))
  ([sep ss]
   (if (nil? ss) "" (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn strip-ns-path
  "Remove the leading colon."
  ^String [path] (cs/replace (str path) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn svtbl
  "Hook parent vtable."
  [vt par] (assoc vt :____proto par))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gvtbl
  "Find key from vtable."
  [vt kee]
  (if (map? vt)
    (if (in? vt kee)
      (vt kee) (gvtbl (:____proto vt) kee))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cvtbl?
  "key in vtable?"
  [vt kee]
  (if (map? vt)
    (if (in? vt kee)
      (vt kee) (gvtbl (:____proto vt) kee)) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gvtbl'
  "Find key from parent vtable."
  [vt kee] (gvtbl (:____proto vt) kee))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rvtbl'
  "Find key from parent vtable and run func."
  [vt kee & args]
  (let [f (gvtbl' vt kee)]
    (if (fn? f) (apply f vt args) f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rvtbl
  "Find key from vtable and run func."
  [vt kee & args]
  (let [f (gvtbl vt kee)] (if (fn? f) (apply f vt args) f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn identity-n
  "Like identity but returns positional param.
  e.g. (def get-2nd (identity-n 2))
       (get-2nd a b c) => b"
  ([n] (identity-n n false))
  ([n index-zero?]
   {:pre [(number? n)]}
     (fn [& xs]
       (let [z (count xs)]
         (if index-zero?
           (assert (and (>= n 0) (< n z)) "index out of bound")
           (assert (and (>= (- n 1) 0) (< (- n 1) z)) "index out of bound"))
         (if index-zero? (nth xs n) (nth xs (- n 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn merge+
  "Merge (deep) of clojure data."
  [a b & more]
  (loop [[z & M] (seq b)
         tmp (tmap* a)]
    (if (nil? z)
      (ps! tmp)
      (let [[k vb] z
            va (get a k)]
        (recur M
               (assoc! tmp
                       k
                       (if-not (in? a k)
                         vb
                         (cond (and (map? vb)
                                    (map? va))
                               (merge+ va vb)
                               (and (set? vb)
                                    (set? va))
                               (ct/union va vb)
                               :else vb))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

