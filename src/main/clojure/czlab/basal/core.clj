;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "General helpers."
      :author "Kenneth Leung"}

  czlab.basal.core

  (:require [czlab.basal.log :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.edn :as edn])

  (:use [clojure.walk])

  (:import [czlab.jasal Idable DataError MonoFlop Watch RunnableWithId]
           [java.util.concurrent.atomic AtomicLong AtomicInteger]
           [java.util.zip DataFormatException Deflater Inflater]
           [java.util.concurrent TimeUnit]
           [java.security SecureRandom]
           [java.nio.charset Charset]
           [czlab.basal Stateful]
           [clojure.lang
            PersistentList
            Keyword
            APersistentMap
            APersistentVector]
           [java.net URL]
           [java.io
            Serializable
            InputStream
            PrintStream
            File
            FileInputStream
            ObjectOutputStream
            ObjectInputStream
            ByteArrayInputStream
            ByteArrayOutputStream]
           [java.util
            TimerTask
            Map
            Properties
            Date
            Calendar
            HashMap
            HashSet
            ArrayList
            GregorianCalendar
            TimeZone]
           [java.sql Timestamp]
           [java.rmi.server UID]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;{:keys [a] {:keys []} :b} - destruct nested map b
;; #^"[Ljava.lang.Object;"

(def ^:private CSCZ (class (.toCharArray "")))
(def ^:private BSCZ (class (.getBytes "")))
(def ^:private SGCZ (class ""))

(def ^String PATHSEP (System/getProperty "file.separator"))
(def ^String USASCII "ISO-8859-1")
(def ^String UTF16 "UTF-16")
(def ^String UTF8 "UTF-8")
(def ^String SLASH "/")

(def _empty-set_ #{})
(def _empty-map_ {})
(def _empty-vec_ [])

(def BOOLS #{"true", "yes", "on", "ok", "active", "1"})
(def ^String HEX-CHARS "0123456789ABCDEF")
(def ^String HEX-CHS (cs/lower-case HEX-CHARS))

(def KiloBytes 1024)
(def BUF-SZ (* 4 KiloBytes))
(def MegaBytes (* KiloBytes KiloBytes))
(def GigaBytes (* KiloBytes MegaBytes))

(def OneK 1024)
(def FourK (* 4 OneK))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-long-var "Define a long array[1] for local operation"
  ([] `(decl-long-var 0))
  ([n] `(long-array 1 ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-int-var "Define a int array[1] for local operation"
  ([] `(decl-int-var 0))
  ([n] `(int-array 1 ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn int-var ""
  ([^ints arr v] (aset arr 0 (int v)) (int v))
  ([^ints arr] (aget arr 0))
  ([^ints arr op nv]
   (let [v (int (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn long-var ""
  ([^longs arr ^long v] (aset arr 0 v) v)
  ([^longs arr] (aget arr 0))
  ([^longs arr op nv]
   (let [v (long (op (aget arr 0) nv))] (aset arr 0 v) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-var-docstring
  "Add docstring to var" [v s] `(alter-meta! ~v assoc :doc ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-var-private
  "Make var private" [v] `(alter-meta! ~v assoc :private true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!!
  "println with format" [fmt & args] `(println (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!
  "print with format" [fmt & args] `(print (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ist? "instance?" [type obj] `(instance? ~type ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nth?? "Safely get the nth element"
  [coll pos]
  {:pre [(> pos 0)]}
  (first (drop (dec pos) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;hack to wrap a macro as fn so that you can use *apply* on it
(defmacro ^:private make-fn [m]
 `(fn [& args#] (eval (cons '~m args#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro reset-atomic
  "Do a reset on the atom/volatile"
  [atomic arg]
  `(let [s# ~(with-meta atomic
                        {:tag 'czlab.basal.Stateful})
         d# (.state s#)]
     (if (volatile? d#) (vreset! d# ~arg) (reset! d# ~arg)) s#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro alter-atomic
  "Do a swap on the atom/volatile"
  [atomic & args]
  `(let [s# ~(with-meta atomic
                        {:tag 'czlab.basal.Stateful})
         d# (.state s#)]
     (if (volatile? d#) (vswap! d# ~@args) (swap! d# ~@args)) s#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;using defrecord causes issues with print-match#multimethod(IDeref,IRecord clash)
(defmacro decl-atomic
  "Define a simple stateful type" [name & more]
  `(deftype ~name [~'_data]
     ~'czlab.basal.Stateful
     ~'(deref [_] @_data)
     ~'(state [_] _data) ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-volatile
  "Define a simple type" [name & more]
  `(decl-muble-types ~name :volatile-mutable ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-mutable
  "Define a simple type" [name & more]
  `(decl-muble-types ~name :unsynchronized-mutable ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-object
  "Define a simple type" [name & more] `(defrecord ~name [] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro atomic<>
  "Create a new entity"
  ([classname] `(atomic<> ~classname {}))
  ([classname seed] `(atomic<> ~classname ~seed false))
  ([classname seed volatile??]
   `(let [s# ~seed
          ~'_ (assert (map? s#))]
      (new ~classname (if-not ~volatile?? (atom s#) (volatile! s#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro object<>
  "Create a new object"
  ([classname] `(object<> ~classname {}))
  ([classname seed]
   `(let [s# ~seed
          ~'_ (assert (map? s#))
          r# (merge (new ~classname) s#)] (assert (record? r#)) r#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro mutable<>
  "Create a new object"
  ([classname] `(mutable<> ~classname {}))
  ([classname seed]
   `(let [s# ~seed
          ~'_ (assert (map? s#))
          r# (new ~classname s#)]
      (assert (satisfies? czlab.basal.core/Muable r#)) r#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro vargs "Coerce into java vargs" [z c] `(into-array ~z ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro pcoll! "Persist a transient" [t] `(persistent! ~t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<map>
  "Reduce with a transient map, returning a map"
  [f c] `(persistent! (reduce ~f (transient {}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<set>
  "Reduce with a transient set, returning a set"
  [f c] `(persistent! (reduce ~f (transient #{}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<vec>
  "Reduce with a transient vec, returning a vec"
  [f c] `(persistent! (reduce ~f (transient []) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sreduce<>
  "Reduce with a string builder, returning a string"
  [f c] `(str (reduce ~f (StringBuilder.) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro rset!
  "Reset a atom"
  ([a] `(rset! ~a nil))
  ([a v] `(reset! ~a ~v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro exp!
  "Create an exception instance"
  [e & args]
  (if (empty? args) `(new ~e) `(new ~e ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trap! "Throw this exception" [e & args] `(throw (exp! ~e ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn vargs* "Coerce into java array" [clazz & args] (vargs clazz args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private decl-throw-xxx "" [name etype docs]
  `(defn ~name ~docs
     [~'fmt ~'& ~'xs]
     (->> (apply format ~'fmt ~'xs) str (trap! ~etype ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private decl-throw-exp "" [name etype docs]
  `(defn ~name ~docs
     [~'t]
     (assert (instance? Throwable ~'t))
     (trap! ~etype ~(with-meta 't {:tag 'Throwable}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-throw-xxx
  throwISE
  IllegalStateException
  "Throw illegal state exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-throw-xxx
  throwUOE
  UnsupportedOperationException
  "Throw unsupported operation exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-throw-xxx
  throwBadArg
  IllegalArgumentException
  "Throw bad parameter exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-throw-xxx
  throwBadData
  DataError
  "Throw Bad Data Exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-throw-xxx
  throwIOE
  java.io.IOException
  "Throw IO Exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-throw-exp
  throwIOExp
  java.io.IOException
  "Throw IO Exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (meta nil) is fine, so no need to worry
(defmacro getTypeId "typeId from metadata" [m] `(:typeid (meta ~m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro do-with
  "e.g. (do-with [x expr] ... x)"
  [bindings & more]
  (assert (= 2 (count bindings)))
  (let [a (first bindings)
        b (last bindings)]
      `(let [~a ~b] ~@more ~a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro bool! "Shorthand for boolean" [x] `(boolean ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try!!
  "Eat & log the exception, returning a default value"
  [defv & exprs]
  `(try ~@exprs (catch Throwable e# (czlab.basal.log/warn e# "") ~defv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trye!
  "Eat the exception, returning a default value"
  [defv & exprs]
  `(try ~@exprs
        (catch Throwable e#
          (czlab.basal.log/warn "Just ate a %s, yummy" e#) ~defv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trye!!
  "Eat the exception, returning nil"
  [defv & exprs]
  `(try ~@exprs (catch Throwable e#  ~defv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try! "Eat the exp, return nil" [& forms] `(try!! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro run-able+id<>
  "Create a RunnableWithId wrapper"
  [rid & forms]
  `(reify
     czlab.jasal.Idable
     (id [_] ~rid)
     Runnable
     (run [_] (czlab.basal.core/try! ~@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro run-able<>
  "Create a Runnable wrapper"
  [& forms]
  `(reify Runnable
     (run [_] (czlab.basal.core/try! ~@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro if-some+
  "bindings => binding-form test. When test is not empty, evaluates body
  with binding-form bound to the value of test"
  ([bindings then]
   `(if-some+ ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (bindings 0)
         tst (bindings 1)]
     `(let [temp# ~tst]
        (if (empty? temp#)
          ~else
          (let [~form temp#] ~then))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro when-some+
  "bindings => binding-form test. When test is not empty, evaluates body
  with binding-form bound to the value of test"
  [bindings & body]
  (let [form (bindings 0)
        tst (bindings 1)]
    `(let [temp# ~tst]
       (if-not (empty? temp#)
         (let [~form temp#] ~@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro if-fn?
  "bindings => binding-form test. When test is a fn?, evaluates body
  with binding-form bound to the value of test"
  ([bindings then]
   `(if-fn? ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (bindings 0)
         tst (bindings 1)]
     `(let [temp# ~tst]
        (if-not (fn? temp#)
          ~else
          (let [~form temp#] ~then))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro when-fn?
  "bindings => binding-form test. When test is a fn?, evaluates body
  with binding-form bound to the value of test"
  [bindings & body]
  (let [form (bindings 0)
        tst (bindings 1)]
    `(let [temp# ~tst]
       (if (fn? temp#)
         (let [~form temp#] ~@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro some** "Safely handle nil"
  ([x form]
   `(let [x# ~x] (if-not (nil? x#) (. x# ~form))))
  ([x form & more]
   `(let [x# ~x] (if-not (nil? x#) (some** (. x# ~form) ~@more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro doto->>
  "Combine doto and ->>" [x & forms]

  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (if (seq? f)
                  `(~@f ~gx)
                  `(~f ~gx)))
              forms)
       ~gx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro
  ^:private doto->
  "Combine doto and ->" [x & forms]

  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (if (seq? f)
                  (let [z (first f) r (rest f)]
                    `(~z ~gx ~@r))
                  `(~f ~gx)))
              forms)
       ~gx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro !self?
  "Shorthand for not identical?" [me other] `(not (identical? ~me ~other)))
(defmacro self?
  "Shorthand for identical?" [me other] `(identical? ~me ~other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro notin?
  "Shorthand for not contains?" [coll k] `(not (contains? ~coll ~k)))
(defmacro in?
  "Shorthand for contains?" [coll k] `(contains? ~coll ~k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro do->false "Do and return false" [& forms] `(do ~@forms false))
(defmacro do->nil "Do and return nil" [& forms] `(do ~@forms nil))
(defmacro do->true "Do and return true" [& forms] `(do ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try!-let
  "a try let combo" [bindings & forms] `(try! (let ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro let-try
  "a let try combo" [bindings & forms] `(let ~bindings (try ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro let-when
  "a let when combo"
  [bindings kond & forms] `(let ~bindings (when ~kond ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
#_
(defmacro XXcast?
  ""
  [someType obj]

  `^{:tag ~someType}
  ((fn []
    (let [x# ~obj]
      (if (instance? ~someType x#)
        (.cast ~someType x#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;had to use this trick to prevent reflection warning
(defmacro cast?
  "Cast object of this type else nil"
  [someType obj]
  `(let [x# ~obj]
     (if (instance? ~someType x#)
          ^{:tag ~someType} (identity (.cast ~someType x#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro cexp? "Try casting to Throwable" [e] `(cast? Throwable ~e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private !nil? "is x not nil" [x] `(not (nil? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn sortby "" [kfn cmp coll]
  (sort-by kfn
           (reify java.util.Comparator
             (compare [_ t1 t2] (cmp t1 t2))) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn monoFlop<>
  "Flip on first call,
  useful for one-time logic" ^MonoFlop []

  (let [toggled (atom false)]
    (reify
      MonoFlop
      (isFirstCall [_]
        (if @toggled
          false
          (do->true (reset! toggled true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn watch<>
  "Use to mark elapsed time" ^Watch []

  (let [start (atom (System/nanoTime))
        f #(.convert ^TimeUnit %
                     (- (System/nanoTime) @start)
                     TimeUnit/NANOSECONDS)]
    (reify
      Watch
      (reset [_] (reset! start (System/nanoTime)))
      (elapsedMillis [_] (f TimeUnit/MILLISECONDS))
      (elapsedNanos [_] (f TimeUnit/NANOSECONDS)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local hack
(defn- get-czldr
  "" {:tag ClassLoader}
  ([] (get-czldr nil))
  ([cl] (or cl (. (Thread/currentThread) getContextClassLoader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro rnil "Get rid of nil(s) in seq" [seq] `(remove nil? ~seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro flatnil
  "Get rid of nil(s) in seq, returning a vec"
  [seq] `(into [] (remove nil? ~seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- interject
  "Run the function on the current field value,
   replacing the key with the returned value.
   function(pojo oldvalue) -> newvalue"
  [pojo field func]
  {:pre [(map? pojo) (fn? func)]}
  (assoc pojo field (func pojo field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro szero? "Safe zero?" [e]
  `(let [e# ~e] (and (number? e#)(zero? e#))))
(defmacro sneg? "Safe neg?" [e]
  `(let [e# ~e] (and (number? e#)(neg? e#))))
(defmacro spos? "Safe pos?" [e]
  `(let [e# ~e] (and (number? e#)(pos? e#))))
(defmacro snneg? "Safe not neg?" [e]
  `(let [e# ~e] (or (szero? e#)(spos? e#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro envVar
  "Get value for this env var"
  ^String [vname] `(if-some+ [e# ~vname] (System/getenv e#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn asFQKeyword
  "Scope name as a fully-qualified keyword"
  [t]
  {:pre [(string? t)
         (and (< (.indexOf ^String t (int \/)) 0)
              (< (.indexOf ^String t (int \:)) 0))]}
  (keyword (str *ns*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isFQKeyword?
  "true if this is a scoped keyword"
  [kw] (and (keyword? kw) (> (.indexOf (str kw) (int \/)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro uuid<> "RFC4122, v4 format" [] `(str (java.util.UUID/randomUUID)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;happens to be all hex chars
(defn jid<>
  "Generate a unique id using std java"
  ^String [] (.replaceAll (str (UID.)) "[:\\-]+" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn uid<> "UUID, no dash" ^String [] (cs/replace (uuid<>) #"-" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn randSign
  "Randomly choose a sign"
  ^long [] (if (even? (rand-int Integer/MAX_VALUE)) 1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn randBool
  "Randomly choose a boolean value"
  [] (if (even? (rand-int Integer/MAX_VALUE)) true false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rand<>
  "A new random object"
  {:tag SecureRandom}

  ([] (rand<> false))
  ([strong?]
   (doto (if strong?
           (SecureRandom/getInstanceStrong)
           (SecureRandom.))
     (.setSeed (SecureRandom/getSeed 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro date<> "A java Date" [] `(java.util.Date.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strCharset
  "Charset as string"
  ^String
  [enc]
  (if (ist? Charset enc)
    (.name ^Charset enc)
    (if (empty? enc) "utf-8" enc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn toCharset
  "A java Charset of the encoding"
  {:tag Charset}

  ([] (toCharset "utf-8"))
  ([enc] (if (ist? Charset enc)
           enc
           (Charset/forName (or enc "utf-8")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fpath
  "Get the file path" ^String [arg]

  (if (ist? File arg)
    (fpath (.getCanonicalPath ^File arg))
    (let [fp (str arg)]
      (cs/replace fp #"\\" "/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sysProp!
  "Set a system property"
  [prop value] `(System/setProperty ~prop ~value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sysProp
  "Get value of a system property"
  [prop] `(System/getProperty (str ~prop) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro getUser "Get the user login name" [] `(sysProp "user.name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro homeDir "Get user's home dir" []
  `(clojure.java.io/file (sysProp "user.home")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro getCwd "Get current dir" []
  `(clojure.java.io/file (sysProp "user.dir")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn trimLastPathSep
  "Get rid of trailing dir paths"
  ^String [path] (.replaceFirst (str path) "[/\\\\]+$" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn serialize
  "Object serialization"
  ^bytes [obj] {:pre [(some? obj)]}

  (with-open [out (ByteArrayOutputStream. BUF-SZ)
              oos (ObjectOutputStream. out)]
    (.writeObject oos ^Serializable obj)
    (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deserialize
  "Object deserialization"
  ^Serializable [^bytes bits] {:pre [(some? bits)]}

  (with-open [in (ByteArrayInputStream. bits)
              ois (ObjectInputStream. in)] (.readObject ois)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn numSign
  "Find the sign of a number" [n]
  {:pre [(number? n)]}
  (cond (> n 0) 1 (< n 0) -1 :else 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gczn
  "Get object's short class name" ^String [obj]
  (cond
    (nil? obj) ""
    (ist? Class obj) (.getSimpleName ^Class obj)
    :else (gczn (class obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getClassname
  "Get object's class name" ^String [obj]
  (cond
    (nil? obj) ""
    (ist? Class obj) (.getName ^Class obj)
    :else (getClassname (class obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isWindows?
  "Is Windows OS?" []
  (>= (.indexOf (cs/lower-case (sysProp "os.name")) "windows") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isMacOS?
  "Is Mac OS?" [] (>= (.indexOf (sysProp "os.name") "Mac ") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isLinux?
  "Is Linux OS?" [] (and (not (isWindows?)) (not (isMacOS?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private str!? "" [s] `(or (nil? ~s)(string? ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convLong
  "String as a long value" {:tag Long}

  ([s] (convLong s 0))
  ([s dftLongVal]
   {:pre [(str!? s)]}
   (trye!! dftLongVal (Long/parseLong ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convInt
  "String as an int value" {:tag Integer}

  ([s] (convInt s 0))
  ([s dftIntVal]
   {:pre [(str!? s)]}
   (trye!! (int dftIntVal) (Integer/parseInt ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convDouble
  "String as a double value" {:tag Double}

  ([s] (convDouble s 0.0))
  ([s dftDblVal]
   {:pre [(str!? s)]}
   (trye!! dftDblVal (Double/parseDouble ^String s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convBool
  "String as a boolean value"
  [s] (contains? BOOLS (cs/lower-case (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loadJavaProps
  "Load properties from source"
  ^Properties [arg]
  (cond
    (ist? File arg) (loadJavaProps (io/as-url arg))
    (ist? URL arg) (with-open [inp (.openStream  ^URL arg)]
                     (loadJavaProps inp))
    :else
    (do-with [p (Properties.)]
             (some->> (cast? InputStream arg) (.load p )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strit
  "Coerce to a string"
  {:tag String}
  ([obj] (strit obj "utf-8"))
  ([obj enc]
   (cond
     (string? obj) obj
     (= CSCZ (class obj)) (String. ^chars obj)
     (= BSCZ  (class obj)) (String. ^bytes obj (toCharset enc))
     :else (str obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsit
  "Get chars from string"
  ^chars [obj]
  (cond
    (= CSCZ (class obj)) obj
    (string? obj) (.toCharArray ^String obj)
    (some? obj) (charsit (str obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesit
  "Get bytes with the right encoding"
  {:tag "[B"}
  ([obj] (bytesit obj "utf-8"))
  ([obj enc]
   (cond
     (= ByteArrayOutputStream (class obj))
     (.toByteArray ^ByteArrayOutputStream obj)
     (= BSCZ (class obj)) obj
     (= CSCZ (class obj)) (bytesit (strit obj) enc)
     (string? obj) (.getBytes ^String obj (toCharset enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStream
  "Load the resource as stream" {:tag InputStream}

  ([rcPath] (resStream rcPath nil))
  ([rcPath ^ClassLoader ldr]
   {:pre [(string? rcPath)]}
   (when (not-empty rcPath)
     (-> (get-czldr ldr)
         (.getResourceAsStream  ^String rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resUrl
  "Load the resource as URL" {:tag URL}

  ([rcPath] (resUrl rcPath nil))
  ([rcPath ^ClassLoader ldr]
   {:pre [(string? rcPath)]}
   (when (not-empty rcPath)
     (-> (get-czldr ldr)
         (.getResource ^String rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStr
  "Load the resource as string" {:tag String}

  ([rcPath encoding] (resStr rcPath encoding nil))
  ([rcPath] (resStr rcPath "utf-8" nil))
  ([rcPath encoding ^ClassLoader ldr]
   (if-some
     [res (resStream rcPath ldr)]
     (with-open
       [out (ByteArrayOutputStream. BUF-SZ)
        inp res]
       (io/copy inp out :buffer-size BUF-SZ)
       (-> (.toByteArray out)
           (strit encoding))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resBytes
  "Load the resource as byte[]" {:tag "[B"}

  ([rcPath] (resBytes rcPath nil))
  ([rcPath ldr]
  (if-some
    [res (resStream rcPath ldr)]
    (with-open
      [out (ByteArrayOutputStream. BUF-SZ)
       inp res]
      (io/copy inp out :buffer-size BUF-SZ)
      (.toByteArray out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deflate
  "Compress bytes" ^bytes [^bytes bits]

  (if (some? bits)
    (let [buf (byte-array BUF-SZ)
          cpz (doto (Deflater.)
                (.setLevel Deflater/BEST_COMPRESSION)
                (.setInput bits)
                .finish)]
      (with-open
        [baos (ByteArrayOutputStream. (alength bits))]
        (loop []
          (if (.finished cpz)
            (.toByteArray baos)
            (do
              (.write baos
                      buf 0 (.deflate cpz buf))
              (recur))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn inflate
  "Decompress bytes" ^bytes [^bytes bits]

  (if (some? bits)
    (let [baos (ByteArrayOutputStream. (alength bits))
          buf (byte-array BUF-SZ)
          decr (doto (Inflater.)
                 (.setInput bits))]
      (loop []
        (if (.finished decr)
          (.toByteArray baos)
          (do
            (.write baos
                    buf 0 (.inflate decr buf))
            (recur)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn normalize
  "Hex-code all non-alpha
  chars in a file path"
  ^String [fname]

  (sreduce<>
    (fn [^StringBuilder buf ^Character ch]
      (if (or (java.lang.Character/isLetterOrDigit ch)
              (contains? #{\_ \- \.} ch))
        (.append buf ch)
        (.append buf
                 (str "0x"
                      (Integer/toString (int ch) 16)))))
    (charsit fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro now<> "current time in millis" [] `(System/currentTimeMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro !false? "Explicit not false" [x] `(not (false? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro !true? "Explicit not true" [x] `(not (true? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
(defn fmtFileUrl
  "File path as URL" ^URL [path]

  (when (and (string? path)
             (not-empty path))
    (io/as-url (if (cs/starts-with? path "file:")
                 path
                 (str "file:" path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getFPath
  "The file path only"
  ^String
  [fileUrlPath]
  (str (some-> (fmtFileUrl fileUrlPath) .getPath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn test-isa "" [reason par chi]
  (do->true
    (->
      (cond
        (and (class? par)(class? chi)) (isa? chi par)
        (or (nil? par)(nil? chi)) false
        (not (class? par)) (test-isa reason (class par) chi)
        (not (class? chi)) (test-isa reason par (class chi)))
      (assert (str chi " not-isa " par)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-some
  "Object is not null" [reason obj]
  (do->true (assert (some? obj) (str reason " is null"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-cond
  "verify a true condition" [reason cnd]
  (do->true (assert cnd (str reason))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro assert-not
  "verify a false condition" [cnd] `(assert (not ~cnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-hgl
  "String is not empty"
  [reason s]
  (do->true
    (assert (and (string? s)
                 (not-empty s)) (str reason " is empty"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti test-pos0
  "Check number is not negative"
  (fn [a b]
    (condp instance? b
      Double  :double
      Long  :long
      Float  :double
      Integer  :long
      (throwBadArg "allow numbers only"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti test-pos
  "Check number is positive"
  (fn [a b]
    (condp instance? b
      Double  :double
      Long  :long
      Float  :double
      Integer  :long
      (throwBadArg "allow numbers only"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos0
  :double
  [reason v]
  (do->true (assert (snneg? v)
                    (str reason " must be >= 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos0
  :long
  [reason v]
  (do->true (assert (snneg? v)
                    (str reason " must be >= 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos
  :double
  [reason v]
  (do->true (assert (spos? v)
                    (str reason " must be > 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos
  :long
  [reason v]
  (do->true (assert (spos? v)
                    (str reason " must be > 0"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-seq+
  "Check sequence is not empty"
  [reason v]
  (do->true (assert (> (count v) 0)
                    (str reason  " must be non empty"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rootCause
  "Find root error"
  ^Throwable
  [root]
  (loop [r root
         t (some-> ^Throwable
                   root .getCause)]
    (if (nil? t)
      r
      (recur t (.getCause t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rootCauseMsg
  "Find root error msg"
  [root] (str (some-> (rootCause root) .getMessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn sortJoin
  "Sort a list of strings and then concatenate them"

  ([ss] (sortJoin "" ss))
  ([sep ss]
   (if (nil? ss) "" (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pmap<>
  "Java Map into Clojure Map"
  {:tag APersistentMap}

  ([props] (pmap<> props true))
  ([props key?]
   {:pre [(ist? Map props)]}
   (preduce<map>
     #(assoc! %1
              (if key? (keyword %2) (str %2))
              (.get ^Map props %2)) (.keySet ^Map props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tmtask<>
  "A timer task"
  ^TimerTask [func] {:pre [(fn? func)]}
  (proxy [TimerTask][] (run [] (try! (func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol Muable
  "Something that is mutable"
  (get?setf! [_ k v] "Get or set a field")
  (setf! [_ k v] "Set a field")
  (unsetf! [_ k] "Clear a field")
  (wipe! [_] "Clear all")
  (copy* [_ mp] "Copy from this map"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-muble-types
  "deftype something mutable" [name dtype & more]
  `(deftype ~name
     [~(with-meta '_data {dtype true})]
  ~'czlab.basal.core.Muable
  ~'(get?setf! [me k v]
      (when-not (contains? _data k) (.setf! me k v)) (get _data k))
  ~'(wipe! [_ ] (set! _data {}))
  ~'(copy* [me x]
      (let [m (if (and (satisfies? czlab.basal.core/Muable x)
                       (instance? clojure.lang.IDeref x)) @x x)
            m (if (map? m) m nil)
            m (if (czlab.basal.core/!self? _data m) m)]
        (czlab.basal.core/do->nil  (if m (set! _data (merge _data m))))))
  ~'(setf! [_ k v] (set! _data (assoc _data k v)) v)
  ~'(unsetf! [_ k] (let [v (get _data k)]
                     (set! _data (dissoc _data k)) v))
  ~'czlab.basal.Stateful
  ~'(deref [_] _data)
  ~'(state [_] _data)
  ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-muble-types GenericMutable :unsynchronized-mutable)
(decl-var-private #'->GenericMutable)
;(ns-unmap *ns* '->GenericMutable)
(decl-muble-types VolatileMutable :volatile-mutable)
(decl-var-private #'->VolatileMutable)
;(ns-unmap *ns* '->VolatileMutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn prnMuable "Print this mutable"
  ([ctx] (prnMuable ctx true))
  ([ctx dbg]
   (let [s (pr-str @ctx)]
     (if dbg (log/debug "%s" s)(log/info "%s" s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn prtStk "Print stack" [exp] (some-> ^Throwable exp .printStackTrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dumpStk
  "Dump stack trace"
  ^String
  [^Throwable e]
  (with-open
    [out (ByteArrayOutputStream. BUF-SZ)
     ps (PrintStream. out true "utf-8")]
    (.printStackTrace e ps)
    (String. (.toByteArray out) "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn stripNSPath
  "Remove the leading colon"
  ^String [path] (cs/replace (str path) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare convToJava)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convList
  "To Java List"
  ^ArrayList [obj]
  (do-with [rc (ArrayList.)]
    (doseq [v obj]
      (.add rc (convToJava v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convSet
  "To Java Set"
  ^HashSet [obj]
  (do-with [rc (HashSet.)]
    (doseq [v obj]
      (.add rc (convToJava v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convMap
  "To Java Map"
  ^HashMap [obj]
  (do-with [rc (HashMap.)]
    (doseq [[k v] obj]
      (.put rc
            (stripNSPath k) (convToJava v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convToJava
  "Convert a clojure data structure
  to its Java equivalent"
  ^Object [obj]

  (cond
    (map? obj)
    (convMap obj)

    (set? obj)
    (convSet obj)

    (or (vector? obj)
        (list? obj))
    (convList obj)

    :else obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defonce ^:private ^AtomicInteger _numInt (AtomicInteger. 1))
(defonce ^:private ^AtomicLong _numLng (AtomicLong. 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqint
  "A sequence number (int)" [] (.getAndIncrement _numInt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqint2
  "A sequence number (long)" [] (.getAndIncrement _numLng))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cancelTimerTask
  "Cancel a timer task" [^TimerTask t] (trye! nil (some-> t .cancel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro marray "n-size java array" [type n] `(make-array ~type ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro zarray "Zero length java array" [type] `(marray ~type 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro countCpus "How many cpus?"
  [] `(.availableProcessors (Runtime/getRuntime)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pause
  "Block current thread for some millisecs"
  [millisecs] (trye! nil
                     (if (spos? millisecs) (Thread/sleep millisecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sysTmpDir "Java tmp dir" [] `(sysProp "java.io.tmpdir"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn id??
  "Get id of this object"
  [obj] (if (ist? Idable obj)
          (.id ^Idable obj) (if (map? obj) (:id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn objEQ?
  "Are these 2 objects identical?"
  [^Object this ^Object obj]
  (and this
       obj
       (or (identical? this obj)
           (and (= (.getClass this)
                   (.getClass obj))
                (= (.hashCode obj)
                   (.hashCode this))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn idEQ?
  "Do these 2 objects have same id?"
  [this obj] (and (objEQ? this obj) (= (id?? obj) (id?? this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn svtbl "Hook parent vtable" [vt par] (assoc vt :$proto par))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gvtbl
  "Find key from vtable"
  [vt kee]
  (if (map? vt)
    (if (in? vt kee)
      (vt kee) (gvtbl (:$proto vt) kee))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cvtbl?
  "key in vtable?"
  [vt kee]
  (if (map? vt)
    (if (in? vt kee)
      (vt kee) (gvtbl (:$proto vt) kee)) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gvtbl'
  "Find key from parent vtable" [vt kee] (gvtbl (:$proto vt) kee))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rvtbl'
  "Find key from parent vtable and run func"
  [vt kee & args]
  (let [f (gvtbl' vt kee)] (if (fn? f) (apply f vt args) f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rvtbl
  "Find key from vtable and run func"
  [vt kee & args]
  (let [f (gvtbl vt kee)] (if (fn? f) (apply f vt args) f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private _defvtable_
  "" [name & args]
  (assert (even? (count args)))
  `(def ~name
     (-> {}
        ~@(reduce (fn [acc [k v]]
                    (conj acc `(assoc ~k ~v))) [] (partition 2 args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro vtbl**
  "Make a virtual table - op1 f1 op2 f2, with parent vtbl"
  [par & args]
  (assert (even? (count args))) `(hash-map :$proto ~par ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro vtbl*
  "Make a virtual table - op1 f1 op2 f2" [& args] `(vtbl** nil ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol JEnumProto
  "Mimic Java Enum"
  (lookup-enum-str [_ s] "Get enum from string")
  (get-enum-str [_ e] "Get string value of enum")
  (lookup-enum-int [_ n] "Get enum from int"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Loose equivalent of a Java Enum
(decl-object JEnum
             JEnumProto
             (get-enum-str [me e]
               (if (in? me e)
                 (cs/replace (str e) #"^:" "")))
             (lookup-enum-str [me s]
               (let [kee (keyword s)]
                 (some #(if (= kee (first %)) (first %)) me)))
             (lookup-enum-int [me n]
              (some #(if (= n (last %)) (first %)) me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro decl-generic-enum
  "e.g. (decl-generic-enum
          weather
          hot
          cold)"
  [name base & more]
  (assert (and (not-empty more)
               (integer? base)))
  `(def ~name
     (czlab.basal.core/object<>
       czlab.basal.core.JEnum
       (-> {}
           ~@(reduce
               #(conj %1
                      `(assoc (keyword (str *ns*) ~(str %2))
                              ~(+ base (count %1)))) [] more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;how to make it private
;;(alter-meta! #'weather assoc :private true)
(defmacro decl-special-enum
  "e.g. (decl-special-enum
          weather
          hot 3
          cold 7)"
  [name & more]
  (assert (even? (count more)))
  (let [ps (partition 2 more)]
    `(def ~name
       (czlab.basal.core/object<>
         czlab.basal.core.JEnum
         (-> {}
             ~@(mapv #(do
                        `(assoc (keyword (str *ns*)
                                         ~(str (first %)))
                                ~(last %))) ps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

