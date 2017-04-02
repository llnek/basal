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

  (:require [czlab.basal.logging :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.edn :as edn])

  (:use [clojure.walk])

  (:import [czlab.jasal Idable BadDataError MonoFlop Muble Watch RunnableWithId]
           [java.util.concurrent.atomic AtomicLong AtomicInteger]
           [java.util.zip DataFormatException Deflater Inflater]
           [java.util.concurrent TimeUnit]
           [java.security SecureRandom]
           [java.nio.charset Charset]
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
;; #^"[Ljava.lang.Object;"
(def ^:private CSCZ (class (.toCharArray "")))
(def ^:private BSCZ (class (.getBytes "")))
(def ^:private SGCZ (class ""))

(def ^String PATHSEP (System/getProperty "file.separator"))
(def ^String USASCII "ISO-8859-1" )
(def ^String UTF16 "UTF-16" )
(def ^String UTF8 "UTF-8" )
(def ^String SLASH   "/" )

(def BOOLS #{ "true", "yes", "on", "ok", "active", "1"} )
(def ^String HEX_CHARS "0123456789ABCDEF")
(def ^String HEX_CHS "0123456789abcdef")

(def KiloBytes 1024)
(def BUF_SZ (* 4 KiloBytes))
(def MegaBytes (* KiloBytes KiloBytes))
(def GigaBytes (* KiloBytes MegaBytes))

(def OneK 1024)
(def FourK (* 4 OneK))

(def _empty-set_ #{})
(def _empty-map_ {})
(def _empty-vec_ [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this causes issues with print-match#multimethod(IDeref,IRecord clash)
(comment
(defmacro defentity
  "Define a statful record" [name & more]
  `(defrecord
     ~name
     [~'data]
     ~'czlab.basal.Stateful
     ~'(update [_ c] (swap! data merge c))
     ~'(state [_] data)
     ~'(deref [_] @data)
     ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro defentity
  "Define a statful type" [name & more]
  `(deftype
     ~name
     [~'data]
     ~'czlab.basal.Stateful
     ~'(update [_ c] (swap! data merge c))
     ~'(state [_] data)
     ~'(deref [_] @data)
     ~'czlab.jasal.Idable
     ~'(id [_] (:id @data))
     ~'Object
     ~'(toString [me] (str (id?? me)))
     ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro entity<>
  "Create a new entity"
  ([classname] `(entity<> ~classname {}))
  ([classname seed] `(entity<> ~classname ~seed false))
  ([classname seed volatile??]
   `(let [s# ~seed]
      (new ~classname (if-not ~volatile?? (atom s#) (volatile! s#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol GetSetClr
  "Generic interface to get, set and clear values"
  (s [_ x])
  (c [_])
  (g [_]))

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
  ([a v] `(reset! ~a ~v))
  ([a] `(rset! ~a nil)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn throwUOE
  "Throw unsupported operation exception"
  [fmt & xs]
  (->> ^String (apply format fmt xs)
       (trap! UnsupportedOperationException )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn throwBadArg
  "Throw bad parameter exception"
  [fmt & xs]
  (->> ^String (apply format fmt xs)
       (trap! IllegalArgumentException )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn throwBadData
  "Throw Bad Data Exception"
  [fmt & xs]
  (->> ^String (apply format fmt xs) (trap! BadDataError )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti throwIOE "Throw IO Exception" (fn [a & xs] (class a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod throwIOE
  Throwable
  [^Throwable t & xs] (trap! java.io.IOException t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod throwIOE
  String
  [fmt & xs]
  (->> ^String (apply format fmt xs)
       (trap! java.io.IOException )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti loadJavaProps
  "Load java properties from source" {:tag Properties} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti fpath
  "Nice format a path, no-backslash" {:tag String} class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype TypeNichts [])
;;(ns-unmap *ns* '->TypeNichts)
(alter-meta! #'->TypeNichts assoc :private true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (meta nil) is fine, so no need to worry
(defmacro getTypeId "typeId from metadata" [m] `(:typeid (meta ~m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro bool! "Shorthand for boolean" [x] `(boolean ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try!!
  "Eat the exception, log and return a default value"
  [defv & exprs]
  `(try ~@exprs (catch Throwable e# (log/warn e# "") ~defv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trye!
  "Eat the exception and return a default value"
  [defv & exprs]
  `(try ~@exprs
        (catch Throwable e#
          (log/warn "Just ate a %s, yummy" e#) ~defv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trye!!
  "Eat the exception returning nil"
  [defv & exprs]
  `(try ~@exprs (catch Throwable e#  ~defv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try! "Eat the exception, return nil" [& forms] `(try!! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro runnable<>
  "Create a Runnable or RunnableWithId wrapper"

  ([func]
   `(reify Runnable (run [_] (~func))))

  ([func rid]
   `(reify
      RunnableWithId
      (run [_] (~func))
      (id [_] ~rid))))

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
(defmacro some.. "Safely handle nil"

  ([x form] `(let [x# ~x] (if-not (nil? x#) (. x# ~form))))
  ([x form & more]
   `(let [x# ~x]
      (if-not (nil? x#)
        (some.. (. x# ~form) ~@more)))))

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
(defmacro ist? "instance?" [type obj] `(instance? ~type ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(defmacro same? "identical?" [x y] `(identical? ~x ~y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try-let
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defonce NICHTS (TypeNichts.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn monoFlop<>
  "Flip on first call,
  useful for one-time logic" ^MonoFlop []

  (let [toggled (atom false)]
    (reify
      MonoFlop
      (firstCall [_]
        (if @toggled
          false
          (do->true (reset! toggled true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn watch<>
  "Use to mark elapsed time" ^Watch []

  (let [start (atom (System/nanoTime))
        f #(. ^TimeUnit %
              convert
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
(defn nilNichts "If nil, return NICHTS" ^:no-doc [obj] (or obj NICHTS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isNichts? "Is it NICHTS" ^:no-doc [obj] (identical? obj NICHTS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro rnil "Get rid of nil(s) in seq" [seq] `(remove nil? ~seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro flatnil
  "Get rid of nil(s) in seq"
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
;;(defmacro ndz "0.0 if param is nil" ^double [d] `(or ~d 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(defmacro nnz "0 is param is nil" ^long [n] `(or ~n 0))

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
  (keyword (str *ns* "/" t)))

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
(defn toCharset
  "A java Charset of the encoding"
  {:tag Charset}

  ([^String enc] (Charset/forName enc))
  ([] (toCharset "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod fpath
  String
  [^String fp]
  (if-not (empty? fp) (cs/replace fp #"\\" "/") fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod fpath
  File
  [^File aFile]
  (if (nil? aFile)
    ""
    (fpath (.getCanonicalPath aFile))))

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
(defmacro homeDir "Get user's home dir" [] `(io/file (sysProp "user.home")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro getUser "Get the user login name" [] `(sysProp "user.name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro getCwd "Get current dir" [] `(io/file (sysProp "user.dir")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn trimLastPathSep
  "Get rid of trailing dir paths"
  ^String [path] (. (str path) replaceFirst "[/\\\\]+$"  ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn serialize
  "Object serialization"
  ^bytes [obj] {:pre [(some? obj)]}

  (with-open [out (ByteArrayOutputStream. BUF_SZ)
              oos (ObjectOutputStream. out)]
    (. oos writeObject ^Serializable obj)
    (. out toByteArray)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deserialize
  "Object deserialization"
  ^Serializable [^bytes bits] {:pre [(some? bits)]}

  (with-open [in (ByteArrayInputStream. bits)
              ois (ObjectInputStream. in)] (. ois readObject)))

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
    (ist? Class obj) (. ^Class obj getSimpleName)
    :else (.. ^Object obj getClass getSimpleName)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getClassname
  "Get object's class name" ^String [obj]
  (cond
    (nil? obj) ""
    (ist? Class obj) (. ^Class obj getName)
    :else (.. ^Object obj getClass getName)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro filePath "Get the file path" [aFile] `(fpath ~aFile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isWindows?
  "Is platform Windows?" []
  (>= (.indexOf (cs/lower-case (sysProp "os.name")) "windows") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isMacOS?
  "Is platform MacOS?" []
  (>= (.indexOf (sysProp "os.name") "Mac ") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isUnix?
  "Is platform *nix"
  [] (and (not (isWindows?))
          (not (isMacOS?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convLong
  "String as a long value" {:tag Long}

  ([s] (convLong s 0))
  ([^String s dftLongVal]
   (trye!! dftLongVal (Long/parseLong s) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convInt
  "String as an int value" {:tag Integer}

  ([s] (convInt s 0))
  ([^String s dftIntVal]
   (trye!! (int dftIntVal) (Integer/parseInt s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convDouble
  "String as a double value" {:tag Double}

  ([s] (convDouble s 0.0))
  ([^String s dftDblVal]
   (trye!! dftDblVal (Double/parseDouble s) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convBool
  "String as a boolean value"
  [^String s] (contains? BOOLS (cs/lower-case (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadJavaProps
  InputStream
  [^InputStream inp] (doto (Properties.) (.load inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadJavaProps
  File
  [aFile] (loadJavaProps (io/as-url aFile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadJavaProps
  URL
  [^URL aFile]
  (with-open [inp (.openStream aFile)] (loadJavaProps inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn strit
  "Make a string from bytes or chars" {:tag String}

  ([obj] (strit obj "utf-8"))
  ([obj ^String encoding]
   (cond
     (= BSCZ  (class obj)) (String. ^bytes obj encoding)
     (= CSCZ (class obj)) (String. ^chars obj)
     (string? obj) obj
     (some? obj) (str obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsit
  "Get chars from string" ^chars [obj]
  (cond
    (string? obj) (.toCharArray ^String obj)
    (= CSCZ (class obj)) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesit
  "Get bytes with the right encoding" {:tag "[B"}

  ([obj] (bytesit obj "utf-8"))
  ([obj ^String encoding]
   (cond
    (string? obj)
    (. ^String obj getBytes (or encoding "utf-8"))
    (= CSCZ (class obj))
    (bytesit (String. ^chars obj) encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStream
  "Load the resource as stream" {:tag InputStream}

  ([rcPath] (resStream rcPath nil))
  ([^String rcPath ^ClassLoader ldr]
    (when-not (empty? rcPath)
      (-> (get-czldr ldr)
          (.getResourceAsStream  rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resUrl
  "Load the resource as URL" {:tag URL}

  ([rcPath] (resUrl rcPath nil))
  ([^String rcPath ^ClassLoader ldr]
   (when-not (empty? rcPath)
     (-> (get-czldr ldr)
         (.getResource rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStr
  "Load the resource as string" {:tag String}

  ([rcPath ^String encoding] (resStr rcPath encoding nil))
  ([rcPath] (resStr rcPath "utf-8" nil))
  ([^String rcPath
    ^String encoding ^ClassLoader ldr]
   (if-some
     [res (resStream rcPath ldr)]
     (with-open
       [out (ByteArrayOutputStream. BUF_SZ)
        inp res]
       (io/copy inp out :buffer-size BUF_SZ)
       (-> (.toByteArray out)
           (strit encoding))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resBytes
  "Load the resource as byte[]"
  ^bytes [^String rcPath & [^ClassLoader ldr]]

  (if-some
    [res (resStream rcPath ldr)]
    (with-open
      [out (ByteArrayOutputStream. BUF_SZ)
       inp res]
      (io/copy inp out :buffer-size BUF_SZ)
      (.toByteArray out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deflate
  "Compress bytes" ^bytes [^bytes bits]

  (if (some? bits)
    (let [buf (byte-array BUF_SZ)
          cpz (Deflater.)]
      (doto cpz
        (.setLevel (Deflater/BEST_COMPRESSION))
        (.setInput bits)
        (.finish))
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
    (let [buf (byte-array BUF_SZ)
          decr (Inflater.)
          baos (ByteArrayOutputStream. (alength bits))]
      (.setInput decr bits)
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
  ^String [^String fname]

  (sreduce<>
    (fn [^StringBuilder buf ^Character ch]
      (if (or (java.lang.Character/isLetterOrDigit ch)
              (contains? #{\_ \- \.} ch))
        (.append buf ch)
        (.append buf
                 (str "0x"
                      (Integer/toString (int ch) 16)))))
    (.toCharArray fname)))

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

  (when-not (empty? path)
    (io/as-url (if (cs/starts-with? path "file:")
                 path
                 (str "file:" path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getFPath
  "The file path only"
  ^String
  [^String fileUrlPath]
  (str (some-> (fmtFileUrl fileUrlPath) .getPath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti test-isa
  "Is subclass of parent"
  (fn [a b c] (if (class? c) :class :object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-isa :class [reason parz cz]
  (assert (and (some? cz)
               (class? parz)) "NPE!")
  (assert (. ^Class parz isAssignableFrom ^Class cz)
          (str reason " not-isa " (gczn parz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-isa :object [reason parz obj]
  (assert (and (class? parz)
               (some? obj)) "NPE!")
  (assert (instance? parz obj)
          (str reason " not-isa " (gczn parz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-some
  "Object is not null" [reason obj]
  (assert (some? obj) (str reason " is null")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-cond
  "verify a true condition" [reason cnd] (assert cnd (str reason)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro assert-not
  "verify a false condition" [cnd] `(assert (not ~cnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-hgl
  "String is not empty"
  [reason s] (assert (not (empty? s)) (str reason " is empty")))

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
  (assert (snneg? v)
          (str reason " must be >= 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos0
  :long
  [reason v]
  (assert (snneg? v)
          (str reason " must be >= 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos
  :double
  [reason v]
  (assert (spos? v)
          (str reason " must be > 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos
  :long
  [reason v]
  (assert (spos? v)
          (str reason " must be > 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-seq+
  "Check sequence is not empty"
  [reason v]
  (assert (> (count v) 0)
          (str reason  " must be non empty")))

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
   (if (empty? ss) "" (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pmap<>
  "Java Map into Clojure Map"
  {:tag APersistentMap}

  ([props] (pmap<> props true))
  ([^java.util.Map props key?]
   (preduce<map>
     #(assoc! %1
              (if key? (keyword %2) (str %2))
              (. props get %2)) (.keySet props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype UnsynchedMObj
  [^:unsynchronized-mutable data]
  GetSetClr

  (s [_ x] (set! data x))
  (c [_] (set! data {}))
  (g [_] data))

;;(ns-unmap *ns* '->UnsynchedMObj)
(alter-meta! #'->UnsynchedMObj
             assoc :private true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype VolatileMObj
  [^:volatile-mutable data]
  GetSetClr

  (s [_ x] (set! data x))
  (c [_] (set! data {}))
  (g [_] data))

;;(ns-unmap *ns* '->VolatileMObj)
(alter-meta! #'->VolatileMObj
             assoc :private true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn muble<>
  "A (unsynced/volatile) mutable" {:tag Muble}

  ([seed] (muble<> seed false))
  ([] (muble<> {}))
  ([seed volatile??]
   (let [^czlab.basal.core.GetSetClr
         data (if volatile??
                (VolatileMObj. (or seed {}))
                (UnsynchedMObj. (or seed {})))]
     (reify Muble
       (setv [_ k v]
         (->> (assoc (.g data) k v) (.s data)) v)
       (unsetv [_ k]
         (let [m (.g data)
               v (get m k)]
           (.s data (dissoc m k)) v))
       (getOrSet [this k v]
         (when-not
           (.contains this k)
           (.setv this k v))
         (.getv this k))
       (toEDN [_] (pr-str (.g data)))
       (intern [_] (.g data))
       (copyEx [_ m]
         (let [d (.g data)]
           (if (and (map? m)
                    (not (identical? d m)))
             (.s data (merge d m)))))
       (copy [this x]
         (when (and (ist? Muble x)
                    (not (identical? this x)))
           (doseq [[k v] (.seq ^Muble x)]
             (.setv this k v))))
       (getv [_ k] (get (.g data) k))
       (seq [_] (seq (.g data)))
       (contains [_ k]
         (contains? (.g data) k))
       (clear [_ ] (.c data))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tmtask<>
  "A timer task"
  ^TimerTask [func] {:pre [(fn? func)]}
  (proxy [TimerTask][] (run [] (try! (func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn prnMuble "Print this mutable"

  ([ctx] (prnMuble ctx false))
  ([^Muble ctx dbg]
   (let
     [buf (StringBuilder. "\n")]
     (doseq [[k v] (.seq ctx)]
       (.append buf (str k " = " v "\n")))
     (.append buf "\n")
     (let [s (str buf)]
       (if dbg (log/debug "%s" s)(log/info "%s" s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn prtStk "Print stack" [e] (some-> ^Throwable e .printStackTrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dumpStk
  "Dump stack trace" ^String [^Throwable e]

  (with-open
    [out (ByteArrayOutputStream. BUF_SZ)
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
(defn normalizeEmail
  "Check email address" ^String [^String email]

  (cond
    (empty? email)
    email

    (or (not (> (.indexOf email (int \@)) 0))
        (not= (.lastIndexOf email (int \@))
              (.indexOf email (int \@))))
    (throwBadData (str "Bad email address " email))

    :else
    (let [ss (.split email "@")]
      (if (== 2 (alength ss))
        (str (aget ss 0) "@" (cs/lower-case (aget ss 1)))
        (throwBadData (str "Bad email address " email))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare convToJava)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convList
  "to Java List" ^ArrayList [obj]

  (let [rc (ArrayList.)]
    (doseq [v obj]
      (.add rc (convToJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convSet
  "to Java Set" ^HashSet [obj]

  (let [rc (HashSet.)]
    (doseq [v obj]
      (.add rc (convToJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convMap
  "to Java Map" ^HashMap [obj]

  (let [rc (HashMap.)]
    (doseq [[k v] obj]
      (.put rc
            (stripNSPath (name k)) (convToJava v)))
    rc))

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
(defonce ^:private _numInt (AtomicInteger. 1))
(defonce ^:private _numLng (AtomicLong. 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- seqnext
  "Get the next atomic number"
  [n]
  (cond
    (ist? AtomicInteger n)
    (. ^AtomicInteger n getAndIncrement)

    (ist? AtomicLong n)
    (. ^AtomicLong n getAndIncrement)

    :else
    (throwBadArg "expecting atomic-number type")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqint "A sequence number (integer)" ^Integer [] (seqnext _numInt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqint2 "A sequence number (long)" ^long [] (seqnext _numLng))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!!
  "println with format" [fmt & args] `(print (apply format (str ~fmt "\n") ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!
  "print with format" [fmt & args] `(print (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cancelTimerTask
  "Cancel a timer task" [^TimerTask t] (try! (some-> t .cancel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro marray
  "n-size java array" [type n] `(make-array ~type ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro zarray
  "Zero length java array" [type] `(make-array ~type 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn countCpus "How many cpus?"
  [] (. (Runtime/getRuntime) availableProcessors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pause
  "Block current thread for some millisecs"
  [millisecs] (try! (if (spos? millisecs) (Thread/sleep millisecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sysTmpDir "Java tmp dir" [] `(sysProp "java.io.tmpdir"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro id?? "" [x]
  `(.id ~(with-meta x {:tag 'czlab.jasal.Idable})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn objEQ? "" [this obj]
  (and obj
       (or (identical? this obj)
           (and (= (.getClass ^Object this)
                   (.getClass ^Object obj))
                (= (.id ^Idable obj)
                   (.id ^Idable this))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


