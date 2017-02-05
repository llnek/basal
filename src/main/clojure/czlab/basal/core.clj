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

  (:use [czlab.basal.consts]
        [clojure.walk])

  (:import [czlab.jasal BadDataError MonoFlop Muble Watch RunnableWithId]
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol GetSetClr

  ""

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
  "" [f c] `(persistent! (reduce ~f (transient {}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<set>
  "" [f c] `(persistent! (reduce ~f (transient #{}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<vec>
  "" [f c] `(persistent! (reduce ~f (transient []) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sreduce<>
  "" [f c] `(str (reduce ~f (StringBuilder.) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro rset!
  "Reset a atom"
  ([a v]
   `(reset! ~a ~v))
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
(defn vargs* "" [clazz & args] (vargs clazz args))

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
(defn throwBadData
  "Throw Bad Data Exception"
  [fmt & xs]
  (->> ^String (apply format fmt xs) (trap! BadDataError )))

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
(defmacro try! "Eat the exception, return nil" [& forms] `(try!! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro runnable<>
  "Create a Runnable wrapper"

  ([func]
   `(reify Runnable (run [_] (~func))))

  ([func rid]
   `(reify
      RunnableWithId
      (run [_] (~func))
      (id [_] ~rid))))

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
(defmacro doto->>

  "Combine doto and ->>"
  [x & forms]

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
(defmacro ^:private doto->

  "Combine doto and ->"
  [x & forms]

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
(defmacro inst? "instance?" [theType theObj] `(instance? ~theType ~theObj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try-let
  ""
  [bindings & forms]
  `(try (let ~bindings ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro let-try
  ""
  [bindings & forms]
  `(let ~bindings (try ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro let-when
  ""
  [bindings kond & forms]

  `(let ~bindings (when ~kond ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro XXcast?
  ""
  [someType obj]

  `^{:tag ~someType}
  ((fn []
    (let [x# ~obj]
      (if (instance? ~someType x#)
        (.cast ~someType x#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro cast?
  "If object is of this type else nil"
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
  "Flip on first call, useful for one-time logic"
  ^MonoFlop
  []
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
  "Use to mark elapsed time"
  ^Watch
  []
  (let [start (atom (System/nanoTime))]
    (reify
      Watch
      (reset [_] (reset! start (System/nanoTime)))
      (elapsedMillis [_]
        (-> TimeUnit/MILLISECONDS
            (.convert (- (System/nanoTime) @start)
                      TimeUnit/NANOSECONDS)))
      (elapsedNanos [_]
        (-> TimeUnit/NANOSECONDS
            (.convert (- (System/nanoTime) @start)
                      TimeUnit/NANOSECONDS))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local hack
(defn- get-czldr
  ""
  {:tag ClassLoader}

  ([] (get-czldr nil))
  ([cl]
    (or cl (. (Thread/currentThread) getContextClassLoader))))

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
  [someseq]
  `(into [] (remove nil? ~someseq)))

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
  ^String
  [envname]
  `(when-some+ [e# ~envname] (System/getenv e#)))

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
  ""
  [kw]
  (and (keyword? kw)
       (> (.indexOf (str kw) (int \/)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn juid
  "Generate a unique id using std java"
  ^String
  []
  ;;happens to be all hex chars
  (.replaceAll (str (UID.)) "[:\\-]+" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn randSign
  "Randomly choose a sign, positive or negative"
  ^long
  []
  (if (even? (rand-int Integer/MAX_VALUE)) 1 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn randBool
  "Randomly choose a boolean value"
  []
  (if (even? (rand-int Integer/MAX_VALUE)) true false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rand<>
  "A new random object"
  {:tag SecureRandom }

  ([] (rand<> false))
  ([strong?]
   (let [r (if strong?
             (SecureRandom/getInstanceStrong)
             (SecureRandom.))]
     (->> (SecureRandom/getSeed 4)
          (. r setSeed))
     r)))

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
  "Set a system property" [prop value] `(System/setProperty ~prop ~value))

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
  ^String
  [path]
  (. (str path) replaceFirst "[/\\\\]+$"  ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn serialize
  "Object serialization"
  ^bytes
  [obj]
  {:pre [(some? obj)]}
  (with-open
    [out (ByteArrayOutputStream. BUF_SZ)
     oos (ObjectOutputStream. out)]
    (. oos writeObject ^Serializable obj)
    (. out toByteArray )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deserialize
  "Object deserialization"
  ^Serializable
  [^bytes bits]
  {:pre [(some? bits)]}
  (with-open
    [in (ByteArrayInputStream. bits)
     ois (ObjectInputStream. in)]
    (. ois readObject )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro gczn
  "Basename of this class"
  [c]
  `(let [x# ~c]
     (if (instance? Class x#) (.getSimpleName x#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getClassname
  "Get the object's class name"
  ^String
  [obj]
  (if (nil? obj)
    "null"
    (if (inst? Class obj)
      (. ^Class obj getName)
      (.. ^Object obj getClass getName))))

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
  "String as a long value"
  {:tag Long}
  ([s] (convLong s 0))
  ([^String s dftLongVal]
   (trye! dftLongVal (Long/parseLong s) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convInt
  "String as an int value"
  {:tag Integer}
  ([s] (convInt s 0))
  ([^String s dftIntVal]
   (trye! (int dftIntVal) (Integer/parseInt s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convDouble
  "String as a double value"
  {:tag Double}
  ([s] (convDouble s 0.0))
  ([^String s dftDblVal]
   (trye! dftDblVal (Double/parseDouble s) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convBool
  "String as a boolean value"
  [^String s]
  (contains? BOOLS (cs/lower-case (str s))))

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
(defn stringify
  "Make a string from bytes"
  {:tag String}

  ([bits] (stringify bits "utf-8"))
  ([^bytes bits ^String encoding]
    (some-> bits
            (String. encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesify
  "Get bytes with the right encoding"
  ^bytes
  [^String s & [^String encoding]]
  (some-> s
          (.getBytes (or encoding "utf-8"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStream
  "Load the resource as stream"
  {:tag InputStream}
  ([rcPath] (resStream rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
    (when-not (empty? rcPath)
      (-> (get-czldr czLoader)
          (.getResourceAsStream  rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resUrl
  "Load the resource as URL"
  {:tag URL}
  ([rcPath] (resUrl rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
   (when-not (empty? rcPath)
     (-> (get-czldr czLoader)
         (.getResource rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStr
  "Load the resource as string"
  {:tag String}

  ([rcPath ^String encoding] (resStr rcPath encoding nil))
  ([rcPath] (resStr rcPath "utf-8" nil))
  ([^String rcPath
    ^String encoding ^ClassLoader czLoader]
   (if-some
     [res (resStream rcPath czLoader)]
     (with-open
       [out (ByteArrayOutputStream. BUF_SZ)
        inp res]
       (io/copy inp out :buffer-size BUF_SZ)
       (-> (.toByteArray out)
           (stringify encoding)))
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resBytes
  "Load the resource as byte[]"
  ^bytes
  [^String rcPath & [^ClassLoader czLoader]]
  (if-some
    [res (resStream rcPath czLoader)]
    (with-open
      [out (ByteArrayOutputStream. BUF_SZ)
       inp res]
      (io/copy inp out :buffer-size BUF_SZ)
      (.toByteArray out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deflate
  "Compress the given byte[]"
  ^bytes
  [^bytes bits]

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
                      buf
                      0
                      (.deflate cpz buf))
              (recur))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn inflate
  "Decompress the given byte[]"
  ^bytes
  [^bytes bits]

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
                    buf
                    0
                    (.inflate decr buf))
            (recur)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn normalize
  "Normalize a filepath, hex-code all non-alpha characters"
  ^String
  [^String fname]

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
  "The file path as URL"
  ^URL
  [^String path]

  (when-not (empty? path)
    (io/as-url (if (.startsWith path "file:")
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
  (fn [a b c] (if (instance? Class b) :class :object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-isa :class [^String reason
                            ^Class cz ^Class parz]
  (assert (and (some? cz)
               (some? parz)) "NPE!")
  (assert (.isAssignableFrom parz cz)
          (str reason " not-isa " (.getName parz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-isa :object [^String reason
                             ^Object obj ^Class parz]
  (assert (and (some? parz)
               (some? obj)) "NPE!")
  (assert (instance? parz obj)
          (str reason " not-isa " (.getName parz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-some
  "Object is not null"
  [^String reason ^Object obj]
  (assert (some? obj)
          (str reason " is null")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-cond "" [^String reason cnd] (assert cnd (str reason)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro assert-not "" [cnd] `(assert (not ~cnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-hgl
  "String is not empty"
  [^String reason ^String v]
  (assert (not (empty? v))
          (str reason " is empty")))

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
  [^String reason v]
  (assert (snneg? v)
          (str reason " must be >= 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos0
  :long
  [^String reason v]
  (assert (snneg? v)
          (str reason " must be >= 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos
  :double
  [^String reason v]
  (assert (spos? v)
          (str reason " must be > 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos
  :long
  [^String reason v]
  (assert (spos? v)
          (str reason " must be > 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-seq+
  "Check sequence is not empty"
  [^String reason v]
  (assert (> (count v) 0)
          (str reason  " must be non empty")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rootCause
  "Find the root error"
  ^Throwable
  [root]
  (loop [r root
         t (some-> ^Throwable root .getCause)]
    (if (nil? t)
      r
      (recur t (.getCause t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rootCauseMsg
  "Find the root error message"
  [root]
  (str (some-> (rootCause root) .getMessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn sortJoin
  "Sort a list of strings and then concatenate them"
  ([ss] (sortJoin "" ss))
  ([sep ss]
   (if (empty? ss)
     ""
     (cs/join sep (sort ss)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pmap<>
  "Convert Java Map into Clojure Map"
  ^APersistentMap
  [^java.util.Map props]
  (preduce<map>
    #(assoc! %1
             (keyword %2)
             (. props get %2)) (.keySet props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype UnsynchedMObj
  [^:unsynchronized-mutable data]
  GetSetClr

  (s [_ x] (set! data x))
  (c [_] (set! data {}))
  (g [_] data))

;;(ns-unmap *ns* '->UnsynchedMObj)
(alter-meta! #'->UnsynchedMObj assoc :private true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype VolatileMObj
  [^:volatile-mutable data]
  GetSetClr

  (s [_ x] (set! data x))
  (c [_] (set! data {}))
  (g [_] data))

;;(ns-unmap *ns* '->VolatileMObj)
(alter-meta! #'->VolatileMObj assoc :private true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn muble<>
  "Create a (unsynced/volatile), mutable object"
  {:tag Muble}

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
         (when (and (instance? Muble x)
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
  "Create a new timer task"
  ^TimerTask
  [func]
  {:pre [(fn? func)]}
  (proxy [TimerTask][] (run [] (try! (func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn prnMuble
  "Print out this mutable object"

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
(defn prtStk "Print stack" [^Throwable e] (some-> e .printStackTrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dumpStk
  "Dump stack trace to string"
  ^String
  [^Throwable e]
  (with-open
    [out (ByteArrayOutputStream. BUF_SZ)
     ps (PrintStream. out true "utf-8")]
    (.printStackTrace e ps)
    (String. (.toByteArray out) "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn stripNSPath
  "Remove the leading colon"
  ^String
  [path]
  (cs/replace (str path) #"^:" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn normalizeEmail
  "Normalize an email address"
  ^String
  [^String email]

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
  "Convert sequence to Java List"
  ^ArrayList
  [obj]
  (let [rc (ArrayList.)]
    (doseq [v (seq obj)]
      (.add rc (convToJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convSet
  "Convert to Java Set"
  ^HashSet
  [obj]
  (let [rc (HashSet.)]
    (doseq [v (seq obj)]
      (.add rc (convToJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convMap
  "Convert to Java Map"
  ^HashMap
  [obj]
  (let [rc (HashMap.)]
    (doseq [[k v] (seq obj)]
      (.put rc
            (stripNSPath (name k)) (convToJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convToJava
  "Convert a clojure data structure to its Java equivalent"
  ^Object
  [obj]

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
    (inst? AtomicInteger n)
    (. ^AtomicInteger n getAndIncrement)

    (inst? AtomicLong n)
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
  "" [fmt & args] `(println (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!
  "" [fmt & args] `(print (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cancelTimerTask
  "" [^TimerTask t] (try! (some-> t .cancel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn countCpus "How many cpus?"
  [] (. (Runtime/getRuntime) availableProcessors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn pause
  "Block current thread for some millisecs"
  [millisecs]
  (try! (if (spos? millisecs) (Thread/sleep millisecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sysTmpDir
  "Java tmp dir" [] `(sysProp "java.io.tmpdir"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


