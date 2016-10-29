;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Copyright (c) 2013-2016, Kenneth Leung. All rights reserved.

(ns ^{:doc "Various general helpers."
      :author "Kenneth Leung" }

  czlab.xlib.core

  (:require
    [czlab.xlib.logging :as log]
    [clojure.walk :refer :all]
    [clojure.java.io :as io]
    [clojure.string :as cs]
    [clojure.core :as ccore]
    [clojure.edn :as edn])

  (:use [czlab.xlib.consts])

  (:import
    [java.util.concurrent.atomic AtomicLong AtomicInteger]
    [java.util.zip DataFormatException Deflater Inflater]
    [czlab.xlib MonoFlop Muble Watch]
    [java.util.concurrent TimeUnit]
    [java.security SecureRandom]
    [czlab.xlib BadDataError]
    [clojure.lang
     PersistentList
     Keyword
     APersistentMap
     APersistentVector]
    [java.net URL]
    [java.nio.charset Charset]
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
(defmacro jarray "" [z c] `(into-array ~z ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro pcoll! "" [t] `(persistent! ~t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<map> "" [f c] `(persistent! (reduce ~f (transient {}) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro preduce<vec> "" [f c] `(persistent! (reduce ~f (transient []) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sreduce<> "" [f c] `(str (reduce ~f (StringBuilder.) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro exp!

  "Create an exception instance"
  {:tag Throwable}
  [e & args]

  (if (empty? args)
    `(new ~e)
    `(new ~e ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trap!

  "Throw this exception"
  [e & args]

  `(throw (exp! ~e ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn tovargs "" [clazz & args] (into-array clazz args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn throwUOE

  "Force throw an unsupported operation exception"
  [^String fmt & xs]

  (->> ^String
       (apply format fmt xs)
       (trap! UnsupportedOperationException )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn throwBadArg

  "Force throw a bad parameter exception"
  [^String fmt & xs]

  (->> ^String
       (apply format fmt xs)
       (trap! IllegalArgumentException )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti throwIOE
  "Throw an IO Exception" (fn [a & xs] (class a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod throwIOE

  Throwable
  [^Throwable t & xs]

  (trap! java.io.IOException t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod throwIOE

  String
  [^String fmt & xs]

  (->> ^String
       (apply format fmt xs)
       (trap! java.io.IOException )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn throwBadData

  "Throw an Bad Data Exception"
  [^String fmt & xs]

  (->> ^String
       (apply format fmt xs)
       (trap! BadDataError )))

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
(defmacro getTypeId

  "Get the typeid from the metadata"
  [m]

  `(:typeid (meta ~m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try!!

  "Eat the exception, log and return a default value"
  [defv & exprs]

  `(try ~@exprs (catch Throwable e# (log/warn e# "") ~defv )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro trye!

  "Eat the exception and return a default value"
  [defv & exprs]

  `(try ~@exprs
        (catch Throwable e#
          (log/warn "Just ate a %s, yummy" (.toString e#)) ~defv )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro try!

  "Eat the exception and return nil"
  [& forms]

  `(try!! nil ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro runnable<>

  "Create a Runnable wrapper"
  {:tag Runnable}
  [func]

  `(reify Runnable (run [_] (~func))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro when-some+

  "bindings => binding-form test
   When test is not empty, evaluates body with binding-form bound to the
   value of test"
  [bindings & body]

  (let [form (bindings 0)
        tst (bindings 1)]
    `(let [temp# ~tst]
       (if-not (empty? temp#)
         (let [~form temp#] ~@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro if-some+

  "bindings => binding-form test
   When test is not empty, evaluates body with binding-form bound to the
   value of test"

  ([bindings then] `(if-some+ ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (empty? temp#)
          ~else
          (let [~form temp#] ~then))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defmacro do->false
  "Do and return false" [& forms] `(do ~@forms false))
(defmacro do->nil
  "Do and return nil" [& forms] `(do ~@forms nil))
(defmacro do->true
  "Do and return true" [& forms] `(do ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro inst?

  "Same as clojure's instance?"
  [theType theObj]

  `(instance? ~theType ~theObj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro let-when

  ""
  [bindings kond & forms]

  `(let ~bindings (when ~kond ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro cast?

  "If object is an instance of this type,
   return it else nil"
  [someType obj]

  `^{:tag ~someType}
  ((fn []
    (let [x# ~obj]
      (if (instance? ~someType x#)
        (.cast ~someType x#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro cexp?

  "Try to cast into an exception"
  {:tag Throwable}
  [e]

  (cast? Throwable e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro notnil?

  "is x not nil"
  [x]

  `(not (nil? ~x)))

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
    (or cl (.getContextClassLoader (Thread/currentThread)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nilNichts

  "If object is nil, return a NICHTS"
  {:tag Object :no-doc true}
  [obj]

  (or obj NICHTS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isNichts?

  "true if the object is the NICHTS"
  ^:no-doc
  [obj]

  (identical? obj NICHTS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro flatnil

  "Get rid of any nil(s) in a sequence"
  ^APersistentVector
  [somesequence]

  `(into [] (remove nil? ~somesequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro rnil

  "Get rid of any nil(s) in a sequence"
  ^PersistentList
  [somesequence]

  `(remove nil? ~somesequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn interject

  "Run the function on the current field value,
   replacing the key with the returned value.
   function(pojo oldvalue) -> newvalue"
  [pojo field func]
  {:pre [(map? pojo)(fn? func)]}

  (->> (apply func pojo field [])
       (assoc pojo field )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro spos?

  "Safely test positive number"
  [e]

  `(let [e# ~e]
     (and (number? e#)(pos? e#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ndz

  "0.0 if param is nil"
  ^double
  [d]

  `(or ~d 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro nnz

  "0 is param is nil"
  ^long
  [n]

  `(or ~n 0))

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
  ^Keyword
  [^String t]
  {:pre [(not (empty? t))
         (< (.indexOf t "/") 0)]}

  (keyword (str *ns* "/" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn juid

  "Generate a unique id using std java"
  ^String
  []

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
          (.setSeed r))
     r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro now<date>

  "A java Date"
  {:tag java.util.Date}
  []

  `(java.util.Date.))

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

  "Set the value of a system property"
  [prop value]

  `(System/setProperty ~prop ~value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro sysProp

  "Get the value of a system property"
  {:tag String}
  [prop]

  `(System/getProperty (str ~prop) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro homeDir

  "Get the user's home directory"
  {:tag File}
  []

  `(io/file (sysProp "user.home")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro getUser

  "Get the current user login name"
  {:tag String}
  []

  `(sysProp "user.name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro getCwd

  "Get the current dir"
  {:tag File}
  []

  `(io/file (sysProp "user.dir")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn trimLastPathSep

  "Get rid of trailing dir paths"
  ^String
  [path]

  (.replaceFirst (str path) "[/\\\\]+$"  ""))

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
    (.writeObject oos ^Serializable obj)
    (.toByteArray out)))

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
    (.readObject ois)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro gczn

  "The basename of this class"
  {:tag String}
  [c]

  `(let [x# ~c]
     (if (instance? Class x#) (.getSimpleName x#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getClassname

  "Get the object's class name"
  ^String
  [^Object obj]

  (if (nil? obj)
    "null"
    (.getName (.getClass obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro filePath

  "Get the file path"
  {:tag String}
  [aFile]

  `(fpath aFile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isWindows?

  "true if platform is windows"
  []

  (>= (.indexOf (cs/lower-case
                  (sysProp "os.name"))
                "windows") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isMacOS?

  "true if platform is MacOS"
  []

  (>= (.indexOf (sysProp "os.name")
                "Mac ") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isUnix?

  "true if platform is *nix"
  []

  (not (isWindows?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convLong

  "Parse string as a long value"
  {:tag Long}

  ([^String s] (convLong s 0))
  ([^String s dftLongVal]
   (trye! dftLongVal (Long/parseLong s) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convInt

  "Parse string as an int value"
  {:tag Integer}

  ([^String s] (convInt s 0))
  ([^String s dftIntVal]
   (trye! (int dftIntVal) (Integer/parseInt s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convDouble

  "Parse string as a double value"
  {:tag Double}

  ([^String s] (convDouble s 0.0))
  ([^String s dftDblVal]
   (trye! dftDblVal (Double/parseDouble s) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn convBool

  "Parse string as a boolean value"
  [^String s]

  (contains? BOOLS (cs/lower-case (str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadJavaProps

  InputStream
  [^InputStream inp]

  (doto (Properties.) (.load inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadJavaProps

  File
  [^File aFile]

  (loadJavaProps (io/as-url aFile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod loadJavaProps

  URL
  [^URL aFile]

  (with-open
    [inp (.openStream aFile)]
    (loadJavaProps inp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn stringify

  "Make a string from bytes"
  {:tag String}

  ([^bytes bits] (stringify bits "utf-8"))
  ([^bytes bits ^String encoding]
    (when (some? bits) (String. bits encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesify

  "Get bytes with the right encoding"
  ^bytes

  ([^String s] (bytesify s "utf-8"))
  ([^String s ^String encoding]
    (when (some? s) (.getBytes s encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStream

  "Load the resource as stream"
  {:tag InputStream}

  ([^String rcPath] (resStream rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
    (when-not (empty? rcPath)
      (-> (get-czldr czLoader)
          (.getResourceAsStream  rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resUrl

  "Load the resource as URL"
  {:tag URL}

  ([^String rcPath] (resUrl rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
   (when-not (empty? rcPath)
     (-> (get-czldr czLoader)
         (.getResource rcPath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resStr

  "Load the resource as string"
  {:tag String}

  ([^String rcPath ^String encoding]
   (resStr rcPath encoding nil))

  ([^String rcPath]
   (resStr rcPath "utf-8" nil))

  ([^String rcPath
    ^String encoding ^ClassLoader czLoader]
   (with-open
     [out (ByteArrayOutputStream. BUF_SZ)
      inp (resStream rcPath czLoader)]
     (io/copy inp out :buffer-size BUF_SZ)
     (-> (.toByteArray out)
         (stringify encoding)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn resBytes

  "Load the resource as byte[]"
  ^bytes

  ([^String rcPath] (resBytes rcPath nil))
  ([^String rcPath ^ClassLoader czLoader]
   (with-open
     [out (ByteArrayOutputStream. BUF_SZ)
      inp (resStream rcPath czLoader) ]
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
          baos (ByteArrayOutputStream. (alength bits)) ]
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

  (->>
    (reduce
      (fn [^StringBuilder buf ^Character ch]
        (if (or (java.lang.Character/isLetterOrDigit ch)
                (contains? #{\_ \- \.} ch))
          (.append buf ch)
          (.append buf
                   (str "0x"
                        (Integer/toString (int ch) 16)))))
      (StringBuilder.)
      (.toCharArray fname))
    (str )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro now<>

  "the current time in milliseconds"
  []
  `(System/currentTimeMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getFPath

  "the file path only"
  ^String
  [^String fileUrlPath]

  (if (empty? fileUrlPath)
    ""
    (.getPath (io/as-url fileUrlPath))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtFileUrl

  "the file path as URL"
  ^URL
  [^String path]

  (when-not (empty? path)
    (io/as-url (if (.startsWith "file:" path)
                 path
                 (str "file://" path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test and assert funcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti test-isa

  "Check object is subclass of parent"

  (fn [a b c]
    (if (instance? Class b) :class :object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-isa

  :class
  [^String reason ^Class childz ^Class parz]

  (assert (and (some? childz)
               (some? parz)) "NPE!")
  (assert (.isAssignableFrom parz childz)
          (str reason " not-isa " (.getName parz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-isa

  :object
  [^String reason ^Object obj ^Class parz]

  (assert (and (some? parz)
               (some? obj)) "NPE!")
  (assert (instance? parz obj)
          (str reason " not-isa " (.getName parz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-some

  "Check object is not null"
  [^String reason ^Object obj]

  (assert (some? obj)
          (str reason " is null")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-cond

  "Check a condition"
  [^String reason cnd]

  (assert cnd (str reason)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro assert-not

  ""
  [cnd]

  `(assert (not ~cnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn test-hgl

  "Check string is not empty"
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

  (assert (>= v 0.0)
          (str reason " must be >= 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos0

  :long
  [^String reason v]

  (assert (>= v 0)
          (str reason " must be >= 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos

  :double
  [^String reason v]

  (assert (> v 0.0)
          (str reason " must be > 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod test-pos

  :long
  [^String reason v]

  (assert (> v 0)
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

  "Dig into error and find the root exception"
  ^Throwable
  [root]

  (loop [r root
         t (if (some? root)
             (.getCause ^Throwable root))]
    (if (nil? t)
      r
      (recur t (.getCause t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn rootCauseMsg

  "Dig into error and find the root exception message"
  [root]

  (if-some [e (rootCause root)]
    (str (.getName (.getClass e))
         ": "
         (.getMessage e))
    ""))

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

  (persistent!
    (reduce #(assoc! %1 (keyword %2) (.get props %2))
            (transient {})
            (.keySet props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype UnsynchedMObj

  [^:unsynchronized-mutable data]
  GetSetClr

  (s [_ x] (set! data x))
  (c [_] (set! data {}))
  (g [_] data))

(ns-unmap *ns* '->UnsynchedMObj)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype VolatileMObj

  [^:volatile-mutable data]
  GetSetClr

  (s [_ x] (set! data x))
  (c [_] (set! data {}))
  (g [_] data))

(ns-unmap *ns* '->VolatileMObj)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn muble<>

  "Create a (unsynced/volatile), mutable object"
  {:tag Muble}

  ([seed] (muble<> seed false))
  ([] (muble<> {} false))
  ([seed volatile??]
   (let [^czlab.xlib.core.GetSetClr
         data (if volatile??
                (VolatileMObj. (or seed {}))
                (UnsynchedMObj. (or seed {})))]
     (reify Muble
       (setv [_ k v]
         (->> (assoc (.g data) k v)
                     (.s data))
         v)
       (unsetv [_ k]
         (let [v (get (.g data) k)]
           (->> (dissoc (.g data) k)
                (.s data))
           v))

       (getOrSet [this k v]
         (when-not
           (.contains this k)
           (.setv this k v))
         (.getv this k))
       (toEDN [_] (pr-str (.g data)))
       (impl [_] (.g data))
       (copyEx [_ m]
         (if (map? m) (.s data m)))
       (copy [this x]
         (when (and (some? x)
                    (not (identical? this x)))
           (doseq [[k v] (.seq ^Muble x)]
             (.setv this k v))))
       (seq [_] (seq (.g data)))
       (contains [_ k] (contains? (.g data) k))
       (getv [_ k] (get (.g data) k))
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

  ([^Muble ctx] (prnMuble ctx false))

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
(defn prtStk

  "Print stack trace"
  [^Throwable e]

  (some-> e (.printStackTrace)))

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

  (let [s (str path)]
    (if (.startsWith s ":")
      (.substring s 1)
      s)))

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
    (let [ss (.split email "@") ]
      (if (== 2 (alength ss))
        (str (aget ss 0) "@" (cs/lower-case (aget ss 1)))
        (throwBadData (str "Bad email address " email))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare toJava)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convList

  "Convert sequence to Java List"
  ^ArrayList
  [obj]

  (let [rc (ArrayList.)]
    (doseq [v (seq obj)]
      (.add rc (toJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convSet

  "Convert to Java Set"
  ^HashSet
  [obj]

  (let [rc (HashSet.)]
    (doseq [v (seq obj)]
      (.add rc (toJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- convMap

  "Convert to Java Map"
  ^HashMap
  [obj]

  (let [rc (HashMap.)]
    (doseq [[k v] (seq obj)]
      (.put rc (name k) (toJava v)))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- toJava

  "Convert a clojure collection to its Java equivalent"
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
(defn convToJava

  "Convert a clojure object to a Java object"
  ^Object
  [obj]

  (toJava obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defonce ^:private _numInt (AtomicInteger. 1))
(defonce ^:private _numLng (AtomicLong. 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqnext

  "Get the next atomic number"
  [n]

  (cond
    (inst? AtomicInteger n)
    (.getAndIncrement ^AtomicInteger n)

    (inst? AtomicLong n)
    (.getAndIncrement ^AtomicLong n)

    :else
    (throwBadArg "expecting atomic-number type")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqint

  "A sequence number (integer)"
  ^Integer
  []

  (seqnext _numInt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn seqint2

  "A sequence number (long)"
  ^long
  []

  (seqnext _numLng))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!!

  ""
  [fmt & args]

  `(println (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro prn!

  ""
  [fmt & args]

  `(print (apply format ~fmt ~@args [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cancelTimerTask
  ""
  [^TimerTask t]
  (try! (do->nil (some-> t (.cancel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


