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
;; Copyright © 2013-2024, Kenneth Leung. All rights reserved.

(ns czlab.basal.util

  "Useful additions to clojure core, (imports java stuff)."

  (:refer-clojure :exclude [shuffle])

  (:require [czlab.basal.core :as c]
            [clojure.string :as cs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.json :as js])

  ;(:use [clojure.walk])

  (:import [java.util.zip
            Deflater
            Inflater
            DataFormatException]
           [java.net
            URLEncoder
            URLDecoder]
           [java.security
            SecureRandom]
           [java.nio.charset
            Charset]
           [java.lang
            StringBuilder]
           [clojure.lang
            IFn
            RT
            Var
            Symbol
            PersistentList
            Keyword
            APersistentMap
            APersistentVector]
           [java.net
            URL]
           [java.io
            Serializable
            InputStream
            PrintStream
            File
            StringWriter
            FileInputStream
            ObjectOutputStream
            ObjectInputStream
            ByteArrayInputStream
            ByteArrayOutputStream]
           [java.util
            TimerTask
            Arrays
            Map
            Properties
            Date
            Calendar
            HashMap
            HashSet
            ArrayList
            TimeZone
            Locale
            Collections
            ResourceBundle
            StringTokenizer
            GregorianCalendar
            PropertyResourceBundle]
           [java.sql
            Timestamp]
           [java.rmi.server
            UID]
           [java.util UUID]
           [java.lang Math]
           [java.net
            InetAddress]
           [java.io
            DataInputStream]
           [java.util.concurrent
            TimeUnit]
           [java.util.concurrent.atomic
            AtomicBoolean
            AtomicLong
            AtomicInteger]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;; #^"[Ljava.lang.Object;"
(def ^{:tag String
       :doc "System's file separator."} PATHSEP (System/getProperty "file.separator"))

(def ^{:doc "Java's char-array class."} CSCZ (class (.toCharArray "")))
(def ^{:doc "Java's byte-array class."} BSCZ (class (.getBytes "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Cljrt
  "Clojure environment that can load and run a function."
  (call* [_ v arglist] "Invoke a function dynamically.")
  (var* [_ name] "Load the named var.")
  (var?? [_ name] "Load the named var.")
  (require* [_ namespacelist] "Load list of namespaces."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol MonoFlop
  "Acts like a one-time flip flip."
  (is-first-call? [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Watch
  "A simple watch timer."
  (elapsed-millis [_] "")
  (reset-watch! [_] "")
  (elapsed-nanos [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alen*

  "Same as alength on object-array."
  {:arglists '([arr])}
  [arr]

  `(alength ~(with-meta arr {:tag "[Ljava.lang.Object;"})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aget*

  "Same as aget on object-array."
  {:arglists '([arr pos])}
  [arr pos]

  `(aget ~(with-meta arr {:tag "[Ljava.lang.Object;"}) ~pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aset*

  "Same as aset on object-array."
  {:arglists '([arr pos value])}
  [arr pos value]

  `(aset ~(with-meta arr {:tag "[Ljava.lang.Object;"}) ~pos ~value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ostream

  "clojure-io's output-stream."
  {:arglists '([out])}
  [out]

  `(clojure.java.io/output-stream ~out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro istream

  "clojure-io's input-stream."
  {:arglists '([in])}
  [in]

  `(clojure.java.io/input-stream ~in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!!!

  "Eat, log the exception and return nil."
  {:arglists '([& exprs])}
  [& exprs]

  `(czlab.basal.util/try!! nil ~@exprs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!!

  "Eat, log the exception and return a value."
  {:arglists '([value & exprs])}
  [value & exprs]

  `(try ~@exprs
        (catch Throwable e#
          (czlab.basal.core/warn e# "") ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro system-time

  "Current time in millis."
  {:arglists '([])}
  []

  `(System/currentTimeMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-assert-exp assert-ISE IllegalStateException)
(c/decl-throw-exp throw-ISE IllegalStateException)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-assert-exp assert-UOE UnsupportedOperationException)
(c/decl-throw-exp throw-UOE UnsupportedOperationException)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-assert-exp assert-BadArg IllegalArgumentException)
(c/decl-throw-exp throw-BadArg IllegalArgumentException)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-assert-exp assert-BadData czlab.basal.DataError)
(c/decl-throw-exp throw-BadData czlab.basal.DataError)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-assert-exp assert-IOE java.io.IOException)
(c/decl-throw-exp throw-IOE java.io.IOException)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-assert-exp assert-FFE czlab.basal.FailFast)
(c/decl-throw-exp throw-FFE czlab.basal.FailFast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run<>

  "A Runnable wrapper - eat errors."
  {:arglists '([& forms])}
  [& forms]

  `(reify
     java.lang.Runnable
     (run [~'_] (czlab.basal.core/try! ~@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run<+>

  "A Runnable wrapper - log errors."
  {:arglists '([& forms])}
  [& forms]

  `(reify
     java.lang.Runnable
     (run [~'_] (czlab.basal.util/try!!! ~@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-env-var

  "Get value for this env var."
  {:arglists '([vname])}
  [vname]

  `(czlab.basal.core/if-some+ [e# ~vname] (System/getenv e#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro uuid<>

  "RFC4122, v4 format."
  {:arglists '([])}
  []

  `(str (java.util.UUID/randomUUID)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro date<>

  "A Java Date."
  {:arglists '([])}
  []

  `(java.util.Date.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro set-sys-prop!

  "Set a system property."
  {:arglists '([prop value])}
  [prop value]

  `(System/setProperty ~prop ~value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-sys-prop

  "Get value of a system property."
  {:arglists '([prop])}
  [prop]

  `(System/getProperty (str ~prop) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-user-name

  "Get the user login name."
  {:arglists '([])}
  []

  `(czlab.basal.util/get-sys-prop "user.name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-user-home

  "Get user's home dir."
  {:arglists '([])}
  []

  `(clojure.java.io/file (czlab.basal.util/get-sys-prop "user.home")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-user-dir

  "Get current dir."
  {:arglists '([])}
  []

  `(clojure.java.io/file (czlab.basal.util/get-sys-prop "user.dir")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end-macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trim-last-pathsep

  "Get rid of trailing dir paths."
  {:tag String
   :arglists '([path])}
  [path]

  (.replaceFirst (str path) "[/\\\\]+$" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mono-flop<>

  "One time logic, flip on first call."
  {:arglists '([][flipOnCreate?])}

  ([]
   (mono-flop<> nil))

  ([flipOnCreate?]
   (let [flag (AtomicBoolean. false)]
     (c/do-with [m (reify MonoFlop
                     (is-first-call? [_]
                       (if-not (.get flag)
                         (c/do->true (.set flag true)))))]
       (if flipOnCreate? (is-first-call? m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn watch<>

  "Use to mark elapsed time."
  {:arglists '([])}
  []

  (let [start (atom (System/nanoTime))
        f #(.convert ^TimeUnit
                     %
                     (- (System/nanoTime) @start) TimeUnit/NANOSECONDS)]
    (reify Watch
      (elapsed-millis [_] (f TimeUnit/MILLISECONDS))
      (elapsed-nanos [_] (f TimeUnit/NANOSECONDS))
      (reset-watch! [_] (reset! start (System/nanoTime))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;happens to be all hex chars
(defn jid<>

  "Generate a unique id using std java."
  {:tag String
   :arglists '([])}
  []

  (.replaceAll (str (UID.)) "[:\\-]+" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn uid<>

  "UUID, no dash!"
  {:tag String
   :arglists '([])}
  []

  (cs/replace (uuid<>) #"-" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand<>

  "A new random object."
  {:tag SecureRandom
   :arglists '([][strong?])}

  ([]
   (rand<> false))

  ([strong?]
   (doto
     (if-not strong?
       (SecureRandom.)
       (SecureRandom/getInstanceStrong))
     (.setSeed (SecureRandom/getSeed 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-bytes

  "Generate random bytes."
  {:arglists '([num-bytes]
               [rand-obj num-bytes])}

  ([num-bytes]
   (rand-bytes (rand<>) num-bytes))

  ([rand-obj num-bytes]
   (c/do-with
     [b (byte-array num-bytes)] (.nextBytes ^SecureRandom rand-obj b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn emsg

  "Get exception message."
  {:tag String
   :arglists '([e])}
  [^Throwable e]

  (some-> e .getMessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn objid??

  "Java object id, actually it's hashcode."
  {:arglists '([obj])}
  [obj]

  (if-some [obj (c/cast? Object obj)]
    (str (-> (class obj) .getSimpleName)
         "@" (Integer/toHexString (.hashCode obj))) "null@null"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn encoding??

  "Charset as string."
  {:tag String
   :arglists '([enc])}
  [enc]

  (if (instance? Charset enc)
    (.name ^Charset enc)
    (if (empty? enc) "utf-8" enc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn charset??

  "A java Charset of the encoding."
  {:tag Charset
   :arglists '([][enc][enc dv])}

  ([enc]
   (charset?? enc nil))

  ([]
   (charset?? "utf-8"))

  ([enc dv]
   (if (instance? Charset enc)
     enc
     (some-> (or enc dv) Charset/forName))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pthreads

  "Default thread count = 2 x available processors."
  {:arglists '([][n])}

  ([]
   (pthreads nil))

  ([n]
   (* (c/num?? n 2) (.availableProcessors (Runtime/getRuntime)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fpath

  "Get the file path."
  {:tag String
   :arglists '([arg])}
  [arg]

  (if-not (instance? File arg)
    (cs/replace (str arg) #"\\" "/")
    (fpath (.getCanonicalPath ^File arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn serialize

  "Object serialization."
  {:tag "[B"
   :arglists '([obj])}
  [obj]
  {:pre [(some? obj)]}

  (c/wo* [out (ByteArrayOutputStream. c/BUF-SZ)
          oos (ObjectOutputStream. out)]
    (.writeObject oos ^Serializable obj) (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deserialize

  "Object deserialization."
  {:tag Serializable
   :arglists '([bits])}
  [^bytes bits]
  {:pre [(some? bits)]}

  (c/wo* [ois (-> bits
                  ByteArrayInputStream.
                  ObjectInputStream.)] (.readObject ois)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gczn

  "Get object's short class name."
  {:tag String
   :arglists '([obj])}
  [obj]

  (cond (nil? obj) ""
        (instance? Class obj)
        (.getSimpleName ^Class obj) :else (gczn (class obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-class-name

  "Get object's class name."
  {:tag String
   :arglists '([obj])}
  [obj]

  (cond (nil? obj) ""
        (instance? Class obj)
        (.getName ^Class obj) :else (get-class-name (class obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-windows?

  "Is Windows OS?"
  {:arglists '([])}
  []

  (cs/includes? (cs/lower-case (get-sys-prop "os.name")) "windows"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-macos?

  "Is Mac OS?"
  {:arglists '([])}
  []

  (cs/includes? (get-sys-prop "os.name") "Mac "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-linux?

  "Is Linux OS?"
  {:arglists '([])}
  []

  (and (not (is-macos?)) (not (is-windows?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-java-props

  "Load properties from source."
  {:tag Properties
   :arglists '([arg])}
  [arg]

  (if (or (instance? URL arg)
          (instance? File arg))
    (c/wo* [inp (istream arg)]
           (load-java-props inp))
    (c/do-with [p (Properties.)]
      (some->> (c/cast? InputStream arg) (.load p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->str

  "Coerce to a string."
  {:tag String
   :arglists '([obj]
               [obj enc])}

  ([obj]
   (x->str obj "utf-8"))

  ([obj enc]
   (let [cz (class obj)]
     (cond (string? obj)
           obj
           (= CSCZ cz)
           (String. ^chars obj)
           (= BSCZ cz)
           (String. ^bytes obj
                    (charset?? enc))
           :else (str obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->chars

  "Get chars from string."
  {:tag "[C"
   :arglists '([obj])}
  [obj]

  (cond (= CSCZ (class obj))
        obj
        (string? obj)
        (.toCharArray ^String obj)
        (some? obj)
        (x->chars (str obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->bytes

  "Get bytes with the right encoding."
  {:tag "[B"
   :arglists '([obj][obj enc])}

  ([obj]
   (x->bytes obj "utf-8"))

  ([obj enc]
   (let [cz (class obj)]
     (cond (= ByteArrayOutputStream cz)
           (.toByteArray ^ByteArrayOutputStream obj)
           (= BSCZ cz)
           obj
           (= CSCZ cz)
           (x->bytes (x->str obj) enc)
           (string? obj)
           (.getBytes ^String obj (charset?? enc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deflate

  "Compress bytes."
  {:tag "[B"
   :arglists '([bits])}
  [^bytes bits]

  (if bits
    (let [buf (byte-array c/BUF-SZ)
          cpz (doto (Deflater.)
                (.setLevel Deflater/BEST_COMPRESSION)
                (.setInput bits)
                .finish)]
      (c/wo*
        [baos (ByteArrayOutputStream. (alength bits))]
        (loop []
          (if (.finished cpz)
            (.toByteArray baos)
            (do (.write baos
                        buf 0 (.deflate cpz buf)) (recur))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn inflate

  "Decompress bytes."
  {:tag "[B"
   :arglists '([bits])}
  [^bytes bits]

  (if bits
    (let [baos (ByteArrayOutputStream. (alength bits))
          buf (byte-array c/BUF-SZ)
          decr (doto (Inflater.)
                 (.setInput bits))]
      (loop []
        (if (.finished decr)
          (.toByteArray baos)
          (do (.write baos
                      buf 0 (.inflate decr buf)) (recur)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn safe-fpath

  "Hex-code all non-alpha chars in a file path."
  {:tag String
   :arglists '([fname])}
  [fname]

  (c/sreduce<>
    (fn [^StringBuilder buf ^Character ch]
      (if (or (java.lang.Character/isLetterOrDigit ch)
              (contains? #{\_ \- \. } ch))
        (.append buf ch)
        (.append buf
                 (str "0x"
                      (Integer/toString (int ch) 16)))))
    (x->chars fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
(defn fmt-file-url

  "File path as URL."
  {:tag URL
   :arglists '([path])}
  [path]

  (when (c/hgl? path)
    (io/as-url
      (if (cs/starts-with? path "file:") path (str "file:" path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-fpath

  "The file path only."
  {:tag String
   :arglists '([fpath])}
  [fpath]

  (if-some
    [u (cond
         (c/is? URL fpath) fpath
         (string? fpath) (fmt-file-url fpath))] (.getPath ^URL u) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn root-cause

  "Find root error."
  {:tag Throwable
   :arglists '([root])}
  [root]

  (loop [r root
         t (some-> ^Throwable root .getCause)]
    (if (nil? t) r (recur t (.getCause t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn root-cause-msg

  "Find root error msg."
  {:arglists '([root])}
  [root]

  (str (some-> (root-cause root) .getMessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pmap<>

  "Java Map into Clojure Map."
  {:arglists '([props]
               [props key?])}

  ([props]
   (pmap<> props true))

  ([props key?]
   {:pre [(instance? Map props)]}
   (c/preduce<map>
     #(assoc! %1
              (if key? (keyword %2) (str %2))
              (.get ^Map props %2)) (.keySet ^Map props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmtask<>

  "A timer task."
  {:tag TimerTask
   :arglists '([func])}
  [func]
  {:pre [(fn? func)]}

  (proxy [TimerTask][] (run [] (c/try! (func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn prn-stk

  "Print stack."
  {:arglists '([exp])}
  [exp]

  (some-> ^Throwable exp .printStackTrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dump-stk

  "Dump stack trace."
  {:tag String
   :arglists '([e])}
  [^Throwable e]

  (c/wo*
    [out (ByteArrayOutputStream. c/BUF-SZ)
     ps (PrintStream. out true "utf-8")]
    (.printStackTrace e ps)
    (String. (.toByteArray out) "utf-8")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare x->java)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- conv->list
  ^ArrayList [obj]
  (c/do-with [rc (ArrayList.)]
    (doseq [v obj] (.add rc (x->java v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- conv->set
  ^HashSet [obj]
  (c/do-with [rc (HashSet.)]
    (doseq [v obj] (.add rc (x->java v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- conv->map
  ^HashMap [obj]
  (c/do-with [rc (HashMap.)]
    (doseq [[k v] obj]
      (.put rc (c/strip-ns-path k) (x->java v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->java

  "Convert a clojure data structure
  to its Java equivalent."
  {:tag Object
   :arglists '([obj])}
  [obj]

  (cond (map? obj)
        (conv->map obj)
        (set? obj)
        (conv->set obj)
        (sequential? obj)
        (conv->list obj)
        :else obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/def- ^AtomicInteger _num-int (AtomicInteger. 1))
(c/def- ^AtomicLong _num-long (AtomicLong. 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seqint

  "A sequence number (int)."
  {:arglists '([])}
  []

  (.getAndIncrement _num-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seqint2

  "A sequence number (long)."
  {:arglists '([])}
  []

  (.getAndIncrement _num-long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cancel-timer-task!

  "Cancel a timer task."
  {:arglists '([t])}
  [t]

  (c/try! (some-> ^TimerTask t .cancel)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro count-cpus

  "How many cpu(s)?"
  {:arglists '([])}
  []

  `(.availableProcessors (Runtime/getRuntime)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pause

  "Block current thread for some millisecs."
  {:arglists '([millis])}
  [^long millis]

  (c/try! (if (c/spos? millis) (Thread/sleep millis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sys-tmp-dir

  "Java's tmp dir."
  {:arglists '([])}
  []

  `(czlab.basal.util/get-sys-prop "java.io.tmpdir"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- same-obj?
  "Are these 2 objects identical?"
  [^Object this ^Object obj]
  (or (and (nil? this)(nil? obj))
      (and this obj
           (or (identical? this obj)
               (and (= (.getClass this)
                       (.getClass obj))
                    (== (.hashCode obj)
                        (.hashCode this)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn obj-eq?

  "If these 2 objs are equal?"
  {:arglists '([a b])}
  [a b]

  (cond (and (nil? a) (nil? b))
        true
        (or (nil? a) (nil? b))
        false
        (and (bytes? a)
             (bytes? b))
        (== 0 (Arrays/compare ^bytes a ^bytes b))
        :else
        (.equals ^Object a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn url-encode

  "URL encode the string."
  {:tag String
   :arglists '([s][s enc])}

  ([s]
   (url-encode s "utf-8"))

  ([s enc]
   (.replace (URLEncoder/encode (str s) (encoding?? enc)) "+" "%20")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn url-decode

  "URL decode the string."
  {:tag String
   :arglists '([s][s enc])}

  ([s]
   (url-decode s "utf8"))

  ([s enc]
   (if (string? s)
     (URLDecoder/decode ^String s (encoding?? enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sortby

  "sort-by with comparator."
  {:arglists '([kfn cmp coll])}
  [kfn cmp coll]

  (sort-by kfn
           (reify java.util.Comparator
             (compare [_ t1 t2] (cmp t1 t2))) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;in memory store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol MemSet
  "A simple in-memory object(atom) store."
  (ms-drop [_ obj] "Free the object from the store.")
  (ms-add [_ obj] "Add new item to the set.")
  (ms-count [_] "Count items in the set.")
  (ms-capacity [_] "Capacity of the set.")
  (ms-nth [_ pos] "The nth item in the set.")
  (ms-each [_ cb] "Run function on each item in the set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-memset<>

  "New in-memory object store. Object must be an atom."
  {:arglists '([][batch])}

  ([]
   (new-memset<> nil))

  ([batch]
   (let [batch (c/num?? batch 16)
         impl (atom {:size 0
                     :next 0
                     :slots (object-array 0)})]
     (reify MemSet
       (ms-capacity [_] (:size @impl))
       (ms-count [_] (:next @impl))
       (ms-nth [_ n]
         (let [{:keys [next slots]} @impl]
           (if (< n next) (nth slots n))))
       (ms-each [me cb]
         (let [{:keys [next slots]} @impl]
           (dotimes [i next] (cb (nth slots i) i)) me))
       (ms-add [me obj]
         (c/pre (c/atom? obj)
                (not (contains? @obj :____slot)))
         (swap! impl
                (fn [{:keys [next size slots] :as root}]
                  (let [next1 (+ 1 next)
                        arr (if (< next size)
                              slots
                              (Arrays/copyOf ^"[Ljava.lang.Object;"
                                             slots (int (+ size batch))))]
                    ;inject a marker into object
                    (swap! obj #(assoc % :____slot next))
                    (aset* arr next obj)
                    (assoc root
                           :slots arr
                           :next next1
                           :size (alen* arr))))) me)
       (ms-drop [me obj]
         (swap! impl
                (fn [{:keys [next slots] :as root}]
                  (let [next1 (- next 1)
                        tail (aget* slots next1)
                        _
                        (c/pre (c/atom? tail)
                               (c/atom? obj)
                               (pos? next)
                               (c/in? @obj :____slot))
                        slot' (:____slot @tail)
                        epos' (:____slot @obj)]
                    ;move the tail to old slot
                    ;freeing up tail
                    (aset* slots next1 nil)
                    (aset* slots epos' tail)
                    (c/dissoc!! obj :____slot)
                    (c/assoc!! tail :____slot epos')
                    (merge root {:next next1})))) me)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-cldr

  "Get current classloader."
  {:tag ClassLoader
   :arglists '([][cl])}

  ([]
   (get-cldr nil))

  ([cl]
   (or cl (.getContextClassLoader (Thread/currentThread)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-cldr

  "Set classloader."
  {:arglists '([c])}
  [c]

  (if-some [cl (c/cast? ClassLoader c)]
    (.setContextClassLoader (Thread/currentThread) cl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-resource

  "Load file with localized strings. e.g. a/b/c/foo.txt"
  {:tag ResourceBundle
   :arglists '([arg])}
  [arg]

  (if-some [inp (cond (or (c/is? URL arg)
                          (c/is? File arg))
                      (istream arg)
                      (string? arg)
                      (-> (get-cldr)
                          (.getResourceAsStream ^String arg)))]
    (c/wo* [inp' inp]
      (PropertyResourceBundle. inp'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-resource

  "Get the named resource bundle, defaults to en-US."
  {:tag ResourceBundle
   :arglists '([basename]
               [basename locale]
               [basename locale cl])}

  ([basename]
   (get-resource basename (Locale. "en" "US") nil))

  ([basename locale]
   (get-resource basename locale nil))

  ([basename locale cl]
   (if (and locale (c/hgl? basename))
     (ResourceBundle/getBundle ^String basename
                               ^Locale locale (get-cldr cl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol I18NApi
  ""
  (set-bundle-path [_ bkey path] "")
  (set-bundle [_ bkey b] "")
  (get-base [_] "")
  (get-bundle [_ bkey] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-object<> I18N
                 c/AtomicGS
                 (getf [me n]
                       (get @(:o me) n))
                 (setf [me n v]
                       (swap! (:o me) assoc n v))
                 I18NApi
                 (get-base [me] (.getf me :base))
                 (get-bundle [me bkey]
                             (.getf me bkey))
                 (set-bundle [me bkey b]
                             (.setf me bkey b))
                 (set-bundle-path [me bkey path]
                                  (.setf me bkey (load-resource path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn i18n<>

  ""
  [base]

  (czlab.basal.core/atomic<> I18N {:base base}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rstr

  "The string value for this key,
  pms may contain values
  for positional substitutions."
  {:tag String
   :arglists '([bundle pkey & pms])}
  [bundle pkey & pms]

  (if (and bundle
           (c/hgl? pkey))
    (loop [src (str (.getString ^ResourceBundle
                                bundle ^String pkey))
           pos 0
           SZ (count pms)]
      ;;(c/debug "RStr key = %s, value = %s" pkey kv)
      (if (>= pos SZ)
        src
        (recur (.replaceFirst src
                              "\\{\\}"
                              (str (nth pms pos))) (+ 1 pos) SZ))) pkey))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rstr*

  "Handle a bunch of resource keys
  (rstr bundle [\"k1\" p1 p2] [\"k2\" p3 p4] )."
  {:arglists '([bundle & pms])}
  [bundle & pms]

  (mapv #(apply rstr bundle (first %) (drop 1 %)) pms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn block!

  "Block forever, or wait on this lock."
  {:arglists '([]
               [lock waitMillis])}

  ([]
   (try (.join (Thread/currentThread))
        (catch Throwable _ (c/exception _))))

  ([^Object lock waitMillis]
   (try (locking lock
          (if-not (pos? waitMillis)
            (.wait lock)
            (.wait lock waitMillis)))
        (catch Throwable _ (c/exception _)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unblock!

  "Notify all threads waiting on this lock."
  {:arglists '([lock])}
  [^Object lock]

  (try (locking lock
         (.notifyAll lock))
       (catch Throwable _ (c/exception _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn shuffle

  "Shuffle characters in string."
  {:arglists '([s])}
  [s]

  (let [lst (java.util.ArrayList.)]
    (doseq [c (seq s)] (.add lst c))
    (Collections/shuffle lst)
    (loop [i 0
           SZ (.size lst)
           out (char-array (.size lst))]
      (if (>= i SZ)
        (String. out)
        (do (aset-char out
                       i
                       (.get lst i))
            (recur (+ 1 i) SZ out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cljrt<>

  "A clojure runtime."
  {:arglists '([][cl])}

  ([]
   (cljrt<> nil))

  ([cl]
   (let [^IFn _require (RT/var "clojure.core" "require")
         cl (or cl (get-cldr))
         ^IFn _resolve (RT/var "clojure.core" "resolve")]
     (reify
       Cljrt
       (require* [_ nsps]
         (doseq [n nsps]
           (.invoke _require (Symbol/create n))))
       (call* [me v args]
         (if (or (string? v)
                 (keyword? v))
           (.call* me (.var* me v) args)
           (if-some [f (c/cast? IFn v)]
             (let [[a b c d e g] args]
               (case (count args)
                 0 (.invoke f)
                 1 (.invoke f a)
                 2 (.invoke f a b)
                 3 (.invoke f a b c)
                 4 (.invoke f a b c d)
                 5 (.invoke f a b c d e)
                 6 (.invoke f a b c d e g)
                 (throw-BadArg  "too many args to invoke"))))))
       (var?? [me name]
         (let [fname (c/kw->str name)
               v (or (.invoke _resolve
                              (Symbol/create fname))
                     (let [[a b]
                           (cs/split fname #"/")]
                       (.invoke _require
                                (Symbol/create a))
                       (RT/var a b)))]
           (if-not (var? v)
             (c/raise! "Var %s not found!" fname)) v))
       (var* [_ name]
         (let [v (.var?? _ name)
               v' (var-get v)
               x (str (type v'))]
           (if (cs/includes? x "$Unbound")
             (c/raise! "Var %s not bound!" name)) v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-options

  "Parse command line, returning options and args.
  e.g.  --a b -c d -e f g
  =>
  [{:a \"b\" :c \"d\" :e \"f\"} '(\"g\")]"
  {:arglists '([cargs]
               [cargs key?])}

  ([cargs]
   (parse-options cargs true))

  ([cargs key?]
   (letfn
     [(is-option? [option]
        (and (string? option)
             (not (.equals "--" option))
             (or (cs/starts-with? option "--")
                 (cs/starts-with? option "-"))))
      (maybe-option [option key?]
        (if (is-option? option)
          (c/if-some+
            [s (cs/replace option
                           #"^(-|/)+" "")] (if key? (keyword s) s))))]
     (loop [options (c/tmap*)
            [p1 p2 & more :as args] cargs]
       (if-some [o1 (maybe-option p1 key?)]
         (let [b? (or (nil? p2)
                      (is-option? p2))]
           (recur (assoc! options
                          o1 (if b? true p2))
                  (if b?
                    (if (nil? p2)
                      more (cons p2 more)) more)))
         (vector (persistent! options)
                 (if (.equals "--" p1)
                   (if p2 (cons p2 more)) args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pre-shuffle the chars in string
(c/def- ^String _ss
  (shuffle (str "abcdefghijklmnopqrstuvwxyz"
                "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(c/def- _chars (.toCharArray _ss))
(c/def- _uuid-len (count _ss))
(c/def- ^String int-mask "00000")
(c/def- ^String long-mask "0000000000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/def- maybe-set-ip
  (memoize
    #(let [neta (InetAddress/getLocalHost)
           b (.getAddress neta)
           ^long n (cond (.isLoopbackAddress neta)
                         (.nextLong (rand<>))
                         :else
                         (c/wo* [dis (DataInputStream.
                                       (io/input-stream b))]
                           (if (== 4 (alength b))
                             (long (.readInt dis)) (.readLong dis))))]
       (Math/abs n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;at i==19 set the high bits of clock
;;sequence as per rfc4122, sec. 4.1.5
(defn uuid-v4<>

  "RFC4122, v4 format."
  {:tag String
   :arglists '([])}
  []

  (let [rnd (rand<>)
        rc (char-array _uuid-len)]
    (dotimes [n (alength rc)]
      (aset-char rc
                 n
                 (case n
                   (8 13 18 23) \-
                   (14) \4
                   (let [d (* (.nextDouble rnd) 16)
                         r (bit-or 0 (.intValue (Double. d)))
                         pos (if-not (== n 19)
                               (bit-and r 0xf)
                               (bit-or (bit-and r 0x3) 0x8))]
                     (aget ^chars _chars pos))))) (String. rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wwid<>

  "UID based on time/ip"
  {:tag String
   :arglists '([])}
  []

  (letfn
    [(fmt [pad mask]
       (let [plen (count pad)
             mlen (count mask)]
         (if (>= mlen plen)
           (subs mask 0 plen)
           (str (.replace (c/sbf<> pad)
                          (int (- plen mlen))
                          (int plen) ^String mask)))))
     (fmt-int [nm]
       (fmt int-mask (Integer/toHexString nm)))
     (fmt-long [nm]
       (fmt long-mask (Long/toHexString nm)))
     (split-time []
       (let [s (fmt-long (system-time))
             n (count s)]
         [(c/lefts s (/ n 2))
          (c/rights s (max 0 (- n (/ n 2))))]))]
    (let [seed (.nextInt (rand<>)
                         (Integer/MAX_VALUE))
          [hi lo] (split-time)]
      (str hi
           (fmt-long (maybe-set-ip)) (fmt-int seed) (fmt-int (seqint)) lo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

