;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "General helpers (imports java stuff)."
      :author "Kenneth Leung"}

  czlab.basal.util

  (:require [czlab.basal.indent :as in]
            [czlab.basal.core :as c]
            [czlab.basal.log :as l]
            [czlab.basal.str :as s]
            [clojure.string :as cs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.json :as js]
            [clojure.pprint :as pp])

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
            ResourceBundle
            StringTokenizer
            GregorianCalendar
            PropertyResourceBundle]
           [java.sql
            Timestamp]
           [java.rmi.server
            UID]
           [java.util.concurrent
            TimeUnit]
           [java.util.concurrent.atomic
            AtomicLong
            AtomicInteger]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;; #^"[Ljava.lang.Object;"
(def ^String PATHSEP (System/getProperty "file.separator"))
(def CSCZ (class (.toCharArray "")))
(def BSCZ (class (.getBytes "")))
(def ^:private SGCZ (class ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol MonoFlop "" (is-first-call? [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Watch  ""
  (watch-elapsed-millis [_] "")
  (watch-reset! [_] "")
  (watch-elapsed-nanos [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro try!!
  "Eat, log the exception and return a value."
  [value & exprs]
  `(try ~@exprs
        (catch Throwable e#
          (czlab.basal.log/warn e# "") ~value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro system-time
  "Curren time in millis." [] `(System/currentTimeMillis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-throw-exp throw-ISE
                  IllegalStateException
                  "Throw illegal state exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-throw-exp throw-UOE
                  UnsupportedOperationException
                  "Throw unsupported operation exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-throw-exp throw-BadArg
                  IllegalArgumentException
                  "Throw bad parameter exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-throw-exp throw-BadData
                  RuntimeException
                  "Throw Bad Data Exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/decl-throw-exp throw-IOE
                  java.io.IOException "Throw IO Exception")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run<>
  "A Runnable wrapper."
  [& forms]
  `(reify
     ~'java.lang.Runnable
     (~'run [~'_] (~'czlab.basal.core/try! ~@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-env-var
  "Get value for this env var."
  [vname] `(czlab.basal.core/if-some+ [e# ~vname] (System/getenv e#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro uuid<>
  "RFC4122, v4 format" [] `(str (java.util.UUID/randomUUID)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro date<> "A java Date" [] `(java.util.Date.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sys-prop!
  "Set a system property."
  [prop value] `(System/setProperty ~prop ~value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sys-prop
  "Get value of a system property."
  [prop] `(System/getProperty (str ~prop) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro user-name
  "Get the user login name." [] `(sys-prop "user.name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro home-dir
  "Get user's home dir." [] `(clojure.java.io/file (sys-prop "user.home")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-cwd
  "Get current dir." [] `(clojure.java.io/file (sys-prop "user.dir")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro decl-generic-enum
  "e.g. (decl-generic-enum weather hot cold)."
  [name base & more]
  (assert (and (number? base)
               (not-empty more)))
  `(def
     ~name
     (~'czlab.basal.core/object<>
       ~'czlab.basal.java.JEnum
       (-> {}
           ~@(reduce
               #(conj %1
                      `(assoc (keyword (str *ns*) ~(str %2))
                              ~(+ base (count %1)))) [] more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;how to make it private
;;(alter-meta! #'weather assoc :private true)
(defmacro decl-special-enum
  "e.g. (decl-special-enum weather hot 3 cold 7)."
  [name & more]
  (assert (even? (count more)))
  (let [ps (partition 2 more)]
    `(def
       ~name
       (~'czlab.basal.core/object<>
         ~'czlab.basal.java.JEnum
         (-> {}
             ~@(mapv #(do
                        `(assoc (keyword (str *ns*)
                                         ~(str (first %)))
                                ~(last %))) ps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end-macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trim-last-pathsep
  "Get rid of trailing dir paths."
  ^String [path] (.replaceFirst (str path) "[/\\\\]+$" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mono-flop<>
  "Flip on first call,
  for one-time logic."
  []
  (let [toggled (atom false)]
    (reify
      MonoFlop
      (is-first-call? [_]
        (if-not @toggled
          (c/do#true (reset! toggled true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn watch<>
  "Use to mark elapsed time"
  []
  (let [start (atom (System/nanoTime))
        f #(.convert ^TimeUnit
                     %
                     (- (System/nanoTime) @start) TimeUnit/NANOSECONDS)]
    (reify Watch
      (watch-reset! [_] (reset! start (System/nanoTime)))
      (watch-elapsed-millis [_] (f TimeUnit/MILLISECONDS))
      (watch-elapsed-nanos [_] (f TimeUnit/NANOSECONDS)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local hack
(defn- get-czldr
  {:tag ClassLoader}
  ([]
   (get-czldr nil))
  ([cl]
   (or cl (. (Thread/currentThread) getContextClassLoader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;happens to be all hex chars
(defn jid<>
  "Generate a unique id using std java."
  ^String [] (.replaceAll (str (UID.)) "[:\\-]+" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn uid<>
  "UUID, no dash!"
  ^String [] (cs/replace (uuid<>) #"-" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand<>
  "A new random object."
  {:tag SecureRandom}
  ([]
   (rand<> false))
  ([strong?]
   (doto (if strong?
           (SecureRandom/getInstanceStrong)
           (SecureRandom.))
     (.setSeed (SecureRandom/getSeed 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-bytes
  "Generate random bytes."
  ([num-bytes]
   (rand-bytes (rand<>) num-bytes))
  ([rand-obj num-bytes]
   (c/do-with
     [b (byte-array num-bytes)] (.nextBytes ^SecureRandom rand-obj b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn objid??
  "Java object id, which is actually it's hashcode." [obj]
  (if-some [obj (c/cast? Object obj)]
    (str (-> (class obj) .getSimpleName)
         "@" (Integer/toHexString (.hashCode obj))) "null@null"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn encoding??
  "Charset as string."
  ^String
  [enc]
  (if (instance? Charset enc)
    (.name ^Charset enc)
    (if (empty? enc) "utf-8" enc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn charset??
  "A java Charset of the encoding."
  {:tag Charset}
  ([]
   (charset?? "utf-8"))
  ([enc]
   (if (instance? Charset enc)
     enc
     (Charset/forName (or enc "utf-8")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fpath
  "Get the file path." ^String [arg]
  (if (instance? File arg)
    (fpath (.getCanonicalPath ^File arg))
    (cs/replace (str arg) #"\\" "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn serialize
  "Object serialization."
  ^bytes [obj] {:pre [(c/!nil? obj)]}
  (c/wo* [out (ByteArrayOutputStream. c/BUF-SZ)
          oos (ObjectOutputStream. out)]
    (.writeObject oos ^Serializable obj) (.toByteArray out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deserialize
  "Object deserialization."
  ^Serializable
  [^bytes bits]
  {:pre [(c/!nil? bits)]}
  (c/wo* [in (ByteArrayInputStream. bits)
          ois (ObjectInputStream. in)] (.readObject ois)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gczn
  "Get object's short class name."
  ^String [obj]
  (cond (nil? obj) ""
        (instance? Class obj) (.getSimpleName ^Class obj)
        :else (gczn (class obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-class-name
  "Get object's class name."
  ^String [obj]
  (cond (nil? obj) ""
        (instance? Class obj) (.getName ^Class obj)
        :else (get-class-name (class obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-windows?
  "Is Windows OS?" []
  (cs/includes? (cs/lower-case (sys-prop "os.name")) "windows"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-macos?
  "Is Mac OS?" [] (cs/includes? (sys-prop "os.name") "Mac "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-linux?
  "Is Linux OS?" [] (and (not (is-windows?)) (not (is-macos?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-java-props
  "Load properties from source."
  ^Properties [arg]
  (cond (instance? File arg)
        (load-java-props (io/as-url arg))
        (instance? URL arg)
        (c/wo*
          [inp (.openStream ^URL arg)] (load-java-props inp))
        :else
        (c/do-with [p (Properties.)]
          (some->> (c/cast? InputStream arg) (.load p )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->str
  "Coerce to a string."
  {:tag String}
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
  ^chars [obj]
  (cond (= CSCZ (class obj))
        obj

        (string? obj)
        (.toCharArray ^String obj)

        (c/!nil? obj)
        (x->chars (str obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->bytes
  "Get bytes with the right encoding."
  {:tag "[B"}
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
  ^bytes [^bytes bits]
  (if (c/!nil? bits)
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
  ^bytes [^bytes bits]
  (if (c/!nil? bits)
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
  ^String [fname]
  (s/sreduce<>
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
  "File path as URL"
  ^URL [path]
  (when (and (string? path)
             (not-empty path))
    (io/as-url
      (if (cs/starts-with? path "file:") path (str "file:" path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-fpath
  "The file path only."
  ^String
  [fpath]
  (if-some [u (cond
                (c/is? URL fpath) fpath
                (string? fpath) (fmt-file-url fpath))]
    (.getPath ^URL u)
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn root-cause
  "Find root error."
  ^Throwable
  [root]
  (loop [r root
         t (some-> ^Throwable root .getCause)]
    (if (nil? t) r (recur t (.getCause t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn root-cause-msg
  "Find root error msg."
  [root] (str (some-> (root-cause root) .getMessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pmap<>
  "Java Map into Clojure Map."
  {:tag APersistentMap}
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
  ^TimerTask [func]
  {:pre [(fn? func)]}
  (proxy [TimerTask][]
    (run [] (c/try! (func)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn prt-stk
  "Print stack."
  [exp] (some-> ^Throwable exp .printStackTrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dump-stk
  "Dump stack trace."
  ^String
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
  "To Java List."
  ^ArrayList [obj]
  (c/do-with [rc (ArrayList.)]
    (doseq [v obj] (.add rc (x->java v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- conv->set
  "To Java Set."
  ^HashSet [obj]
  (c/do-with [rc (HashSet.)]
    (doseq [v obj] (.add rc (x->java v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- conv->map
  "To Java Map."
  ^HashMap [obj]
  (c/do-with [rc (HashMap.)]
    (doseq [[k v] obj]
      (.put rc (c/strip-ns-path k) (x->java v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->java
  "Convert a clojure data structure
  to its Java equivalent."
  ^Object [obj]
  (cond (map? obj)
        (conv->map obj)
        (set? obj)
        (conv->set obj)
        (or (vector? obj)
            (list? obj))
        (conv->list obj)
        :else obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce ^:private ^AtomicInteger _num-int (AtomicInteger. 1))
(defonce ^:private ^AtomicLong _num-long (AtomicLong. 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seqint
  "A sequence number (int)." [] (.getAndIncrement _num-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seqint2
  "A sequence number (long)." [] (.getAndIncrement _num-long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cancel-timer-task
  "Cancel a timer task."
  [t] (c/try! (some-> ^TimerTask t .cancel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro count-cpus
  "How many cpus?"
  [] `(.availableProcessors (Runtime/getRuntime)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pause
  "Block current thread for some millisecs."
  [millis]
  (c/try! (if (c/spos? millis) (Thread/sleep millis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sys-tmp-dir
  "Java tmp dir." [] `(sys-prop "java.io.tmpdir"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- same-obj?
  "Are these 2 objects identical?"
  [^Object this ^Object obj]
  (or (and (nil? this)(nil? obj))
      (and this obj
           (or (identical? this obj)
               (and (= (.getClass this)
                       (.getClass obj))
                    (= (.hashCode obj)
                       (.hashCode this)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn obj-eq?
  "If these 2 objs are equal?"
  [a b]
  (cond (and (nil? a) (nil? b))
        true
        (or (nil? a) (nil? b))
        false
        :else (.equals ^Object a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol JEnumProto
  "Mimic Java Enum."
  (lookup-enum-str [_ s] "Get enum from string")
  (get-enum-str [_ e] "Get string value of enum")
  (lookup-enum-int [_ n] "Get enum from int"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Loose equivalent of a Java Enum
(defrecord JEnum []
  JEnumProto
  (get-enum-str [me e]
    (if (contains? me e)
      (cs/replace (str e) #"^:" "")))
  (lookup-enum-str [me s]
    (let [kee (keyword s)]
      (some #(if (= kee (first %)) (first %)) me)))
  (lookup-enum-int [me n]
    (some #(if (= n (last %)) (first %)) me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn url-encode
  "HTML encode."
  {:tag String}
  ([s] (url-encode s "utf-8"))
  ([s enc]
   (URLEncoder/encode (str s) (encoding?? enc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn url-decode
  "HTML decode."
  {:tag String}
  ([s]
   (url-decode s "utf8"))
  ([s enc]
   (if (string? s)
     (URLDecoder/decode ^String s (encoding?? enc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sortby
  "" [kfn cmp coll]
  (sort-by kfn
           (reify java.util.Comparator
             (compare [_ t1 t2] (cmp t1 t2))) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;in memory store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol MemSet ""
  (ms-drop! [_ obj] "Free the object from the store.")
  (ms-add! [_ obj] "Add new item to the set.")
  (ms-count [_] "Count items in the set.")
  (ms-capacity [_] "Capacity of the set.")
  (ms-nth [_ pos] "The nth item in the set.")
  (ms-each [_ cb] "Run function on each item in the set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-memset<>
  "New in-memory object store. Object must be an atom."
  ([] (new-memset<> 10))
  ([_batch]
   (let [batch (c/num?? _batch 10)
         impl (atom {:size 0
                     :next 0
                     :slots (object-array 0)})]
     (reify MemSet
       (ms-capacity [_] (:size @impl))
       (ms-count [_] (:next @impl))
       (ms-nth [_ n]
         (let [{:keys [next slots]} @impl]
           (if (< n next) (nth slots n))))
       (ms-each [_ cb]
         (let [{:keys [next slots]} @impl]
           (dotimes [i next] (cb (nth slots i) i))))
       (ms-add! [_ obj]
         {:pre [(c/atom? obj)]}
         (c/do-with [obj]
           (swap! impl
                  (c/fn_1
                    (let [{:keys [next size slots] :as root} ____1
                          next1 (+ 1 next)
                          arr (if (< next size)
                                slots
                                (Arrays/copyOf ^"[Ljava.lang.Object;"
                                               slots (int (+ size batch))))]
                      (swap! obj #(assoc % :____slot next))
                      (aset ^"[Ljava.lang.Object;" arr next obj)
                      (assoc root :slots arr :next next1 :size (count arr)))))))
       (ms-drop! [_ obj]
         (if (c/atom? obj)
           (swap! impl
                  (c/fn_1
                    (let [{:keys [next slots] :as root} ____1
                          next1 (- next 1)
                          tail (aget ^"[Ljava.lang.Object;" slots next1)
                          slot' (:____slot @tail)
                          epos' (:____slot @obj)]
                      ;move the tail to old slot
                      (aset ^"[Ljava.lang.Object;" slots next1 nil)
                      (aset ^"[Ljava.lang.Object;" slots epos' tail)
                      (swap! tail #(assoc % :____slot epos'))
                      (swap! obj #(dissoc % :____slot))
                      (merge root {:next next1}))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-cldr
  "Get current classloader."
  {:tag ClassLoader}
  ([]
   (get-cldr nil))
  ([cl]
   (or cl (.getContextClassLoader (Thread/currentThread)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-cldr
  "Set classloader."
  [c]
  (if-some [cl (c/cast? ClassLoader c)]
    (.setContextClassLoader (Thread/currentThread) cl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-resource
  "Load file with localized strings."
  ^ResourceBundle
  [arg]
  (if-some [inp (cond (or (c/is? URL arg)
                          (c/is? File arg))
                      (io/input-stream arg)
                      (string? arg)
                      (-> (get-cldr)
                          (.getResourceAsStream ^String arg)))]
    (c/wo* [inp' inp]
      (PropertyResourceBundle. inp'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-resource
  "A resource bundle."
  {:tag ResourceBundle}
  ([basename] (get-resource basename (Locale/getDefault) nil))
  ([basename locale] (get-resource basename locale nil))
  ([basename locale cl]
   (if (and locale (s/hgl? basename))
     (ResourceBundle/getBundle ^String basename
                               ^Locale locale (get-cldr cl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rstr
  "The string value for this key,
  pms may contain values
  for positional substitutions."
  ^String
  [bundle pkey & pms]
  (if (and bundle
           (s/hgl? pkey))
    (loop [src (str (.getString ^ResourceBundle
                                bundle ^String pkey))
           SZ (count pms)
           pos 0]
      ;;(log/debug "RStr key = %s, value = %s" pkey kv)
      (if (>= pos SZ)
        src
        (recur (.replaceFirst src
                              "\\{\\}"
                              (str (nth pms pos))) SZ (+ 1 pos))))
    pkey))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rstr*
  "Handle a bunch of resource keys
  (rstr bundle [\"k1\" p1 p2] [\"k2\" p3 p4] )."
  [bundle & pms]
  (mapv #(apply rstr bundle (first %) (drop 1 %)) pms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn block!
  "Block forever, or wait on this lock."
  ([]
   (try (.join (Thread/currentThread))
        (catch Throwable _ (l/exception _))))
  ([^Object lock waitMillis]
   (try (locking lock
          (if (pos? waitMillis)
            (.wait lock waitMillis)
            (.wait lock)))
        (catch Throwable _ (l/exception _)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unblock!
  "Notify all threads waiting on this lock."
  [^Object lock]
  (try (locking lock
         (.notifyAll lock))
       (catch Throwable _ (l/exception _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

