;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.coreutils

  (:require [czlab.basal.log :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [czlab.basal.cmdline :as cl]
            [czlab.basal.core :as c])

  (:use [clojure.test])

  (:import [czlab.basal.core Muable GenericMutable VolatileMutable]
           [czlab.jasal Idable DataError]
           [java.security SecureRandom]
           [clojure.lang IDeref]
           [java.util
            ArrayList
            HashMap
            HashSet
            Map
            List
            Set
            Properties
            Date
            Calendar
            TimerTask]
           [java.sql Timestamp]
           [java.net URL]
           [java.io
            File
            InputStream
            IOException
            FileOutputStream
            ByteArrayInputStream]
           [java.nio.charset Charset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private dummyResourcePath "czlab/basal/etc/Resources_en.properties")
(def ^{:private true :tag  Properties} dummyProperties (Properties.))
(def ^:private VAR_USER (System/getProperty "user.name"))
(def ^:private VAR_PATH (System/getenv "PATH"))
(def ^:private ^GenericMutable MUBLE (GenericMutable. {:a 1 :b 2}))
(def ^:private ^Muable VMU (VolatileMutable. {:a 1 :b 2}))
(def ^:private idobj (reify Idable (id [_] "hello")))

(eval '(do
  (. dummyProperties put "1" "hello${user.name}")
  (. dummyProperties put "2" "hello${PATH}")
  (. dummyProperties put "3" "${user.name}${PATH}")))

(c/decl-special-enum YYY a 1 b (+ 2 3))
(c/decl-generic-enum xxx 0 a b c)

(c/decl-atomic TestEnt
  czlab.jasal.Idable
  (id [_] (:id @_data))
  Object
  (toString [_] (str (c/id?? _)))
  (hashCode [_] 999)
  czlab.jasal.Initable
  (init [_ arg] (swap! _data assoc :id arg)))

(c/decl-volatile TestCtxV
  czlab.jasal.Idable
  (id [me] (:id @me)))

(c/decl-mutable TestCtx
  czlab.jasal.Idable
  (id [me] (:id @me)))

(c/decl-object TestClass
  Idable
  (id [_] (:id _)))

(def ^:private
  TestVT-C (c/vtbl* :c (fn [_ a b] (* a b))))

(def ^:private
  TestVT-D (c/vtbl** TestVT-C :d (fn [_ a b] (/ a b))))

(def ^:private
  TestVT-E (c/vtbl**
             TestVT-D
             :c (fn [_ a b] (- a b)) :e (fn [_ a b] (+ a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-coreutils

  (testing
    "related to: core functions"
    (is (let [a (c/vargs String ["a" "b"])]
          (== 2 (alength #^"[Ljava.lang.String;" a))))

    (is (map? (c/pcoll! (transient {}))))

    (is (== 1 (:a (c/preduce<map> #(assoc! %1 :a %2) [1]))))

    (is (== 1 (last (c/preduce<vec> #(conj! %1 %2) [1]))))

    (is (= "a"
           (c/sreduce<>
             #(.append ^StringBuilder %1 %2) ["a"])))

    (is (= "a" (.getMessage (c/exp! Exception "a"))))

    (is (thrown? IOException (c/trap! IOException "a")))

    (is (let [a (c/vargs* String "a" "b")]
          (== 2 (alength #^"[Ljava.lang.String;" a))))

    (is (thrown? UnsupportedOperationException (c/throwUOE "%s" "a")))

    (is (thrown? IllegalArgumentException (c/throwBadArg "%s" "a")))

    (is (thrown? IOException (c/throwIOExp (Exception.))))

    (is (thrown? IOException (c/throwIOE "%s" "a")))

    (is (thrown? DataError (c/throwBadData "bad")))

    (is (= ::yo (c/getTypeId (with-meta {} {:typeid ::yo}))))

    (is (instance? Runnable (c/run-able<> (let [] 0))))

    (is (= "a" (c/when-some+ [a "a"] a)))

    (is (nil? (c/when-some+ [a ""] a)))

    (is (= "a" (c/if-some+ [a "a"] a)))

    (is (= "b" (c/if-some+ [a ""] a "b")))

    (is (== 3 (c/when-fn? [f inc] nil (f 2))))
    (is (== 6 (c/if-fn? [f "x"] (f 2) (inc 5))))
    (is (== 3 (c/if-fn? [f inc] (f 2))))

    (is (c/notin? #{:a :b} :c))

    (is (c/in? #{:a :b} :a))

    (is (false? (c/do->false nil nil "")))

    (is (nil? (c/do->nil nil nil "")))

    (is (true? (c/do->true nil nil "")))

    (is (c/ist? String ""))

    (is (== 3 (c/let-when [a 1 b 2] (pos? a) (+ a b))))

    (is (nil? (c/let-when [a 1 b 0] (pos? b) (+ a b))))

    (is (true? (let [x (atom 9)
                     y (atom false)]
                 (c/do-with [a (+ 3 4)]
                          (->> (-> (+ a 2) (* 3))
                               (reset! x))
                          (if (= 27 @x)
                            (reset! y true)))
                 @y)))

    (is (c/ist? String (c/cast? String (.cast String "a"))))

    (is (c/cexp? (Exception. "a")))

    (is (let [x (Exception. "ho" nil)
              e (Exception. "hi" nil)]
          (nil? (c/some** e getCause getCause))))

    (is (let [y (Exception. "yo")
              x (Exception. "ho" y)
              e (Exception. "hi" x)]
          (= "yo" (c/some** e getCause getCause getMessage))))

    (is (== 2 (c/some** "hi" toString length)))

    (is (false? (.isFirstCall (c/doto->> (c/monoFlop<>)
                                     .isFirstCall
                                     .isFirstCall))))

    (is (.isFirstCall (c/monoFlop<>)))

    (is (let [w (c/watch<>) _ (Thread/sleep 1000)
              m (.elapsedMillis w) n (.elapsedNanos w)]
          (and (>= m 1000) (>= n 1000000 ))))

    (is (== 3 (count (c/rnil [1 2 nil 3]))))

    (is (not (vector? (c/rnil [1 2 nil 3]))))

    (is (== 3 (count (c/flatnil [1 2 nil 3]))))

    (is (vector? (c/flatnil [1 2 nil 3])))

    (is (not (c/szero? nil)))
    (is (not (c/sneg? nil)))
    (is (not (c/spos? nil)))
    (is (c/snneg? 1))

    (is (> (.indexOf (c/envVar "PATH") "/bin") 0))

    (is (not (c/isFQKeyword? :a)))
    (is (c/isFQKeyword? ::a))

    (is (< (.indexOf (c/jid<>) ":\\-") 0))

    (is (let [r (c/randSign)] (or (pos? r)(neg? r))))

    (is (let [b (c/randBool)] (or (false? b)(true? b))))

    (is (c/ist? SecureRandom (c/rand<>)))
    (is (c/ist? Date (c/date<>)))

    (is (c/ist? Charset (c/toCharset "utf-16")))
    (is (> (.indexOf (c/fpath "/tmp/abc/def.txt") "/abc/") 0))
    (is (> (.indexOf (c/fpath (io/file "/t/a/d.txt")) "/a/") 0))
    (is (= "joe"
           (do (c/sysProp! "hello" "joe") (c/sysProp "hello"))))

    (is (c/ist? File (c/homeDir)))
    (is (not-empty (c/getUser)))
    (is (c/ist? File (c/getCwd)))

    (is (= "a/b/c" (c/trimLastPathSep "a/b/c/")))
    (is (= "a\\b" (c/trimLastPathSep "a\\b\\")))

    (is (let [s (c/deserialize (c/serialize "a"))]
          (and (string? s)
               (= "a" s))))

    (is (= "java.lang.String" (c/getClassname String)))
    (is (= "java.lang.String" (c/getClassname "")))
    (is (= "String" (c/gczn String)))

    (is (> (.indexOf (c/fpath "c/tmp/a.txt") "/tmp/") 0))
    (is (> (.indexOf
             (c/fpath (io/file "c/tmp/a.txt")) "/tmp/") 0))

    (is (if-not (c/isWindows?)
          (or (c/isLinux?)(c/isMacOS?)) true))
    (is (if (c/isMacOS?) (not (c/isWindows?)) true))
    (is (if (c/isLinux?) (not (c/isWindows?)) true))

    (is (and (= -1 (c/numSign -233))
             (= 1 (c/numSign 675))
             (= 0 (c/numSign 0))))

    (is (and (== 911 (c/convLong "911"))
             (== 111 (c/convLong nil 111))))

    (is (and (== 911 (c/convInt "911"))
             (== 111 (c/convInt nil 111))))

    (is (and (> (c/convDouble "911.123") 911.0)
             (> (c/convDouble nil 111.333) 111.0)))

    (is (and (c/convBool "true")
             (false? (c/convBool "false"))
             (false? (c/convBool "555"))))

    (is (= "AAA"
           (let [p (-> (.getBytes "a=AAA")
                       ByteArrayInputStream.
                       c/loadJavaProps)]
             (.getProperty p "a"))))

    (is (= "AAA"
           (let [p (-> (doto (io/file
                               (c/sysProp "java.io.tmpdir") (c/jid<>))
                         (spit "a=AAA"))
                       c/loadJavaProps)]
             (.getProperty p "a"))))

    (is (= "aaa" (c/strit (.toCharArray "aaa"))))
    (is (= "aaa" (c/strit "aaa")))
    (is (= "" (c/strit nil)))
    (is (= "3" (c/strit 3)))

    (is (= "aaa"
           (c/strit (.getBytes "aaa" "utf-8") "utf-8")))

    (is (== 97 (aget (c/bytesit "a" "utf-8") 0)))

    (is (= \e (aget (c/charsit "hello") 1)))

    (is (with-open
          [s (c/resStream "czlab/basal/etc/sample.ini")]
          (c/ist? InputStream s)))

    (is (c/ist? URL (c/resUrl "czlab/basal/etc/sample.ini")))

    (is (string? (c/resStr "czlab/basal/etc/sample.ini")))

    (is (> (alength (c/resBytes "czlab/basal/etc/sample.ini")) 0))

    (is (= "aaa"
           (c/strit (c/inflate (c/deflate (c/bytesit "aaa"))))))

    (is (not (.endsWith
               (c/normalize "/a/b/c!@#*.dat") "!@#*")))

    (is (<= (c/now<>) (c/now<>)))

    (is (let [x (c/decl-long-var 5)
              y (c/long-var x + 3)
              z (c/long-var x - 2)
              _ (c/long-var x 99)
              k (c/long-var x)]
          (and (= y 8)
               (= k 99)
               (= z 6))))

    (is (let [x (c/decl-int-var 5)
              y (c/int-var x + 3)
              z (c/int-var x - 2)
              _ (c/int-var x 99)
              k (c/int-var x)]
          (and (= y 8)
               (= k 99)
               (= z 6))))

    (is (= "/tmp/a.txt" (c/getFPath "/tmp/a.txt")))
    (is (= "/tmp/a.txt"
           (.getPath (c/fmtFileUrl "/tmp/a.txt"))))
    (is (= "/tmp/a.txt"
           (.getPath (c/fmtFileUrl "file:/tmp/a.txt"))))

    (is (thrown? Throwable (c/test-isa "reason" InputStream String)))
    (is (thrown? Throwable (c/test-isa "reason" InputStream "")))
    (is (thrown? Throwable (c/test-some "reason" nil)))
    (is (thrown? Throwable (c/test-cond "reason" (= 1 2))))
    (is (thrown? Throwable (c/assert-not (= 1 1))))
    (is (thrown? Throwable (c/test-hgl "reason" "")))

    (is (c/do->true (c/test-pos0 "reason" 0)))
    (is (c/do->true (c/test-pos0 "reason" 1)))
    (is (thrown? Throwable (c/test-pos0 "reason" -1)))

    (is (c/do->true (c/test-pos "reason" 1)))
    (is (thrown? Throwable (c/test-pos "reason" 0)))

    (is (c/do->true (c/test-seq+ "reason" [1 2 3])))
    (is (thrown? Throwable (c/test-seq+ "reason" [])))

    (is (let [a (Exception.) b (Exception. a)
              c (Exception. b) r (c/rootCause c)]
          (identical? a r)))

    (is (= "a" (let [a (Exception. "a") b (Exception. a)
                     c (Exception. b)]
                 (c/rootCauseMsg c))))

    (is (= "a,p,z" (c/sortJoin "," ["z" "p" "a"])))

    (is (= "A" (let [m (doto (HashMap.)
                         (.put "a" "A")
                         (.put "z" "Z"))] (:a (c/pmap<> m)))))

    (is (== 1 (:a (.deref (VolatileMutable. {:a 1})))))
    (is (== 1 (:a (.deref (GenericMutable. {:a 1})))))

    (is (string? (c/dumpStk (Exception. "a"))))

    (is (not= \: (.charAt (c/stripNSPath (str ::yo)) 0)))

    (is (c/ist? TimerTask (c/tmtask<> #(let [] 1))))
    (is (c/do->true (c/cancelTimerTask (c/tmtask<> #(let [] 1)))))

    (is (== 9 (do (c/setf! MUBLE :a 9)
                  (get @MUBLE :a))))

    (is (nil? (do (c/unsetf! MUBLE :b)
                  (get @MUBLE :b))))

    (is (== 7 (do (c/get?setf! MUBLE :b 7)
                  (get @MUBLE :b))))

    (is (== 7 (do (c/get?setf! MUBLE :b 6)
                  (get @MUBLE :b))))

    (is (string? (pr-str @MUBLE)))

    (is (== 9 (:a (.deref ^IDeref MUBLE))))

    (is (== 9 (:a @MUBLE)))

    (is (== 1 (do (.copy* MUBLE {:a 1 :y 4 :z 2})
                  (get @MUBLE :a))))

    (is (== 4 (count (seq @MUBLE))))

    (is (== 2 (do (c/copy* MUBLE MUBLE)
                  (get @MUBLE :z))))

    (is (== 6 (do (.wipe! MUBLE)
                  (.copy* MUBLE {:p 1 :q 5})
                  (+ (get @MUBLE :p)
                     (get @MUBLE :q)))))

    (is (not (contains? @MUBLE :z)))
    (is (== 2 (count (seq @MUBLE))))

    (is (nil? (do (.wipe! MUBLE) (get @MUBLE :q))))

    (is (thrown? DataError (c/normalizeEmail "xxxx@@@ddddd")))
    (is (thrown? DataError (c/normalizeEmail "xxxx")))
    (is (= "abc@abc.com" (c/normalizeEmail "abc@ABC.cOm")))

    (is (== 1 (.get ^Map (c/convToJava {:a 1}) "a")))
    (is (== 3 (.get ^List (c/convToJava [1 2 3]) 2)))
    (is (.contains ^Set (c/convToJava #{1 2 3}) 3))

    (is (== 1 (c/seqint2)))
    (is (== 1 (c/seqint)))
    (is (== 2 (c/seqint2)))
    (is (== 2 (c/seqint)))

    (is (= "23\n" (with-out-str (c/prn!! "%d%d" 2 3))))
    (is (= "23" (with-out-str (c/prn! "%d%d" 2 3))))

    (is (c/spos? (c/countCpus)))

    (is (let [s (c/now<>)
              _ (c/pause 1000)
              z (c/now<>)]
          (>= z (+ s 1000))))

    (is (> (.length (c/sysTmpDir)) 0))

    (is (> (c/seqint2) 0))
    (is (> (c/seqint) 0)))

  (testing
    "cmdline options"
    (is (let [[o v] (cl/parseOptions ["--a" "b" "/c" "d" "-e" "f" "g"])]
          (and (= "b" (:a o))
               (= "d" (:c o))
               (= "f" (:e o))
               (= "g" (cs/join "" v)))))
    (is (let [[o v] (cl/parseOptions ["--" "a" "b" "c"])]
          (and (empty? o)
               (= "abc" (cs/join "" v)))))
    (is (let [[o v] (cl/parseOptions ["a" "b" "c"])]
          (and (empty? o)
               (= "abc" (cs/join "" v))))))

  (testing
    "related to: vtable"
    (is (let []
          (= 2 (c/rvtbl TestVT-E :c 5 3))))
    (is (let []
          (= 15 (c/rvtbl' TestVT-E :c 5 3))))
    (is (let []
          (nil? (c/rvtbl TestVT-D :e 3 5))))
    (is (let [z {:z 99}
              x (c/svtbl TestVT-C z)]
          (= 99 (c/rvtbl x :z 3 5))))
    (is (let [c {:a (fn [_ a b] (+ a b))}]
          (= 8 (c/rvtbl c :a 3 5)))))

  (testing
    "related to: entity"
    (is (let [e (c/object<> TestClass {:a 999})]
          (= 999 (:a e))))
    (is (let [e (c/object<> TestClass {})]
          (nil? (:a e))))
    (is (let [e (c/object<> TestClass {:id 8})]
          (= 8 (c/id?? e))))
    (is (let [e (c/mutable<> TestCtxV {:id 8})]
          (= 8 (c/id?? e))))
    (is (let [e (c/mutable<> TestCtx {:id 8})]
          (c/setf! e :z 9)
          (c/copy* e {:w 3 :q 4})
          (c/get?setf! e :z 444)
          (c/get?setf! e :k 444)
          (and (= 8 (c/id?? e))
               (= 7 (+ (:w @e) (:q @e)))
               (= 9 (:z @e))
               (= 444 (:k @e)))))
    (is (let [e (c/atomic<> TestEnt)]
          (.init e "hello")
          (= "hello" (.id e))))
    (is (let [e (c/atomic<> TestEnt)]
          (.init e "hello")
          (c/alter-atomic e update-in [:w] assoc :y 9)
          (= 9 (get-in @e [:w :y]))))
    (is (let [e (c/atomic<> TestEnt)]
          (c/alter-atomic e assoc :a 3 :id 4)
          (and (= 4 (.id e))
               (= 999 (.hashCode e))
               (= 3 (:a @e))))))

  (testing
    "extra macros"
    (is (= "hello" (c/id?? idobj)))
    (is (= ::b (c/lookup-enum-str xxx "czlab.test.basal.coreutils/b")))
    (is (= "czlab.test.basal.coreutils/a" (c/get-enum-str xxx ::a)))
    (is (= ::c (c/lookup-enum-int xxx 2)))
    (is (c/self? MUBLE MUBLE))
    (is (c/!self? MUBLE VMU))
    (is (c/!true? false))
    (is (c/!false? true))
    (is (not (c/!true? true)))
    (is (not (c/!false? false)))
    (is (= (c/try!-let [a 3 b 4]  (-> (inc a) (+ b)))
           8))
    (is (= (c/let-try [a 3 b "x"] (let [z (+ 3 a)] (inc z)))
           7)))

  (testing
    ""
    (is (let [c (c/sortby :start
                          (fn [^long t1 ^long t2]
                            (.compareTo (Long/valueOf t1) t2))
                          [{:start 100 :end 4}
                           {:start 2 :end 5 }
                           {:start 60 :end 89}
                           {:start 43 :end 33}])
              f1 (first c)
              fe (last c)]
          (and (== 5 (:end f1))
               (== 4 (:end fe))))))

  (is (string? "that's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(clojure.test/run-tests 'czlab.test.basal.coreutils)
;;EOF

