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

(ns czlabtest.xlib.coreutils

  (:require [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.xlib.core]
        [clojure.test])

  (:import  [java.util
             ArrayList
             HashMap
             HashSet
             Map
             Properties
             Date
             Calendar
             TimerTask]
            [java.sql Timestamp]
            [czlab.xlib Muble BadDataError]
            [java.security SecureRandom]
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
(set! *warn-on-reflection* false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private dummyResourcePath "czlab/xlib/Resources_en.properties")
(def ^:private dummyProperties (Properties.))
(def ^:private VAR_USER (System/getProperty "user.name"))
(def ^:private VAR_PATH (System/getenv "PATH"))
(def ^:private MUBLE (muble<> {:a 1 :b 2}))
(eval '(do
  (.put ^Properties dummyProperties "1" "hello${user.name}")
  (.put ^Properties dummyProperties "2" "hello${PATH}")
  (.put ^Properties dummyProperties "3" "${user.name}${PATH}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-coreutils

  (is (let [a (vargs String ["a" "b"])] (== 2 (alength a))))

  (is (map? (pcoll! (transient {}))))

  (is (== 1 (:a (preduce<map> #(assoc! %1 :a %2) [1]))))

  (is (== 1 (last (preduce<vec> #(conj! %1 %2) [1]))))

  (is (= "a" (sreduce<> #(.append %1 %2) ["a"])))

  (is (= "a" (.getMessage (exp! Exception "a"))))

  (is (thrown? IOException (trap! IOException "a")))

  (is (let [a (tovargs String "a" "b")] (== 2 (alength a))))

  (is (thrown? UnsupportedOperationException (throwUOE "%s" "a")))

  (is (thrown? IllegalArgumentException (throwBadArg "%s" "a")))

  (is (thrown? IOException (throwIOE (Exception.))))

  (is (thrown? IOException (throwIOE "%s" "a")))

  (is (thrown? BadDataError (throwBadData "bad")))

  (is (= ::yo (getTypeId (with-meta {} {:typeid ::yo}))))

  ;;(is (= "a" (try!! "a" (let [] (/ 1 0)))))

  ;;(is (= "a" (trye! "a" (let [] (/ 1 0)))))

  ;;(is (nil? (try! (let [] (/ 1 0)))))

  (is (instance? Runnable (runnable<> #(let [] 0))))

  (is (= "a" (when-some+ [a "a"] a)))

  (is (nil? (when-some+ [a ""] a)))

  (is (= "a" (if-some+ [a "a"] a)))

  (is (= "b" (if-some+ [a ""] a "b")))

  (is (notin? #{:a :b} :c))

  (is (in? #{:a :b} :a))

  (is (false? (do->false nil nil "")))

  (is (nil? (do->nil nil nil "")))

  (is (true? (do->true nil nil "")))

  (is (inst? String ""))

  (is (== 3 (let-when [a 1 b 2] (pos? a) (+ a b))))

  (is (nil? (let-when [a 1 b 0] (pos? b) (+ a b))))

  (is (inst? String (cast? String (.cast String "a"))))

  (is (cexp? (Exception. "a")))

  (is (false? (notnil? nil)))

  (is (notnil? ""))

  (is (false? (.firstCall (doto->> (monoFlop<>)
                                   (.firstCall )
                                   (.firstCall )))))

  (is (.firstCall (monoFlop<>)))

  (is (let [w (watch<>) _ (Thread/sleep 1000)
            m (.elapsedMillis w) n (.elapsedNanos w)]
        (and (>= m 1000) (>= n 1000000 ))))

  (is (== 3 (count (rnil [1 2 nil 3]))))

  (is (not (vector? (rnil [1 2 nil 3]))))

  (is (== 3 (count (flatnil [1 2 nil 3]))))

  (is (vector? (flatnil [1 2 nil 3])))

  (is (== 2 (:a (interject {:a 1} :a #(inc (get %1 %2))))))

  (is (identical? (nilNichts nil) NICHTS))

  (is (not (szero? nil)))
  (is (not (sneg? nil)))
  (is (not (spos? nil)))
  (is (snneg? 1))

  (is (isNichts? NICHTS))

  (is (not (isNichts? "")))

  (is (= (nilNichts "") ""))

  (is (> (.indexOf (envVar "PATH") "/bin") 0))

  ;;(is (= ::yo (asFQKeyword "yo")))

  (is (notnil? (juid)))

  (is (< (.indexOf (juid) ":\\-") 0))

  (is (let [r (randSign)] (or (pos? r)(neg? r))))

  (is (let [b (randBool)] (or (false? b)(true? b))))

  (is (inst? SecureRandom (rand<>)))
  (is (inst? Date (now<date>)))

  (is (inst? Charset (toCharset "utf-16")))
  (is (> (.indexOf (fpath "/tmp/abc/def.txt") "/abc/") 0))
  (is (> (.indexOf (fpath (io/file "/t/a/d.txt")) "/a/") 0))
  (is (= "joe"
         (do (sysProp! "hello" "joe") (sysProp "hello"))))

  (is (inst? File (homeDir)))
  (is (not-empty (getUser)))
  (is (inst? File (getCwd)))

  (is (= "a/b/c" (trimLastPathSep "a/b/c/")))
  (is (= "a\\b" (trimLastPathSep "a\\b\\")))

  (is (let [s (deserialize (serialize "a"))]
        (and (string? s)
             (= "a" s))))

  (is (= "java.lang.String" (getClassname String)))
  (is (= "java.lang.String" (getClassname "")))
  (is (= "String" (gczn String)))

  (is (> (.indexOf (filePath "c/tmp/a.txt") "/tmp/") 0))
  (is (> (.indexOf
           (filePath (io/file "c/tmp/a.txt")) "/tmp/") 0))

  (is (if-not (isWindows?)
        (or (isUnix?)(isMacOS?)) true))
  (is (if (isMacOS?) (not (isWindows?)) true))
  (is (if (isUnix?) (not (isWindows?)) true))

  (is (and (== 911 (convLong "911"))
           (== 111 (convLong nil 111))))

  (is (and (== 911 (convInt "911"))
           (== 111 (convInt nil 111))))

  (is (and (> (convDouble "911.123") 911.0)
           (> (convDouble nil 111.333) 111.0)))

  (is (and (convBool "true")
           (false? (convBool "false"))
           (false? (convBool "555"))))

  (is (= "AAA"
         (let [p (-> (.getBytes "a=AAA")
                     (ByteArrayInputStream. )
                     (loadJavaProps ))]
           (.getProperty p "a"))))

  (is (= "AAA"
         (let [p (-> (doto (io/file
                             (sysProp "java.io.tmpdir")
                             (juid))
                       (spit "a=AAA"))
                     (loadJavaProps))]
           (.getProperty p "a"))))

  (is (= "aaa"
         (stringify (.getBytes "aaa" "utf-8") "utf-8")))

  (is (== 97 (aget (bytesify "a" "utf-8") 0)))

  (is (with-open
        [s (resStream "czlab/xlib/sample.ini")]
        (inst? InputStream s)))

  (is (inst? URL (resUrl "czlab/xlib/sample.ini")))

  (is (string? (resStr "czlab/xlib/sample.ini")))

  (is (> (alength (resBytes "czlab/xlib/sample.ini")) 0))

  (is (= "aaa"
         (stringify (inflate (deflate (bytesify "aaa"))))))

  (is (not (.endsWith
             (normalize "/a/b/c!@#*.dat") "!@#*")))

  (is (= (now<>)(now<>)))

  (is (= "/tmp/a.txt" (getFPath "/tmp/a.txt")))
  (is (= "/tmp/a.txt"
         (.getPath (fmtFileUrl "/tmp/a.txt"))))
  (is (= "/tmp/a.txt"
         (.getPath (fmtFileUrl "file:/tmp/a.txt"))))

  (is (thrown? Throwable (test-isa "reason" String InputStream)))
  (is (thrown? Throwable (test-isa "reason" "" InputStream)))
  (is (thrown? Throwable (test-some "reason" nil)))
  (is (thrown? Throwable (test-cond "reason" (= 1 2))))
  (is (thrown? Throwable (assert-not (= 1 1))))
  (is (thrown? Throwable (test-hgl "reason" "")))

  (is (do->true (test-pos0 "reason" 0)))
  (is (do->true (test-pos0 "reason" 1)))
  (is (thrown? Throwable (test-pos0 "reason" -1)))

  (is (do->true (test-pos "reason" 1)))
  (is (thrown? Throwable (test-pos "reason" 0)))

  (is (do->true (test-seq+ "reason" [1 2 3])))
  (is (thrown? Throwable (test-seq+ "reason" [])))

  (is (let [a (Exception.) b (Exception. a)
            c (Exception. b) r (rootCause c)]
        (identical? a r)))

  (is (= "a" (let [a (Exception. "a") b (Exception. a)
                   c (Exception. b)]
               (rootCauseMsg c))))

  (is (= "a,p,z" (sortJoin "," ["z" "p" "a"])))

  (is (= "A" (let [m (doto (HashMap.)
                       (.put "a" "A")
                       (.put "z" "Z"))] (:a (pmap<> m)))))

  (is (== 1 (:a (.g (czlab.xlib.core.UnsynchedMObj. {:a 1})))))

  (is (== 1 (:a (.g (czlab.xlib.core.VolatileMObj. {:a 1})))))

  (is (string? (dumpStk (Exception. "a"))))

  (is (not= \: (.charAt (stripNSPath (str ::yo)) 0)))

  (is (inst? TimerTask (tmtask<> #(let [] 1))))
  (is (do->true (cancelTimerTask (tmtask<> #(let [] 1)))))

  (is (== 9 (do (.setv MUBLE :a 9)
                (.getv MUBLE :a))))

  (is (nil? (do (.unsetv MUBLE :b)
                (.getv MUBLE :b))))

  (is (== 7 (do (.getOrSet MUBLE :b 7)
                (.getv MUBLE :b))))

  (is (== 7 (do (.getOrSet MUBLE :b 6)
                (.getv MUBLE :b))))

  (is (string? (.toEDN MUBLE)))

  (is (== 9 (:a (.impl MUBLE))))

  (is (== 1 (do (.copyEx MUBLE {:a 1 :y 4 :z 2})
                (.getv MUBLE :a))))

  (is (== 4 (count (.seq MUBLE))))

  (is (== 2 (do (.copy MUBLE MUBLE)
                (.getv MUBLE :z))))

  (is (== 6 (do (.clear MUBLE)
                (.copy MUBLE (muble<> {:p 1 :q 5}))
                (+ (.getv MUBLE :p)
                   (.getv MUBLE :q)))))

  (is (not (.contains MUBLE :z)))
  (is (== 2 (count (.seq MUBLE))))

  (is (nil? (do (.clear MUBLE) (.getv MUBLE :q))))

  (is (thrown? BadDataError (normalizeEmail "xxxx@@@ddddd")))
  (is (thrown? BadDataError (normalizeEmail "xxxx")))
  (is (= "abc@abc.com" (normalizeEmail "abc@ABC.cOm")))

  (is (== 1 (.get (convToJava {:a 1}) "a")))
  (is (== 3 (.get (convToJava [1 2 3]) 2)))
  (is (== 3 (.size (convToJava #{1 2 3}))))

  (is (== 1 (seqint2)))
  (is (== 1 (seqint)))
  (is (== 2 (seqint2)))
  (is (== 2 (seqint)))

  (is (= "23\n" (with-out-str (prn!! "%d%d" 2 3))))
  (is (= "23" (with-out-str (prn! "%d%d" 2 3))))

  (is (string? "that's all folks!")))

;;(clojure.test/run-tests 'czlabtest.xlib.coreutils)

