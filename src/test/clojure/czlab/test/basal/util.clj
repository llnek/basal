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
;; Copyright © 2013-2022, Kenneth Leung. All rights reserved.

(ns czlab.test.basal.util

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.util :as u]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c])

  (:import [java.net
            URL]
           [java.util
            List
            Set
            Map
            TimerTask
            ResourceBundle]
           [java.io
            File
            InputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-util

  ;(ensure?? "try!!!" (nil? (u/try!!! (/ 1 0))))
  ;(ensure?? "try!!" (== 4 (u/try!! 4 (/ 1 0))))

  (ensure?? "system-time" (pos? (u/system-time)))

  (ensure?? "run<>;run<+>"
            (let [a (atom 0)
                  _ (.run (u/run<> (reset! a 44)))
                  ok? (== 44 @a)
                  _ (.run (u/run<+> (reset! a 77)))]
              (and ok? (== 77 @a))))

  (ensure?? "get-env-var" (let [s (u/get-env-var "PATH")]
                            (and (string? s)
                                 (pos? (count s)))))

  (ensure?? "uuid<>" (let [s (u/uuid<>)]
                       (and (== 36 (count s))
                            (cs/includes? s "-"))))

  (ensure?? "date<>" (instance? java.util.Date (u/date<>)))

  (ensure?? "set-sys-prop!;get-sys-prop"
            (= "a" (do (u/set-sys-prop! "hello.joe.test" "a")
                       (u/get-sys-prop "hello.joe.test"))))

  (ensure?? "get-user-name" (let [s (u/get-user-name)]
                              (and (string? s) (pos? (count s)))))

  (ensure?? "get-home-dir" (instance? java.io.File (u/get-user-home)))

  (ensure?? "get-user-dir" (instance? java.io.File (u/get-user-dir)))

  (ensure?? "trim-last-pathsep"
            (= "/tmp/abc" (u/trim-last-pathsep "/tmp/abc/")))

  (ensure?? "trim-last-pathsep"
            (= "\\tmp\\abc" (u/trim-last-pathsep "\\tmp\\abc\\///")))

  (ensure?? "mono-flop<>" (let [out (atom 0)
                               m (u/mono-flop<>)]
                           (dotimes [_ 10]
                             (if (u/is-first-call? m)
                               (swap! out inc))) (== 1 @out)))

  (ensure?? "watch<>;pause" (let [w (u/watch<>)]
                              (u/pause 100)
                              (and (pos? (u/elapsed-millis w))
                                   (pos? (u/elapsed-nanos w))
                                   (pos? (do (u/pause 100)
                                             (u/reset-watch! w)
                                             (u/elapsed-nanos w))))))

  (ensure?? "jid<>" (let [s (u/jid<>)]
                      (and (string? s)
                           (pos? (count s))
                           (not (cs/includes? s "-"))
                           (not (cs/includes? s ":")))))

  (ensure?? "uid<>" (let [s (u/uid<>)]
                      (and (string? s)
                           (pos? (count s))
                           (not (cs/includes? s "-")))))

  (ensure?? "rand<>" (some? (u/rand<>)))

  (ensure?? "rand-bytes" (let [b (u/rand-bytes 10)]
                           (== 10 (count b))))

  (ensure?? "emsg" (= "what!"
                      (.getMessage (Exception. "what!"))))

  (ensure?? "objid??"
            (cs/includes? (u/objid?? (Exception. "aaa")) "@"))

  (ensure?? "encoding??;charset??"
            (= "utf-8"
               (cs/lower-case (u/encoding?? (u/charset?? "utf-8")))))

  (ensure?? "pthreads" (pos? (u/pthreads)))

  (ensure?? "fpath" (= "c:/windows/win32/"
                       (u/fpath "c:\\windows\\win32\\")))

  (ensure?? "serialize;deserialize"
            (let [b (u/serialize {:a 1})
                  m (u/deserialize b)] (== 1 (:a m))))

  (ensure?? "serialize;deserialize"
            (let [b (u/serialize (Exception. "hi"))
                  e (u/deserialize b)]
              (= "hi" (.getMessage ^Exception e))))

  (ensure?? "gczn" (= "String" (u/gczn "a")))

  (ensure?? "get-class-name"
            (= "java.lang.String" (u/get-class-name "a")))

  (ensure?? "is-windows?;is-macos?;is-linux?"
            (let [m (u/is-macos?)
                  x (u/is-linux?)
                  w (u/is-windows?)]
              (cond m (and (not x)(not w))
                    w (and (not m)(not x))
                    x (and (not w)(not m)))))

  (ensure?? "x->str;x->chars;x->bytes"
            (let [c0 (u/x->chars "hello")
                  b0 (u/x->bytes c0)
                  s0 (u/x->str c0)
                  b1 (u/x->bytes s0)
                  s1 (u/x->str b1)
                  s2 (u/x->str b0)]
              (and (= s0 s1)
                   (= s1 s2))))

  (ensure?? "load-java-props"
            (let [p (u/load-java-props
                      (-> (u/get-cldr)
                          (.getResource "czlab/basal/etc/Resources_en.properties")))
                  p1 (.getProperty p "test")
                  p2 (.getProperty p "test2")]
              (and (string? p1)
                   (string? p2)
                   (= p1 p2))))

  (ensure?? "deflate;inflate"
            (let [b (u/deflate (u/x->bytes "hello joe"))
                  s (u/x->str (u/inflate b))]
              (= s "hello joe")))

  (ensure?? "safe-fpath"
            (let [s (u/safe-fpath "/tmp/abc def")]
              (= s "0x2ftmp0x2fabc0x20def")))

  (ensure?? "fmt-file-url"
            (= (u/fmt-file-url "/tmp/a.txt")
               (u/fmt-file-url "file:/tmp/a.txt")))

  (ensure?? "get-fpath"
            (let [u (u/fmt-file-url "file:/tmp/a.txt")]
              (= "/tmp/a.txt" (u/get-fpath u))))

  (ensure?? "root-cause"
            (let [a (Exception. "a")
                  b (Exception. a)
                  c (Exception. b)]
              (identical? a (u/root-cause c))))

  (ensure?? "root-cause-msg"
            (let [a (Exception. "a")
                  b (Exception. a)
                  c (Exception. b)]
              (= "a" (.getMessage (u/root-cause c)))))

  (ensure?? "pmap<>"
            (let [p (u/load-java-props
                      (-> (u/get-cldr)
                          (.getResource "czlab/basal/etc/Resources_en.properties")))
                  m (u/pmap<> p)]
              (and (string? (:test m))
                   (= (:test2 m) (:test m)))))

  (ensure?? "tmtask<>;cancel-timer-task!"
            (let [a (atom 0)
                  t (u/tmtask<> #(reset! a 3))]
              (and (c/is? TimerTask t)
                   (do (.run t)
                       (u/cancel-timer-task! t) (== 3 @a)))))

  ;(ensure?? "prt-stk" (u/prt-stk (Exception. "e")))

  (ensure?? "dump-stk" (let [s (u/dump-stk (Exception. "e"))]
                         (and (string? s)
                              (pos? (count s)))))

  (ensure?? "x->java"
            (let [obj (u/x->java {:a 1
                                  :b #{1 2 3}
                                  :c [7 8 {:z 3}]})
                  m (c/cast? Map obj)
                  _ (if (nil? m) (c/raise! "expect Map1"))
                  a (.get m "a")
                  b (c/cast? Set (.get m "b"))
                  _ (if (nil? b) (c/raise! "expect Set"))
                  c (c/cast? List (.get m "c"))
                  _ (if (nil? c) (c/raise! "expect List"))
                  z (c/cast? Map (.get c 2))
                  _ (if (nil? z) (c/raise! "expect Map3"))
                  z3 (.get z "z")]
              (and (== 1 a)
                   (== 3 (.size b))
                   (and (== 7 (.get c 0))
                        (== 8 (.get c 1)))
                   (== 3 z3))))

  (ensure?? "seqint"
            (let [old (u/seqint)
                  a (u/seqint)
                  b (u/seqint)] (and (= a (+ 1 old))
                                     (= b (+ 1 a)))))

  (ensure?? "seqint2"
            (let [old (u/seqint2)
                  a (u/seqint2)
                  b (u/seqint2)] (and (= a (+ 1 old))
                                      (= b (+ 1 a)))))

  (ensure?? "count-cpus" (pos? (u/count-cpus)))

  (ensure?? "sys-tmp-dir" (let [f (new File (u/sys-tmp-dir))]
                            (.exists f)))

  (ensure?? "obj-eq?" (let [a (String. "a")
                            b (String. "a")]
                        (u/obj-eq? a b)))

  (ensure?? "url-encode;url-decode"
            (let [s (u/url-encode "a+c+b")
                  z (u/url-decode s)]
              (= z "a+c+b")))

  (ensure?? "sortby"
            (let [c [{:age 5} {:age 3}
                     {:age 1} {:age 2}]
                  a (u/sortby :age (c/compare-asc*) c)
                  d (u/sortby :age (c/compare-des*) c)]
              (and (= [1 2 3 5] (mapv #(:age %) a))
                   (= [5 3 2 1] (mapv #(:age %) d)))))

  (ensure?? "new-memset" (let [m (u/new-memset<> 2)
                               _ (u/ms-add m (atom {:a 1}))
                               z0 (u/ms-count m)]
                           (u/ms-add m (atom {:a 2}))
                           (u/ms-add m (atom {:a 3}))
                           (and (== 1 z0)
                                (== 3 (u/ms-count m))
                                (== 4 (u/ms-capacity m)))))

  (ensure?? "each-set" (let [acc (atom 0)
                             m (u/new-memset<> 2)]
                         (u/ms-add m (atom {:a 1}))
                         (u/ms-add m (atom {:a 2}))
                         (u/ms-each m
                                    (fn [obj _]
                                      (swap! acc + (:a @obj))))
                         (== 3 @acc)))
  (ensure?? "drop->set!" (let [m (u/new-memset<> 2)
                               a (atom {:a 1})
                               b (atom {:a 1})]
                           (u/ms-add m a)
                           (u/ms-add m b)
                           (u/ms-drop m a)
                           (u/ms-drop m b)
                           (zero? (u/ms-count m))))

  (ensure?? "get-cldr" (not (nil? (u/get-cldr))))
  (ensure?? "set-cldr" (do (u/set-cldr (u/get-cldr)) true))

  (ensure?? "load-resource"
            (= "hello joe, how is your dawg"
               (-> (u/load-resource
                     (-> (u/get-cldr)
                         (.getResource "czlab/basal/etc/Resources_en.properties")))
                   (u/rstr "test"  "joe" "dawg" ))))

  (ensure?? "load-resource"
            (= ["hello joe, how is your dawg" "hello joe, how is your dawg"]
               (-> (u/load-resource
                     (-> (u/get-cldr)
                         (.getResource "czlab/basal/etc/Resources_en.properties")))
                   (u/rstr* ["test"  "joe" "dawg"] ["test2"  "joe" "dawg"] ))))

  (ensure?? "get-resource" (c/is? ResourceBundle
                                  (u/get-resource "czlab/basal/etc/Resources")))

  (ensure?? "shuffle" (and (not= "abcde"
                                 (u/shuffle "abcde"))
                           (== 3 (count (u/shuffle "abc")))))

  (ensure?? "cljrt<>"
            (let [rt (u/cljrt<>)
                  m (u/call* rt
                             :czlab.basal.util/emsg
                             [(Exception. "hello world")])
                  v (u/var* rt :czlab.basal.util/cljrt<>)]
              (and (= "hello world" m)
                   (var? v))))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-util basal-test-util
  (ct/is (c/clj-test?? test-util)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


