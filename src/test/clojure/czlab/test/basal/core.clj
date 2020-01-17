;; Copyright ©  2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.core

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.core
             :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for testing state monad
(defn- mult3
  [x] (* 3 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add2
  [x] (+ 2 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- exlog

  "wrapper so that the actual computation is inside
  a state-monadic value, together with the log msg."
  [expr log]

  (fn [s]
    (let [{v :value slog :log} s
          v' (expr v)
          log' (conj slog
                     (str log "(" v ")"))]
      [v' {:value v' :log log'}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ZZZ 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol QQQ (foo [_]))
(defprotocol AAA (goo [_]))
(defrecord KKK [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-core

  (ensure?? "funcit??"
            (== 12
                (c/funcit?? (fn [a b c] (+ a b c)) 3 4 5)))

  (ensure-thrown?? "pre"
                   :any
                   (c/pre (nil? nil) (= 3 3) (= 4 5)))

  (ensure?? "atom?"
            (and (c/atom? (atom 0))
                 (not (c/atom? 0))))

  (ensure?? "sas?;!sas?"
            (and (c/sas? QQQ (reify QQQ (foo [_])))
                 (c/!sas? AAA (reify QQQ (foo [_])))))

  (ensure?? "is?;!is?"
            (and (c/is? String "a") (c/!is? String 3)))

  (ensure?? "map->"
            (= {:b 2 :z 1}
               (c/map-> [[:z 1] [:b 2]])))

  (ensure?? "vec->"
            (= [3 2 1] (c/vec-> '(3 2 1))))

  (ensure?? "set->"
            (= #{3 2 1} (c/set-> '(3 2 1))))

  (ensure?? "cc+1"
            (= '(3 4 5) (c/cc+1 3 [4 5])))

  (ensure?? "cc+"
            (= '(2 3 4 5) (c/cc+ [2 3] [4 5])))

  (ensure?? "_2" (== 3 (c/_2 [1 3])))

  (ensure?? "_1" (== 1 (c/_1 [1 3])))

  (ensure?? "_3" (== 4 (c/_3 [1 3 4])))

  (ensure?? "_E;_NE" (and (== 4 (c/_E [1 3 4]))
                          (== 4 (c/_NE [1 3 4]))))

  (ensure?? "!zero?" (c/!zero? 7))

  (ensure?? "n#" (== 3 (c/n# [1 2 4])))

  (ensure?? "car;cdr"
            (and (== 2 (c/car '(2 3 4)))
                 (= '(3 4) (c/cdr '(2 3 4)))))

  (ensure?? "one?;two?;one+?"
            (and (c/one? [1])
                 (c/two? [1 2])
                 (c/one+? [1 2 3])))

  (ensure?? "assoc!!;dissoc!!"
            (and (= {:b 3} @(c/dissoc!! (atom {:a 2 :b 3 :c 7}) :a :c))
                 (= {:a 2 :b 3 :c 7} @(c/assoc!! (atom {:b 3}) :a 2 :c 7))))

  (ensure?? "fn_*;fn_3;fn_2;fn_1;fn_0"
            (and (= '(1 2 3) ((c/fn_* ____xs) 1 2 3))
                 (= "yo" ((c/fn_0 "yo")))
                 (== 2 ((c/fn_1 ____1) 2))
                 (== 3 ((c/fn_2 (+ ____1 ____2)) 1 2))
                 (== 6 ((c/fn_3 (+ ____1 ____2 ____3)) 1 2 3))))

  (ensure?? "tmap*;tvec*;tset*"
            (and (= {:a 1} (persistent! (c/tmap* {:a 1})))
                 (= {:a 1} (persistent! (assoc! (c/tmap*) :a 1)))
                 (= [:a 1] (persistent! (c/tvec* [:a 1])))
                 (= [:a] (persistent! (conj! (c/tvec*) :a)))
                 (= #{:a} (persistent! (c/tset* #{:a})))
                 (= #{:a} (persistent! (conj! (c/tset*) :a)))))

  (ensure?? "ps!" (= #{:a} (c/persist! (conj! (transient #{}) :a))))

  (ensure?? "atomic" (= {:a 1 :b 3} @(c/atomic :a 1 :b 3)))

  (ensure?? "mapfv" (= [5 6 7] (c/mapfv + 4 1 2 3)))

  (ensure?? "last-index;nexth"
            (and (== 3 (c/last-index '(1 2 3 4)))
                 (== 4 (c/nexth '(1 2 3 4) 2))))

  (ensure-thrown?? "nexth-out-of-bound"
                   java.lang.IndexOutOfBoundsException
                   (nil? (c/nexth '(1 2 3 4) 99)))

  (ensure?? "chop" (= [[1 2] [3 4]] (c/chop 2 [1 2 3 4])))

  (ensure?? "inc*" (== 4 (c/inc* (+ 1 2))))

  (ensure?? "dec*" (== 2 (c/dec* (+ 2 1))))

  (ensure?? "do->false" (false? (c/do->false "aaa")))
  (ensure?? "do->nil" (nil? (c/do->nil "aaa")))
  (ensure?? "do->true" (true? (c/do->true "aaa")))

  (ensure?? "let->false" (false? (c/let->false [a 3] a)))
  (ensure?? "let->nil" (nil? (c/let->nil [a 3] a)))
  (ensure?? "let->true" (true? (c/let->true [a 3] a)))

  (ensure?? "defenum"
            (== 4 (do (c/defenum xxx 1 a b c) (+ xxx-a xxx-c))))

  (ensure?? "n#-even?"
            (and (c/n#-even? [])
                 (c/n#-even? [1 2]) (not (c/n#-even? [1]))))

  (ensure?? "condp??" (let [arg 8]
                        (and (== 13 (c/condp?? = arg 1 1 3 3 13))
                             (nil? (c/condp?? = arg 1 1 3 3)))))

  (ensure?? "case??" (let [arg 8]
                       (and (== 13 (c/case?? arg 1 1 3 3 13))
                            (nil? (c/case?? arg 1 1 3 3)))))

  (ensure?? "if-inst;when-inst"
            (and (== 1 (c/if-inst String "aaa" 1 2))
                 (== 2 (c/when-inst String "aaa" 2))))

  (ensure?? "if-proto;when-proto"
            (and (== 1 (c/if-proto QQQ (reify QQQ (foo [_])) 1 2))
                 (== 2 (c/when-proto QQQ (reify QQQ (foo [_])) 2))))

  (ensure?? "if-nil;when-nil"
            (and (== 1 (c/if-nil nil 1 2))
                 (== 2 (c/when-nil nil 2))))

  (ensure?? "if-var;when-var"
            (and (== 1 (c/if-var [v #'ZZZ] 1 2))
                 (== 3 (c/when-var [v #'ZZZ] 3))))

  (ensure?? "if-number;if-string"
            (and (== 13 (c/if-number [x (+ 3 4)] (+ x 6) 333))
                 (= "hello!" (c/if-string [x "hello"] (str x "!") "eee"))
                 (== 13 (c/when-number [x (+ 3 4)] (+ x 6)))
                 (= "hello!" (c/when-string [x "hello"] (str x "!")))))

  (ensure?? "if-throw;when-throw"
            (and (some? (c/if-throw [x (Exception. "")] x nil))
                 (some? (c/when-throw [x (Exception. "")] x))))

  (ensure?? "nloop" (== 3 (let [acc (atom 0)]
                            (c/nloop 3 (swap! acc inc)) @acc)))

  (ensure?? "each*"
            (= [[:a 0] [:b 1] [:c 2]]
               (let [acc (atom [])]
                 (c/each* #(swap! acc conj [%1 %2]) [:a :b :c]) @acc)))

  (ensure?? "each"
            (= [:a :b :c]
               (let [acc (atom [])]
                 (c/each #(swap! acc conj %) [:a :b :c]) @acc)))

  (ensure?? "mput!;mdel!;mget"
            (let [m (doto (java.util.HashMap.)
                      (.put "a" 1) (.put "b" 2))]
              (c/mput! m "c" 3)
              (and (== 1 (c/mget m "a"))
                   (== 2 (c/mdel! m "b"))
                   (== 3 (c/mget m "c")))))

  (ensure?? "object<>"
            (and (== 1 (:a (c/object<> KKK :a 1)))
                 (== 1 (:a (c/object<> KKK {:a 1})))))

  (ensure?? "vargs" (= "Long[]"
                       (.getSimpleName (class (c/vargs Long [1 2])))))

  (ensure?? "preduce<map,set,vec>"
            (and (= {:a "a" :b "b" :c "c"}
                    (c/preduce<map> #(assoc! %1 %2 (name %2)) [:a :b :c]))
                 (= #{:a :c}
                    (c/preduce<set> #(conj! %1 %2) [:a :a :c :a]))
                 (= [1 2]
                    (c/preduce<vec> #(conj! %1 (last %2)) {:a 1 :b 2}))))

  (ensure?? "rset!"
            (== 7 (let [a (atom 3)] (c/rset! a 7) @a)))

  (ensure-thrown?? "exp!"
                   java.lang.Exception
                   (throw (c/exp! java.lang.Exception)))

  (ensure-thrown?? "trap!"
                   java.lang.Exception
                   (c/trap! java.lang.Exception "hello"))

  (ensure-thrown?? "raise!"
                   java.lang.Exception
                   (c/raise! "Hello %s" "world!"))

  (ensure-thrown?? "assert-on!"
                   java.lang.Exception
                   (c/assert-on! (== 1 2) java.lang.Exception "hello"))

  (ensure?? "decl-(throw;assert)-exp"
            (and
              (= "hello joe"
                 (try (c/decl-throw-exp _eee java.lang.Exception)
                      (_eee "hello %s" "joe")
                      (catch Throwable e (.getMessage e))))
              (= "hello joe"
                 (try (c/decl-assert-exp _aaa java.lang.Exception)
                      (_aaa (== 1 2) "hello %s" "joe")
                      (catch Throwable e (.getMessage e))))))

  (ensure?? "do-with(*)"
            (and (== 99 (c/do-with [a 99] 3 4 5))
                 (= "99" (c/do-with-str [b 99] 3 4 5))
                 (== 99 (c/do-with-atom [a (atom 99)] (str "dummy")))))

  (ensure?? "bool!" (boolean? (c/bool! [])))

  (ensure?? "try!;trye!"
            (and (nil? (c/try! (/ 1 (- 1 1))))
                 (= "hello" (c/trye! "hello" (/ 1 (- 1 1))))))

  (ensure?? "if-some+;when-some+"
            (and (= [1 2 3]
                    (c/if-some+ [x (conj [] 1 2 3)] x))
                 (= "hello" (c/if-some+ [_ []] 3 "hello"))
                 (= [1 2 3]
                    (c/when-some+ [x [1 2]] (conj x 3)))
                 (nil? (c/when-some+ [x []] (conj x 3)))))

  (ensure?? "if-fn;when-fn"
            (and (== 4 (c/if-fn [x #(+ 1 %)] (x 3)))
                 (nil? (c/if-fn [x 1] 3))
                 (== 4 (c/when-fn [x #(+ 1 %)] (x 3)))))

  (ensure?? "doto->;doto->>"
            (and (== 21
                     @(let [f (fn [a b c]
                                (reset! c (+ a b @c)))]
                        (c/doto->> (atom 0)
                                   (f 1 2) (f 3 4) (f 5 6))))
                 (== 21
                     @(let [f (fn [c a b]
                                (reset! c (+ a b @c)))]
                        (c/doto-> (atom 0)
                                  (f 1 2) (f 3 4) (f 5 6))))))

  (ensure?? "=?;!=?" (and (c/!=? "a" "b")
                       (c/=? "a" "a")))

  (ensure?? "in?;!in?" (and (c/!in? #{1 2 3} 6)
                            (c/in? #{1 2 3} 2)))

  (ensure?? "eq?;!eq?"
            (and (c/eq? "a" "a")
                 (c/!eq? "a" "b")))

  (ensure?? "cast?"
            (and (= "[1]"
                    (let [x (.toString [1])] (c/cast? String x)))
                 (nil? (let [x [1 2]] (c/cast? String x)))))

  (ensure?? "cexp?"
            (and (nil? (c/cexp? "a"))
                 (= "a" (.getMessage
                          (let [x (Exception. "a")] (c/cexp? x))))))

  (ensure?? "!nil?" (and (c/!nil? 3)
                         (false? (c/!nil? nil))))

  (ensure?? "rnil" (and (= [1 2] (c/rnilv '(1 nil 2)))
                         (== 2 (count (c/rnil [1 nil 2])))))

  (ensure?? "szero?" (and (c/szero? 0)
                          (false? (c/szero? nil))))

  (ensure?? "sneg?" (and (c/sneg? -2)
                         (false? (c/sneg? nil))))

  (ensure?? "spos?" (and (c/spos? 2)
                         (false? (c/spos? nil))))

  (ensure?? "snneg?" (and (c/snneg? 2)
                          (false? (c/snneg? nil))))

  (ensure?? "!false?" (and (c/!false? nil)
                           (not (c/!false? false))))

  (ensure?? "!true?" (and (c/!true? 3)
                          (not (c/!true? true))))

  (ensure?? "assert-not" (do (c/assert-not (== 3 2)) true))

  (ensure?? "marray;zarray"
            (and (zero? (count (c/zarray Long)))
                 (= "Long[]" (.getSimpleName (class (c/marray Long 2))))))

  (ensure?? "vtbl"
            (and (== 1 (let [v (c/vtbl** {:a 1}
                                         :op1 identity :op2 identity)]
                         (:a (:____proto v))))
                 (== 1 (let [v (c/vtbl* :op1 identity :op2 identity)]
                         ((:op1 v) 1)))))

  (ensure?? "wo*;wm*"
            (and (== 2 (:b (meta (c/wm* {:a 1} {:b 2}))))
                 (do (c/wo* [x (java.io.ByteArrayOutputStream.)]) true)))

  (ensure?? "->str" (= "aaa" (c/->str "aaa")))

  (ensure?? "kvs->map"
            (let [m (c/kvs->map '(:a 1 :b 2 :c 3))]
              (and (= '(1 2 3) (sort (vals m)))
                   (= '(:a :b :c)  (sort (keys m))))))

  (ensure?? "repeat-str"  (= "aaa" (c/repeat-str 3 "a")))

  (ensure?? "num??" (and (== 4 (c/num?? 4 9))
                         (== 44 (c/num?? nil 44))))

  (ensure?? "!==" (and (c/!== 2 3 4) (not (c/!== 2 2 2 2))))

  (ensure?? "flip" (let [x (c/flip 2)]
                     (and (> x 0.49)(< x 0.51))))

  (ensure?? "percent" (let [x (c/percent 2 3)]
                        (and (> x 66.0)(< x 67.0))))

  (ensure?? "split-seq" (= '((1 2 3) (4 5))
                           (c/split-seq [1 2 3 4 5] 3)))

  (ensure?? "min-by;max-by"
            (and (== 1 (c/min-by identity [2 4 8 5 3 1 7]))
                 (== 8 (c/max-by identity [2 4 8 5 3 1 7]))))

  (ensure?? "rip-fn-name"
            (= "exlog" (c/rip-fn-name exlog)))

  (ensure?? "mu-int" (== 4 (let [x (c/mu-int)]
                             (c/mu-int x 4)
                             (c/mu-int x))))

  (ensure?? "mu-long" (== 4 (let [x (c/mu-long)]
                             (c/mu-long x 4)
                             (c/mu-long x))))

  (ensure?? "nth??" (== 5 (c/nth?? [1 2 5 3 4] 3)))

  (ensure?? "vargs*" (== 4 (count (c/vargs* Long 1 2 3 4))))

  (ensure?? "or??" (and (c/or?? [= 1] 2 1)
                        (c/or?? [= 2] 2 1)
                        (not (c/or?? [= 2] 3 4))))

  (ensure?? "scoped-keyword"
            (let [x (c/scoped-keyword "hey")
                  xs (str x)]
              (and (keyword? x)
                   (cs/includes? xs "/"))))

  (ensure?? "is-scoped-keyword?" (c/is-scoped-keyword? ::abc))

  (ensure?? "rand-sign" (let [x (c/rand-sign)]
                          (or (== x 1) (== x -1))))

  (ensure?? "rand-bool" (let [x (c/rand-bool)]
                          (or (false? x) (true? x))))

  (ensure?? "num-sign" (and (neg? (c/num-sign -444))
                            (pos? (c/num-sign 444))
                            (zero? (c/num-sign 0))))

  (ensure?? "s->bool" (c/s->bool "true"))

  (ensure?? "s->long" (and (== 100 (c/s->long "100"))
                           (== 100 (c/s->long "sss" 100))))

  (ensure?? "s->int" (and (== 100 (c/s->int "100"))
                          (== 100 (c/s->int "sss" 100))))

  (ensure?? "s->double" (and (== 100.0 (c/s->double "100"))
                             (== 100.0 (c/s->double "sss" 100.0))))

  (ensure?? "test-isa"
            (c/test-isa "" Throwable (Exception. "a")))

  (ensure?? "test-some" (c/test-some "" "aaa"))

  (ensure?? "test-cond" (c/test-cond "" (== 1 1)))

  (ensure?? "test-hgl" (c/test-hgl "" "aaa"))

  (ensure?? "test-pos0" (and (c/test-pos0 "" 3)
                             (c/test-pos0 "" 0)))

  (ensure?? "test-pos" (c/test-pos "" 3))

  (ensure?? "test-seq+" (c/test-seq+ "" [1 2]))

  (ensure?? "sort-join" (= "g-k-z" (c/sort-join "-" ["z" "k" "g"])))

  (ensure?? "strip-ns-path"
            (let [x (c/scoped-keyword "hey")
                  xs (c/strip-ns-path x)] (= (str *ns* "/hey") xs)))

  (ensure?? "vt-set-proto" (let [v (c/vtbl* :m1 identity)
                                 p (c/vtbl* :p1 identity)
                                 v' (c/vt-set-proto v p)]
                             (and (nil? (:____proto v))
                                  (fn? (:p1 (:____proto v'))))))

  (ensure?? "vt-find??" (let [v (c/vtbl* :m1 identity)
                              p (c/vtbl* :p1 identity)
                              v' (c/vt-set-proto v p)]
                          (fn? (c/vt-find?? v' :p1))))

  (ensure?? "vt-has??" (let [v (c/vtbl* :m1 identity)
                             p (c/vtbl* :p1 identity)
                             v' (c/vt-set-proto v p)]
                         (c/vt-has? v' :p1)))

  (ensure?? "vt-find??" (let [v (c/vtbl* :m1 3)
                              p (c/vtbl* :m1 identity)
                              v' (c/vt-set-proto v p)]
                          (fn? (c/vt-find?? v' :m1 true))))

  (ensure?? "vt-run??"
            (and (let [v (c/vtbl* :m1 3)
                       p (c/vtbl* :m1 (c/identity-n 2))
                       v' (c/vt-set-proto v p)]
                   (== 7 (c/vt-run?? v' :m1 [7] true)))
                 (let [v (c/vtbl* :m1 (c/identity-n 2))
                       p (c/vtbl* :m1 3)
                       v' (c/vt-set-proto v p)]
                   (== 7 (c/vt-run?? v' :m1 [7])))))

  (ensure?? "identity-n"
            (== 3 ((c/identity-n 3) 1 2 3 4)))

  (ensure?? "merge+,map"
            (and (= {:a 5 :z 9 :b {:c 2 :d 2}}
                    (c/merge+ {:a 1 :b {:c 2}}
                              {:z 9 :a 5 :b {:d 2}}))
                 (= {:a 5 :z 9 :b #{ :c 2 :d 3 4}}
                    (c/merge+ {:a 1 :b #{ :c 2 4}}
                              {:z 9 :a 5 :b #{ :d 2 3}}))
                 (= {:a 5 :z 9 :b [1 3]}
                    (c/merge+ {:a 1 :b [4]}
                              {:z 9 :a 5 :b [1 3]}))))

  (ensure?? "sbf<>"
            (let [b (c/sbf<>)
                  b (c/sbf+ b "a" "b" "c")
                  b (c/sbf-join b "-" "333")
                  n (c/sbfz b)]
              (and (= "abc-333" (str b)) (== 7 n))))

  (ensure?? "sreduce"
            (= "abc" (c/sreduce<> #(c/sbf+ %1 %2) ["a" "b" "c"])))

  (ensure?? "nichts?;hgl?"
            (and (c/hgl? "a")(c/nichts? nil)(not (c/hgl? ""))))

  (ensure?? "stror"
            (and (= "a" (c/stror nil "a"))
                 (= "a" (c/stror* nil nil nil "" "" "a"))))

  (ensure?? "identity monad" (== 3 (c/domonad c/m-identity
                                             [a 1 b (inc a)] (+ a b))))

  (ensure-thrown?? "identity monad->boom"
                   :any
                   (c/domonad c/m-identity
                              [a nil
                               b a
                               c (.toString ^Object b)] (+ a b c)))

  (ensure?? "maybe monad" (== 3 (c/domonad c/m-maybe
                                          [a 1 b (inc a)] (+ a b))))

  (ensure?? "maybe monad->nil" (nil? (c/domonad c/m-maybe
                                                [a 1
                                                 b (inc a)
                                                 c nil] (+ a b c))))

  (ensure?? "state monad" (= [5, {:value 5 :log ["mult3(1)" "add2(3)"]}]
                             ((c/domonad c/m-state
                                         [c1 (exlog mult3 "mult3")
                                          c2 (exlog add2 "add2")]
                                         c2)
                              {:value 1 :log []})))

  (ensure?? "continuation monad"
            (= 3 (c/run-cont
                   (c/domonad c/m-continuation
                              [x ((fn [v] (fn [c] (c v))) 1)
                               y ((fn [v] (fn [c] (c v))) 2)]
                              (+ x y)))))

  (ensure?? "monad rule 1: bind(unit(x), f) ≡ f(x)"
            (let [f (fn [v] (fn [s] [v s]))
                  lhs ((:bind c/m-state) ((:unit c/m-state) 911) f)
                  rhs (f 911)
                  lf (lhs "hello")
                  rt (rhs "hello")]
              (and (= (c/_1 lf)(c/_1 rt))
                   (= (last lf)(last rt)))))

  (ensure?? "monad rule 2: bind(m, unit) ≡ m"
            (let [mv (fn [s] [3 s])
                  lhs ((:bind c/m-state) mv (:unit c/m-state))
                  lf (lhs "hello")
                  rt (mv "hello")]
              (and (= (c/_1 lf)(c/_1 rt))
                   (= (last lf)(last rt)))))

  (ensure?? (str "monad rule 3:"
                 " bind(bind(m, f), g)"
                 " ≡ bind(m, v ⇒ bind(f(v), g))")
            (let [f (fn [v] (fn [s] [3 s]))
                  g (fn [v] (fn [s] [5 s]))
                  bb (:bind c/m-state)
                  mv (fn [s] [7 s])
                  lhs (bb (bb mv f) g)
                  rhs (bb mv (fn [v] (bb (f v) g)))
                  lf (lhs "hello")
                  rt (rhs "hello")]
              (and (= (c/_1 lf)(c/_1 rt))
                   (= (last lf)(last rt)))))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-core basal-test-core
  (ct/is (c/clj-test?? test-core)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


