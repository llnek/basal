;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.test.basal.core

  (:require [clojure.string :as cs]
            [clojure.test :as ct]
            [czlab.basal.core
             :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for testing state monad
(defn- mult3 "" [x] (* 3 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add2 "" [x] (+ 2 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- exlog
  "wrapper so that the actual computation is inside
  a state-monadic value, together with the log msg"
  [expr log]
  (fn [s]
    (let [{v :value slog :log} s
          v' (expr v)
          log' (conj slog
                     (str log "(" v ")"))]
      [v' {:value v' :log log'}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-core

  (ensure?? "atom?"
            (and (c/atom? (atom 0))
                 (not (c/atom? 0))))

  (ensure?? "is?"
            (c/is? String "a"))

  (ensure?? "map->"
            (= {:b 2 :z 1}
               (c/map-> [[:z 1] [:b 2]])))

  (ensure?? "vec->"
            (= [3 2 1] (c/vec-> '(3 2 1))))

  (ensure?? "cc+1"
            (= '(3 4 5) (c/cc+1 3 [4 5])))

  (ensure?? "cc+"
            (= '(2 3 4 5) (c/cc+ [2 3] [4 5])))

  (ensure?? "_2" (= 3 (c/_2 [1 3])))

  (ensure?? "_1" (= 1 (c/_1 [1 3])))

  (ensure?? "_3" (= 4 (c/_3 [1 3 4])))

  (ensure?? "_E" (= 4 (c/_E [1 3 4])))

  (ensure?? "!zero?" (c/!zero? 7))

  (ensure?? "n#" (= 3 (c/n# [1 2 4])))

  (ensure?? "car" (= 2
                     (c/car '(2 3 4))))

  (ensure?? "cdr" (= '(3 4)
                     (c/cdr '(2 3 4))))

  (ensure?? "dissoc!!"
            (= {:b 3} @(c/dissoc!! (atom {:a 2 :b 3 :c 7}) :a :c)))

  (ensure?? "assoc!!"
            (= {:a 2 :b 3 :c 7} @(c/assoc!! (atom {:b 3}) :a 2 :c 7)))

  (ensure?? "fn_*"
            (= '(1 2 3) ((c/fn_* ____xs) 1 2 3)))

  (ensure?? "fn_3"
            (= 6 ((c/fn_3 (+ ____1 ____2 ____3)) 1 2 3)))

  (ensure?? "fn_2"
            (= 3 ((c/fn_2 (+ ____1 ____2)) 1 2)))

  (ensure?? "fn_1" (= 2 ((c/fn_1 ____1) 2)))

  (ensure?? "fn_0" (= "yo" ((c/fn_0 "yo"))))

  (ensure?? "tmap*" (= {:a 1} (persistent! (c/tmap* {:a 1}))))

  (ensure?? "tmap*" (= {:a 1} (persistent! (assoc! (c/tmap*) :a 1))))

  (ensure?? "tvec*" (= [:a 1] (persistent! (c/tvec* [:a 1]))))

  (ensure?? "tvec*" (= [:a] (persistent! (conj! (c/tvec*) :a))))

  (ensure?? "ps!" (= #{:a} (c/ps! (conj! (transient #{}) :a))))

  (ensure?? "atomic" (= {:a 1 :b 3} @(c/atomic :a 1 :b 3)))

  (ensure?? "mapfv" (= [5 6 7] (c/mapfv + 4 1 2 3)))

  (ensure?? "last-index" (= 3 (c/last-index '(1 2 3 4))))

  (ensure?? "nexth" (= 4 (c/nexth '(1 2 3 4) 2)))

  (ensure-thrown?? "nexth-out-of-bound"
                   java.lang.IndexOutOfBoundsException
                   (nil? (c/nexth '(1 2 3 4) 99)))

  (ensure?? "chop" (= [[1 2] [3 4]] (c/chop 2 [1 2 3 4])))

  (ensure?? "o+" (= 4 (c/o+ (+ 1 2))))

  (ensure?? "o-" (= 2 (c/o- (+ 2 1))))

  (ensure?? "do#false" (false? (c/do#false "aaa")))
  (ensure?? "do#nil" (nil? (c/do#nil "aaa")))
  (ensure?? "do#true" (true? (c/do#true "aaa")))

  (ensure?? "let#false" (false? (c/let#false [a 3] a)))
  (ensure?? "let#nil" (nil? (c/let#nil [a 3] a)))
  (ensure?? "let#true" (true? (c/let#true [a 3] a)))

  (ensure?? "defenum"
            (= 4 (do (c/defenum xxx a 1 b c) (+ xxx-a xxx-c))))

  (ensure?? "n#-even?,n#-odd?"
            (and (c/n#-even? [])
                 (c/n#-even? [1 2]) (c/n#-odd? [1])))

  (ensure?? "kvs->map"
            (let [m (c/kvs->map '(:a 1 :b 2 :c 3))]
              (and (= '(1 2 3) (vals m))
                   (= '(:a :b :c) (keys m)))))

  (ensure?? "condp??" (let [arg 8]
                        (and (= 13 (c/condp?? = arg 1 1 3 3 13))
                             (nil? (c/condp?? = arg 1 1 3 3)))))

  (ensure?? "case??" (let [arg 8]
                       (and (= 13 (c/case?? arg 1 1 3 3 13))
                            (nil? (c/case?? arg 1 1 3 3)))))

  (ensure?? "if-number" (= 13 (c/if-number [x (+ 3 4)] (+ x 6))))

  (ensure?? "if-string" (= "hello!" (c/if-string [x "hello"] (str x "!"))))

  (ensure?? "nloop" (= 3 (let [acc (atom 0)]
                           (c/nloop 3 (swap! acc inc)) @acc)))

  (ensure?? "each*"
            (= [[:a 0] [:b 1] [:c 2]]
               (let [acc (atom [])]
                 (c/each* #(swap! acc conj [%1 %2]) [:a :b :c]) @acc)))

  (ensure?? "each"
            (= [:a :b :c]
               (let [acc (atom [])]
                 (c/each #(swap! acc conj %) [:a :b :c]) @acc)))

  (ensure?? "vargs" (= "Long[]"
                       (.getSimpleName (class (c/vargs Long [1 2])))))

  (ensure?? "preduce<map>"
            (= {:a "a" :b "b" :c "c"}
               (c/preduce<map> #(assoc! %1 %2 (name %2)) [:a :b :c])))

  (ensure?? "preduce<set>"
            (= #{:a :c}
               (c/preduce<set> #(conj! %1 %2) [:a :a :c :a])))

  (ensure?? "preduce<vec>"
            (= [1 2]
               (c/preduce<vec> #(conj! %1 (last %2)) {:a 1 :b 2})))

  (ensure?? "rset!"
            (= 7 (let [a (atom 3)] (c/rset! a 7) @a)))

  (ensure-thrown?? "exp!"
                   java.lang.Exception
                   (throw (c/exp! java.lang.Exception)))

  (ensure-thrown?? "trap!"
                   java.lang.Exception
                   (c/trap! java.lang.Exception "hello"))

  (ensure?? "decl-throw-exp"
            (= "hello joe"
               (try (c/decl-throw-exp _eee java.lang.Exception)
                    (_eee "hello %s" "joe")
                    (catch Throwable e (.getMessage e)))))

  (ensure?? "do-with-atom"
            (= 99 (c/do-with-atom [a (atom 99)] (str "dummy"))))

  (ensure?? "do-with"
            (= 100 @(c/do-with [a (atom 99)] (swap! a inc))))

  (ensure?? "do-with"
            (= 100 @((fn [b]
                       (c/do-with [b] (swap! b inc))) (atom 99))))

  ;(ensure?? "bool!" (boolean? (c/bool! 3)))

  (ensure?? "trye!" (= "hello" (c/trye! "hello" (/ 1 (- 1 1)))))

  (ensure?? "try!" (nil? (c/try! (/ 1 (- 1 1)))))

  (ensure?? "if-some+" (= [1 2 3]
                          (c/if-some+ [x (conj [] 1 2 3)] x)))

  (ensure?? "if-some+" (= "hello"
                          (c/if-some+ [_ []] 3 "hello")))

  (ensure?? "when-some+" (= [1 2 3]
                            (c/when-some+ [x [1 2]] (conj x 3))))

  (ensure?? "when-some+" (nil?
                            (c/when-some+ [x []] (conj x 3))))

  (ensure?? "if-fn?" (= 4 (c/if-fn? [x #(+ 1 %)] (x 3))))

  (ensure?? "if-fn?" (nil? (c/if-fn? [x 1] 3)))

  (ensure?? "when-fn?" (= 4 (c/when-fn? [x #(+ 1 %)] (x 3))))

  (ensure?? "doto->>" (= 21
                         @(let [f (fn [a b c]
                                    (reset! c (+ a b @c)))]
                            (c/doto->> (atom 0)
                                       (f 1 2) (f 3 4) (f 5 6)))))

  (ensure?? "doto->" (= 21
                        @(let [f (fn [c a b]
                                   (reset! c (+ a b @c)))]
                           (c/doto-> (atom 0)
                                     (f 1 2) (f 3 4) (f 5 6)))))

  (ensure?? "!=?" (c/!=? "a" "b"))

  (ensure?? "=?" (c/=? "a" "a"))

  (ensure?? "!in?" (c/!in? #{1 2 3} 6))

  (ensure?? "in?" (c/in? #{1 2 3} 2))

  (ensure?? "cast?" (= "[1]"
                       (let [x (.toString [1])] (c/cast? String x))))

  (ensure?? "cast?" (nil? (let [x [1 2]] (c/cast? String x))))

  (ensure?? "cexp?" (= "a" (.getMessage
                             (let [x (Exception. "a")] (c/cexp? x)))))

  (ensure?? "cexp?" (nil? (c/cexp? "a")))

  (ensure?? "!nil?" (c/!nil? 3))

  (ensure?? "!nil?" (false? (c/!nil? nil)))

  (ensure?? "rnil" (= 2 (count (c/rnil [1 nil 2]))))

  (ensure?? "rnilv" (= [1 2] (c/rnilv '(1 nil 2))))

  (ensure?? "szero?" (false? (c/szero? nil)))
  (ensure?? "szero?" (c/szero? 0))

  (ensure?? "sneg?" (false? (c/sneg? nil)))
  (ensure?? "sneg?" (c/sneg? -2))

  (ensure?? "spos?" (false? (c/spos? nil)))
  (ensure?? "spos?" (c/spos? 2))

  (ensure?? "snneg?" (false? (c/snneg? nil)))
  (ensure?? "snneg?" (c/snneg? 2))

  (ensure?? "!false?" (not (c/!false? false)))
  (ensure?? "!false?" (c/!false? nil))

  (ensure?? "!true?" (not (c/!true? true)))
  (ensure?? "!true?" (c/!true? 3))

  (ensure?? "assert-not" (do (c/assert-not (= 3 2)) true))

  (ensure?? "marray" (= "Long[]" (.getSimpleName (class (c/marray Long 2)))))

  (ensure?? "zarray" (= 0 (count (c/zarray Long))))

  (ensure?? "vtbl**" (= 1
                        (let [v (c/vtbl** {:a 1}
                                          :op1 identity :op2 identity)]
                          (:a (:____proto v)))))

  (ensure?? "vtbl*" (= 1 (let [v (c/vtbl* :op1 identity :op2 identity)]
                           ((:op1 v) 1))))

  (ensure?? "wo*" (do (c/wo* [x (java.io.ByteArrayOutputStream.)]) true))

  (ensure?? "repeat-str"  (= "aaa" (c/repeat-str 3 "a")))

  (ensure?? "num??" (= 44 (c/num?? nil 44)))

  (ensure?? "num??" (= 4 (c/num?? 4 9)))

  (ensure?? "split-seq" (= '((1 2 3) (4 5))
                           (c/split-seq [1 2 3 4 5] 3)))

  (ensure?? "int-var" (= 4 (let [x (c/int-var)]
                             (c/int-var x 4)
                             (c/int-var x))))

  (ensure?? "long-var" (= 4 (let [x (c/long-var)]
                             (c/long-var x 4)
                             (c/long-var x))))

  (ensure?? "nth??" (= 5 (c/nth?? [1 2 5 3 4] 3)))

  (ensure?? "vargs*" (= 4 (count (c/vargs* Long 1 2 3 4))))

  (ensure?? "scoped-keyword"
            (let [x (c/scoped-keyword "hey")
                  xs (str x)]
              (and (keyword? x)
                   (cs/includes? xs "/"))))

  (ensure?? "is-scoped-keyword?" (c/is-scoped-keyword? ::abc))

  (ensure?? "rand-sign" (let [x (c/rand-sign)]
                          (or (= x 1) (= x -1))))

  (ensure?? "rand-bool" (let [x (c/rand-bool)]
                          (or (false? x) (true? x))))

  (ensure?? "num-sign" (and (neg? (c/num-sign -444))
                            (pos? (c/num-sign 444))
                            (zero? (c/num-sign 0))))

  (ensure?? "s->long" (= 100 (c/s->long "sss" 100)))

  (ensure?? "s->long" (= 100 (c/s->long "100")))

  (ensure?? "s->int" (= 100 (c/s->int "sss" 100)))

  (ensure?? "s->int" (= 100 (c/s->int "100")))

  (ensure?? "s->double" (= 100.0 (c/s->double "sss" 100.0)))

  (ensure?? "s->double" (= 100.0 (c/s->double "100")))

  (ensure?? "s->bool" (c/s->bool "true"))

  (ensure?? "test-isa" (c/test-isa "" Throwable (Exception. "a")))

  (ensure?? "test-some" (c/test-some "" "aaa"))

  (ensure?? "test-cond" (c/test-cond "" (= 1 1)))

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

  (ensure?? "find??" (let [v (c/vtbl* :m1 3)
                           p (c/vtbl* :m1 identity)
                           v' (c/vt-set-proto v p)]
                       (fn? (c/vt-find?? v' :m1 true))))

  (ensure?? "vt-run??" (let [v (c/vtbl* :m1 3)
                             p (c/vtbl* :m1 (c/identity-n 2))
                             v' (c/vt-set-proto v p)]
                         (= 7 (c/vt-run?? v' :m1 [7] true))))

  (ensure?? "vt-run??" (let [v (c/vtbl* :m1 (c/identity-n 2))
                             p (c/vtbl* :m1 3)
                             v' (c/vt-set-proto v p)]
                         (= 7 (c/vt-run?? v' :m1 [7]))))

  (ensure?? "merge+,map" (= {:a 5 :z 9 :b {:c 2 :d 2}}
                            (c/merge+ {:a 1 :b {:c 2}}
                                      {:z 9 :a 5 :b {:d 2}})))

  (ensure?? "merge+,set" (= {:a 5 :z 9 :b #{ :c 2 :d 3 4}}
                            (c/merge+ {:a 1 :b #{ :c 2 4}}
                                      {:z 9 :a 5 :b #{ :d 2 3}})))

  (ensure?? "merge+" (= {:a 5 :z 9 :b [1 3]}
                        (c/merge+ {:a 1 :b [4]}
                                  {:z 9 :a 5 :b [1 3]})))

  (ensure?? "identity monad" (= 3 (c/domonad c/m-identity
                                             [a 1 b (inc a)] (+ a b))))

  (ensure-thrown?? "identity monad->boom"
                   :any
                   (c/domonad c/m-identity
                              [a nil
                               b a
                               c (.toString ^Object b)] (+ a b c)))

  (ensure?? "maybe monad" (= 3 (c/domonad c/m-maybe
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

  (ensure?? "test-end" (= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest ^:test-core basal-test-core
  (ct/is (let [[ok? r]
               (c/runtest test-core "test-core")] (println r) ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


