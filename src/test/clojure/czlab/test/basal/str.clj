;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.test.basal.str

  (:require [clojure.string :as cs]
            [czlab.basal.str :as s]
            [clojure.test :as ct]
            [czlab.basal.core
             :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-str

  (ensure?? "sbf<>" (= "a" (str (s/sbf<> "a"))))

  (ensure?? "sbf-join" (= "a,a"
                          (-> (s/sbf-join (s/sbf<>) "," "a")
                              (s/sbf-join "," "a")
                              (str))))

  (ensure?? "sbf+" (= "abc" (str (s/sbf+ (s/sbf<>) "a" "b" "c"))))


  (ensure?? "nichts?" (and (s/nichts? "")
                           (s/nichts? nil)
                           (s/nichts? 4)))

  (ensure?? "hgl?" (and (s/hgl? "a") (not (s/hgl? ""))))

  (ensure?? "stror" (= "a" (s/stror nil "a")))

  (ensure?? "stror*" (= "a" (s/stror* nil nil nil nil "a")))

  (ensure?? "lcase" (= "aaa" (s/lcase "AAA")))

  (ensure?? "ucase" (= "AAA" (s/ucase "aaa")))

  (ensure?? "triml" (= "abc" (s/triml "123673abc" "123456789")))

  (ensure?? "trimr" (= "abc" (s/trimr "abc123456789" "123456789")))

  (ensure?? "has?" (s/has? "ab cd" \space))

  (ensure?? "embeds?" (and (s/has? "ab cd" "cd")
                           (not (s/has? "ab cd" "ecd"))))

  (ensure?? "has-no-case?" (s/has-no-case? "ab cd" "AB"))

  (ensure?? "index-any" (and (= 5 (s/index-any "hello joe" "793 Z"))
                             (neg? (s/index-any "hello joe" "793"))))

  (ensure?? "count-str" (and (= 3 (s/count-str "abagabrabt" "ab"))
                             (zero? (s/count-str "abagabrabt" "AA"))))

  (ensure?? "count-char" (and (= 4 (s/count-char "abagabrabt" \a))
                              (zero? (s/count-char "abagabrabt" \space))))

  (ensure?? "sname" (and (= "a" (s/sname :a))
                         (= "a" (s/sname "a"))
                         (= "" (s/sname nil))))

  (ensure?? "nsb" (and (= "a" (s/nsb :a))
                       (= "a" (s/nsb "a"))
                       (= "" (s/nsb nil))))

  (ensure?? "kw->str" (= "czlab.test.basal.str/a" (s/kw->str ::a)))

  (ensure?? "x->kw" (= :tmp/abc (s/x->kw "tmp" "/" "abc")))

  (ensure?? "nsn" (and (= "a" (s/nsn "a"))
                       (= "(null)" (s/nsn nil))))

  (ensure?? "match-char?" (and (s/match-char? \d #{\a \b \d})
                               (not (s/match-char? \e #{\a \b \d}))))

  (ensure?? "strim" (and (= "" (s/strim nil))
                         (= "a" (s/strim "   a   "))))

  (ensure?? "strim-any" (and (= "  ab123" (s/strim-any "  ab123ab" "ab"))
                             (= "123" (s/strim-any "  ab123ab   " "ab" true))))

  (ensure?? "splunk" (= ["1234" "5678" "9"]
                        (s/splunk "123456789" 4)))

  (ensure?? "hasic-any?"
            (and (s/hasic-any? "hello good morning" ["he" "OO" "in"])
                 (not (s/hasic-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "has-any?"
            (and (s/has-any? "hello good morning" ["OO" "in"])
                 (not (s/has-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "ewic-any?"
            (and (s/ewic-any? "hello good morning" ["he" "OO" "NG"])
                 (not (s/ewic-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "ew-any?"
            (and (s/ew-any? "hello good morning" ["OO" "ing"])
                 (not (s/ew-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "swic-any?"
            (and (s/swic-any? "hello good morning" ["OO" "HE"])
                 (not (s/swic-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "sw-any?"
            (and (s/sw-any? "hello good morning" ["OO" "hell"])
                 (not (s/sw-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "eqic?" (s/eqic? "AbcDE" "abcde"))

  (ensure?? "eqic-any?"
            (and (s/eqic-any? "hello" ["OO" "HellO"])
                 (not (s/eqic-any? "hello" ["xx" "yy"]))))

  (ensure?? "eq-any?"
            (and (s/eq-any? "hello" ["OO" "hello"])
                 (not (s/eq-any? "hello" ["xx" "yy"]))))

  (ensure?? "wrapped?"
            (and (s/wrapped? "hello" "h" "o")
                 (not (s/wrapped? "hello" "x" "y"))))

  (ensure?? "rights" (and (= "joe" (s/rights "hello joe" 3))
                          (= "" (s/rights nil 3))
                          (= "" (s/rights "aaa" 0))
                          (= "hello joe" (s/rights "hello joe" 30))))

  (ensure?? "lefts" (and (= "he" (s/lefts "hello joe" 2))
                          (= "" (s/lefts  nil 3))
                          (= "" (s/lefts  "aaa" 0))
                          (= "hello joe" (s/lefts "hello joe" 30))))

  (ensure?? "drop-head" (and (= "lo joe" (s/drop-head "hello joe" 3))
                             (= "" (s/drop-head nil 3))
                             (= "aaa" (s/drop-head "aaa" 0))
                             (= "" (s/drop-head "hello joe" 30))))

  (ensure?? "drop-tail" (and (= "hello " (s/drop-tail "hello joe" 3))
                             (= "" (s/drop-tail  nil 3))
                             (= "aaa" (s/drop-tail  "aaa" 0))
                             (= "" (s/drop-tail "hello joe" 30))))

  (ensure?? "split-str"
            (and (= ["a" "b" "c"]
                    (s/split-str "/a/b/c/" "/"))
                 (= ["/" "a" "/" "b" "/" "c" "/"]
                    (s/split-str "/a/b/c/" "/" true))))

  (ensure?? "shuffle-str" (let [s "abcdefg"
                                z (s/shuffle-str s)]
                            (and (count s)
                                 (count z)
                                 (not= s z))))

  (ensure?? "esc-xml" (= (s/esc-xml "<abc\"'&>")
                         "&lt;abc&quot;&apos;&amp;&gt;"))




  (ensure?? "test-end" (= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest ^:test-str basal-test-str
  (ct/is (let [[ok? r]
               (c/runtest test-str "test-str")] (println r) ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


