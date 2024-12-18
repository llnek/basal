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

(ns czlab.test.basal.str

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.util :as u]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-str

  (ensure?? "char-array??"
            (let [z (c/char-array?? "")
                  a (c/char-array?? "abcde")]
              (and (empty? z)
                   (= "e" (last a))
                   (= "a" (first a)))))

  (ensure?? "fmt" (= "a09z" (c/fmt "%s%02d%s" "a" 9 "z")))

  (ensure?? "sbf<>" (= "abc" (str (c/sbf<> "a" "b" "c"))))

  (ensure?? "sbf<>" (= "a" (str (c/sbf<> "a"))))

  (ensure?? "sbf-join" (= "a,a"
                          (-> (c/sbf-join (c/sbf<>) "," "a")
                              (c/sbf-join "," "a")
                              (str))))

  (ensure?? "sbf+" (= "abc" (str (c/sbf+ (c/sbf<>) "a" "b" "c"))))


  (ensure?? "nichts?" (and (c/nichts? "")
                           (c/nichts? nil)
                           (c/nichts? 4)))

  (ensure?? "hgl?" (and (c/hgl? "a") (not (c/hgl? ""))))

  (ensure?? "stror" (= "a" (c/stror nil "a")))

  (ensure?? "stror*" (= "a" (c/stror* nil nil nil nil "a")))

  (ensure?? "lcase" (= "aaa" (c/lcase "AAA")))

  (ensure?? "ucase" (= "AAA" (c/ucase "aaa")))

  (ensure?? "triml" (= "abc" (c/triml "123673abc" "123456789")))

  (ensure?? "trimr" (= "abc" (c/trimr "abc123456789" "123456789")))

  (ensure?? "includes?" (c/includes? "ab cd" \space))

  (ensure?? "embeds?" (and (c/includes? "ab cd" "cd")
                           (not (c/includes? "ab cd" "ecd"))))

  (ensure?? "has-no-case?" (c/has-no-case? "ab cd" "AB"))

  (ensure?? "index-any" (and (== 5 (c/index-any "hello joe" "793 Z"))
                             (neg? (c/index-any "hello joe" "793"))))

  (ensure?? "count-str" (and (== 3 (c/count-str "abagabrabt" "ab"))
                             (zero? (c/count-str "abagabrabt" "AA"))))

  (ensure?? "count-char" (and (== 4 (c/count-char "abagabrabt" \a))
                              (zero? (c/count-char "abagabrabt" \space))))

  (ensure?? "sname" (and (= "a" (c/sname :a))
                         (= "a" (c/sname "a"))
                         (= "" (c/sname nil))))

  (ensure?? "nsb" (and (= "a" (c/nsb :a))
                       (= "a" (c/nsb "a"))
                       (= "" (c/nsb nil))))

  (ensure?? "kw->str" (= "czlab.test.basal.str/a" (c/kw->str ::a)))

  (ensure?? "x->kw" (= :tmp/abc (c/x->kw "tmp" "/" "abc")))

  (ensure?? "nsn" (and (= "a" (c/nsn "a"))
                       (= "(null)" (c/nsn nil))))

  (ensure?? "match-char?" (and (c/match-char? \d #{\a \b \d})
                               (not (c/match-char? \e #{\a \b \d}))))

  (ensure?? "strim" (and (= "" (c/strim nil))
                         (= "a" (c/strim "   a   "))))

  (ensure?? "strim-any" (and (= "  ab123" (c/strim-any "  ab123ab" "ab"))
                             (= "123" (c/strim-any "  ab123ab   " "ab" true))))

  (ensure?? "splunk" (= ["1234" "5678" "9"]
                        (c/splunk "123456789" 4)))

  (ensure?? "hasic-any?"
            (and (c/hasic-any? "hello good morning" ["he" "OO" "in"])
                 (not (c/hasic-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "has-any?"
            (and (c/has-any? "hello good morning" ["OO" "in"])
                 (not (c/has-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "hasic-all?"
            (and (c/hasic-all? "hello good morning" ["he" "OO" "in"])
                 (not (c/hasic-all? "hello good morning" ["he" "yy"]))))

  (ensure?? "has-all?"
            (and (c/has-all? "hello gOOd morning" ["OO" "in"])
                 (not (c/has-all? "Hello good morning" ["he" "oo"]))))

  (ensure?? "ewic-any?"
            (and (c/ewic-any? "hello good morning" ["he" "OO" "NG"])
                 (not (c/ewic-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "ew-any?"
            (and (c/ew-any? "hello good morning" ["OO" "ing"])
                 (not (c/ew-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "swic-any?"
            (and (c/swic-any? "hello good morning" ["OO" "HE"])
                 (not (c/swic-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "sw-any?"
            (and (c/sw-any? "hello good morning" ["OO" "hell"])
                 (not (c/sw-any? "hello good morning" ["xx" "yy"]))))

  (ensure?? "eqic?" (c/eqic? "AbcDE" "abcde"))

  (ensure?? "eqic-any?"
            (and (c/eqic-any? "hello" ["OO" "HellO"])
                 (not (c/eqic-any? "hello" ["xx" "yy"]))))

  (ensure?? "eq-any?"
            (and (c/eq-any? "hello" ["OO" "hello"])
                 (not (c/eq-any? "hello" ["xx" "yy"]))))

  (ensure?? "wrapped?"
            (and (c/wrapped? "hello" "h" "o")
                 (not (c/wrapped? "hello" "x" "y"))))

  (ensure?? "rights" (and (= "joe" (c/rights "hello joe" 3))
                          (= "" (c/rights nil 3))
                          (= "" (c/rights "aaa" 0))
                          (= "hello joe" (c/rights "hello joe" 30))))

  (ensure?? "lefts" (and (= "he" (c/lefts "hello joe" 2))
                          (= "" (c/lefts  nil 3))
                          (= "" (c/lefts  "aaa" 0))
                          (= "hello joe" (c/lefts "hello joe" 30))))

  (ensure?? "drop-head" (and (= "lo joe" (c/drop-head "hello joe" 3))
                             (= "" (c/drop-head nil 3))
                             (= "aaa" (c/drop-head "aaa" 0))
                             (= "" (c/drop-head "hello joe" 30))))

  (ensure?? "drop-tail" (and (= "hello " (c/drop-tail "hello joe" 3))
                             (= "" (c/drop-tail  nil 3))
                             (= "aaa" (c/drop-tail  "aaa" 0))
                             (= "" (c/drop-tail "hello joe" 30))))

  (ensure?? "matches?" (c/matches? "abc55jjK8K" "[a-z0-9]+K[0-9]+K"))

  (ensure?? "sreduce<>"
            (= "123"
               (c/sreduce<> #(c/sbf+ %1 %2) [1 2 3])))

  (ensure?? "split"
            (= '("abc" "K" "K")
               (c/split "abc55jjK8K" "(\\d|jj)")))

  (ensure?? "split-str"
            (and (= ["a" "b" "c"]
                    (c/split-str "/a/b/c/" "/"))
                 (= ["/" "a" "/" "b" "/" "c" "/"]
                    (c/split-str "/a/b/c/" "/" true))))

  (ensure?? "shuffle" (let [s "abcdefg"
                            z (u/shuffle s)]
                        (and (count s)
                             (count z)
                             (not= s z))))

  (ensure?? "esc-xml" (= (c/esc-xml "<abc\"'&>")
                         "&lt;abc&quot;&apos;&amp;&gt;"))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-str basal-test-str
  (ct/is (c/clj-test?? test-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


