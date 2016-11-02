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

(ns czlabtest.xlib.strutils

  (:use [czlab.xlib.core]
        [czlab.xlib.str]
        [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-strutils


  (is (and (nichts? nil)(nichts? "")(nichts? [])))
  (is (hgl? "sss"))

  (is (= "a/a" (str (stror nil "a")(stror "/" "a")(stror "" "a"))))

  (is (= "az" (str (lcase nil)(lcase "A")(lcase "z"))))
  (is (= "AZ" (str (ucase nil)(ucase "A")(ucase "z"))))

  (is (= "abcdefghijk" (triml "abcdefghijk" "xyz")))
  (is (= "defghijk" (triml "abcdefghijk" "abc")))
  (is (= "" (triml "abcdefghijk" "abcdefghijk")))

  (is (= "abcdefghijk" (trimr "abcdefghijk" "xyz")))
  (is (= "abcdefgh" (trimr "abcdefghijk" "ijk")))
  (is (= "" (trimr "abcdefghijk" "abcdefghijk")))

  (is (= ["abc" "def" "ghijk"] (splitTokens "abc,def,ghijk,,,," ",")))
  (is (= ["abc" ":" "def"] (splitTokens "abc:def" ":" true)))
  (is (= ["abc:def" ] (splitTokens "abc:def" "?")))

  (is (not (embeds? "hello joe" "john")))
  (is (embeds? "hello joe" "joe"))
  (is (not (embeds? "hello joe" "JOE")))

  (is (hasNoCase? "hello joe" "JOE"))

  (is (not (has? "hallowed are the ori" \z)))
  (is (has? "hallowed are the ori" \w))

  (is (== -1 (indexAny "hallowed are the ori" "zku")))
  (is (== -1 (indexAny "hallowed are the ori" "")))
  (is (== 5 (indexAny "hallowed are the ori" "zkw")))
  (is (== 1 (indexAny "hallowed are the ori" "akz")))

  (is (== 2 (countStr "abcaaabzzz" "ab")))
  (is (== 4 (countStr "abcaaazzz" "a")))
  (is (== 0 (countStr "abcaaazzz" "A")))

  (is (== 4 (countChar "abcaaazzz" \a)))
  (is (== 0 (countChar "abcaaazzz" \A)))

  (is (= "abc" (sname "abc")))
  (is (= "abc" (sname :abc)))
  (is (= "" (sname nil)))

  (is (= "heeloo" (nsb "heeloo")))
  (is (= "" (nsb nil)))

  (is (= :a/b/c (toKW "a" "b" "c")))
  (is (nil? (toKW )))

  (is (= "heeloo" (nsn "heeloo")))
  (is (= "(null)" (nsn nil)))

  (is (not (matchChar? \space #{ \a \b \x })))
  (is (matchChar? \x #{ \a \b \x }))

  (is (not (same? "aaa" "axa")))
  (is (same? "aaa" "aaa"))

  (is (= "aaa" (strim "   aaa   ")))
  (is (= "" (strim nil)))

  (is (= "aaabbbccc" (strimAny "aaabbbccc" "xyz")))
  (is (= "bbb" (strimAny "aaabbbccc" "ac")))
  (is (= "bbb" (strimAny "   aaa  bbb  ccc  " "ac" true)))

  (is (= "joe;blogg" (str (doto (StringBuilder.)
                            (addDelim! ";" "joe")
                            (addDelim! ";" "blogg")))))

  (is (= 4 (count (splunk "hello, how are you" 5))))

  (is (hasicAny? "hello, how are you?" ["HELLO" ]))
  (is (hasicAny? "hello, how are you?" ["you" ]))
  (is (not (hasicAny? "hello, how are you?" [])))
  (is (not (hasicAny? "hello, how are you?" ["z" "x"])))

  (is (not (hasAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))
  (is (hasAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ori" ]))

  (is (ewicAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Ori" ]))

  (is (ewAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ori" ]))
  (is (not (ewAny? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

  (is (swicAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Hall" ]))

  (is (swAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ha" ]))
  (is (not (swAny? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

  (is (not (eqic? "abc" "AbCd")))
  (is (eqic? "abc" "AbC"))

  (is (eqicAny? "heeloo" [ "sdfsdg" "jffflf" "HeeLoo" ]))

  (is (eqAny? "heeloo" [ "sdfsdg" "jffflf" "heeloo" ]))
  (is (not (eqAny? "heeloo" [ "sdfsdg" "jffflf" ])))

  (is (inst? StringBuilder (strbf<> )))
  (is (= "aaa" (str (strbf<> "aaa"))))

  (is (= 10 (.length (str<> 10 \x))))
  (is (= "xxx" (str<> 3 \x)))

  (is (= "ori" (rights "Hallowed are the ori" 3)))
  (is (= "Hal" (lefts "Hallowed are the ori" 3)))

  (is (= "abc def" (urlDecode (urlEncode "abc def"))))
  (is (not= "abc def" (urlEncode "abc def")))

  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlabtest.xlib.strutils)

