;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.strutils

  (:require [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:use [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-strutils

  (testing
    "related to: string operations"

    (is (and (s/nichts? nil)(s/nichts? "")(s/nichts? [])))
    (is (s/hgl? "sss"))

    (is (= "a/a" (str (s/stror nil "a")(s/stror "/" "a")(s/stror "" "a"))))

    (is (not (s/wrapped? "hello joe, are you well?" "xell" "!")))
    (is (s/wrapped? "hello joe, are you well?" "hell" "?"))

    (is (= "az" (str (s/lcase nil)(s/lcase "A")(s/lcase "z"))))
    (is (= "AZ" (str (s/ucase nil)(s/ucase "A")(s/ucase "z"))))

    (is (= "abcdefghijk" (s/triml "abcdefghijk" "xyz")))
    (is (= "defghijk" (s/triml "abcdefghijk" "abc")))
    (is (= "" (s/triml "abcdefghijk" "abcdefghijk")))

    (is (= "abcdefghijk" (s/trimr "abcdefghijk" "xyz")))
    (is (= "abcdefgh" (s/trimr "abcdefghijk" "ijk")))
    (is (= "" (s/trimr "abcdefghijk" "abcdefghijk")))

    (is (= ["abc" "def" "ghijk"] (s/splitTokens "abc,def,ghijk,,,," ",")))
    (is (= ["abc" ":" "def"] (s/splitTokens "abc:def" ":" true)))
    (is (= ["abc:def" ] (s/splitTokens "abc:def" "?")))

    (is (not (s/embeds? "hello joe" "john")))
    (is (s/embeds? "hello joe" "joe"))
    (is (not (s/embeds? "hello joe" "JOE")))

    (is (s/hasNoCase? "hello joe" "JOE"))

    (is (not (s/has? "hallowed are the ori" \z)))
    (is (s/has? "hallowed are the ori" \w))

    (is (== -1 (s/indexAny "hallowed are the ori" "zku")))
    (is (== -1 (s/indexAny "hallowed are the ori" "")))
    (is (== 5 (s/indexAny "hallowed are the ori" "zkw")))
    (is (== 1 (s/indexAny "hallowed are the ori" "akz")))

    (is (== 2 (s/countStr "abcaaabzzz" "ab")))
    (is (== 4 (s/countStr "abcaaazzz" "a")))
    (is (== 0 (s/countStr "abcaaazzz" "A")))

    (is (== 4 (s/countChar "abcaaazzz" \a)))
    (is (== 0 (s/countChar "abcaaazzz" \A)))

    (is (= "abc" (s/sname "abc")))
    (is (= "abc" (s/sname :abc)))
    (is (= "" (s/sname nil)))

    (is (= "heeloo" (s/nsb "heeloo")))
    (is (= "" (s/nsb nil)))

    (is (= :abc (s/toKW "a" "b" "c")))
    (is (nil? (s/toKW )))

    (is (= "heeloo" (s/nsn "heeloo")))
    (is (= "(null)" (s/nsn nil)))

    (is (not (s/matchChar? \space #{ \a \b \x })))
    (is (s/matchChar? \x #{ \a \b \x }))

    (is (not (s/eq? "aaa" "axa")))
    (is (s/eq? "aaa" "aaa"))

    (is (= "aaa" (s/strim "   aaa   ")))
    (is (= "" (s/strim nil)))

    (is (= "aaabbbccc" (s/strimAny "aaabbbccc" "xyz")))
    (is (= "bbb" (s/strimAny "aaabbbccc" "ac")))
    (is (= "bbb" (s/strimAny "   aaa  bbb  ccc  " "ac" true)))

    (is (= "joe;blogg" (str (doto (StringBuilder.)
                              (s/addDelim! ";" "joe")
                              (s/addDelim! ";" "blogg")))))

    (is (= 4 (count (s/splunk "hello, how are you" 5))))

    (is (s/hasicAny? "hello, how are you?" ["HELLO" ]))
    (is (s/hasicAny? "hello, how are you?" ["you" ]))
    (is (not (s/hasicAny? "hello, how are you?" [])))
    (is (not (s/hasicAny? "hello, how are you?" ["z" "x"])))

    (is (not (s/hasAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Are" ])))
    (is (s/hasAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ori" ]))

    (is (s/ewicAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Ori" ]))

    (is (s/ewAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ori" ]))
    (is (not (s/ewAny? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

    (is (s/swicAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "Hall" ]))

    (is (s/swAny? "hallowed are the ori" [ "sdfsdg" "jffflf" "ha" ]))
    (is (not (s/swAny? "hallowed are the ori" [ "sdfsdg" "jffflf" ])))

    (is (not (s/eqic? "abc" "AbCd")))
    (is (s/eqic? "abc" "AbC"))

    (is (s/eqicAny? "heeloo" [ "sdfsdg" "jffflf" "HeeLoo" ]))

    (is (s/eqAny? "heeloo" [ "sdfsdg" "jffflf" "heeloo" ]))
    (is (not (s/eqAny? "heeloo" [ "sdfsdg" "jffflf" ])))

    (is (c/ist? StringBuilder (s/strbf<> )))
    (is (= "aaa" (str (s/strbf<> "aaa"))))

    (is (= 10 (.length (s/str<> 10 \x))))
    (is (= "xxx" (s/str<> 3 \x)))

    (is (= "ori" (s/rights "Hallowed are the ori" 3)))
    (is (= "Hal" (s/lefts "Hallowed are the ori" 3)))

    (is (= "abc def" (s/urlDecode (s/urlEncode "abc def"))))
    (is (not= "abc def" (s/urlEncode "abc def")))

    (is (= "-world" (s/drophead "hello-world" 5)))
    (is (= "hello-" (s/droptail "hello-world" 5)))
    (is (= "" (s/drophead "hello-world" 50)))
    (is (= "" (s/droptail "hello-world" 50)))
    (is (= "" (s/drophead "hello" 5)))
    (is (= "" (s/droptail "hello" 5))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


