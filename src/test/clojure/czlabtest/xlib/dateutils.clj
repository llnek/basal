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

(ns czlabtest.xlib.dateutils

  (:use [czlab.xlib.dates]
        [czlab.xlib.core]
        [clojure.test])

  (:import [java.sql Timestamp]
           [java.util TimeZone Calendar Date]))

;;(println "tssss === " (fmtTimestamp (Timestamp. (now<>))))
;;(println "dddd === " (fmtDate (now<date>)))
;;(println "gggg === " (fmtGMT (now<date>)))
;;(println "gggg === " (fmtCal (gcal<>)))
;;(println "gggg === " (debugCal (gcal<gmt>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-dateutils

  (is (false? (leapYear? 1999)))
  (is (leapYear? 2000))
  (is (leapYear? 2020))

  (is (inst? Timestamp (parseTimestamp "2000-12-31 13:14:15")))

  (is (inst? Date (parseDate "1999/12/12 13:13:13" "yyyy/MM/dd HH:mm:ss")))
  (is (inst? String (fmtDate (Date.) "yyyy/MM/dd HH:mm:ss Z")))
  (is (inst? String (fmtTime "yyyy/MM/dd HH:mm:ss Z")))

  (is (inst? Date (parseIso8601 "1999-12-25T11:12:13.444")))

  (is (string? (fmtTimestamp (Timestamp. (now<>)))))

  (is (string? (fmtDate (now<date>))))
  (is (string? (fmtGMT (now<date>))))

  (is (inst? Calendar (gcal<> (TimeZone/getTimeZone "GMT"))))
  (is (inst? Calendar (gcal<> (now<date>))))
  (is (inst? Calendar (gcal<> (now<>))))
  (is (inst? Calendar (gcal<>)))

  (is (inst? Calendar (gcal<gmt> (now<date>))))
  (is (inst? Calendar (gcal<gmt> (now<>))))
  (is (inst? Calendar (gcal<gmt>)))

  (is (let [c (gcal<>)
            y (.get c Calendar/YEAR)
            _ (addYears c 5)
            y2 (.get c Calendar/YEAR)]
        (== 5 (- y2 y))))

  (is (let [c (gcal<>)
            m (.get c Calendar/MONTH)
            _ (addMonths c (if (> m 5) -2 2))
            m2 (.get c Calendar/MONTH)]
        (if (> m 5)
          (== 2 (- m m2))
          (== 2 (- m2 m)))))

  (is (let [c (gcal<>)
            d (.get c Calendar/DAY_OF_MONTH)
            _ (addDays c (if (> d 15) -5 5))
            d2 (.get c Calendar/DAY_OF_MONTH)]
        (if (> d 15)
          (== 5 (- d d2))
          (== 5 (- d2 d)))))

  (is (let [c (gcal<>)
            y (.get c Calendar/YEAR)
            c2 (+years 5)
            y2 (.get c2 Calendar/YEAR)]
        (== 5 (- y2 y))))

  (is (let [c (gcal<>)
            m (.get c Calendar/MONTH)
            c2 (+months (if (> m 5) -2 2))
            m2 (.get c2 Calendar/MONTH)]
        (if (> m 5)
          (== 2 (- m m2))
          (== 2 (- m2 m)))))

  (is (let [c (gcal<>)
            d (.get c Calendar/DAY_OF_MONTH)
            c2 (+days (if (> d 15) -5 5))
            d2 (.get c2 Calendar/DAY_OF_MONTH)]
        (if (> d 15)
          (== 5 (- d d2))
          (== 5 (- d2 d)))))

  (is (string? (fmtCal (gcal<>))))

  (is (> (dtime) 0))


  (is (string? (debugCal (gcal<>))))


  (is (string? "That's all folks!")))


;;(clojure.test/run-tests 'czlabtest.xlib.dateutils)

