;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.dateutils

  (:use [czlab.basal.dates]
        [czlab.basal.core]
        [clojure.test])

  (:import [java.sql Timestamp]
           [java.util TimeZone Calendar Date]))

;;(println "tssss === " (fmtTimestamp (Timestamp. (now<>))))
;;(println "dddd === " (fmtDate (date<>)))
;;(println "gggg === " (fmtGMT (date<>)))
;;(println "gggg === " (fmtCal (gcal<>)))
;;(println "gggg === " (debugCal (gmt<>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-dateutils

  (testing
    "related to: leap years"
    (is (false? (leapYear? 1999)))
    (is (leapYear? 2000))
    (is (leapYear? 2020)))

  (testing
    "related to: parsing dates"
    (is (ist? Date (parseDate "1999/12/12 13:13:13" "yyyy/MM/dd HH:mm:ss")))
    (is (ist? Timestamp (parseTimestamp "2000-12-31 13:14:15")))
    (is (ist? Date (parseIso8601 "1999-12-25T11:12:13.444"))))

  (testing
    "related to: formatting dates"
    (is (ist? String (fmtDate (Date.) "yyyy/MM/dd HH:mm:ss Z")))
    (is (ist? String (fmtTime "yyyy/MM/dd HH:mm:ss Z")))
    (is (string? (fmtTimestamp (Timestamp. (now<>)))))
    (is (string? (fmtDate (date<>))))
    (is (string? (fmtGMT (date<>)))))

  (testing
    "related to: calendars"
    (is (ist? Calendar (gcal<> (TimeZone/getTimeZone "GMT"))))
    (is (ist? Calendar (gcal<> (date<>))))
    (is (ist? Calendar (gcal<> (now<>))))
    (is (ist? Calendar (gcal<>)))
    (is (ist? Calendar (gmt<> (date<>))))
    (is (ist? Calendar (gmt<> (now<>))))
    (is (ist? Calendar (gmt<>)))

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
              c2 (addYears 5)
              y2 (.get c2 Calendar/YEAR)]
          (== 5 (- y2 y))))

    (is (let [c (gcal<>)
              m (.get c Calendar/MONTH)
              c2 (addMonths (if (> m 5) -2 2))
              m2 (.get c2 Calendar/MONTH)]
          (if (> m 5)
            (== 2 (- m m2))
            (== 2 (- m2 m)))))

    (is (let [c (gcal<>)
              d (.get c Calendar/DAY_OF_MONTH)
              c2 (addDays (if (> d 15) -5 5))
              d2 (.get c2 Calendar/DAY_OF_MONTH)]
          (if (> d 15)
            (== 5 (- d d2))
            (== 5 (- d2 d)))))

    (is (> (dtime) 0))
    (is (string? (debugCal (gcal<>)))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


