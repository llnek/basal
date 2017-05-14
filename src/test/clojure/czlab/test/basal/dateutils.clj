;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.dateutils

  (:require [czlab.basal.dates :as d]
            [czlab.basal.core :as c])

  (:use [clojure.test])

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
    (is (false? (d/leapYear? 1999)))
    (is (d/leapYear? 2000))
    (is (d/leapYear? 2020)))

  (testing
    "related to: parsing dates"
    (is (c/ist? Date (d/parseDate "1999/12/12 13:13:13" "yyyy/MM/dd HH:mm:ss")))
    (is (c/ist? Timestamp (d/parseTimestamp "2000-12-31 13:14:15")))
    (is (c/ist? Date (d/parseIso8601 "1999-12-25T11:12:13.444"))))

  (testing
    "related to: formatting dates"
    (is (c/ist? String (d/fmtDate (Date.) "yyyy/MM/dd HH:mm:ss Z")))
    (is (c/ist? String (d/fmtTime "yyyy/MM/dd HH:mm:ss Z")))
    (is (string? (d/fmtTimestamp (Timestamp. (c/now<>)))))
    (is (string? (d/fmtDate (c/date<>))))
    (is (string? (d/fmtGMT (c/date<>)))))

  (testing
    "related to: calendars"
    (is (c/ist? Calendar (d/gcal<> (TimeZone/getTimeZone "GMT"))))
    (is (c/ist? Calendar (d/gcal<> (c/date<>))))
    (is (c/ist? Calendar (d/gcal<> (c/now<>))))
    (is (c/ist? Calendar (d/gcal<>)))
    (is (c/ist? Calendar (d/gmt<> (c/date<>))))
    (is (c/ist? Calendar (d/gmt<> (c/now<>))))
    (is (c/ist? Calendar (d/gmt<>)))

    (is (let [c (d/gcal<>)
              y (.get c Calendar/YEAR)
              _ (d/addYears c 5)
              y2 (.get c Calendar/YEAR)]
          (== 5 (- y2 y))))

    (is (let [c (d/gcal<>)
              m (.get c Calendar/MONTH)
              _ (d/addMonths c (if (> m 5) -2 2))
              m2 (.get c Calendar/MONTH)]
          (if (> m 5)
            (== 2 (- m m2))
            (== 2 (- m2 m)))))

    (is (let [c (d/gcal<>)
              d (.get c Calendar/DAY_OF_MONTH)
              _ (d/addDays c (if (> d 15) -5 5))
              d2 (.get c Calendar/DAY_OF_MONTH)]
          (if (> d 15)
            (== 5 (- d d2))
            (== 5 (- d2 d)))))

    (is (let [c (d/gcal<>)
              y (.get c Calendar/YEAR)
              c2 (d/addYears 5)
              y2 (.get c2 Calendar/YEAR)]
          (== 5 (- y2 y))))

    (is (let [c (d/gcal<>)
              m (.get c Calendar/MONTH)
              c2 (d/addMonths (if (> m 5) -2 2))
              m2 (.get c2 Calendar/MONTH)]
          (if (> m 5)
            (== 2 (- m m2))
            (== 2 (- m2 m)))))

    (is (let [c (d/gcal<>)
              d (.get c Calendar/DAY_OF_MONTH)
              c2 (d/addDays (if (> d 15) -5 5))
              d2 (.get c2 Calendar/DAY_OF_MONTH)]
          (if (> d 15)
            (== 5 (- d d2))
            (== 5 (- d2 d)))))

    (is (> (d/dtime) 0))
    (is (string? (d/debugCal (d/gcal<>)))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


