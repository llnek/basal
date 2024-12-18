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

(ns czlab.test.basal.dates

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.dates :as d]
            [czlab.basal.core
             :refer [ensure?? ensure-thrown??] :as c])

  (:import [java.sql
            Timestamp]
           [java.util
            Date
            TimeZone
            Calendar]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-dates

  (ensure?? "is-leap-year?"
            (and (false? (d/is-leap-year? 1999))
                 (d/is-leap-year? 2000)
                 (d/is-leap-year? 2020)))

  (ensure?? "parse-date"
            (c/is? Date
                   (d/parse-date "1999/12/12 13:13:13"
                                 "yyyy/MM/dd HH:mm:ss")))

  (ensure?? "pare-timestamp"
            (c/is? Timestamp
                   (d/parse-timestamp "2000-12-31 13:14:15")))

  (ensure?? "parse-iso8601"
            (c/is? Date (d/parse-iso8601 "1999-12-25T11:12:13.444")))

  (ensure?? "fmt-date"
            (string? (d/fmt-date (Date.) "yyyy/MM/dd HH:mm:ss Z")))

  (ensure?? "fmt-time"
            (string? (d/fmt-time "yyyy/MM/dd HH:mm:ss Z")))

  (ensure?? "fmt-timestamp"
            (string? (d/fmt-timestamp (Timestamp.
                                        (.getTime (new java.util.Date))))))

  (ensure?? "fmt-date" (string? (d/fmt-date (new java.util.Date))))

  (ensure?? "fmt-gmt" (string? (d/fmt-gmt (new java.util.Date))))

  (ensure?? "gcal<>" (c/is? Calendar (d/gcal<> (TimeZone/getTimeZone "GMT"))))

  (ensure?? "gcal<>" (c/is? Calendar (d/gcal<> (new java.util.Date))))

  (ensure?? "gcal<>" (c/is? Calendar (d/gcal<>)))

  (ensure?? "gmt<>" (c/is? Calendar (d/gmt<> (new java.util.Date))))

  (ensure?? "gmt<>" (c/is? Calendar (d/gmt<>)))

  (ensure?? "add-years"
            (let [c (d/gcal<>)
                  y (.get c Calendar/YEAR)
                  _ (d/add-years c 5)
                  y2 (.get c Calendar/YEAR)]
              (== 5 (- y2 y))))

  (ensure?? "add-months"
            (let [c (d/gcal<>)
                  m (.get c Calendar/MONTH)
                  _ (d/add-months c (if (> m 5) -2 2))
                  m2 (.get c Calendar/MONTH)]
              (if (> m 5)
                (== 2 (- m m2)) (== 2 (- m2 m)))))

  (ensure?? "add-days"
            (let [c (d/gcal<>)
                  d (.get c Calendar/DAY_OF_MONTH)
                  _ (d/add-days c (if (> d 15) -5 5))
                  d2 (.get c Calendar/DAY_OF_MONTH)]
              (if (> d 15)
                (== 5 (- d d2)) (== 5 (- d2 d)))))

  (ensure?? "add-years"
            (let [c (d/gcal<>)
                  y (.get c Calendar/YEAR)
                  c2 (d/add-years 5)
                  y2 (.get c2 Calendar/YEAR)]
              (== 5 (- y2 y))))

  (ensure?? "add-months"
            (let [c (d/gcal<>)
                  m (.get c Calendar/MONTH)
                  c2 (d/add-months (if (> m 5) -2 2))
                  m2 (.get c2 Calendar/MONTH)]
              (if (> m 5)
                (== 2 (- m m2)) (== 2 (- m2 m)))))

  (ensure?? "add-days"
            (let [c (d/gcal<>)
                  d (.get c Calendar/DAY_OF_MONTH)
                  c2 (d/add-days (if (> d 15) -5 5))
                  d2 (.get c2 Calendar/DAY_OF_MONTH)]
              (if (> d 15)
                (== 5 (- d d2)) (== 5 (- d2 d)))))

  (ensure?? "dtime" (> (d/dtime) 0))

  (ensure?? "debug-cal"
            (string? (d/debug-cal (d/gcal<>))))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-dates basal-test-dates
  (ct/is (c/clj-test?? test-dates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


