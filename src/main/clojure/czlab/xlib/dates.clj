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

(ns ^{:doc "Date helpers."
      :author "Kenneth Leung" }

  czlab.xlib.dates

  (:require
    [czlab.xlib.str :refer [hgl? has? hasAny? indexAny]]
    [czlab.xlib.core :refer [inst? try!]]
    [czlab.xlib.logging :as log]
    [clojure.string :as cs])

  (:use [czlab.xlib.consts])

  (:import
    [java.text ParsePosition SimpleDateFormat]
    [java.util
     Locale
     TimeZone
     SimpleTimeZone
     Date
     Calendar
     GregorianCalendar]
    [java.sql Timestamp]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn leapYear?

  "true if it's leap year"
  [year]

  (cond
    (zero? (mod year 400)) true
    (zero? (mod year 100)) false
    :else (zero? (mod year 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hastzpart?

  "Does the string contain time zone info"
  [^String s]

  (let [pos (indexAny s ",; \t\r\n\f")
        ss (if (> pos 0)
             (.substring s (inc pos))
             "") ]
    (or (hasAny? ss ["+" "-"])
        (.matches ss "\\s*[a-zA-Z]+\\s*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hastz?

  "Does the string contain time zone info"
  [^String dateStr]

  (let [p1 (.lastIndexOf dateStr (int \.))
        p2 (.lastIndexOf dateStr (int \:))
        p3 (.lastIndexOf dateStr (int \-))
        p4 (.lastIndexOf dateStr (int \/)) ]
    (cond
      (> p1 0)
      (hastzpart? (.substring dateStr (inc p1)))

      (> p2 0)
      (hastzpart? (.substring dateStr (inc p2)))

      (> p3 0)
      (hastzpart? (.substring dateStr (inc p3)))

      (> p4 0)
      (hastzpart? (.substring dateStr (inc p4)))

      :else false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parseTimestamp

  "Convert string into a valid Timestamp object
  *tstr* conforming to the format \"yyyy-mm-dd hh:mm:ss.[fff...]\""
  ^Timestamp
  [^String s]

  (try! (Timestamp/valueOf s) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parseDate

  "Convert string into a Date object"
  ^Date
  [^String tstr ^String fmt]

  (when (and (hgl? tstr)
             (hgl? fmt))
    (.parse (SimpleDateFormat. fmt) tstr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- parseIso8601

  "Parses datetime in ISO8601 format"
  ^Date
  [^String tstr]

  (when (hgl? tstr)
    (let [fmt (if (has? tstr \:)
                (if (has? tstr \.) DT_FMT_MICRO DT_FMT )
                DATE_FMT ) ]
      (parseDate tstr (if (hastz? tstr) (str fmt "Z") fmt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtTimestamp

  "Convert Timestamp into a string value"
  ^String
  [^Timestamp ts]

  (str ts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtDate

  "Convert Date into string value"
  (^String
    [^Date dt]
    (fmtDate dt DT_FMT_MICRO nil))

  (^String
    [^Date dt fmt]
    (fmtDate dt fmt nil))

  (^String
    [^Date dt fmt ^TimeZone tz]
    (if (or (nil? dt)
            (empty? fmt))
      ""
      (let [df (SimpleDateFormat. fmt) ]
        (when (some? tz)
          (.setTimeZone df tz))
        (.format df dt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtGMT

  "Convert Date object into a string - GMT timezone"
  ^String
  [^Date dt]

  (fmtDate dt DT_FMT_MICRO (SimpleTimeZone. 0 "GMT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- add

  "Add some amount to the current date"
  ^Calendar
  [^Calendar cal calendarField amount]

  (when (some? cal)
    (doto (GregorianCalendar. (.getTimeZone cal))
      (.setTime (.getTime cal))
      (.add (int calendarField) ^long amount))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gcal<>

  "Make a Calendar"
  ^Calendar
  [& [arg]]

  (cond
    (inst? TimeZone arg)
    (GregorianCalendar. ^TimeZone arg)

    (inst? Date arg)
    (doto
      (GregorianCalendar.)
      (.setTime ^Date arg))

    :else
    (doto
      (GregorianCalendar.)
      (.setTime (Date.)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addYears

  "Add n more years to the calendar"
  ^Calendar
  [^Calendar cal yrs]

  (add cal Calendar/YEAR yrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addMonths

  "Add n more months to the calendar"
  ^Calendar
  [^Calendar cal mts]

  (add cal Calendar/MONTH mts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addDays

  "Add n more days to the calendar"
  ^Calendar
  [^Calendar cal days]

  (add cal Calendar/DAY_OF_YEAR days))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn +months

  "Add n months"
  ^Date
  [months]

  (-> (gcal<> (Date.))
      (addMonths  months)
      (.getTime)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn +years

  "Add n years"
  ^Date
  [years]

  (-> (gcal<> (Date.))
      (addYears years)
      (.getTime)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn +days

  "Add n days"
  ^Date
  [days]

  (-> (gcal<> (Date.))
      (addDays days)
      (.getTime)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtTime

  "Format current time"
  [^String fmt]

  (-> (SimpleDateFormat. fmt)
      (.format (-> (GregorianCalendar.)
                   (.getTimeInMillis)
                   (Date.)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtCal

  "Formats time to yyyy-MM-ddThh:mm:ss"
  ^String
  [^Calendar cal]

  (java.lang.String/format
    (Locale/getDefault)
    "%1$04d-%2$02d-%3$02dT%4$02d:%5$02d:%6$02d"
    (into-array Object
                [(.get cal Calendar/YEAR)
                 (+ 1 (.get cal Calendar/MONTH))
                 (.get cal Calendar/DAY_OF_MONTH)
                 (.get cal Calendar/HOUR_OF_DAY)
                 (.get cal Calendar/MINUTE)
                 (.get cal Calendar/SECOND)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gcal<gmt>

  "Make a Calendar (GMT)"
  ^GregorianCalendar
  []

  (GregorianCalendar. (TimeZone/getTimeZone "GMT")) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dtime

  "Get the time in millis"
  [^Date d]

  (.getTime d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn debugCal

  "Debug show a calendar's internal data"
  ^String
  [^Calendar cal]

  (cs/join ""
           ["{" (.. cal (getTimeZone) (getDisplayName) )  "} "
            "{" (.. cal (getTimeZone) (getID)) "} "
            "[" (.getTimeInMillis cal) "] "
            (fmtCal cal) ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


