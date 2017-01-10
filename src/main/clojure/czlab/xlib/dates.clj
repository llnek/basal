;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Date helpers."
      :author "Kenneth Leung"}

  czlab.xlib.dates

  (:require [czlab.xlib.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.xlib.consts]
        [czlab.xlib.core]
        [czlab.xlib.str])

  (:import [java.text
            ParsePosition
            SimpleDateFormat]
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

(def ^String ^:private ts-regex "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])\\s\\d\\d:\\d\\d:\\d\\d")
(def ^String ^:private dt-regex "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$")

(def ^String ^:private ts-fmt-nano "yyyy-MM-dd HH:mm:ss.fffffffff" )
(def ^String ^:private ts-fmt "yyyy-MM-dd HH:mm:ss")

(def ^String ^:dynamic *iso8601-fmt* "yyyy-MM-dd'T'HH:mm:ss.SSSZ" )
(def ^String ^:dynamic *dt-fmt-micro* "yyyy-MM-dd'T'HH:mm:ss.SSS" )
(def ^String ^:dynamic *dt-fmt* "yyyy-MM-dd'T'HH:mm:ss" )
(def ^String ^:dynamic *date-fmt* "yyyy-MM-dd" )

(def ^:private months ["JAN" "FEB" "MAR" "APR" "MAY" "JUN"
                       "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn leapYear?
  "If it's leap year"
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
             "")]
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
        p4 (.lastIndexOf dateStr (int \/))]
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
  (try! (Timestamp/valueOf s)))

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
(defn parseIso8601
  "Parses datetime in ISO8601 format"
  ^Date
  [^String tstr]
  (when (hgl? tstr)
    (let [fmt (if (has? tstr \:)
                (if (has? tstr \.) *dt-fmt-micro* *dt-fmt*)
                *date-fmt*)]
      (parseDate tstr (if (hastz? tstr) (str fmt "Z") fmt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fmtTimestamp "Timestamp as stringvalue" [ts] `(str ~ts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtDate
  "Convert Date into string value"
  {:tag String}

  ([dt] (fmtDate dt *dt-fmt-micro* nil))
  ([dt fmt] (fmtDate dt fmt nil))
  ([^Date dt fmt ^TimeZone tz]
   (if (or (nil? dt)
           (nichts? fmt))
     ""
     (let [df (SimpleDateFormat. fmt)]
       (when (some? tz)
         (.setTimeZone df tz))
       (.format df dt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtGMT
  "Convert Date object into a string - GMT timezone"
  ^String
  [^Date dt]
  (fmtDate dt *dt-fmt-micro* (TimeZone/getTimeZone "GMT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- add
  "Add some amount to the current date"
  ^Calendar
  [^Calendar cal calendarField amount]
  (doto cal
    (.add (int calendarField) ^long amount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gcal<>
  "Make a Calendar"
  {:tag Calendar}

  ([] (gcal<> (Date.)))
  ([arg]
   (cond
     (inst? TimeZone arg)
     (GregorianCalendar. ^TimeZone arg)

     (inst? Date arg)
     (doto
      (GregorianCalendar.)
      (.setTime ^Date arg))

     (spos? arg)
     (doto
      (GregorianCalendar.)
      (.setTimeInMillis ^long arg))

     :else (gcal<>))))

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
  ^Calendar
  [months]
  (-> (gcal<> (Date.)) (addMonths  months)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn +years
  "Add n years"
  ^Calendar
  [years]
  (-> (gcal<> (Date.)) (addYears years)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn +days
  "Add n days"
  ^Calendar
  [days]
  (-> (gcal<> (Date.)) (addDays days)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fmtTime
  "Format current time"
  [^String fmt] `(fmtDate (now<date>) ~fmt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtCal
  "Formats time to yyyy-MM-ddThh:mm:ss"
  ^String
  [^Calendar cal]
  (java.lang.String/format
    (Locale/getDefault)
    "%1$04d-%2$02d-%3$02dT%4$02d:%5$02d:%6$02d"
    (vargs* Object
            (.get cal Calendar/YEAR)
            (+ 1 (.get cal Calendar/MONTH))
            (.get cal Calendar/DAY_OF_MONTH)
            (.get cal Calendar/HOUR_OF_DAY)
            (.get cal Calendar/MINUTE)
            (.get cal Calendar/SECOND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gcal<gmt>
  "Make a Calendar (GMT)"
  {:tag GregorianCalendar}

  ([] (GregorianCalendar.
        (TimeZone/getTimeZone "GMT")))
  ([arg]
   (let [^Calendar c (gcal<gmt>)]
     (cond
       (inst? Date arg)
       (.setTime c ^Date arg)
       (spos? arg)
       (.setTimeInMillis c ^long arg))
     c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro dtime
  "Get the time in millis"
  ([d] `(.getTime ^java.util.Date ~d))
  ([] `(now<>)))

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


