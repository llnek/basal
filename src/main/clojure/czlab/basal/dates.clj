;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Date helpers."
      :author "Kenneth Leung"}

  czlab.basal.dates

  (:require [czlab.basal.log :as log]
            [clojure.string :as cs]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:import [java.text
            ParsePosition
            SimpleDateFormat]
           [java.util
            Locale
            TimeZone
            Date
            Calendar
            SimpleTimeZone
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
  "If it's leap year" [year]
  (cond
    (zero? (mod year 400)) true
    (zero? (mod year 100)) false
    :else (zero? (mod year 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hastzpart?
  "String has time zone?" [^String s]

  (let [pos (s/indexAny s ",; \t\r\n\f")
        ss (if (> pos 0) (.substring s (inc pos)) "")]
    (or (s/hasAny? ss ["+" "-"])
        (.matches ss "\\s*[a-zA-Z]+\\s*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hastz?
  "String has timezone?" [^String dateStr]

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
  *tstr* conforming to the format
  \"yyyy-mm-dd hh:mm:ss.[fff...]\""
  ^Timestamp
  [^String s] (c/trye! nil (Timestamp/valueOf s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parseDate
  "String to Date"
  ^Date [^String tstr ^String fmt]

  (when (and (s/hgl? tstr)
             (s/hgl? fmt))
    (.parse (SimpleDateFormat. fmt) tstr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn parseIso8601
  "To ISO8601 format" ^Date [^String tstr]

  (when (s/hgl? tstr)
    (let [fmt (if (s/has? tstr \:)
                (if (s/has? tstr \.)
                  *dt-fmt-micro* *dt-fmt*)
                *date-fmt*)]
      (parseDate tstr (if (hastz? tstr) (str fmt "Z") fmt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fmtTimestamp "Timestamp as stringvalue" [ts] `(str ~ts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtDate
  "Date to string" {:tag String}

  ([dt] (fmtDate dt *dt-fmt-micro* nil))
  ([dt fmt] (fmtDate dt fmt nil))
  ([^Date dt fmt ^TimeZone tz]
   (if (or (nil? dt)
           (s/nichts? fmt))
     ""
     (let [df (SimpleDateFormat. fmt)]
       (some->> tz (.setTimeZone df))
       (.format df dt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fmtGMT
  "Date to string - GMT"
  ^String [^Date dt]
  (fmtDate dt *dt-fmt-micro* (TimeZone/getTimeZone "GMT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- add
  "Add to the current date"
  ^Calendar
  [^Calendar cal field amount]
  (doto cal (.add (int field) ^long amount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gcal<>
  "Make a Calendar" {:tag Calendar}

  ([] (gcal<> (Date.)))
  ([arg]
   (cond
     (c/ist? TimeZone arg)
     (GregorianCalendar. ^TimeZone arg)

     (c/ist? Date arg)
     (doto
       (GregorianCalendar.)
       (.setTime ^Date arg))

     (c/spos? arg)
     (doto
       (GregorianCalendar.)
       (.setTimeInMillis ^long arg))

     :else (gcal<>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addYears
  "Add years to the calendar" {:tag Calendar}

  ([years] (addYears (gcal<> (Date.)) years))
  ([^Calendar cal years] (add cal Calendar/YEAR years)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addMonths
  "Add more months to the calendar" {:tag Calendar}

  ([months] (addMonths (gcal<> (Date.)) months))
  ([^Calendar cal months] (add cal Calendar/MONTH months)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addDays
  "Add more days to the calendar" {:tag Calendar}

  ([days] (addDays (gcal<> (Date.)) days))
  ([^Calendar cal days] (add cal Calendar/DAY_OF_YEAR days)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro fmtTime
  "Format current time" [^String fmt] `(fmtDate (c/date<>) ~fmt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn gmt<>
  "Make a Calendar (GMT)"
  {:tag GregorianCalendar}

  ([] (GregorianCalendar.
        (TimeZone/getTimeZone "GMT")))
  ([arg]
   (let [^Calendar c (gmt<>)]
     (cond
       (c/ist? Date arg)
       (.setTime c ^Date arg)
       (c/spos? arg)
       (.setTimeInMillis c ^long arg))
     c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dtime
  "Get the time in millis"
  ([] (c/now<>))
  ([^java.util.Date d] (.getTime d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- fmtCal
  "" ^String [^Calendar cal]

  (java.lang.String/format
    (Locale/getDefault)
    "%1$04d-%2$02d-%3$02dT%4$02d:%5$02d:%6$02d"
    (c/vargs* Object
              (.get cal Calendar/YEAR)
              (+ 1 (.get cal Calendar/MONTH))
              (.get cal Calendar/DAY_OF_MONTH)
              (.get cal Calendar/HOUR_OF_DAY)
              (.get cal Calendar/MINUTE)
              (.get cal Calendar/SECOND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn debugCal
  "Debug show a calendar's internal data"
  ^String
  [^Calendar cal]
  (cs/join ""
           ["{" (.. cal getTimeZone getDisplayName)  "} "
            "{" (.. cal getTimeZone getID) "} "
            "[" (.getTimeInMillis cal) "] "
            (fmtCal cal)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


