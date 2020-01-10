;; Copyright © 2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.dates

  "Useful additions for handling dates."

  (:require [clojure.string :as cs]
            [czlab.basal.core :as c])

  (:import [java.text
            ParsePosition
            SimpleDateFormat]
           [java.sql
            Timestamp]
           [java.util
            Locale
            TimeZone
            Date
            Calendar
            SimpleTimeZone
            GregorianCalendar]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(c/def- ^String ts-regex "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])\\s\\d\\d:\\d\\d:\\d\\d")
(c/def- ^String dt-regex "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$")

(c/def- ^String ts-fmt-nano "yyyy-MM-dd HH:mm:ss.fffffffff")
(c/def- ^String ts-fmt "yyyy-MM-dd HH:mm:ss")

(def ^String iso8601-fmt "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
(def ^String dt-fmt-micro "yyyy-MM-dd'T'HH:mm:ss.SSS")
(def ^String dt-fmt "yyyy-MM-dd'T'HH:mm:ss")
(def ^String date-fmt "yyyy-MM-dd")

(c/def- months ["JAN" "FEB" "MAR" "APR" "MAY" "JUN"
                "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fmt-timestamp

  ^{:arglists '([ts])
    :doc "Java's Timestamp as string."} [ts] `(str ~ts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fmt-time

  ^{:arglists '([fmt])
    :doc "Format current time."}

  [fmt] `(fmt-date (new java.util.Date) ~fmt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-leap-year?

  ^{:arglists '([year])
    :doc "If it's leap year?"}

  [year]

  (cond (zero? (mod year 400)) true
        (zero? (mod year 100)) false
        :else (zero? (mod year 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- has-tz?

  "String has timezone?"
  [dateStr]

  (letfn
    [(tz-part? [s]
       (let [pos (c/index-any s ",; \t\r\n\f")
             ss (if-not (pos? pos)
                  "" (subs s (+ 1 pos)))]
         (or (c/has-any? ss ["+" "-"])
             (c/matches? ss "\\s*[a-zA-Z]+\\s*"))))]
    (let [p1 (cs/last-index-of dateStr \.)
          p2 (cs/last-index-of dateStr \:)
          p3 (cs/last-index-of dateStr \-)
          p4 (cs/last-index-of dateStr \/)]
      (cond (c/spos? p1)
            (tz-part? (subs dateStr (+ 1 p1)))
            (c/spos? p2)
            (tz-part? (subs dateStr (+ 1 p2)))
            (c/spos? p3)
            (tz-part? (subs dateStr (+ 1 p3)))
            (c/spos? p4)
            (tz-part? (subs dateStr (+ 1 p4)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-timestamp

  ^{:arglists '([s])
    :tag Timestamp
    :doc "Convert string into a valid Timestamp object
         *tstr* conforming to the format
         \"yyyy-mm-dd hh:mm:ss.[fff...]\""}

  [s] (c/try! (Timestamp/valueOf ^String s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-date

  ^{:arglists '([tstr fmt])
    :tag Date
    :doc "String to Java's Date."}

  [tstr fmt]

  (when (and (c/hgl? fmt)
             (c/hgl? tstr))
    (.parse (SimpleDateFormat. ^String fmt) ^String tstr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-iso8601

  ^{:arglists '([tstr])
    :tag Date
    :doc "To ISO8601 format."}

  [tstr]

  (when (c/hgl? tstr)
    (let [fmt (if-not (c/includes? tstr \:)
                date-fmt
                (if-not (c/includes? tstr \.)
                  dt-fmt
                  dt-fmt-micro))]
      (parse-date tstr (if-not
                         (has-tz? tstr) fmt (str fmt "Z"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt-date

  ^{:arglists '([dt]
                [dt fmt]
                [dt fmt tz])
    :doc "Date to string."}

  {:tag String}

  ([dt] (fmt-date dt dt-fmt-micro nil))

  ([dt fmt] (fmt-date dt fmt nil))

  ([dt fmt tz]
   (str (if-not (or (nil? dt)
                    (c/nichts? fmt))
          (let [df (SimpleDateFormat. ^String fmt)]
            (some->> ^TimeZone tz
                     (.setTimeZone df)) (.format df ^Date dt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt-gmt

  ^{:arglists '([dt])
    :tag String
    :doc "Date to string - GMT."}

  [dt]

  (fmt-date dt dt-fmt-micro (TimeZone/getTimeZone "GMT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add

  "Add to the current date."
  ^Calendar
  [^Calendar cal field amount]
  (doto cal (.add (int field) ^long amount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gcal<>

  ^{:arglists '([][arg])
    :doc "Make a Calendar."}

  {:tag Calendar}

  ([] (gcal<> (Date.)))

  ([arg]
   (->> (gcal<>)
        (or (cond (c/is? TimeZone arg)
             (GregorianCalendar. ^TimeZone arg)
             (c/is? Date arg)
             (doto
               (GregorianCalendar.)
               (.setTime ^Date arg))
             (c/spos? arg)
             (doto
               (GregorianCalendar.)
               (.setTimeInMillis ^long arg)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-years

  ^{:arglists '([years]
                [cal years])
    :doc "Add years to the calendar."}

  {:tag Calendar}

  ([cal years] (add cal Calendar/YEAR years))

  ([years] (add-years (gcal<> (Date.)) years)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-months

  ^{:arglists '([months]
                [cal months])
    :doc "Add more months to the calendar."}

  {:tag Calendar}

  ([months] (add-months (gcal<> (Date.)) months))

  ([cal months] (add cal Calendar/MONTH months)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-days

  ^{:arglists '([days][cal days])
    :doc "Add more days to the calendar."}

  {:tag Calendar}

  ([days] (add-days (gcal<> (Date.)) days))

  ([cal days] (add cal Calendar/DAY_OF_YEAR days)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmt<>

  ^{:arglists '([][arg])
    :doc "Make a Calendar (GMT)."}

  {:tag GregorianCalendar}

  ([] (GregorianCalendar.
        (TimeZone/getTimeZone "GMT")))

  ([arg]
   (let [^Calendar c (gmt<>)]
     (cond (c/is? Date arg)
           (.setTime c ^Date arg)
           (c/spos? arg)
           (.setTimeInMillis c ^long arg)) c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dtime

  ^{:arglists '([][d])
    :doc "Get the time in millis."}

  ([d] (.getTime ^Date d))

  ([] (dtime (new java.util.Date))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- fmt-cal

  ^String [^Calendar cal]

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
(defn debug-cal

  ^{:arglists '([cal])
    :tag String
    :doc "Debug show a calendar's internal data."}

  [^Calendar cal]

  (cs/join ""
           ["{" (.. cal getTimeZone getDisplayName)  "} "
            "{" (.. cal getTimeZone getID) "} "
            "[" (.getTimeInMillis cal) "] "
            (fmt-cal cal)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

