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

  "Java's Timestamp as string."
  {:arglists '([ts])}
  [ts]

  `(str ~ts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fmt-time

  "Format current time."
  {:arglists '([fmt])}
  [fmt]

  `(fmt-date (new java.util.Date) ~fmt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-leap-year?

  "If it's leap year?"
  {:arglists '([year])}
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

  "Convert string into a valid Timestamp object
  *tstr* conforming to the format
  \"yyyy-mm-dd hh:mm:ss.[fff...]\""
  {:tag Timestamp
   :arglists '([s])}
  [s]

  (c/try! (Timestamp/valueOf ^String s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-date

  "String to Java's Date."
  {:tag Date
   :arglists '([tstr fmt])}
  [tstr fmt]

  (when (and (c/hgl? fmt)
             (c/hgl? tstr))
    (.parse (SimpleDateFormat. ^String fmt) ^String tstr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-iso8601

  "To ISO8601 format."
  {:tag Date
   :arglists '([tstr])}
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

  "Date to string."
  {:tag String
   :arglists '([dt]
               [dt fmt]
               [dt fmt tz])}

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

  "Date to string - GMT."
  {:tag String
   :arglists '([dt])}
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

  "Make a Calendar."
  {:tag Calendar
   :arglists '([][arg])}

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

  "Add years to the calendar."
  {:tag Calendar
   :arglists '([years]
               [cal years])}

  ([cal years] (add cal Calendar/YEAR years))

  ([years] (add-years (gcal<> (Date.)) years)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-months

  "Add more months to the calendar."
  {:tag Calendar
   :arglists '([months]
               [cal months])}

  ([months] (add-months (gcal<> (Date.)) months))

  ([cal months] (add cal Calendar/MONTH months)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-days

  "Add more days to the calendar."
  {:tag Calendar
   :arglists '([days][cal days])}

  ([days] (add-days (gcal<> (Date.)) days))

  ([cal days] (add cal Calendar/DAY_OF_YEAR days)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmt<>

  "Make a Calendar (GMT)."
  {:arglists '([][arg])
   :tag GregorianCalendar}

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

  "Get the time in millis."
  {:arglists '([][d])}

  ([d]
   (.getTime ^Date d))

  ([]
   (dtime (new java.util.Date))))

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

  "Debug show a calendar's internal data."
  {:tag String
   :arglists '([cal])}
  [^Calendar cal]

  (cs/join ""
           ["{" (.. cal getTimeZone getDisplayName)  "} "
            "{" (.. cal getTimeZone getID) "} "
            "[" (.getTimeInMillis cal) "] "
            (fmt-cal cal)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

