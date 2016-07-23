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

(ns ^{:doc "Some useful constants."
      :author "Kenneth Leung" }

  czlab.xlib.consts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^String TS_REGEX "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])\\s\\d\\d:\\d\\d:\\d\\d")
(def ^String DT_REGEX "^\\d\\d\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$")

(def ^String TS_FMT_NANO "yyyy-MM-dd HH:mm:ss.fffffffff" )
(def ^String TS_FMT "yyyy-MM-dd HH:mm:ss")

(def ^String DT_FMT_MICRO "yyyy-MM-dd'T'HH:mm:ss.SSS" )
(def ^String DT_FMT "yyyy-MM-dd'T'HH:mm:ss" )
(def ^String DATE_FMT "yyyy-MM-dd" )

(def ^String ISO8601_FMT "yyyy-MM-dd'T'HH:mm:ss.SSSZ" )

(def ^String USASCII "ISO-8859-1" )
(def ^String UTF16 "UTF-16" )
(def ^String UTF8 "UTF-8" )
(def ^String SLASH   "/" )
(def ^String PATHSEP (System/getProperty "file.separator"))

(def EV_OPTS :____eventoptions)
(def JS_LAST :____lastresult)
(def JS_CRED :credential)
(def JS_USER :principal)
(def JS_FLATLINE :____flatline)

(def BOOLS #{ "true", "yes", "on", "ok", "active", "1"} )

(def MONTHS ["JAN" "FEB" "MAR" "APR" "MAY" "JUN"
             "JUL" "AUG" "SEP" "OCT" "NOV" "DEC" ] )

(def ^String HEX_CHARS "0123456789ABCDEF")
(def ^String HEX_CHS "0123456789abcdef")

(def KiloBytes 1024)
(def BUF_SZ (* 4 KiloBytes))
(def MegaBytes (* KiloBytes KiloBytes))
(def GigaBytes (* 1024 MegaBytes))

(def OneK 1024)
(def FourK (* 4 OneK))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


