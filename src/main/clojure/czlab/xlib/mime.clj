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

(ns ^{:doc "MIME helpers."
      :author "Kenneth Leung" }

  czlab.xlib.mime

  (:require
    [czlab.xlib.core :refer [bytesify try! pmap<>]]
    [czlab.xlib.str
     :refer [indexAny
             stror
             lcase
             ucase
             hgl?
             urlDecode
             urlEncode]]
    [czlab.xlib.meta :refer [bytesClass]]
    [czlab.xlib.logging :as log]
    [czlab.xlib.io :refer [streamify]])

  (:import
    [java.io IOException InputStream File]
    [java.net URL URLEncoder URLDecoder]
    [clojure.lang APersistentMap]
    [java.util.regex Pattern Matcher]
    [java.util Properties]
    [czlab.xlib MimeFileTypes]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def CTE_QUOTED "quoted-printable")
(def CTE_7BIT "7bit")
(def CTE_8BIT "8bit")
(def CTE_BINARY "binary")
(def CTE_BASE64 "base64")

(def MIME_USER_PROP  "mime.rfc2822.user")
(def MIME_USER_JAVAMAIL   "javamail")
(def DEF_USER  "popeye")
(def DEF_HOST  "localhost")
(def MIME_HEADER_MSGID  "Message-ID")
(def MIME_MULTIPART_BOUNDARY  "boundary")
(def DOT   ".")
(def AT  "@")
(def CH_DOT   \. )
(def CH_AT  \@)
(def STR_LT   "<")
(def STR_GT  ">")
(def ALL   -1)
(def ALL_ASCII   1)
(def MOSTLY_ASCII   2)
(def MOSTLY_NONASCII   3)

;; Capitalized MIME constants to use when generating MIME headers)
;; for messages to be transmitted.)
(def AS2_VER_ID    "1.1")
(def UA  "user-agent")
(def TO   "to")
(def FROM  "from")
(def AS2_VERSION    "as2-version")
(def AS2_TO   "as2-to")
(def AS2_FROM  "as2-from")
(def SUBJECT    "subject")
(def CONTENT_TYPE  "content-type")
(def CONTENT     "content")
(def CONTENT_NAME   "content-name")
(def CONTENT_LENGTH  "content-length")
(def CONTENT_LOC  "content-Location")
(def CONTENT_ID    "content-id")
(def CONTENT_TRANSFER_ENCODING  "content-transfer-encoding")
(def CONTENT_DISPOSITION   "content-disposition")
(def DISPOSITION_NOTIFICATION_TO  "disposition-notification-to")
(def DISPOSITION_NOTIFICATION_OPTIONS  "disposition-notification-options")
(def SIGNED_REC_MICALG "signed-receipt-micalg")
(def MESSAGE_ID   "message-id")
(def ORIGINAL_MESSAGE_ID   "original-message-id")
(def RECEIPT_DELIVERY_OPTION   "receipt-delivery-option")
(def DISPOSITION  "disposition")
(def DATE    "date")
(def MIME_VERSION   "mime-version")
(def FINAL_RECIPIENT   "final-recipient")
(def ORIGINAL_RECIPIENT   "original-recipient")
(def RECV_CONTENT_MIC   "received-content-mic")

(def RFC822 "rfc822")
(def RFC822_PFX (str RFC822 "; "))

(def MSG_DISP "message/disposition-notification")
(def APP_OCTET "application/octet-stream")
(def APP_XML "application/xml")
(def TEXT_PLAIN "text/plain")
(def PKCS7SIG "pkcs7-signature")
(def TEXT_HTML "text/html")
(def TEXT_XML "text/xml")

(def ERROR   "error")
(def FAILURE "failure")
(def WARNING  "warning")
(def HEADERS  "headers")

(def ISO_8859_1 "iso-8859-1")
(def US_ASCII "us-ascii")

(def CRLF "\r\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^Pattern _extRegex (Pattern/compile "^.*\\.([^.]+)$"))
(def ^:private _mime_cache (atom {}))
(def ^:private _mime_types (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mimeCache
  "Cache of most MIME types" ^APersistentMap [] @_mime_cache)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- isPkcs7Mime?

  ""
  [^String s]

  (>= (.indexOf s "application/x-pkcs7-mime") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getCharset

  "charset from this content-type"
  ^String
  [^String cType]

  (let [pos (-> (str cType)
                lcase
                (.indexOf "charset="))
        rc "utf-8"]
         ;;rc "ISO-8859-1" ]
    (if (> pos 0)
      (let [s (.substring cType (+ pos 8))
            p (indexAny s "; \t\r\n")]
        (if (> p 0) (.substring s 0 p) s))
      rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isSigned?

  "true if this content-type indicates signed"
  [^String cType]

  (let [ct (lcase cType)]
    (or (>= (.indexOf ct "multipart/signed") 0)
        (and (isPkcs7Mime? ct)
             (>= (.indexOf ct "signed-data") 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isEncrypted?

  "true if this content-type indicates encrypted"
  [^String cType]

  (let [ct (lcase cType)]
    (and (isPkcs7Mime? ct)
         (>= (.indexOf ct "enveloped-data") 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isCompressed?

  "true if this content-type indicates compressed"
  [^String cType]

  (let [ct (lcase cType)]
    (and (>= (.indexOf ct "application/pkcs7-mime") 0)
         (>= (.indexOf ct "compressed-data") 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isMDN?

  "true if this content-type indicates MDN"
  [^String cType]

  (let [ct (lcase cType)]
    (and (>= (.indexOf ct "multipart/report") 0)
         (>= (.indexOf ct "disposition-notification") 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn maybeStream

  "Convert object into some form of stream, if possible"
  ^InputStream
  [obj]

  (condp instance? obj
    String (streamify (bytesify obj))
    InputStream obj
    (bytesClass) (streamify obj)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn guessMimeType

  "Guess the MIME type of file"
  ^String
  [^File file & [dft]]

  (let [^Matcher
        mc (->> (lcase (.getName file))
                (.matcher _extRegex))
        ex (if
             (.matches mc)
             (.group mc 1) "")
        p (if (hgl? ex)
            ((keyword ex) (mimeCache) ))]
   (if (hgl? p) p (str dft))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn guessContentType

  "Guess the content-type of file"
  ^String
  [^File file & [enc dft]]

  (let [dft (stror dft "application/octet-stream")
        enc (stror enc "utf-8")
        mt (guessMimeType file)
        ^String ct (if (hgl? mt) mt dft)]
    (if-not (.startsWith ct "text/")
      ct
      (str ct "; charset=" enc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn setupCache

  "Load file mime-types as a map"
  [^URL fileUrl]

  (with-open [inp (.openStream fileUrl)]
    (let [ps (Properties.)]
      (.load ps inp)
      (reset! _mime_types (MimeFileTypes/makeMimeFileTypes ps))
      (reset! _mime_cache (pmap<> ps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


