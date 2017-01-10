;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Some useful constants."
      :author "Kenneth Leung"}

  czlab.xlib.consts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^String PATHSEP (System/getProperty "file.separator"))
(def ^String USASCII "ISO-8859-1" )
(def ^String UTF16 "UTF-16" )
(def ^String UTF8 "UTF-8" )
(def ^String SLASH   "/" )

(def BOOLS #{ "true", "yes", "on", "ok", "active", "1"} )
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


