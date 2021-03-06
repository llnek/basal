;; Copyright ©  2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.ini

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.ini :as i]
            [czlab.basal.io :as io]
            [czlab.basal.util :as u]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def
  ^{:private true}
  INIFILE (i/win-ini<> (io/res->url "czlab/basal/etc/sample.ini")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-ini

  (ensure?? "headings" (== 2 (count (i/headings INIFILE))))

  (ensure?? "heading" (map? (i/heading INIFILE "operating systems")))

  (ensure?? "heading" (map? (i/heading INIFILE "boot loader")))

  (ensure?? "str-value" (cs/ends-with?
                          (i/str-value INIFILE
                                      "boot loader" "default") "WINDOWS"))

  (ensure?? "long-value" (= (i/long-value INIFILE
                                          "boot loader" "timeout") 30))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-ini basal-test-ini
  (ct/is (c/clj-test?? test-ini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


