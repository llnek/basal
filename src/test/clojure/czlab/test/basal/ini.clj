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


