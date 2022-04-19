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
;; Copyright © 2013-2022, Kenneth Leung. All rights reserved.

(ns czlab.test.basal.misc

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.util :as u]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-misc

  (ensure?? "cljrt<>"
            (let [z (u/cljrt<>)]
              (satisfies? czlab.basal.util/Cljrt z)))

  (ensure?? "cljrt.call*"
            (let [z (u/cljrt<>)]
              (c/hgl? (u/call* z
                               :czlab.basal.proc/process-pid []))))
                            ;:clojure.core/+ [1 2 3]))))

  (ensure?? "wwid<>" (not= (u/wwid<>) (u/wwid<>)))

  (ensure?? "uuid-v4<>" (not= (u/uuid-v4<>) (u/uuid-v4<>)))

  (ensure?? "wwid<>" (> (count (u/wwid<>)) 0))

  (ensure?? "uuid-v4<>;len" (> (count (u/uuid-v4<>)) 0))

  (ensure?? "parse-options"
            (let [[o v] (u/parse-options ["--a" "b" "-c" "d" "-e" "f" "g"])]
              (and (.equals "b" (:a o))
                   (.equals "d" (:c o))
                   (.equals "f" (:e o))
                   (.equals "g" (cs/join "" v)))))

  (ensure?? "parse-options"
            (let [[o v] (u/parse-options ["--" "a" "b" "c"])]
              (and (empty? o)
                   (.equals "abc" (cs/join "" v)))))

  (ensure?? "parse-options"
            (let [[o v] (u/parse-options ["a" "b" "c"])]
              (and (empty? o)
                   (.equals "abc" (cs/join "" v)))))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-misc basal-test-misc
  (ct/is (c/clj-test?? test-misc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


