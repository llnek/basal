;; Copyright ©  2013-2020, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

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


