;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.test.basal.misc

  (:require [czlab.basal.ccodes :as cc]
            [czlab.basal.guids :as g]
            [clojure.string :as cs]
            [clojure.test :as ct]
            [czlab.basal.cli :as i]
            [czlab.basal.cljrt :as rt]
            [czlab.basal.core
             :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-misc

  (ensure?? "cljrt<>"
            (c/wo* [^java.io.Closeable z (rt/cljrt<>)]
              (satisfies? czlab.basal.cljrt/CljrtAPI z)))

  (ensure?? "cljrt.call*"
            (c/wo* [^java.io.Closeable z (rt/cljrt<>)]
              (let [r (rt/call* z
                                :clojure.template/apply-template
                                ['[x] '(+ x x) '[2]])]
                (and (list? r)
                     (= 3 (count r))))))

  (ensure?? "find-country" (= (cc/find-country "AU")
                              (cc/find-country "au")))

  (ensure?? "find-country" (= "Australia" (cc/find-country "AU")))

  (ensure?? "find-country-code" (= "AU" (cc/find-country-code "Australia")))

  (ensure?? "is-usa?" (false? (cc/is-usa? "aa")))

  (ensure?? "is-usa?" (and (cc/is-usa? "US")
                           (= (cc/is-usa? "US") (cc/is-usa? "us"))))

  (ensure?? "list-codes" (> (count (cc/list-codes)) 0))

  (ensure?? "find-state"
            (= (cc/find-state "CA") (cc/find-state "ca")))

  (ensure?? "find-state" (= "California" (cc/find-state "ca")))

  (ensure?? "find-state-code" (= "CA" (cc/find-state-code "California")))

  (ensure?? "list-states" (> (count (cc/list-states)) 0))

  (ensure?? "wwid<>" (not= (g/wwid<>) (g/wwid<>)))

  (ensure?? "uuid<>" (not= (g/uuid<>) (g/uuid<>)))

  (ensure?? "wwid<>" (> (count (g/wwid<>)) 0))

  (ensure?? "uuid<>" (> (count (g/uuid<>)) 0))

  (ensure?? "parse-options"
            (let [[o v] (i/parse-options ["--a" "b" "/c" "d" "-e" "f" "g"])]
              (and (= "b" (:a o))
                   (= "d" (:c o))
                   (= "f" (:e o))
                   (= "g" (cs/join "" v)))))

  (ensure?? "parse-options"
            (let [[o v] (i/parse-options ["--" "a" "b" "c"])]
              (and (empty? o)
                   (= "abc" (cs/join "" v)))))

  (ensure?? "parse-options"
            (let [[o v] (i/parse-options ["a" "b" "c"])]
              (and (empty? o)
                   (= "abc" (cs/join "" v)))))

  (ensure?? "test-end" (= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest ^:test-misc basal-test-misc
  (ct/is (let [[ok? r]
               (c/runtest test-misc "test-misc")] (println r) ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


