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

  (:require [czlab.basal.guids :as g]
            [clojure.string :as cs]
            [clojure.test :as ct]
            [czlab.basal.cmenu :as i]
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
              (let [r (u/call* z
                               :clojure.template/apply-template
                               ['[x] '(+ x x) '[2]])]
                (and (list? r)
                     (= 3 (count r))))))

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
(ct/deftest
  ^:test-misc basal-test-misc
  (ct/is (c/clj-test?? test-misc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


