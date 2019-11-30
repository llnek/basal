;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.meta

  (:require [clojure.test :as ct]
            [clojure.string :as cs]
            [czlab.basal.meta :as m]
            [czlab.basal.core
              :refer [ensure?? ensure-thrown??] :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- f1
  ([])
  ([x])
  ([x y])
  ([x y {:keys [ff]}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- f2
  ([])
  ([x])
  ([x y])
  ([x y {:keys [ff]}])
  ([x y f p d & z]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-meta

  (ensure?? "count-arity"
            (let [[r v?] (m/count-arity f1)]
              (and (false? v?)
                   (contains? r 0)
                   (contains? r 1)
                   (contains? r 2)
                   (contains? r 3))))

  (ensure?? "count-arity" (let [[r v?] (m/count-arity (fn [a b]))]
                            (and (false? v?)
                                 (contains? r 2))))

  (ensure?? "count-arity"
            (let [[r v?] (m/count-arity f2)]
              (and (true? v?)
                   (contains? r 0)
                   (contains? r 1)
                   (contains? r 2)
                   (contains? r 3)
                   (contains? r 5))))

  (ensure?? "is-child?" (m/is-child? Number Integer))
  (ensure?? "is-child?" (m/is-child? Number (Integer. 3)))

  (ensure?? "is-boolean?" (m/is-boolean? (class (boolean true))))
  (ensure?? "is-char?" (m/is-char? (class (char 3))))
  (ensure?? "is-int?" (m/is-int? (class (int 3))))
  (ensure?? "is-long?" (m/is-long? (class (long 3))))
  (ensure?? "is-float?" (m/is-float? (class (float 3.2))))
  (ensure?? "is-double?" (m/is-double? (class (double 3.2))))
  (ensure?? "is-byte?" (m/is-byte? (class (aget (byte-array 1) 0))))
  (ensure?? "is-short?" (m/is-short? (class (short 3))))
  (ensure?? "is-string?" (m/is-string? (class "")))
  (ensure?? "is-bytes?" (m/is-bytes? (class (byte-array 0))))

  (ensure?? "is-bytes?" (not (m/is-bytes? nil)))
  (ensure?? "is-chars?" (not (m/is-chars? nil)))

  (ensure?? "forname" (not (nil? (m/forname "java.lang.String"))))

  (ensure?? "load-class" (not (nil? (m/load-class "java.lang.String"))))

  (ensure?? "obj<>" (c/is? java.lang.StringBuilder
                           (m/obj<> "java.lang.StringBuilder" String "a")))

  (ensure?? "list-parents" (== 1 (count (m/list-parents
                                         (Class/forName "java.lang.String")))))

  (ensure?? "list-methods" (>= (count (m/list-methods
                                        (Class/forName "java.lang.String"))) 40))

  (ensure?? "list-fields" (>= (count (m/list-fields
                                       (Class/forName "java.lang.String"))) 5))

  (ensure?? "test-end" (== 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest
  ^:test-meta basal-test-meta
  (ct/is (c/clj-test?? test-meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


