;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.metautils

  (:require [czlab.basal.core :as c]
            [czlab.basal.meta :as m])

  (:use [clojure.test])

  (:import [czlab.jasal CU XData]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- f1 ""
  ([])
  ([x])
  ([x y])
  ([x y {:keys [ff]}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- f2 ""
  ([])
  ([x])
  ([x y])
  ([x y {:keys [ff]}])
  ([x y f p d & z]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-metautils

  (testing
    "related to: fn arity"
    (is (let [[r v?] (m/countArity f1)]
          (and (false? v?)
               (contains? r 0)
               (contains? r 1)
               (contains? r 2)
               (contains? r 3))))
    (is (let [[r v?] (m/countArity
                       (fn [a b]))]
          (and (false? v?)
               (contains? r 2))))
    (is (let [[r v?] (m/countArity f2)]
          (and (true? v?)
               (contains? r 0)
               (contains? r 1)
               (contains? r 2)
               (contains? r 3)
               (contains? r 5)))))

  (testing
    "related to: class operations"
    (is (m/isChild? Number Integer))
    (is (m/isChild? Number (Integer. 3)))

    (is (identical? (m/bytesClass) (class (byte-array 0))))
    (is (identical? (m/charsClass) (class (char-array 0))))

    (is (m/isBoolean? (class (boolean true))))
    (is (m/isChar? (class (char 3))))
    (is (m/isInt? (class (int 3))))
    (is (m/isLong? (class (long 3))))
    (is (m/isFloat? (class (float 3.2))))
    (is (m/isDouble? (class (double 3.2))))
    (is (m/isByte? (class (aget (byte-array 1) 0))))
    (is (m/isShort? (class (short 3))))
    (is (m/isString? (class "")))
    (is (m/isBytes? (class (byte-array 0))))

    (is (not (m/isBytes? nil)))
    (is (not (m/isChars? nil)))

    (is (not (nil? (m/forname "java.lang.String"))))
    (is (not (nil? (m/getCldr))))

    (is (do (m/setCldr (m/getCldr)) true))

    (is (not (nil? (m/loadClass "java.lang.String"))))

    (is (c/ist? XData (m/objArgs<> "czlab.jasal.XData"
                                   Object ""
                                   Boolean/TYPE false)))

    (is (string? (m/new<> "java.lang.String")))

    (is (= 1 (count (m/listParents
                      (Class/forName "java.lang.String")))))

    (is (>= (count (m/listMethods
                     (Class/forName "java.lang.String"))) 40))

    (is (>= (count (m/listFields
                     (Class/forName "java.lang.String"))) 5)))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



