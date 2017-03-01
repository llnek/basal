;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.basal.metautils

  (:use [czlab.basal.core]
        [czlab.basal.meta]
        [clojure.test])

  (:import [czlab.jasal CU XData]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestbasal-metautils

  (testing
    "related to: class operations"
    (is (isChild? Number Integer))
    (is (isChild? Number (Integer. 3)))

    (is (identical? (bytesClass) (class (byte-array 0))))
    (is (identical? (charsClass) (class (char-array 0))))

    (is (isBoolean? (class (boolean true))))
    (is (isChar? (class (char 3))))
    (is (isInt? (class (int 3))))
    (is (isLong? (class (long 3))))
    (is (isFloat? (class (float 3.2))))
    (is (isDouble? (class (double 3.2))))
    (is (isByte? (class (aget (byte-array 1) 0))))
    (is (isShort? (class (short 3))))
    (is (isString? (class "")))
    (is (isBytes? (class (byte-array 0))))

    (is (not (nil? (forname "java.lang.String"))))
    (is (not (nil? (getCldr))))

    (is (do (setCldr (getCldr)) true))

    (is (not (nil? (loadClass "java.lang.String"))))

    (is (ist? XData (objArgs<> "czlab.jasal.XData"
                                Object ""
                                Boolean/TYPE false)))

    (is (string? (new<> "java.lang.String")))

    (is (= 1 (count (listParents
                      (Class/forName "java.lang.String")))))

    (is (>= (count (listMethods
                     (Class/forName "java.lang.String"))) 40))

    (is (>= (count (listFields
                     (Class/forName "java.lang.String"))) 5)))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



