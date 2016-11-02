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
;; Copyright (c) 2013-2016, Kenneth Leung. All rights reserved.

(ns czlabtest.xlib.metautils

  (:use [czlab.xlib.core]
        [czlab.xlib.meta]
        [clojure.test])

  (:import [czlab.xlib CU XData]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-metautils

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

  (is (inst? XData (objArgs<> "czlab.xlib.XData"
                              Object ""
                              Boolean/TYPE false)))

  (is (string? (new<> "java.lang.String")))

  (is (= 1 (count (listParents
                    (Class/forName "java.lang.String")))))

  (is (>= (count (listMethods
                   (Class/forName "java.lang.String"))) 40))

  (is (>= (count (listFields
                   (Class/forName "java.lang.String"))) 5))

  (is (string? "That's all folks!")))



;;(clojure.test/run-tests 'czlabtest.xlib.metautils)

