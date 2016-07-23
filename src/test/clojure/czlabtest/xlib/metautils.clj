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

  (:require [czlab.xlib.meta :as MU])
  (:use [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestxlib-metautils

  (is (true? (MU/isChild? (Class/forName "java.lang.Number") (Class/forName "java.lang.Integer"))))
  (is (true? (MU/isChild? (Class/forName "java.lang.Number") (Integer. 3))))
  (is (identical? (MU/bytesClass) (class (byte-array 0))))
  (is (identical? (MU/charsClass) (class (char-array 0))))

  (is (true? (MU/isBoolean? (class (boolean true)))))
  (is (true? (MU/isChar? (class (char 3)))))
  (is (true? (MU/isInt? (class (int 3)))))
  (is (true? (MU/isLong? (class (long 3)))))
  (is (true? (MU/isFloat? (class (float 3.2)))))
  (is (true? (MU/isDouble? (class (double 3.2)))))
  (is (true? (MU/isByte? (class (aget (byte-array 1) 0)))))
  (is (true? (MU/isShort? (class (short 3)))))
  (is (true? (MU/isString? (class ""))))
  (is (true? (MU/isBytes? (class (byte-array 0)))))

  (is (not (nil? (MU/forname "java.lang.String"))))
  (is (not (nil? (MU/getCldr))))

  (is (true? (do (MU/setCldr (MU/getCldr)) true)))

  (is (not (nil? (MU/loadClass "java.lang.String"))))

  (is (= "" (MU/new<> "java.lang.String")))

  (is (= 1 (count (MU/listParents (Class/forName "java.lang.String")))))

  (is (>= (count (MU/listMethods (Class/forName "java.lang.String"))) 40))
  (is (>= (count (MU/listFields (Class/forName "java.lang.String"))) 5))

)

;;(clojure.test/run-tests 'czlabtest.xlib.metautils)

