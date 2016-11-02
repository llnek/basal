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

(ns ^{:doc "Class & reflection helpers."
      :author "Kenneth Leung" }

  czlab.xlib.meta

  (:require [czlab.xlib.logging :as log])

  (:use [czlab.xlib.core]
        [czlab.xlib.str])

  (:import [clojure.lang APersistentVector]
           [java.lang.reflect Member Field Method Modifier]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti isChild?
  "If clazz is subclass of this base class"
  (fn [_ b] (if (instance? Class b) :class :object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod isChild?
  :class
  [^Class basz ^Class cz]

  (if (or (nil? basz)
          (nil? cz))
    false
    (.isAssignableFrom basz cz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod isChild?
  :object
  [^Class basz ^Object obj]

  (if (or (nil? basz)
          (nil? obj))
    false
    (instance? basz obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bytesClass "Java class for byte[]" ^Class [] (Class/forName "[B"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn charsClass "Java class for char[]" ^Class [] (Class/forName "[C"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gcn "Get name of this Class" [^Class c] (.getName c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- isXXX?
  ""
  [classObj classes]
  {:pre [(instance? Class classObj)]}

  (eqAny? (gcn classObj) classes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isBoolean?
  "If class is Boolean"
  [classObj]

  (isXXX? classObj ["boolean" "Boolean" "java.lang.Boolean"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isVoid?
  "If class is Void"
  [classObj]

  (isXXX? classObj ["void" "Void" "java.lang.Void"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isChar?
  "If class is Char"
  [classObj]

  (isXXX? classObj [ "char" "Char" "java.lang.Character" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isInt?
  "If class is Int"
  [classObj]

  (isXXX? classObj [ "int" "Int" "java.lang.Integer" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isLong?
  "If class is Long"
  [classObj]

  (isXXX? classObj [ "long" "Long" "java.lang.Long" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isFloat?
  "If class is Float"
  [classObj]

  (isXXX? classObj [ "float" "Float" "java.lang.Float" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isDouble?
  "If class is Double"
  [classObj]

  (isXXX? classObj [ "double" "Double" "java.lang.Double" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isByte?
  "If class is Byte"
  [classObj]

  (isXXX? classObj [ "byte" "Byte" "java.lang.Byte" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isShort?
  "If class is Short"
  [classObj]

  (isXXX? classObj [ "short" "Short" "java.lang.Short" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isString?
  "If class is String"
  [classObj]

  (isXXX? classObj [ "String" "java.lang.String" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isObject?
  "If class is Object"
  [classObj]

  (isXXX? classObj [ "Object" "java.lang.Object" ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isChars?
  "If class is char[]"
  [classObj]

  (= classObj (charsClass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isBytes?
  "If class is byte[]"
  [classObj]

  (= classObj (bytesClass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro instBytes? "Is object byte[]" [b] `(isBytes? (class ~b)))
(defmacro instChars? "Is object char[]" [c] `(isChars? (class ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn forname
  "Load a java class by name"
  {:tag Class}

  ([z] (forname z nil))
  ([^String z cl]
   (if (nil? cl)
     (java.lang.Class/forName z)
     (->> ^ClassLoader cl
          (java.lang.Class/forName z true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getCldr
  "Get the current classloader"
  {:tag ClassLoader}

  ([] (getCldr nil))
  ([cl]
   (or cl (.getContextClassLoader (Thread/currentThread)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn setCldr
  "Set current classloader"
  [^ClassLoader cl]
  {:pre [(some? cl)]}

  (.setContextClassLoader (Thread/currentThread) cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loadClass
  "Load this class by name"
  {:tag Class}

  ([clazzName] (loadClass clazzName nil))
  ([^String clazzName cl]
   (if (hgl? clazzName)
     (.loadClass (getCldr cl) clazzName))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti objArgs<>
  "Instantiate object with arity-n constructor"
  {:tag Object}
  (fn [a & xs] (class a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod objArgs<>
  Class
  [^Class cz & args]
  {:pre [(some? cz)
         (> (count args) 0)
         (even? (count args))]}

  (let [args (partition 2 args)
        len (count args)
        cargs (object-array len)
        ca (make-array Class len)]
    (doseq [n (range len)
            :let [[z v] (nth args n)]]
      (aset #^"[Ljava.lang.Object;" cargs n v)
      (aset #^"[Ljava.lang.Class;" ca n z))
    (-> (.getDeclaredConstructor cz ca)
        (.newInstance cargs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod objArgs<>
  String
  [^String cz & args]
  (apply objArgs<> (loadClass cz) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ctor<>
  "Call the default contructor"
  ^Object
  [^Class cz]
  {:pre [(some? cz)]}

  (-> (.getDeclaredConstructor cz (make-array Class 0))
      (.newInstance (object-array 0)  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn new<>
  "Make an object of this class by calling the default constructor"
  {:tag Object}

  ([clazzName] (new<> clazzName nil))
  ([^String clazzName cl]
   (if (hgl? clazzName)
     (ctor<> (loadClass clazzName cl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listParents
  "List all parent classes"
  ^APersistentVector
  [^Class javaClass]
  {:pre [(some? javaClass)]}
  (let
    [rc (loop [sum (transient [])
               par javaClass]
          (if (nil? par)
            (pcoll! sum)
            (recur (conj! sum par)
                   (.getSuperclass par))))]
    ;; since we always add the original class,
    ;; we need to ignore it on return
    (into [] (drop 1 rc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- iterXXX
  ""
  [cz level getDeclXXX bin]
  (reduce
    (fn [sum ^Member m]
      (let [x (.getModifiers m)]
        (if (and (> level 0)
                 (or (Modifier/isStatic x)
                     (Modifier/isPrivate x)))
          sum
          (assoc! sum (.getName m) m))))
    bin
    (getDeclXXX cz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- listMtds
  ""
  [^Class cz level]

  (let [par (.getSuperclass cz)]
    (iterXXX cz
             level
             #(.getDeclaredMethods ^Class %)
             (if (nil? par)
               (transient {})
               (listMtds par (inc level))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- listFlds
  ""
  [^Class cz level]

  (let [par (.getSuperclass cz)]
    (iterXXX cz
             level
             #(.getDeclaredFields ^Class %)
             (if (nil? par)
               (transient {})
               (listFlds par (inc level))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listMethods
  "List all methods belonging to this class, including inherited ones"
  [^Class javaClass]
  {:pre [(some? javaClass)]}

  (vals (if (nil? javaClass)
          {}
          (pcoll! (listMtds javaClass 0 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listFields
  "List all fields belonging to this class, including inherited ones"
  [^Class javaClass]
  {:pre [(some? javaClass)]}

  (vals (if (nil? javaClass)
          {}
          (pcoll! (listFlds javaClass 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


