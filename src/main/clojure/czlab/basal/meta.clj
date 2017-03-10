;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Class & reflection helpers."
      :author "Kenneth Leung"}

  czlab.basal.meta

  (:require [czlab.basal.logging :as log])

  (:use [czlab.basal.core]
        [czlab.basal.str])

  (:import [clojure.lang RestFn APersistentVector]
           [java.lang.reflect Member Field Method Modifier]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn countArity
  "Figure out arity of function, returning the set of
  arity counts and whether var-args are used.
  e.g.
  [#{0 1 2 3} true]"

  [func] {:pre [(fn? func)]}

  (let [s
        (persistent!
          (reduce
            #(let [^java.lang.reflect.Method m %2
                   n (.getName m)]
               (cond
                 (= "getRequiredArity" n)
                 (-> (conj! %1 (.getRequiredArity ^RestFn func))
                     (conj! 709394))
                 (= "invoke" n)
                 (conj! %1 (.getParameterCount m))
                 :else %1))
            (transient #{})
            (.getDeclaredMethods (class func))))
        v? (contains? s 709394)]
    [(disj s 709394) v?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti isChild?
  "If clazz is subclass of this base class"
  (fn [_ b] (if (instance? Class b) :class :object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod isChild?
  :class
  [basz cz]
  (and (class? basz)
       cz (. ^Class basz isAssignableFrom ^Class cz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod isChild?
  :object
  [basz ^Object obj]
  (and (class? basz) obj (instance? basz obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(let [z (Class/forName "[B")]
  (defn bytesClass "Java class for byte[]" ^Class [] z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(let [z (Class/forName "[C")]
  (defn charsClass "Java class for char[]" ^Class [] z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- gcn "" [z] (some-> ^Class z .getName))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- isXXX?
  "" [c classes]
  (-> (gcn (if-not
             (class? c) (class c) c)) (eqAny? classes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isBoolean?
  "Is class Boolean?"
  [obj] (isXXX? obj ["boolean" "Boolean" "java.lang.Boolean"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isVoid?
  "Is class Void?"
  [obj] (isXXX? obj ["void" "Void" "java.lang.Void"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isChar?
  "Is class Char?"
  [obj] (isXXX? obj ["char" "Char" "java.lang.Character"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isInt?
  "Is class Int?"
  [obj] (isXXX? obj ["int" "Int" "java.lang.Integer"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isLong?
  "Is class Long?"
  [obj] (isXXX? obj ["long" "Long" "java.lang.Long"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isFloat?
  "Is class Float?"
  [obj] (isXXX? obj ["float" "Float" "java.lang.Float"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isDouble?
  "Is class Double?"
  [obj] (isXXX? obj ["double" "Double" "java.lang.Double"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isByte?
  "Is class Byte?"
  [obj] (isXXX? obj ["byte" "Byte" "java.lang.Byte"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isShort?
  "Is class Short?"
  [obj] (isXXX? obj ["short" "Short" "java.lang.Short"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isString?
  "Is class String?"
  [obj] (isXXX? obj ["String" "java.lang.String"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isObject?
  "Is class Object?"
  [obj] (isXXX? obj ["Object" "java.lang.Object"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isChars?
  "Is class char[]?"
  [obj]
  (= (charsClass)
     (if-not
       (class? obj) (class obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn isBytes?
  "Is class byte[]?"
  [obj]
  (= (bytesClass)
     (if-not
       (class? obj) (class obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro instBytes? "Is object byte[]?" [b] `(isBytes? ~b))
(defmacro instChars? "Is object char[]?" [c] `(isChars? ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn forname
  "Load class by name" {:tag Class}

  ([z] (forname z nil))
  ([^String z cl]
   (if cl
     (java.lang.Class/forName z true cl)
     (java.lang.Class/forName z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getCldr
  "Get current classloader" {:tag ClassLoader}

  ([] (getCldr nil))
  ([cl] (or cl (. (Thread/currentThread)
                  getContextClassLoader ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn setCldr
  "Set classloader"
  [^ClassLoader cl]
  (if (some? cl)
    (. (Thread/currentThread) setContextClassLoader cl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loadClass
  "Load class by name" {:tag Class}

  ([clazzName] (loadClass clazzName nil))
  ([^String clazzName cl]
   (if (hgl? clazzName)
     (. (getCldr cl) loadClass clazzName))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti objArgs<>
  "New object with arity-n constructor"
  {:tag Object} (fn [a & xs] (class a)))

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
        cargs (marray Object len)
        ca (marray Class len)]
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
  [cz & args] (apply objArgs<> (loadClass cz) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn ctor<>
  "Call the default contructor"
  ^Object [^Class cz]

  (some-> cz
          (.getDeclaredConstructor (zarray Class))
          (.newInstance (zarray Object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn new<>
  "Make object via the no-arg constructor"
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
  "" [cz level getDeclXXX bin]

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
  "" [^Class cz level]

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
  "" [^Class cz level]

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

  (vals (if (nil? javaClass)
          {}
          (pcoll! (listMtds javaClass 0 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listFields
  "List all fields belonging to this class, including inherited ones"
  [^Class javaClass]

  (vals (if (nil? javaClass)
          {}
          (pcoll! (listFlds javaClass 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


