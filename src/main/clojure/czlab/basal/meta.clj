;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Class & reflection helpers."
      :author "Kenneth Leung"}

  czlab.basal.meta

  (:require [czlab.basal.util :as u]
            [czlab.basal.core :as c])

  (:import [clojure.lang
            RestFn]
           [java.lang.reflect
            Member
            Field
            Method
            Modifier]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-arity
  "Figure out arity of function, returning the set of
  arity counts and whether var-args are used.
  e.g. [#{0 1 2 3} true]."
  [func]
  {:pre [(fn? func)]}
  (let [s (c/preduce<set>
            #(let [^java.lang.reflect.Method m %2
                   n (.getName m)]
               (cond (= "getRequiredArity" n)
                     (-> (conj! %1 (.getRequiredArity ^RestFn func))
                         (conj! 709394))
                     (= "invoke" n)
                     (conj! %1 (.getParameterCount m))
                     :else %1))
            (.getDeclaredMethods (class func)))
        v? (c/in? s 709394)]
    [(disj s 709394) v?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-child?
  "If clazz is subclass of this base class?"
  [par chi]
  (cond (and (class? par)(class? chi)) (isa? chi par)
        (or (nil? par)(nil? chi)) false
        (not (class? par)) (is-child? (class par) chi)
        (not (class? chi)) (is-child? par (class chi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gcn
  [z] (some-> ^Class z .getName))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-XXX?
  [c classes]
  (-> (gcn (if-not
             (class? c) (class c) c)) (c/eq-any? classes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-boolean?
  "Is class Boolean?"
  [obj] (is-XXX? obj ["boolean" "Boolean" "java.lang.Boolean"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-void?
  "Is class Void?"
  [obj] (is-XXX? obj ["void" "Void" "java.lang.Void"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-char?
  "Is class Char?"
  [obj] (is-XXX? obj ["char" "Char" "java.lang.Character"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-int?
  "Is class Int?"
  [obj] (is-XXX? obj ["int" "Int" "java.lang.Integer"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-long?
  "Is class Long?"
  [obj] (is-XXX? obj ["long" "Long" "java.lang.Long"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-float?
  "Is class Float?"
  [obj] (is-XXX? obj ["float" "Float" "java.lang.Float"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-double?
  "Is class Double?"
  [obj] (is-XXX? obj ["double" "Double" "java.lang.Double"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-byte?
  "Is class Byte?"
  [obj] (is-XXX? obj ["byte" "Byte" "java.lang.Byte"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-short?
  "Is class Short?"
  [obj] (is-XXX? obj ["short" "Short" "java.lang.Short"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-string?
  "Is class String?"
  [obj] (is-XXX? obj ["String" "java.lang.String"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-object?
  "Is class Object?"
  [obj] (is-XXX? obj ["Object" "java.lang.Object"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-chars?
  "Is class char[]?"
  [obj]
  (= u/CSCZ (if-not (class? obj) (class obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-bytes?
  "Is class byte[]?"
  [obj]
  (= u/BSCZ (if-not (class? obj) (class obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn forname
  "Load class by name."
  {:tag Class}
  ([z]
   (forname z nil))
  ([^String z cl]
   (if (nil? cl)
     (java.lang.Class/forName z)
     (java.lang.Class/forName z true cl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-class
  "Load class by name."
  {:tag Class}
  ([clazzName]
   (load-class clazzName nil))
  ([clazzName cl]
   (if (c/hgl? clazzName)
     (.loadClass (u/get-cldr cl) clazzName))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn obj<>
  ^Object [cz & args]
  {:pre [(or (string? cz)
             (class? cz))(c/n#-even? args)]}
  (let [cz (if (string? cz) (load-class cz) cz)
        args (partition 2 args)
        len (count args)
        cargs (c/marray Object len)
        ca (c/marray Class len)]
    (doseq [n (range len)
            :let [[z v] (nth args n)]]
      (aset #^"[Ljava.lang.Class;" ca n z)
      (aset #^"[Ljava.lang.Object;" cargs n v))
    (.newInstance (.getDeclaredConstructor ^Class cz ca) cargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-parents
  "List all parent classes."
  [^Class javaClass]
  {:pre [(class? javaClass)]}
  (let [rc (loop [sum (c/tvec*)
                  par javaClass]
             (if (nil? par)
               (c/persist! sum)
               (recur (conj! sum par)
                      (.getSuperclass par))))]
    ;; since we always add the original class,
    ;; we need to ignore it on return
    (c/vec-> (drop 1 rc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- iter-XXX
  [level]
  (fn [sum ^Member m]
    (let [x (.getModifiers m)]
      (if (and (pos? level)
               (or (Modifier/isStatic x)
                   (Modifier/isPrivate x)))
        sum
        (assoc! sum (.getName m) m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- list-mtds
  [^Class cz level]
  (let [par (.getSuperclass cz)]
    (reduce (iter-XXX level)
            (if (nil? par)
              (c/tmap*)
              (list-mtds par (+ 1 level))) (.getDeclaredMethods cz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- list-flds
  [^Class cz level]
  (let [par (.getSuperclass cz)]
    (reduce (iter-XXX level)
            (if (nil? par)
              (c/tmap*)
              (list-flds par (+ 1 level))) (.getDeclaredFields cz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-methods
  "List all methods belonging to this class, including inherited ones."
  [javaClass]
  (vals (if javaClass
          (c/persist! (list-mtds javaClass 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-fields
  "List all fields belonging to this class, including inherited ones."
  [javaClass]
  (vals (if javaClass
          (c/persist! (list-flds javaClass 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

