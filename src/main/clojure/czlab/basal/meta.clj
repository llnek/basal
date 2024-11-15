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
;; Copyright © 2013-2024, Kenneth Leung. All rights reserved.

(ns czlab.basal.meta

  "Class & reflection helpers."

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
  {:arglists '([func])}
  [func]
  {:pre [(fn? func)]}

  (let [s (c/preduce<set>
            #(let [^java.lang.reflect.Method m %2
                   n (.getName m)]
               (cond (.equals "getRequiredArity" n)
                     (-> (conj! %1
                                (.getRequiredArity ^RestFn func))
                         (conj! 709394))
                     (.equals "invoke" n)
                     (conj! %1 (.getParameterCount m))
                     :else %1))
            (.getDeclaredMethods (class func)))
        v? (c/in? s 709394)]
    [(disj s 709394) v?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-child?

  "If clazz is subclass of this base class?"
  {:arglists '([parent child])}
  [parent child]

  (cond (and (class? child)
             (class? parent)) (isa? child parent)
        (or (nil? child)
            (nil? parent)) false
        (not (class? parent)) (is-child? (class parent) child)
        (not (class? child)) (is-child? parent (class child))))

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
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["boolean" "Boolean" "java.lang.Boolean"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-void?

  "Is class Void?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["void" "Void" "java.lang.Void"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-char?

  "Is class Char?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["char" "Char" "java.lang.Character"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-int?

  "Is class Int?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["int" "Int" "java.lang.Integer"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-long?

  "Is class Long?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["long" "Long" "java.lang.Long"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-float?

  "Is class Float?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["float" "Float" "java.lang.Float"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-double?

  "Is class Double?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["double" "Double" "java.lang.Double"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-byte?

  "Is class Byte?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["byte" "Byte" "java.lang.Byte"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-short?

  "Is class Short?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["short" "Short" "java.lang.Short"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-string?

  "Is class String?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["String" "java.lang.String"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-object?

  "Is class Object?"
  {:arglists '([obj])}
  [obj]

  (is-XXX? obj ["Object" "java.lang.Object"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-chars?

  "Is class char[]?"
  {:arglists '([obj])}
  [obj]

  (= u/CSCZ (if-not (class? obj) (class obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-bytes?

  "Is class byte[]?"
  {:arglists '([obj])}
  [obj]

  (= u/BSCZ (if-not (class? obj) (class obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn forname

  "Load class by name."
  {:tag Class
   :arglists '([z][z cl])}

  ([z]
   (forname z nil))

  ([^String z cl]
   (if (nil? cl)
     (java.lang.Class/forName z)
     (java.lang.Class/forName z true cl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn load-class

  "Load class by name."
  {:tag Class
   :arglists '([clazzName]
               [clazzName cl])}

  ([clazzName]
   (load-class clazzName nil))

  ([clazzName cl]
   (if (c/hgl? clazzName)
     (.loadClass (u/get-cldr cl) clazzName))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn obj<>

  "Instantiate the class by
  invoking the constructor with args."
  {:tag Object
   :arglists '([cz & args])}
  [cz & args]
  {:pre [(or (string? cz)
             (class? cz))(c/n#-even? args)]}

  (let [cz (if (string? cz) (load-class cz) cz)
        args (partition 2 args)
        len (count args)
        cargs (c/marray Object len)
        ca (c/marray Class len)]
    (doseq [n (range len)
            :let [[z v] (nth args n)]]
      (u/aset* cargs n v)
      (aset #^"[Ljava.lang.Class;" ca n z))
    (.newInstance (.getDeclaredConstructor ^Class cz ca) cargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-parents

  "List all parent classes."
  {:arglists '([javaClass])}
  [javaClass]
  {:pre [(class? javaClass)]}

  (let [rc (loop [sum (c/tvec*)
                  par javaClass]
             (if (nil? par)
               (c/persist! sum)
               (recur (conj! sum par)
                      (.getSuperclass ^Class par))))]
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
              (list-mtds par
                         (+ 1 level)))
            (.getDeclaredMethods cz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- list-flds

  [^Class cz level]

  (let [par (.getSuperclass cz)]
    (reduce (iter-XXX level)
            (if (nil? par)
              (c/tmap*)
              (list-flds par
                         (+ 1 level)))
            (.getDeclaredFields cz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-methods

  "List methods belonging to this class,
  including inherited ones."
  {:arglists '([javaClass])}
  [javaClass]

  (vals (if javaClass
          (c/persist! (list-mtds javaClass 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-fields

  "List fields belonging to this class,
  including inherited ones."
  {:arglists '([javaClass])}
  [javaClass]

  (vals (if javaClass
          (c/persist! (list-flds javaClass 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

