;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.basal.ebus

  (:require [czlab.basal.core :as c]
            [clojure.string :as cs]
            [czlab.basal.format :as f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:private _SEED (atom 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- splitTopic "" [topic] (if (and (string? topic)
                                      (not-empty topic))
                               (->> (cs/split topic #"/")
                                    (filterv #(if (> (count %) 0) %)))
                               []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nextSEQ "" [] (let [n @_SEED] (swap! _SEED inc) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol EventBus
  ""
  (ev-sub** [_ topics listener] "")
  (ev-sub* [_ topics listener] "")
  (ev-pub [_ topic msg] "")
  (ev-dbg [_] "")
  (ev-resume [_ handle] "")
  (ev-pause [_ handle] "")
  (ev-unsub [_ handle] "")
  (ev-removeAll [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkSubSCR
  ""
  [topic repeat? listener]
  {:pre [(fn? listener)]}

  {:id (keyword (str "s#" (nextSEQ)))
   :repeat? repeat?
   :action listener
   :topic topic
   :status (long-array 1 1)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nodes - children
;; subscribers
(defn- mkTreeNode "" [] {:nodes {} :subcs {}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addOneSub "" [node sub]
  (update-in node [:subcs] assoc (:id sub) sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- remOneSub "" [node sub]
  (update-in node [:subcs] dissoc (:id sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addOneTopic "" [top {:keys [topic] :as sub}]
  (let [path (c/concatv [:nodes] (splitTopic topic))]
    (-> (update-in top path addOneSub sub)
        (update-in [:subcs] assoc (:id sub) sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addTopic "" [root sub] (swap! root addOneTopic sub) (:id sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for each topic, subscribe to it.
(defn- listen "" [root repeat? topics listener]
  (->> (-> (or topics "") cs/trim (cs/split #"\s+"))
       (filter #(if (> (count %) 0) %))
       (mapv #(addTopic root
                        (mkSubSCR % repeat? listener)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- delOneTopic "" [top {:keys [topic] :as sub}]
  (let [path (c/concatv [:nodes] (splitTopic topic))]
    (-> (update-in top path remOneSub sub)
        (update-in [:subcs] dissoc (:id sub)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- unSub "" [root sub] (swap! root delOneTopic sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- run "" [{:keys [subcs]} topic msg]
  (doseq [z subcs
          :let [{:keys [repeat? action status]} z]
          :when (pos? (aget ^longs status 0))]
    (action topic msg)
    ;;if one time only, turn off flag
    (if-not repeat?
      (aset ^longs status 0 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doPub "" [branch pathTokens topic msg]
  (let [{:keys [nodes subcs]} branch
        [p & more] pathTokens
        cur (get nodes p)
        s1 (get nodes "*")
        s1c (:nodes s1)
        s2 (get nodes "**")]
    (if s2
      (run s2 topic msg))
    (if s1
      (cond
        (and (empty? more)
             (empty? s1c))
        (run s1 topic msg)
        (or (and (empty? s1c)
                 (not-empty more))
            (and (empty? more)
                 (not-empty s1c)))
        nil
        :else
        (doPub s1 more topic msg)))
    (if cur
      (if (not-empty more)
        (doPub cur more topic msg)
        (run cur topic msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eventBus<> "" []
  (let [state (atom (mkTreeNode))]
    (reify EventBus
      ;; subscribe once to 1+ topics, return a list of handles
      ;; topics => "/hello/**  /goodbye/*/xyz"
      (ev-sub* [_ topics listener]
        (listen state false topics listener))
      ;; subscribe to 1+ topics, return a list of handles
      ;; topics => "/hello/**  /goodbye/*/xyz"
      (ev-sub** [_ topics listener]
        (listen state true topics listener))
      (ev-pub [_ topic msg]
        (c/let->nil [tokens (splitTopic topic)]
          (if (not-empty tokens)
            (doPub @state tokens topic msg))))
      (ev-resume [_ hd]
        (c/let->nil [sub (get (:subcs @state) hd)
                     st (if sub (:status sub))
                     sv (if st (aget ^longs st 0) -1)]
          (if (= 0 sv) (aset ^longs st 0 1))))
      (ev-pause [_ hd]
        (c/let->nil [sub (get (:subcs @state) hd)
                     st (if sub (:status sub))
                     sv (if st (aget ^longs st 0) -1)]
          (if (pos? sv) (aset ^longs st 0 0))))
      (ev-unsub [_ hd]
        (c/do->nil
          (some->> (get (:subcs @state) hd)
                   (unSub state))))
      (ev-dbg [_] (f/writeEdnStr @state))
      (ev-removeAll [_]
        (c/do->nil (reset! state (mkTreeNode)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

