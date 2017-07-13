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
  (ev-sub+ [_ topics listener] "1+")
  (ev-sub* [_ topics listener] "1")
  (ev-pub [_ topic msg] "")
  (ev-dbg [_] "test")
  (ev-match? [_ topic] "test")
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
;;
(defmulti addOneTopic "" {:private true} (fn [_ b & xs] b))
(defmulti delOneTopic {:private true} (fn [_ b & xs] b))
(defmulti addTopic "" {:private true} (fn [_ b & xs] b))
(defmulti unSub {:private true} (fn [_ b & xs] b))

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
(defn- interleavePath "" [path]
  (into [] (mapcat #(doto [:nodes %]) path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod addOneTopic :rbus [top kind {:keys [topic] :as sub}]
  (let [path (interleavePath (splitTopic topic))]
    (-> (update-in top path addOneSub sub)
        (update-in [:subcs] assoc (:id sub) sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod addTopic :rbus [root kind sub]
  (swap! root addOneTopic kind sub) (:id sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for each topic, subscribe to it.
(defn- listen [root kind repeat? topics listener]
  (->> (-> (or topics "") cs/trim (cs/split #"\s+"))
       (filter #(if (> (count %) 0) %))
       (mapv #(addTopic root
                        kind
                        (mkSubSCR % repeat? listener)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod delOneTopic :rbus [top kind {:keys [topic] :as sub}]
  (let [path (interleavePath (splitTopic topic))]
    (-> (update-in top path remOneSub sub)
        (update-in [:subcs] dissoc (:id sub)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod unSub
  :rbus
  [root kind sub]
  (swap! root delOneTopic kind sub))

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
(defn- dump "" [{:keys [subcs]} topic]
  (doseq [[_ z] subcs
          :let [{:keys [repeat? action status]} z]
          :when (pos? (aget ^longs status 0))]
    (action (:topic z) topic nil)
    ;;if one time only, turn off flag
    (if-not repeat?
      (aset ^longs status 0 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- walkTree "" [func branch pathTokens & args]
  (let [{:keys [nodes subcs]} branch
        [p & more] pathTokens
        cur (get nodes p)
        s1 (get nodes "*")
        s1c (:nodes s1)
        s2 (get nodes "**")]
    (println "walkteee " pathTokens)
    (println "cur " cur)
    (println "s1 " s1)
    (println "s2 " s2)
    (if s2
      (apply func s2 args))
    (if s1
      (cond
        (and (empty? more)
             (empty? s1c))
        (apply func s1 args)
        (and (not-empty s1c)
             (not-empty more))
        (apply walkTree func s1 more args)))
    (if cur
      (if (not-empty more)
        (apply walkTree func cur more args)
        (apply func cur args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- rbus<> "" []
  (let [state (atom (mkTreeNode))]
    (reify EventBus
      ;; subscribe once to 1+ topics, return a list of handles
      ;; topics => "/hello/**  /goodbye/*/xyz"
      (ev-sub* [_ topics listener]
        (listen state :rbus false topics listener))
      (ev-sub+ [_ topics listener]
        (listen state :rbus true topics listener))
      (ev-pub [_ topic msg]
        (c/let->nil
          [tokens (splitTopic topic)]
          (if (not-empty tokens)
            (walkTree run @state tokens topic msg))))
      (ev-resume [_ hd]
        (c/let->nil
          [sub (get (:subcs @state) hd)
           st (if sub (:status sub))
           sv (if st (aget ^longs st 0) -1)]
          (if (= 0 sv) (aset ^longs st 0 1))))
      (ev-pause [_ hd]
        (c/let->nil
          [sub (get (:subcs @state) hd)
           st (if sub (:status sub))
           sv (if st (aget ^longs st 0) -1)]
          (if (pos? sv) (aset ^longs st 0 0))))
      (ev-unsub [_ hd]
        (c/do->nil
          (some->> (get (:subcs @state) hd)
                   (unSub state :rbus))))
      (ev-match? [_ topic]
        (c/let->nil
          [tokens (splitTopic topic)]
          (if (not-empty tokens)
            (walkTree dump @state tokens topic))))
      (ev-dbg [_] (f/writeEdnStr @state))
      (ev-removeAll [_]
        (c/do->nil (reset! state (mkTreeNode)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod addOneTopic :ebus [top
                              kind
                              {:keys [topic] :as sub}]
  (-> (update-in top
                 [:nodes topic]
                 assoc (:id sub) sub)
      (update-in [:subcs] assoc (:id sub) sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod addTopic :ebus [root kind sub]
  (swap! root addOneTopic kind sub) (:id sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod delOneTopic :ebus [top kind {:keys [topic] :as sub}]
  (-> (update-in top [:nodes topic] dissoc (:id sub))
      (update-in [:subcs] dissoc (:id sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod unSub
  :ebus
  [root kind sub]
  (swap! root delOneTopic kind sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ebus<> "" []
  (let [state (atom (mkTreeNode))]
    (reify EventBus
      ;; subscribe once to 1+ topics, return a list of handles
      ;; topics => "/hello/**  /goodbye/*/xyz"
      (ev-sub* [_ topics listener]
        (listen state :ebus false topics listener))
      (ev-sub+ [_ topics listener]
        (listen state :ebus true topics listener))
      (ev-pub [_ topic msg]
        (c/let->nil
          [sub (get (:nodes @state) topic)]
          (if sub (run sub topic msg))))
      (ev-resume [_ hd]
        (c/let->nil
          [sub (get (:subcs @state) hd)
           st (if sub (:status sub))
           sv (if st (aget ^longs st 0) -1)]
          (if (= 0 sv) (aset ^longs st 0 1))))
      (ev-pause [_ hd]
        (c/let->nil
          [sub (get (:subcs @state) hd)
           st (if sub (:status sub))
           sv (if st (aget ^longs st 0) -1)]
          (if (pos? sv) (aset ^longs st 0 0))))
      (ev-unsub [_ hd]
        (c/do->nil
          (some->> (get (:subcs @state) hd)
                   (unSub state :ebus))))
      (ev-match? [_ topic]
        (contains? (:nodes @state) topic))
      (ev-dbg [_] (f/writeEdnStr @state))
      (ev-removeAll [_]
        (c/do->nil (reset! state (mkTreeNode)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eventBus<> ""
  ([] (eventBus<> false))
  ([subjectBased?]
   (if subjectBased? (rbus<>) (ebus<>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


