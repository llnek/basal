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
(defn- XnextSEQ "" []
  (let [n @_SEED] (swap! _SEED inc) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nextSEQ "" [] (c/seqint2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- splitTopic "" [topic]
  (if (string? topic)
    (->> (cs/split topic #"/")
         (filter #(if (> (count %) 0) %))) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol EventBus
  "Event Pub-Sub interface"
  (ev-sub* [_ topics listener] "one time only subscription")
  (ev-sub+ [_ topics listener] "standard subscription")
  (ev-pub [_ topic msg] "send a message")
  (ev-match? [_ topic] "internal: test only")
  (ev-dbg [_] "internal: test only")
  (ev-resume [_ handle] "resume this subscriber")
  (ev-pause [_ handle] "pause this subscriber")
  (ev-unsub [_ handle] "remove this subscriber")
  (ev-removeAll [_] "remove all subscribers"))

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
(defn- mkLevelNode "" [] {:levels {} :subcs {}})
(defn- mkTreeNode "" [] {:topics {} :subcs {}})

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
  (into [] (mapcat #(doto [:levels %]) path)))

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
  (let [r
        (->> (-> (or topics "") cs/trim (cs/split #"\s+"))
             (filter #(if (> (count %) 0) %))
             (mapv #(addTopic root
                              kind
                              (mkSubSCR % repeat? listener))))]
    (if (= 1 (count r)) (first r) r)))

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
(defn- run "" [subcs topic msg]
  (doseq [[_ z] subcs
          :let [{:keys [repeat? action status]} z]
          :when (pos? (aget ^longs status 0))]
    (action (:topic z) topic msg)
    (if-not repeat? (aset ^longs status 0 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- runLevel "" [{:keys [subcs]} topic msg]
  (let [a (try (longs msg) (catch Throwable _ nil))]
    (if a
      (aset ^longs a 0 1)
      (run subcs topic msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- walk "" [branch pathTokens topic msg]
  (let [{:keys [levels subcs]} branch
        [p & more] pathTokens
        cur (get levels p)
        s1 (get levels "*")
        s1c (:levels s1)
        s2 (get levels "**")]
    (if s2
      (runLevel s2 topic msg))
    (if s1
      (cond
        (and (empty? more)
             (empty? s1c))
        (runLevel s1 topic msg)
        (and (not-empty s1c)
             (not-empty more))
        (walk s1 more topic msg)))
    (if cur
      (if (not-empty more)
        (walk cur more topic msg)
        (runLevel cur topic msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resume "" [root hd]
  (c/let->nil
    [sub (get (:subcs root) hd)
     st (if sub (:status sub))
     sv (if st (aget ^longs st 0) -1)]
    (if (= 0 sv) (aset ^longs st 0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pause "" [root hd]
  (c/let->nil
    [sub (get (:subcs root) hd)
     st (if sub (:status sub))
     sv (if st (aget ^longs st 0) -1)]
    (if (pos? sv) (aset ^longs st 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- rbus<> "" []
  (let [state (atom (mkLevelNode))]
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
            (walk @state tokens topic msg))))
      (ev-resume [_ hd]
        (resume @state hd))
      (ev-pause [_ hd]
        (pause @state hd))
      (ev-unsub [_ hd]
        (c/do->nil
          (some->> (get (:subcs @state) hd)
                   (unSub state :rbus))))
      (ev-match? [_ topic]
        (let
          [tokens (splitTopic topic)
           z (long-array 1 0)]
          (if (not-empty tokens)
            (walk @state tokens topic z))
          (pos? (aget ^longs z 0))))
      (ev-dbg [_] (f/writeEdnStr @state))
      (ev-removeAll [_]
        (c/do->nil (reset! state (mkLevelNode)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod addOneTopic :ebus [top
                              kind
                              {:keys [topic] :as sub}]
  (-> (update-in top
                 [:topics topic]
                 assoc (:id sub) sub)
      (update-in [:subcs] assoc (:id sub) sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod addTopic :ebus [root kind sub]
  (swap! root addOneTopic kind sub) (:id sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod delOneTopic :ebus [top kind {:keys [topic] :as sub}]
  (-> (update-in top [:topics topic] dissoc (:id sub))
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
          [sub (get (:topics @state) topic)]
          (if sub (run sub topic msg))))
      (ev-resume [_ hd]
        (resume @state hd))
      (ev-pause [_ hd]
        (pause @state hd))
      (ev-unsub [_ hd]
        (c/do->nil
          (some->> (get (:subcs @state) hd)
                   (unSub state :ebus))))
      (ev-match? [_ topic]
        (contains? (:topics @state) topic))
      (ev-dbg [_] (f/writeEdnStr @state))
      (ev-removeAll [_]
        (c/do->nil (reset! state (mkTreeNode)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eventBus<>
  "A Publish Subscribe event manager.  If subject based is
  used, a more advanced matching scheme will be used - such as
  wild-card matches."
  ([] (eventBus<> false))
  ([subjectBased?]
   (if subjectBased? (rbus<>) (ebus<>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


