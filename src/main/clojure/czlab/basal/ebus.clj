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
            [clojure.core.async
             :as ca
             :refer [>!
                     <!
                     go-loop
                     go
                     chan
                     close!
                     sliding-buffer]]
            [czlab.basal.format :as f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:dynamic *sba-delim* "/")
(def ^:dynamic *chan-bufsz* 16)

(def ^:private _SEED (atom 1))
(defn- XnextSEQ "" []
  (let [n @_SEED] (swap! _SEED inc) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nextSEQ "" [] (c/seqint2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private re-space #"\s+")
(def ^:private re-slash #"/")
(def ^:private re-dot #"\.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- splitTopic "" [topic]
  (if (string? topic)
    (->> (cs/split topic (if (= *sba-delim* ".") re-dot re-slash))
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
(defn- asyncAction "" [action wanted]
  (let [in (chan (sliding-buffer (or *chan-bufsz* 16)))]
    (go-loop [data (<! in)]
             (when data
               (action wanted {:topic data} {:msg data})
               (recur (<! in))))
    in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkSubSCR
  ""
  [topic repeat? listener async?]
  {:pre [(fn? listener)]}

  (-> (if (true? async?)
        {:action (asyncAction listener topic)}
        {:action listener})
      (merge
        {:id (keyword (str "s#" (nextSEQ)))
         :async? (boolean async?)
         :repeat? repeat?
         :topic topic
         :status (long-array 1 1)})))

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
(defn- finzAsync "" [subcs]
  (doseq [[_ z] subcs
          :let [{:keys [async? action]} z]
          :when async?]
    (close! action)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- addOneSub "" [node sub]
  (update-in node [:subcs] assoc (:id sub) sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- remOneSub "" [node sub]
  (if (:async? sub) (close! (:action sub)))
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
(defn- listen [root kind repeat? topics listener async?]
  (let [r
        (->> (-> (or topics "") cs/trim (cs/split re-space))
             (filter #(if (> (count %) 0) %))
             (mapv #(addTopic root
                              kind
                              (mkSubSCR % repeat? listener async?))))]
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
(defn- run "" [async? subcs topic msg]
  (let [data {:topic topic :msg msg}]
    (if (true? async?)
      (go
        (doseq [[_ z] subcs
                :let [{:keys [repeat? action ^longs status]} z]
                :when (pos? (aget status 0))]
          (>! action data)
          (if-not repeat? (aset status 0 -1))))
      (doseq [[_ z] subcs
              :let [{:keys [repeat? action status]} z]
              :when (pos? (aget ^longs status 0))]
        (action (:topic z) topic msg)
        (if-not repeat? (aset ^longs status 0 -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- walk "" [async? branch pathTokens topic msg tst]
  (let [{:keys [levels subcs]} branch
        [p & more] pathTokens
        cur (get levels p)
        s1 (get levels "*")
        s1c (:levels s1)
        s2 (get levels "**")]
    (when s2
      (if tst
        (swap! tst inc)
        (run async? (:subcs s2) topic msg)))
    (if s1
      (cond
        (and (empty? more)
             (empty? s1c))
        (if tst
          (swap! tst inc)
          (run async? (:subcs s1) topic msg))
        (and (not-empty s1c)
             (not-empty more))
        (walk async? s1 more topic msg tst)))
    (when cur
      (if (not-empty more)
        (walk async? cur more topic msg tst)
        (if tst
          (swap! tst inc)
          (run async? (:subcs cur) topic msg))))))

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
(defn- rbus<> "" [{:keys [bufsz
                          subjectDelimiter] :as options} async?]
  (let [state (atom (mkLevelNode))]
    (reify EventBus
      (ev-sub* [_ topics listener]
        (binding [*sba-delim* subjectDelimiter
                  *chan-bufsz* bufsz]
          (listen state :rbus false topics listener async?)))
      (ev-sub+ [_ topics listener]
        (binding [*sba-delim* subjectDelimiter
                  *chan-bufsz* bufsz]
          (listen state :rbus true topics listener async?)))
      (ev-pub [_ topic msg]
        (binding [*sba-delim* subjectDelimiter
                  *chan-bufsz* bufsz]
          (c/let->nil
            [tokens (splitTopic topic)]
            (if (not-empty tokens)
              (walk async? @state tokens topic msg nil)))))
      (ev-resume [_ hd]
        (resume @state hd))
      (ev-pause [_ hd]
        (pause @state hd))
      (ev-unsub [_ hd]
        (c/do->nil
          (some->> (get (:subcs @state) hd)
                   (unSub state :rbus))))
      (ev-match? [_ topic]
        (binding [*sba-delim* subjectDelimiter
                  *chan-bufsz* bufsz]
          (let [tokens (splitTopic topic)
                z (atom 0)]
            (if (not-empty tokens)
              (walk async? @state tokens topic nil z))
            (pos? @z))))
      (ev-dbg [_] (f/writeEdnStr @state))
      (ev-removeAll [_]
        (c/do->nil
          (finzAsync (:subcs @state))
          (reset! state (mkLevelNode)))))))

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
  (if (:async? sub) (close! (:action sub)))
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
(defn- ebus<> "" [async?]
  (let [state (atom (mkTreeNode))]
    (reify EventBus
      (ev-sub* [_ topics listener]
        (listen state :ebus false topics listener async?))
      (ev-sub+ [_ topics listener]
        (listen state :ebus true topics listener async?))
      (ev-pub [_ topic msg]
        (c/let->nil
          [sub (get (:topics @state) topic)]
          (if sub (run async? sub topic msg ))))
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
        (c/do->nil
          (finzAsync (:subcs @state))
          (reset! state (mkTreeNode)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn eventBus<>
  "A Publish Subscribe event manager.  If subject based is
  used, a more advanced matching scheme will be used - such as
  wild-card matches."
  ([] (eventBus<> nil))
  ([{:keys [subjectBased?] :as options}]
   (if subjectBased? (rbus<> options false) (ebus<> false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn goBus<>
  "A Publish Subscribe async event manager.  If subject based is
  used, a more advanced matching scheme will be used - such as
  wild-card matches."
  ([] (goBus<> nil))
  ([{:keys [subjectBased?] :as options}]
   (if subjectBased? (rbus<> options true) (ebus<> true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


