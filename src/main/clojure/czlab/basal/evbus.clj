;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.evbus

  "A pub-sub event bus."

  (:require [clojure.string :as cs]
            [czlab.basal.io :as i]
            [czlab.basal.util :as u]
            [czlab.basal.core :as c]
            [clojure.core.async :as a :refer [>! <!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol EventBus
  "Topic based pub-sub event bus."
  (match? [_ topic] "True if topic is registered.")
  (dbg [_] "Internal.")
  (unsub [_ subid] "Drop subscription.")
  (sub [_ topic listener] "Subscribe to this topic.")
  (pub [_ topic msg] "Publish a message on this topic."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<>
  "A Publish Subscribe event manager."
  ([] (event-bus<> nil))
  ([options]
   (letfn
     [(mk-async [action t bufsz]
        (c/do-with [in (a/chan (a/sliding-buffer
                                 (c/num?? bufsz 16)))]
          (a/go-loop [res (<! in)]
            (when-some [{:keys [topic msg]} res]
              (if (= t topic)
                (action t t msg)) (recur (<! in))))))
      (mk-sub [impl topic cb]
        (let [{:keys [async? bufsz]} @impl]
          {:id (c/x->kw (u/jid<>) "-" (u/seqint2))
           :topic topic
           :action (if-not async?
                     cb (mk-async cb topic bufsz))}))
      (run [async? subcs topic msg]
        (let [data {:topic topic :msg msg}]
          (if async?
            (a/go (doseq [[_ z] subcs
                          :let [{:keys [action]
                                 expected :topic} z]]
                    (>! action (assoc data :expected expected))))
            (doseq [[_ z] subcs
                    :let [{:keys [action]
                           expected :topic} z]]
              (if (= expected topic) (action expected topic msg))))))]
     (let [impl (atom (merge {:subcs {}
                              :topics {} :async? false} options))]
       (reify EventBus
         (sub [bus topic listener]
           (let [{:keys [id] :as sub}
                 (mk-sub impl topic listener)]
             (swap! impl
                    (c/fn_1 (-> (update-in ____1
                                           [:topics topic] assoc id sub)
                                (update-in [:subcs] assoc id sub)))) [id bus]))
         (pub [bus topic msg]
           (let [{:keys [async? topics]} @impl
                 cbs (get topics topic)]
             (if-not (empty? cbs)
               (run async? cbs topic msg)) bus))
         (unsub [bus subid]
           (let [sub ((:subcs @impl) subid)
                 {:keys [action topic]} sub]
             (if sub
               (swap! impl
                      (c/fn_1 (if (:async? ____1) (a/close! action))
                              (-> (update-in ____1
                                             [:topics topic] dissoc subid)
                                  (update-in [:subcs] dissoc subid))))) bus))
         (match? [bus topic]
           (contains? (get @impl :topics) topic))
         (dbg [bus] (i/fmt->edn @impl))
         c/Finzable
         (finz [bus]
           (let [{:keys [async? subcs]} @impl]
             (if async? ;maybe close all go channels
               (doseq [[_ z] subcs] (a/close! (:action z))))
             (swap! impl
                    assoc :topics {} :subcs {}) bus)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/def- re-space #"\s+")
(c/def- re-dot #"\.")
(c/def- re-slash #"/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<+>
  "A Publish Subscribe event manager whereby
  a more advanced matching scheme is used -
  such as wild-card matches."
  ([] (event-bus<+> nil))
  ([options]
   (letfn
     [(split [topic sep]
        (filterv #(if (> (count %) 0) %)
                 (cs/split topic
                           (if (.equals "." sep) re-dot re-slash))))
      (mk-async [cb t bufsz]
        (c/do-with [in (a/chan (a/sliding-buffer
                                 (c/num?? bufsz 16)))]
          (a/go-loop [r (<! in)]
            (when r
              (cb (:expected r) (:topic r) (:msg r)) (recur (<! in))))))
      (mk-sub [impl topic cb]
        (let [{:keys [bufsz async?]} @impl]
          {:id (keyword (c/x->kw (u/jid<>) "-" (u/seqint2)))
           :topic topic
           :action (if async? (mk-async cb topic bufsz) cb)}))
      ;"So that tokens are lined up in nested levels.
      ;e.g. tokens = [a b c] result = [:levels a :levels b :levels c]."
      (fmt-path [tokens]
        (c/vec-> (interleave (repeat :levels) tokens)))
      (run [async? subcs topic msg]
        (let [data {:topic topic :msg msg}]
          (if async?
            (a/go (doseq [[_ z] subcs
                          :let [{expected :topic
                                 :keys [action]} z]]
                    (>! action (assoc data :expected expected))))
            (doseq [[_ z] subcs
                    :let [{expected :topic
                           :keys [action]} z]]
              (action expected topic msg)))))
      (walk [async? branch pathTokens topic msg tst]
        (c/let#nil
          [{:keys [levels subcs]} branch
           [p & more] pathTokens
           cur (levels p)
           s1 (levels "*")
           s1c (:levels s1)
           s2 (levels "**")]
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
                (run async? (:subcs cur) topic msg))))))]
     (let [impl (atom (merge {:delimiter "."
                            :async? false
                            :bufsz 16
                            :levels {} :subcs {}} options))]
       (reify EventBus
         (sub [bus topic listener]
           (let [{:keys [async? delimiter]} @impl
                 {:keys [id] :as sub}
                 (mk-sub impl topic listener)
                 path (fmt-path (split topic delimiter))]
             (swap! impl
                    (c/fn_1
                      (-> (update-in ____1
                                     path
                                     #(update-in %
                                                 [:subcs]
                                                 assoc id sub))
                          (update-in [:subcs] assoc id sub)))) [id bus]))
         (pub [bus topic msg]
           (let [{:keys [async?
                         delimiter] :as B} @impl]
             (c/if-some+
               [ts (split topic delimiter)]
               (walk async? B ts topic msg nil)) bus))
         (unsub [bus subid]
           (if-some [sub ((:subcs @impl) subid)]
             (let [{:keys [async? delimiter]} @impl
                   {:keys [action topic]} sub
                   path (fmt-path (split topic delimiter))]
               (swap! impl
                      (c/fn_1
                        (if async? (a/close! action))
                        (-> (update-in ____1
                                       path
                                       #(update-in %
                                                   [:subcs]
                                                   dissoc subid))
                            (update-in [:subcs]
                                       dissoc subid)))))) bus)
         (match? [_ topic]
           (let [z (atom 0)
                 {:keys [async?
                         delimiter] :as B} @impl]
             (c/if-some+
               [ts (split topic delimiter)]
               (walk async? B ts topic nil z))
             (pos? @z)))
         (dbg [_] (i/fmt->edn @impl))
         c/Finzable
         (finz [me]
           (let [{:keys [async? subcs]} @impl]
             (if async? ;maybe close all go channels
               (doseq [[_ z] subcs] (a/close! (:action z))))
             (swap! impl
                    #(assoc % :levels {} :subcs {})) me)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

