;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "A pub-sub event bus."
      :author "Kenneth Leung"}

  czlab.basal.evbus

  (:require [czlab.basal.xpis :as po]
            [czlab.basal.util :as u]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [clojure.core.async
             :as ca
             :refer [>!
                     <!
                     go-loop
                     go
                     chan
                     close!
                     sliding-buffer]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-async
  "Setup async go channel."
  ([action t]
   (mk-async action t nil))
  ([action t bufsz]
   (c/do-with [in (chan (sliding-buffer
                          (c/num?? bufsz 16)))]
     (go-loop [res (<! in)]
       (when-some [{:keys [topic msg]} res]
         (if (= t topic)
           (action t t msg)) (recur (<! in)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-sub
  "Create a subscriber object."
  [impl topic cb]
  (let [{:keys [async? bufsz]} @impl]
    {:id (c/x->kw (u/jid<>) "-" (u/seqint2))
     :topic topic
     :action (if async?
               (mk-async cb topic bufsz) cb)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run [async? subcs topic msg]
  (let [data {:topic topic :msg msg}]
    (if async?
      (go (doseq [[_ z] subcs
                  :let [{:keys [action]
                         expected :topic} z]]
            (>! action (assoc data :expected expected))))
      (doseq [[_ z] subcs
              :let [{:keys [action]
                     expected :topic} z]]
        (if (= expected topic) (action expected topic msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol EventBus
  "Topic based pub-sub event bus."
  (ev-pub [_ topic msg] "Publish a message on this topic.")
  (ev-match? [_ topic] "True if topic is registered.")
  (ev-dbg [_] "Internal.")
  (ev-unsub [_ subid] "Drop subscription.")
  (ev-sub [_ topic listener] "Subscribe to this topic."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<>
  "A Publish Subscribe event manager."
  ([]
   (event-bus<> nil))
  ([options]
   (let [impl (atom (merge {:subcs {}
                            :topics {} :async? false} options))]
     (reify EventBus
       (ev-sub [bus topic listener]
         (let [{:keys [id] :as sub}
               (mk-sub impl topic listener)]
           (swap! impl
                  (c/fn_1 (-> (update-in ____1
                                         [:topics topic] assoc id sub)
                              (update-in [:subcs] assoc id sub)))) id))
       (ev-pub [bus topic msg]
         (let [{:keys [async? topics]} @impl
               cbs (get topics topic)]
           (if-not (empty? cbs)
             (run async? cbs topic msg)) bus))
       (ev-unsub [bus subid]
         (let [sub ((:subcs @impl) subid)
               {:keys [action topic]} sub]
           (if sub
             (swap! impl
                    (c/fn_1 (if (:async? ____1) (close! action))
                            (-> (update-in ____1
                                           [:topics topic] dissoc subid)
                                (update-in [:subcs] dissoc subid))))) bus))
       (ev-match? [bus topic]
         (contains? (get @impl :topics) topic))
       (ev-dbg [bus] (i/fmt->edn @impl))
       po/Finzable
       (finz [bus]
         (let [{:keys [async? subcs]} @impl]
           (if async? ;maybe close all go channels
             (doseq [[_ z] subcs] (close! (:action z))))
           (swap! impl
                  assoc :topics {} :subcs {}) bus))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


