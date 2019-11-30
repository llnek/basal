;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.basal.evbus

  "A pub-sub event bus."

  (:require [czlab.basal.io :as i]
            [czlab.basal.util :as u]
            [czlab.basal.core :as c]
            [czlab.basal.xpis :as po]
            [clojure.core.async :as a :refer [>! <!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-async

  "Setup async go channel."

  ([action t]
   (mk-async action t nil))

  ([action t bufsz]
   (c/do-with [in (a/chan (a/sliding-buffer
                            (c/num?? bufsz 16)))]
     (a/go-loop [res (<! in)]
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
(defn- run

  [async? subcs topic msg]

  (let [data {:topic topic :msg msg}]
    (if async?
      (a/go (doseq [[_ z] subcs
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
  (match? [_ topic] "True if topic is registered.")
  (dbg [_] "Internal.")
  (unsub [_ subid] "Drop subscription.")
  (sub [_ topic listener] "Subscribe to this topic.")
  (pub [_ topic msg] "Publish a message on this topic."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<>

  "A Publish Subscribe event manager."

  ([]
   (event-bus<> nil))

  ([options]
   (let [impl (atom (merge {:subcs {}
                            :topics {} :async? false} options))]
     (reify EventBus
       (sub [bus topic listener]
         (let [{:keys [id] :as sub}
               (mk-sub impl topic listener)]
           (swap! impl
                  (c/fn_1 (-> (update-in ____1
                                         [:topics topic] assoc id sub)
                              (update-in [:subcs] assoc id sub)))) id))
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
       po/Finzable
       (finz [bus]
         (let [{:keys [async? subcs]} @impl]
           (if async? ;maybe close all go channels
             (doseq [[_ z] subcs] (a/close! (:action z))))
           (swap! impl
                  assoc :topics {} :subcs {}) bus))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


