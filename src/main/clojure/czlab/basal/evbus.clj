;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "A simple pub-sub event bus."
      :author "Kenneth Leung"}

  czlab.basal.evbus

  (:require [czlab.basal.util :as u]
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
  ([action t] (mk-async action t nil))
  ([action t bufsz]
   (c/do-with [in (chan (sliding-buffer
                          (c/num?? bufsz 16)))]
     (go-loop [res (<! in)]
       (when res
         (if (= t (:topic res))
           (action t t (:msg res))) (recur (<! in)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-sub
  "Create a subscriber object."
  [bus topic cb]
  (let [{:keys [async? bufsz]} @bus]
    {:id (keyword (str "s-" (u/seqint2)))
     :topic topic
     :action (if async?
               (mk-async cb topic bufsz) cb)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run [async? subcs topic msg]
  (let [data {:topic topic :msg msg}]
    (if async?
      (go (doseq [[_ z] subcs
                  :let [{expected :topic
                         :keys [action]} z]]
            (>! action (assoc data :expected expected))))
      ;else
      (doseq [[_ z] subcs
              :let [{expected :topic
                     :keys [action]} z]]
        (if (= expected topic) (action expected topic msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-sub
  "Subscribe to a topic."
  [bus topic listener]
  (let [{:keys [id] :as sub}
        (mk-sub bus topic listener)]
    (swap! bus
           (c/fn_1 (-> (update-in ____1
                                  [:topics topic]
                                  assoc id sub)
                       (update-in [:subcs]
                                  assoc id sub)))) id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-pub
  "Publish to a topic."
  [bus topic msg]
  (let [{:keys [async? topics]} @bus
        cbs (get topics topic)]
    (if (not-empty cbs)
      (run async? cbs topic msg)) bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-unsub
  "Remove this subscription."
  [bus subid]
  (let [sub ((:subcs @bus) subid)
        {:keys [action topic]} sub]
    (if sub
      (swap! bus
             (c/fn_1 (if (:async? ____1) (close! action))
                     (-> (update-in ____1
                                    [:topics topic]
                                    dissoc subid)
                         (update-in [:subcs]
                                    dissoc subid))))) bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-match?
  "Topic is registered?" [bus topic]
  (contains? (get @bus :topics) topic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-dbg "" [bus] (i/fmt->edn @bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-dispose
  "Tear down this bus object."
  [bus]
  (let [{:keys [async? subcs]} @bus]
    ;maybe close all go channels
    (if async?
      (doseq [[_ z] subcs] (close! (:action z))))
    (swap! bus
           #(assoc % :topics {} :subcs {})) bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<>
  "A Publish Subscribe event manager."
  ([] (event-bus<> nil))
  ([options]
   (atom (merge {:topics {}
                 :subcs {} :async? false} options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


