;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "A publish subscribe library using subject based topics."
      :author "Kenneth Leung"}

  czlab.basal.rvbus

  (:require [czlab.basal.core :as c]
            [czlab.basal.util :as u]
            [clojure.string :as cs]
            [czlab.basal.io :as i]
            [czlab.basal.str :as s]
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
(def ^:private re-space #"\s+")
(def ^:private re-dot #"\.")
(def ^:private re-slash #"/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- split "Split up topic into parts." [topic sep]
  (filterv #(if (> (count %) 0) %)
           (cs/split topic
                     (if (= sep ".") re-dot re-slash))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-async
  ([cb t] (mk-async cb t nil))
  ([cb t bufsz]
   (c/do-with [in (chan (sliding-buffer
                          (c/num?? bufsz 16)))]
     (go-loop [r (<! in)]
       (when r
         (cb (:expected r) (:topic r) (:msg r)) (recur (<! in)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-sub
  [impl topic cb]
  (let [{:keys [bufsz async?]} @impl]
    {:id (keyword (s/x->kw (u/jid<>) "-" (u/seqint2)))
     :topic topic
     :action (if async?
               (mk-async cb topic bufsz) cb)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- fmt-path
  "So that tokens are lined up in nested levels.
  e.g. tokens = [a b c]
       result = [:levels a :levels b :levels c]."
  [tokens]
  (c/vec-> (interleave (repeat :levels) tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run
  "Dispatch message to subscribers."
  [async? subcs topic msg]
  (let [data {:topic topic :msg msg}]
    (if async?
      (go (doseq [[_ z] subcs
                  :let [{expected :topic
                         :keys [action]} z]]
            (>! action (assoc data :expected expected))))
      (doseq [[_ z] subcs
              :let [{expected :topic
                     :keys [action]} z]]
        (action expected topic msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- walk [async? branch pathTokens topic msg tst]
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
          (run async? (:subcs cur) topic msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol EventBus ""
  (ev-pub [_ topic msg] "")
  (ev-match? [_ topic] "")
  (ev-dbg [_] "")
  (ev-finz [_] "")
  (ev-unsub [_ subid] "")
  (ev-sub [_ topic listener] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<>
  "A Publish Subscribe event manager whereby
  a more advanced matching scheme is used -
  such as wild-card matches."
  ([] (event-bus<> nil))
  ([options]
   (let [impl (atom (merge {:delimiter "."
                            :async? false
                            :bufsz 16
                            :levels {} :subcs {}} options))]
     (reify EventBus
       (ev-sub [_ topic listener]
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
                        (update-in [:subcs] assoc id sub)))) id))
       (ev-pub [bus topic msg]
         (let [{:keys [async?
                       delimiter] :as B} @impl]
           (c/if-some+
             [ts (split topic delimiter)]
             (walk async? B ts topic msg nil)) bus))
       (ev-unsub [bus subid]
         (if-some [sub ((:subcs @impl) subid)]
           (let [{:keys [async? delimiter]} @impl
                 {:keys [action topic]} sub
                 path (fmt-path (split topic delimiter))]
             (swap! impl
                    (c/fn_1
                      (if async? (close! action))
                      (-> (update-in ____1
                                     path
                                     #(update-in %
                                                 [:subcs]
                                                 dissoc subid))
                          (update-in [:subcs]
                                     dissoc subid)))))) bus)
       (ev-match? [_ topic]
         (let [z (atom 0)
               {:keys [async?
                       delimiter] :as B} @impl]
           (c/if-some+
             [ts (split topic delimiter)]
             (walk async? B ts topic nil z))
           (pos? @z)))
       (ev-dbg [_] (i/fmt->edn @impl))
       (ev-finz [_]
         (c/let#nil [{:keys [async? subcs]} @impl]
           (if async? ;maybe close all go channels
             (doseq [[_ z] subcs] (close! (:action z))))
           (swap! impl
                  #(assoc % :levels {} :subcs {}))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


