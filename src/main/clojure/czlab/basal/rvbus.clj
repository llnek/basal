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
  [bus topic cb]
  (let [{:keys [bufsz async?]} @bus]
    {:id (keyword (str "s-" (u/seqint2)))
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
      ;else
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
(defn ev-sub
  "Subscribe to topic."
  [bus topic listener]
  (let [{:keys [async? delimiter]} @bus
        {:keys [id] :as sub}
        (mk-sub bus topic listener)
        path (fmt-path (split topic delimiter))]
    (swap! bus
           (c/fn_1
             (-> (update-in ____1
                            path
                            #(update-in %
                                        [:subcs]
                                        assoc id sub))
                 (update-in [:subcs] assoc id sub)))) id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-pub
  "Publish msg to topic."
  [bus topic msg]
  (let [{:keys [async?
                delimiter] :as B} @bus]
    (c/if-some+
      [ts (split topic delimiter)]
      (walk async? B ts topic msg nil)) bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-unsub
  "Cancel subscription." [bus subid]
  (if-some [sub ((:subcs @bus) subid)]
    (let [{:keys [async? delimiter]} @bus
          {:keys [action topic]} sub
          path (fmt-path (split topic delimiter))]
      (swap! bus
             (c/fn_1
               (if async? (close! action))
               (-> (update-in ____1
                              path
                              #(update-in %
                                          [:subcs]
                                          dissoc subid))
                   (update-in [:subcs]
                              dissoc subid))))))
  bus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-match?
  "Is topic registered?"
  [bus topic]
  (let [{:keys [async?
                delimiter] :as B} @bus
        z (atom 0)]
    (c/if-some+
      [ts (split topic delimiter)]
      (walk async? B ts topic nil z))
    (pos? @z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-dbg "" [bus] (i/fmt->edn @bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ev-dispose
  "Tear down the bus." [bus]
  (let [{:keys [async? subcs]} @bus]
    ;maybe close all go channels
    (if async?
      (doseq [[_ z] subcs] (close! (:action z))))
    (swap! bus
           #(assoc %
                   :levels {} :subcs {})) bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn event-bus<>
  "A Publish Subscribe event manager whereby
  a more advanced matching scheme is used -
  such as wild-card matches."
  ([] (event-bus<> nil))
  ([options]
   (atom (merge {:delimiter "."
                 :async? false
                 :bufsz 16
                 :levels {} :subcs {}} options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


