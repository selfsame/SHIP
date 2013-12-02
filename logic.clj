(ns ship.logic
  (:refer-clojure :exclude [==])
  (:require
   [ship.core :as core]
   [ship.humans :as humans]
   [ship.ship :as ship]
   [ship.items :as items]
   [ship.data :as data]
   [clojure.string :as string]

   )
  (:use
        [clojure.core.logic]

        [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing record-thing clear-data time-stamp]]
        [clojure.set :only [map-invert union difference intersection join subset? superset?]]))




(defrel tool x)
(defrel affect x y)
(defrel effect x y)
(defrel tool-weight t n)
(defrel materials x)
(defrel material-type x y)

(facts materials [[:oak]
                  [:pine]
                  [:birch]
                  [:aluminum]
                  [:copper]
                  [:bronze]
                  [:iron]
                  [:steel]
                  [:plastic]
                  [:glass]])

(facts material-type
                 [[:wood :oak]
                  [:wood :pine]
                  [:wood :birch]
                  [:metal :aluminum]
                  [:metal :iron]
                  [:metal :steel]
                  [:metal :bronze]
                  [:metal :copper]])


(def tool-types (atom {}))

(defrecord Tool [name weight affects effect])




(defn record-tool [name weight a e]
  (let [ t (new Tool name weight a e)]
    (do
      (swap! tool-types conj {(:name t) t})
      (fact tool t)
      (fact effect t e)
      (doall (map (fn [j] (fact affect t j) ) (:affects t)))

  )))

(def g (new Tool :drillpress 450 [:wood :metal :plastic] :hole))

g

(:affects g)
(:effect g)
(fact tool g)
(fact effect g (:effect g))
(map (fn [a] (fact affect g a)) [:wood :metal :plastic])



(record-tool :drillpress 450 [:wood :metal :plastic] :hole)
(record-tool :tablesaw 230 [:wood :plastic] :cut)
(record-tool :handsaw 10 [:wood :plastic] :cut)
(record-tool :bandsaw 500 [:wood :plastic :metal] :cut)

tool-types






(run* [q]
      (tool q)
      (effect q :cut))

(defn tools-that
  ([eff]
    (run* [q]
      (tool q)
      (effect q eff)))
  ([eff aff]
    (run* [q]
      (tool q)
      (effect q eff)
      (affect q aff))))

(map :name (tools-that :cut :wood))


