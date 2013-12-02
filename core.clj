(ns ship.core
  (:require
   [ship.humans :as humans]
   [ship.ship :as ship]
   [ship.items :as items]
   [ship.data :as data]
   [ship.logic :as logic]
   [clojure.string :as string]
   )
  (:refer-clojure :exclude [==])
  (:use [clojure.pprint :only [pprint]]
        [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing record-thing clear-data time-stamp]]
        [clojure.set :only [map-invert union difference intersection join subset? superset?]]))


;TODO

;place objects
;human logic, should decide goals and do things
;random events - wear and tear on machinery, accidents, disasters, person to person drama



(def history (atom []))

(def history-size (atom 0))

(defn record-event [e]
  (swap! history conj (conj e {:time (time-stamp)})))


;STRING FORMATTING

(defn get-name [uid] (get-thing uid :name))

(defn format-nouns [coll]
  (let [tail (cond (= (count coll) 1) ""
                   (= (count coll) 2) " and "
                   :else " and ")]
  (apply str (concat  (string/join ", " (pop coll)) [ tail (last coll)] ))))

(defn pluralize [coll a b] (cond (= (count coll) 1) a :else b))



;GAME STATE REPORTING
(defn movement-report [new]
    (let [data (apply merge-with union (map (fn[a] {(:place a) #{(:UID a)}})(where [:place] new))) ]
    (map (fn[a]
           (let [people (mapv get-name (val a))]
           (str (format-nouns people) (pluralize people " goes " " go ")  "to the "
           (get-thing (key a) :name) ""))) data)

  ))

(defn meet-report [new]
  (let [data (set (map :met (where [:met] new)))]
    (apply str (mapcat (fn[a] [(get-name (last a)) " and " (get-name (first a)) " meet. "]) data))
    ))

(defn report [n]
  (let [new (subvec @history @history-size)
        moved (movement-report new)
        meets (meet-report new)]

    (pprint moved)
    (pprint meets)
    )
  )



;COMPREHENSION OF THE HISTORY STACK

(defn locate [uid]
  (:place (first (filter (fn [a]
     (and
      (:place a)
      (= (:UID a) uid))) (reverse @history)))))

;allows stacking of filter functions
(defn where [filters coll]
  (filter (apply every-pred filters) coll))

(defn humans-in-room [r]
  (set (map :UID (filter (fn [a] (= (:place a) r) ) (filter :place (reverse @history))))) )

(defn have-met [h]
  (set (map (fn [a] (first (disj (:met a) h))) (where [:met h] @history))))



;UPDATE HOOKS

(defn update []
  (dorun (map step (keys @(:h DATA))))
  (report (- (count @history) @history-size))
  (swap! history-size (fn[n] (count @history))))


(defn step [h]
  (let [me (get-thing h)
        r (locate h)
        room (get-thing r)
        others (vec (disj (humans-in-room r) h))
        met (have-met h)]
     (dorun
       (map (fn [a] (record-event {:met #{a h} h r}))  others)



    )))



;INITS

(clear-data)
(data/make-rooms data/rooms)
(data/finalize-rooms)
(data/init)

;place our crew in the ship
(map (fn [h] (ship/place h (rand-nth (keys @(:r DATA))))) (vals @(:h DATA)))



@history


(update)

(where [:met] @history)

(where [#{:met}] @history)












