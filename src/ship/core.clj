(ns ship.core
  (:require
   [ship.humans :as humans]
   [ship.ship :as ship]
   [ship.items :as items]
   [ship.data :as data]
   [ship.mind :as mind]
   [clojure.string :as string]
   )
  (:refer-clojure :exclude [==])
  (:use
     [clojure.pprint :only [pprint]]
     [ship.mind :only [ponder read-book mind_init]]
     [ship.util]
     [ship.types]
     [clojure.set :only [map-invert union difference intersection join subset? superset?]]))


;TODO

;place objects
;human logic, should decide goals and do things
;random events - wear and tear on machinery, accidents, disasters, person to person drama





;STRING FORMATTING

(defn get-name [uid] (get-thing uid :name))

(defn format-nouns [coll]
  (let [tail (cond (= (count coll) 1) ""
                   (= (count coll) 2) " and "
                   :else " and ")]
  (apply str (concat  (string/join ", " (pop coll)) [ tail (last coll)] ))))

(defn pluralize [coll a b] (cond (= (count coll) 1) a :else b))





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




;UPDATE HOOKS

(defn step [h]
  (let [me (get-thing h)
        r (locate h)
        room (get-thing r)
        others (vec (disj (humans-in-room r) h))
        met (have-met h)]
     (dorun
       (map (fn [a] (record-event {:met #{a h} h r}))  others)



    )))

(defn update []
  (dorun (map step (keys @(:h DATA))))
  (report (- (count @history) @history-size))
  (swap! history-size (fn[n] (count @history))))






;INITS

(clear-data)
(data/make-rooms data/rooms)
(data/finalize-rooms)
(items/init)
(repeatedly 5 #(humans/make-human))
(map mind_init (keys @(:h DATA)))
;place our crew in the ship
(map (fn [h] (place h (rand-nth (keys @(:r DATA))))) (vals @(:h DATA)))



;TESTS

@history
 (map :name (map get-thing (map :read (where [:read] @history))))
(where [#{:met}] @history)





(def book (get-thing-where :x :flags #{:readable :basic}))

(:data (first book))


*ns*

(ponder :who :h1  :conds [['concept :q]])

(ponder :who :h1 :fresh '[k v]
                  :conds [['action-req :?k :?v]
                         ['== :q '[k v]]])

(ponder :who :h1 :fresh '[k v] :conds [['property-of :?k :?v] ['== :q '[k v]]])

(in-ns 'ship.core)

(get-thing :h1 :state)





(ponder :who :h1  :conds [['node :q]])














