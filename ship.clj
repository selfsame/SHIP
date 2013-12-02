(ns ship.ship
  (:require

   [ship.core :as core]
   [ship.items :as items])
  (:use [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing record-thing]]))


(defn modify [thing k v]
  (swap! (:state thing) assoc-in [k] v ))



(defn make-room [name desc]
  (let [ my-uid (get-UID :r)
         room {
          :UID my-uid
          :description desc
          :contents {:humans {} :items {}}
          :exits {}
          :state (atom {:oxygen: 80})
          :name name } ]
         (record-thing room)
          my-uid))

(defn link-rooms [name r1 r2]
  (let [ room1 (get-thing r1)
         room2 (get-thing r2)]
    (swap! (:r DATA) assoc-in [r1 :exits r2] name )
    (swap! (:r DATA) assoc-in [r2 :exits r1] name )
        ))

(defn place [thing room]
  ;(cond
  ; (= (:role thing) :human) (swap! (:r DATA) assoc-in [room :contents :humans (:UID thing)]  :true)
  ; (= (:role thing) :item) (swap! (:r DATA) assoc-in [room :contents :items (:UID thing)]  :true))
  (core/record-event {:role (:role thing) :UID (:UID thing) :place room (:UID thing) 1})
  )

(defn unplace [thing room]
  ;(cond
  ; (= (:role thing) :human) (swap! (:r DATA) dissoc-in [room :contents :humans] (:UID thing) )
  ; (= (:role thing) :item) (swap! (:r DATA) dissoc-in [room :contents :items] (:UID thing) ))
  (core/record-event {:role (:role thing) :UID (:UID thing) :unplace room (:UID thing) 1})
  )



(defn look-at [thing]
  (str (:name thing) ". "))

(defn look [uid]
  (let [room (get-thing uid)]
    (str
     (:name room) ":                                                                           "
     (:description room) "                                                                         "
     "Exits: " (apply str (vals (:exits room)))  "                                                                         "
     "You see " (apply str (map :name (map get-thing  (keys(:items (:contents room))) ) )) "                                 "
     "You see "  (apply str (map :name (map get-thing  (keys(:humans (:contents room))) ) ))


            )
    )
  )


(defn get-exits [r]
  (cond
   (= (type :a) clojure.lang.Keyword) (keys (get-thing r :exits))
   :else []))


(defn nth-last [col n]
  (cond (>= (- (count col) (+ n 1)) 0) (nth col (- (count col) (+ n 1)))
        :else :false))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))


; given a vector of nodes, find last node connections and return a set of possible extensions
(defn search [path]
  (cond
    (= (type (last path)) clojure.lang.Keyword)
  (let [ options (get-exits (last path))]
    (mapv (fn [a]
           (cond
            (in? path a) path
            :else (conj path a))) options))
   :else []))


(type :a)

(defn find-path [r1 r2]
  (let [open (remove (fn [a] (= a r1)) (keys @(:r DATA)))
        results (loop [results #{[r1]}  count 0]
          (cond
           (= count 40) results
            :else (recur
                (set (mapcat (fn [a] (search a) ) results))
              (inc count)
            )))]

      (filter #(= (last %) r2) results)
    ))


(get-exits :r1)

(search [:r3])

(find-path :r1 :r12)













