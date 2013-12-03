(ns ship.data
  (:use
   [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing record-thing]]))



(def rooms [

  {:tag :comcap
   :name "The command capsule."
   :description "A mess of displays, controls, and wiring, with two chairs set in the middle.
     Several large glass windows look out into space, they must be incredibly thick."
   :exits [:hall_01]}

  {:tag :hall_01
   :name "Forward corridor."
   :description "A wide hallway connecting the command capsule to the rest of the ship."
   :exits [:comcap :hall_02]}

  {:tag :hall_02
   :name "Forward corridor."
   :description "This wide hallway leads to the command capsule. It's the only area in the ship that is carpeted."
   :exits [:hall_01 :uhub]}

  {:tag :uhub
   :name "Upper hub."
   :description "A round connection between two large halls."
   :exits [:hall_02 :hall_03 :hall_04]}

  {:tag :hall_03
   :name "Port corridor."
   :description "This wide hallway leads to the port side compartments."
   :exits [:uhub :cargo-bay]}

  {:tag :hall_04
   :name "Starboard corridor."
   :description "This wide hallway leads to the starboard side compartments."
   :exits [:uhub :sb_ramp]}

  {:tag :sb_ramp
   :name "Ramp to lower level."
   :description "A shallow ramp connecting the upper and lower level."
   :exits [:hall_04 :engineering]}

  {:tag :engineering
   :name "Engineering."
   :description "The heart of ships systems, from here most parts of the ship can be diagnosed and serviced."
   :exits [:sb_ramp :reactor :machine-room :low-hall]}

  {:tag :low-hall
   :name "A non descript hall."
   :description "This hall connects engineering with the main cargo bay."
   :exits [:cargo-bay :engineering]}

  {:tag :reactor
   :name "Reactor room."
   :description "Though radiation levels are quit low, the reactor room is shielded with a lead Faraday cage."
   :exits [:engineering]}

  {:tag :machine-room
   :name "Machine room"
   :description "Tools, workbenches, equipment, and associated storage."
   :exits [:engineering]}

  {:tag :cargo-bay
   :name "Cargo Bay"
   :description "A brightly lit large room with large containers."
   :exits [:low-hall :hall_03]}


            ])



(defn make-rooms [data]
  (let [results (map
     (fn [r] (let [my-uid (get-UID :r)
                   room (conj r {:UID my-uid :contents {:humans {} :items {}} :state (atom {:oxygen 80})} )]
           room )) data)]
    (map record-thing results) ))

(defn get-by-tag [tag]
  (first (filter (fn [a] (= (:tag a) tag) ) (vals @(:r DATA)))))

(defn swap-room-link [room]
  (let [tag-vec (:exits room)]

    (swap! (:r DATA) assoc-in [(:UID room) :exits ]       (into {} (map (fn [a] {(:UID (get-by-tag a)) a } ) tag-vec))     )

    ))

(defn finalize-rooms []
  (map swap-room-link (vals @(:r DATA))) )




















