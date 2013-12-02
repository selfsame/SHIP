(ns ship.items
  (:require
   [ship.core :as core]
   [ship.ship :as ship] )
  (:use [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing record-thing]]))



(defn modify [thing k v]
  (swap! (:state thing) assoc-in [k] v ))


(defn make-item [& {:keys [name desc flags state] :or {name "untitled" desc "no description" flags #{} state {}}} ]
  (let [ my-uid (get-UID :i)
         item {
          :role :item
          :UID my-uid
          :description desc
          :flags flags
          :room :false
          :state (atom state)
          :name name } ]
         (record-thing item)
          my-uid))















