(ns ship.types
  (:use [ship.util]))

;QUERY FUNCTS

(defn uid? [m]
  (cond (map? m)
    (cond (keyword? (:UID m)) 'true
          :else 'false)
    :else 'false))

(defn atom? [a]
  (= (type a) clojure.lang.Atom))

(defn state? [m]
  (if (and (uid? m)
           (atom? (:state m))
           (map? @(:state m))) 'true 'false))





;PROTOCOLS

(defprotocol Thing
   (data [n])
   (uid [n])
   (delete [n])
   (clone [n]))

(def ^{:private 'true} common-thing-impl
  '(do (data [n] (if (uid? n) n nil))
    (uid [n] (if (uid? n) (:UID n) nil))
    (delete [n]
      (if (uid? n)
          (do (swap! ((uid-prefix (uid n)) DATA) dissoc (uid n)) :success)
          nil))) )

(extend-protocol Thing
  clojure.lang.PersistentArrayMap
    (data [n] (if (uid? n) n nil))
    (uid [n] (if (uid? n) (:UID n) nil))
    (delete [n]
      (if (uid? n)
          (do (swap! ((uid-prefix (uid n)) DATA) dissoc (uid n)) :success)
          nil))
  clojure.lang.PersistentHashMap
    (data [n] (if (uid? n) n nil))
    (uid [n] (if (uid? n) (:UID n) nil))
    (delete [n]
      (if (uid? n)
          (do (swap! ((uid-prefix (uid n)) DATA) dissoc (uid n)) :success)
          nil))

  clojure.lang.Keyword
    (data [n] (get-thing n))
    (uid [n] n)
    (delete [n] (delete (data n)))
  nil
    (data [n] nil)
    (uid [n] nil)
    (delete [n] nil)

  )




(defprotocol Node
   (node? [n])
   (get-links [n]))

(extend-protocol Node
  clojure.lang.PersistentArrayMap
    (node? [n]
       ((cond (uid? n) 'true
              :else 'false))))

;RECORD TYPES


(defrecord Item [UID tag name weight description flags data]
  Thing
  (data [n] (if (uid? n) n nil))
    (uid [n] (if (uid? n) (:UID n) nil))
    (delete [n]
      (if (uid? n)
          (do (swap! ((uid-prefix (uid n)) DATA) dissoc (uid n)) :success)
          nil)))


(defrecord Room [UID links]
  Node
    (node? [n] 'true)
    (get-links [n] links))


(def ii (new Item :u1 :t "d" 5 "f" #{} {}))

(instance? Item (data :x1))

(instance? Item ii)
(uid ii)
(type (data :x1))




