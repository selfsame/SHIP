(ns ship.types
  (:use [ship.util]
        [clojure.set :only [map-invert union difference intersection join subset? superset?]]))





;QUERY FUNCTS

;valid? when a thing is a uid that can be resolved into recorded data, or is data with a valid uid
;is-uid? when the thing is keyword form
;is-data? when the thing is in map form
;recorded? when has been entered into the DATA



(defn atom? [a]
  (= (type a) clojure.lang.Atom))

(defn state? [m]
  (if (and (map? m)
           (atom? (:state m))
           (map? @(:state m))) 'true 'false))

(defn node? [a] 'false)

;PROTOCOLS

(defprotocol Uid
  (uid-valid? [k])
  (to-int [k])
  (to-prefix [k]))

(extend-protocol Uid
  clojure.lang.Keyword
    (uid-valid? [k] (not (some nil? [(to-int k) (to-prefix k)] ) ))
    (to-int [k]
      (let [sn (re-find #"\d+$" (str k))]
        (if (pos? (count sn)) (Integer/parseInt sn) nil)))
    (to-prefix [k]
       (let [result (apply str(rest(re-find #"^[:a-zA-Z\-_]+" (str k)))) ]
            (if (= "" result) nil (keyword result) ))))


(if (:b {:c 5}) 'true 'false)

(defprotocol Thing
   (valid? [n])
   (is-data? [n])
   (is-uid? [n])
   (recorded? [n])
   (data [n])
   (uid [n])
   (delete [n])
   (clone [n]))

(def ^{:private 'true} common-thing-impl
  {:valid? (fn [n] (uid-valid? (uid n)))
   :is-data? #('true)
   :is-uid? #('false)
   :recorded? (fn [n]
                (if (valid? n)
                  (if-let [u (uid n)]
                    (if-let [records ((to-prefix u) DATA)]
                      (if (u @records) 'true 'false )'false) 'false) 'false))
   :data (fn [n] (if (valid? n) n nil))
   :uid (fn [n] (if-let [u (:UID n)] (if (keyword n) (if (valid? n) n nil) nil) nil))
   :delete (fn [n]
      (if (valid? n)
          (do (swap! ((to-prefix (uid n)) DATA) dissoc (uid n)) :success)
          nil))} )

(extend-protocol Thing
  clojure.lang.Keyword
    (valid? [n] (if (recorded? n) 'true 'false))
    (is-data? [n] 'false)
    (is-uid? [n] (valid? n))
    (recorded? [n] (if (data n) 'true 'false))
    (data [n] (if (uid-valid? n)
                   (if-let [records ((to-prefix n) DATA)]
                    (n @records) nil) nil))
    (uid [n] (if (uid-valid? n) n nil))
    (delete [n] (if (uid-valid? n) (delete (data n)) nil))
  nil
    (valid? [n] 'false)
    (is-data? [n] 'false)
    (is-uid? [n] 'false)
    (recorded? [n] 'false)
    (data [n] nil)
    (uid [n] nil)
    (delete [n] nil))

(extend clojure.lang.PersistentArrayMap
  Thing common-thing-impl)
(extend clojure.lang.PersistentHashMap
  Thing common-thing-impl)



(defprotocol Node
   (node? [n])
   (get-links [n]))

(def ^{:private 'true} common-node-impl
  {:node? (fn [n] (if (and (:links (data n)) (uid n)) 'true 'false ))
   :get-links (fn [n] (if (node? n) (:links n) #{})) })

(extend clojure.lang.PersistentArrayMap
  Node common-node-impl)
(extend clojure.lang.PersistentHashMap
  Node common-node-impl)

(extend-protocol Node
  clojure.lang.Keyword
    (node? [n] (if (and (:links (data n)) (uid n)) 'true 'false ))
    (get-links [n] (if (node? n) (:links (data n)) #{})))

;RECORD TYPES

(defrecord Item [UID tag name weight description flags data])
(extend Item
  Thing common-thing-impl)





(defn adjacent? [a b]
  (if (and (node? a)(node? b))
    (if (or
     ((get-links a) (uid b))
     ((get-links b) (uid a)))'true 'false)
    'false))

(defn common-links [& more]
  (if (every? node? more) (apply intersection (map get-links more)) #{}))





; given a vector of nodes, find last node connections and return a set of possible extensions
(defn search [path]
  (cond
    (node? (last path))
  (let [ options (get-links (last path))]
    (into [] (mapv (fn [a]
           (cond
            (in? path a) path
            :else   (concat [:new] path [a]) )) options)))
   :else []))


(concat [5] [:new] (conj [1 2] 3))

(rest [])

(defn path-dead [a]
  (cond (= :new (first a)) (rest a)
        (= :closed (first a)) a
        (= :open (first a)) (concat [:closed] (rest a))
        :else a))

(defn find-path [r1 r2 & {:keys [limit complete open] :or {limit 40 complete 'true open 'false}}]

        (let [results (loop [results #{[:open r1]}  count 0]
          (cond
             (some #(= r2 (last %)) results) results
             (= count limit) results
             :else (recur
               (set (mapcat (fn [a]
                              (let [r (search a)]
                                (mapv path-dead r) )) results))
                 (inc count))))]
     (let [final (cond (and complete open) (filter #(= (last %) r2) (filter #(= (first %) :open) results))
             complete  (filter #(= (last %) r2) results)
             open (filter #(= (first %) :open) results)
            :else results)]
    (map rest final))))
(and (not 'false) 'true)

(search [:r2])

(find-path :r1 :r12 :limit 50 )


