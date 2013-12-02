(ns ship.util)

(defn clear-data []
  (def EPOCH (quot (System/currentTimeMillis) 1000))
  (def DATA {:h (atom {}) :r (atom {}) :i (atom {})})
  (def UIDS {:h (atom 0) :r (atom 0) :i (atom 0)}))

(clear-data)


(defn time-stamp []
  (- (quot (System/currentTimeMillis) 1000) EPOCH ))

(defn get-UID [prefix]
  (let [n (swap! (prefix UIDS) inc)]
  (keyword (str (last (str prefix)) n))))

(defn get-prefix [k]
  (keyword (str (first (rest (str k))))))

(defn get-thing
 ([k]
  (let [pre (get-prefix k)]
  (get @(pre DATA) k)))
 ([k & args]
  (let [pre (get-prefix k)]
  (cond (= (type (get (get @(pre DATA) k) (first args))) clojure.lang.Atom) (get-in @(get (get @(pre DATA) k) (first args)) (rest args))
  :else (get-in (get @(pre DATA) k) args) ))))

(defn record-thing [e]
  (let [pre (get-prefix (:UID e))]
  (swap! (pre DATA) conj {(:UID e) e})))

(defn rand-in [n]
  (get n (rand-nth (keys n))))

(defn dissoc-in [ data keys val ]
  (assoc-in data keys (dissoc (get-in data keys) val)  ) )








