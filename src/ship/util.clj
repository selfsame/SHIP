(ns ship.util
  (:use [clojure.string :only [join] ]))

;h humans
;r rooms
;i item instances
;x item templates

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn symbolize-keyword [k](symbol (join (rest (str k) )  )))

(defn clear-data []
  (def history (atom []))
  (def history-size (atom 0))
  (def EPOCH (quot (System/currentTimeMillis) 1000))
  (def DATA {:h (atom {}) :r (atom {}) :i (atom {}) :x (atom {})})
  (def UIDS {:h (atom 0) :r (atom 0) :i (atom 0) :x (atom 0)}))

(clear-data)


(defn time-stamp []
  (- (quot (System/currentTimeMillis) 1000) EPOCH ))

(defn record-event [e]
  (swap! history conj (conj e {:time (time-stamp)})))



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

(defn get-thing-where [pre k v]
  (filter (fn [a] (= (k a) v) ) (vals @(pre DATA)) ))

(defn record-thing [e]
  (let [pre (get-prefix (:UID e))]
  (swap! (pre DATA) conj {(:UID e) e})))

(defn rand-in [n]
  (get n (rand-nth (keys n))))

(defn dissoc-in [ data keys val ]
  (assoc-in data keys (dissoc (get-in data keys) val)  ) )

(defn mutate [t k v]
  (if (:state t)
    (do
      (swap! (:state t) assoc-in  [k] v)
      )))

(defn use-mind [k]
  (let [mind (get-thing k :state :mind)]
    (in-ns mind)))


(defn place [thing room]
 (record-event {:role (:role thing) :UID (:UID thing) :place room (:UID thing) 1}))

(defn unplace [thing room]
  (record-event {:role (:role thing) :UID (:UID thing) :unplace room (:UID thing) 1}))


















