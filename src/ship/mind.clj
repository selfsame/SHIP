(ns ship.mind
  (:refer-clojure :exclude [==])
  (:use
   [clojure.pprint :only [pprint]]
    [clojure.core.logic]
    [ship.logic :only [make-fact make-rel query query2]]
    [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing get-thing-where record-thing clear-data time-stamp mutate symbolize-keyword use-mind record-event ]]
))

(def ME nil)

(defn new-mind [k]
  (let [current *ns*
        s (symbolize-keyword k)
        new-ns (symbol (str "ship.minds." (str s)))]

    (remove-ns new-ns)
    (eval
     `(ns ~new-ns
      (:refer-clojure :exclude [==])

      (:use
        [clojure.core.logic])
      ))
    (in-ns (ns-name current))
    new-ns))

(defn read-book [book]
  (let [relations (:relations (:data book))
        facts (:facts (:data book)) ]
    (dorun (map (fn[a] (apply make-rel a)) relations))
    (dorun (map (fn[a] (apply make-fact a)) facts))
    (record-event {ME 0 :read (:UID book)})
    ))

(defn mind_init [t]
  (let [current *ns*]
  ;create logic namespace
  (mutate (get-thing t) :mind (new-mind t))
  ;switch to namespace logic for human
  (use-mind t)
    ;refer to some outside logic functions
    (refer 'ship.logic :only '[make-fact make-rel])
    (refer 'ship.mind :only '[read-book])
    (refer 'ship.util :only '[DATA get-UID dissoc-in get-prefix rand-in get-thing get-thing-where record-thing clear-data time-stamp mutate symbolize-keyword use-mind record-event])
    (def ME t)
    ;read some books
    (let [basic-books (get-thing-where :x :flags #{:readable :basic})
          basic-uplinks (get-thing-where :x :flags #{:uplink :basic})]
      (dorun (map read-book basic-books))
      (dorun (map (fn [a] (eval (:code (:data a)))) basic-uplinks)))

  (in-ns (ns-name current)) :success))




(defn ponder [& {:keys [who conds subset fresh] :or {who nil conds nil subset nil fresh nil}}]
  (let [current *ns*]
  ;switch to namespace logic for human
  (use-mind who)
    (let [answer (query2 :conds conds :subset subset :fresh fresh)]
      (in-ns (ns-name current))
      answer
      )))

(in-ns 'ship.mind)



(def book (get-thing-where :x :flags #{:readable :basic}))
book
(read-book (:data (first book)))


*ns*














