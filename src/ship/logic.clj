(ns ship.logic
  (:refer-clojure :exclude [==])
  (:require
   [clojure.string :as string]

   )
  (:use
        [clojure.pprint :only [pprint]]
        [clojure.core.logic]
        [clojure.math.combinatorics :as combo]
        [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing get-thing-where record-thing clear-data symbolize-keyword time-stamp in?]]
        [clojure.set :only [map-invert union difference intersection join subset? superset?]]))




;meta functions to make relations and facts from outside ns

(defn relationize [token]
  (cond (= (type token) clojure.core.logic.Rel) token
        (= (type token) clojure.lang.PersistentList) (eval `~(last token) )
        (string? token) (eval `~(symbol token) )
        (keyword? token) (eval `~(symbolize-keyword token))
        (symbol? token) (eval `~token)
        :else (type token)))

(defn make-rel
  ([s a]
    (eval `(defrel ~s ~a)))
  ([s a b]
    (eval `(defrel ~s ~a ~b)))
  ([s a b c]
    (eval `(defrel ~s ~a ~b ~c)))
  ([s a b c d]
    (eval `(defrel ~s ~a ~b ~c ~d))))

(defn all-but-last [col]
 (reverse (rest (reverse col ))))

(defn best-interleave [a b]
  (let [ac (count a) bc (count b)]
    (cond (< ac bc)  (all-but-last (interleave (conj a nil) b))
          (> ac bc)   (all-but-last  (interleave a (concat b [:?])))
          :else (interleave a b)
          )))


(defn exp-by-sets [v]
  (cond (some set? v) (do
    (let  [sp1 (vec (partition-by set? v))
    iters (map #(vec(first %)) (filter #(set? (first %)) sp1))
    parts (map vec (filter #(not( set? (first %))) sp1))]


              (map (fn[a]
                      (vec (mapcat (fn[a] (cond (vector? a) a :else (vector a)))
                         (best-interleave parts a)))

                      )(apply combo/cartesian-product iters))

    )) :else v))
(exp-by-sets [7 #{:moldy :soft :sliced} 5 #{:gouda :sharp :bree}] )



(defn make-fact
  ([rel t]
   (let [r (relationize rel)]
   (cond (vector? t) (apply fact r t)
   :else (fact r t))) )
  ([rel t & more]
    (let [mor (exp-by-sets (cons t more))
          col (vec (map (fn[a] (cond (vector? a) a :else [a])) mor))
          r (relationize rel)]
   (facts r col)
      )))


(make-rel 'cheese :type :taste)
(make-fact 'cheese [:chedder :sharp])

(run* [q] (cheese q :sharp ))

(some #(= 6 %) [1 2 3 4])

(defn logicate [rel more]
  (str "(" rel " "
       (apply str (map (fn [a]
            (cond (some #(= a %) [:? :?q :q 'q "q"]) "q "
                  (some #(= a %) [:?k :k 'k "k"]) "k "
                  (some #(= a %) [:?v :v 'v "v"]) "v "
                  :else (str a " ") )

             ) more)) " ) " ))

(defn sset-format [s]
  (cond (nil? s)  ""
        (vector? s) (str "(membero q" (str s) ") ")
        :else ""))

(defn query [& {:keys [subset rel props] :or {subset nil rel nil props nil}}]
  (let [sset (sset-format subset)]
  (cond (not (true? (some nil? [rel props]))) (do
      (eval (load-string (str "'(run* [q] " sset (logicate rel props) ")")))
      ))))

(defn query2 [& {:keys [conds subset fresh] :or {conds nil subset nil fresh nil}}]
  (let [sset (sset-format subset)]
  (cond (and (vector? conds) (pos? (count conds)) ) (do
      (let [condstrs  (doall (map (fn[a] (logicate (first a) (vec (rest a)) )) conds))
            frsh (cond (nil? fresh) ["" ""]
                       :else [(str "(fresh " (str fresh) " ") ")"]) ]

        (eval (load-string (str "'(run* [q] " (first frsh) sset (apply str condstrs) ")" (last frsh))))

      )))))











(defrel node n)
(defrel link n n)
(defn adjacent [n1 n2]
  (link n1 n2))


(defrel place r)
(defrel exits r rcol)


(defn adjacent [r1 r2]
  (conde

   [(place r1)(place r2)
    (fresh [e1]
           (exits r1 e1) (membero r2 e1))]
   [(place r1)(place r2)
    (fresh [e2]
           (exits r2 e2) (membero r1 e2))]
   )
  )


(map (fn [r]
       (fact place (:UID r))
       (fact exits (:UID r) (keys (:exits r)))

         ) (vals @(:r DATA)))



(defn not-membero
[x l]
(fresh [head tail]
(conde
( (== l ()) )
( (conso head tail l)
(!= x head)
(not-membero x tail) ))))

(defn prefixo [x y]
  (fresh [a]
         (appendo x a y)))

(defn lasto
  "Declares x as the last item in y"
  [x y]
  (fresh [a]
         (appendo a [x] y)))



(defn find-path [start finish]
  (run 20 [q]
      (fresh [x y z]
        (place start)(place finish)

        (place x)
        (adjacent start x)(!= start y)(!= x y)
        (place y)
        (adjacent x y)

        (prefixo [start x y] q)
        (lasto finish q)
       )))


(find-path :r5 :r10)





(defrel tool t)
(defrel material m)
(defrel affects t m)
(defrel effect t e)
(defrel portable t)

(facts tool [[:wrench] [:handsaw]
             [:tablesaw] [:hacksaw]
             [:drillgun] [:drillpress]
             [:circular-saw]])


(facts material [[:metal] [:wood]
             [:plastic] [:glass]])

(facts effect [[:wrench :torque] [:handsaw :cut]
             [:tablesaw :cut] [:hacksaw :cut]
             [:drillgun :hole] [:drillpress :hole]
             [:circular-saw :cut]])

(facts affects [[:wrench :metal] [:handsaw :wood]
             [:tablesaw :wood] [:hacksaw :metal] [:hacksaw :wood]
             [:drillgun :metal] [:drillgun :wood] [:drillpress :metal] [:drillpress :wood]
             [:circular-saw :wood]])

(run* [q]
      (affects q :metal))

(run* [q]
      (fresh [k v]
        (effect v k)
        (affects v :wood)
        (== [v k] q)))














