(ns ship.logic
  (:refer-clojure :exclude [==])
  (:require
   [clojure.string :as string]

   )
  (:use
        [clojure.core.logic]
        [clojure.math.combinatorics :as combo]
        [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing get-thing-where record-thing clear-data symbolize-keyword time-stamp in?]]
        [clojure.set :only [map-invert union difference intersection join subset? superset?]]))







(defrel tool x)
(defrel tool-affect x y)
(defrel tool-effect x y)
(defrel tool-weight t n)
(defrel materials x)
(defrel material-type x y)
(defrel item-portable x)

(facts materials [[:oak]
                  [:pine]
                  [:birch]
                  [:aluminum]
                  [:copper]
                  [:bronze]
                  [:iron]
                  [:steel]
                  [:plastic]
                  [:glass]])

(facts material-type
                 [[:wood :oak]
                  [:wood :pine]
                  [:wood :birch]
                  [:metal :aluminum]
                  [:metal :iron]
                  [:metal :steel]
                  [:metal :bronze]
                  [:metal :copper]])



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






(defn query-run [r a b]
  (let [rel (relationize r)
       ]
    (cond (= a :?) (run* [q] (rel q b))
          (= b :?) (run* [q] (rel a q))
          )))

(make-rel 'edible 'x)

(type edible)

(make-rel 'genus 'x 'y)

(make-fact edible :peanut)

(make-fact 'edible :peanut)
(make-fact "edible" :fish :cheese :apple :salmon :cod)

(make-fact (relationize genus) [:fish :salmon] [:mammal :cat] [:fish :cod])

(run* [q] ((relationize "genus") q :cod))
(relationize 'edible)
(query-run "genus" :fish :? )
















