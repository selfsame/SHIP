(ns ship.logic
  (:refer-clojure :exclude [==])
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]

   )
  (:use
        [clojure.pprint :only [pprint]]
        [clojure.core.logic]
        [clojure.math.combinatorics :as combo]
        [ship.util]
     [ship.types]
   [clojure.set :only [map-invert union difference intersection join subset? superset?]]))









(defrel node n)
(defrel link n n)
(defn adjacent [n1 n2]
  (link n1 n2))


(defrel location r)
(defrel exits r rcol)


(defn adjacent [r1 r2]
  (conde
   [(location r1)(location r2)
    (fresh [e1]
           (exits r1 e1) (membero r2 e1))]
   [(location r1)(location r2)
    (fresh [e2]
           (exits r2 e2) (membero r1 e2))]
   ))




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



(defn distincto [args]
  (if (> (count args) 1)
  (map
   (fn [g] (str "(!= " (first g) " " (last g) ") "))
   (filter
     (fn [s] (if (> (count s) 1) 'true 'false))
     (into #{} (map set(combo/selections args 2))) ) )""))

;:TODO (factualize in-vars, so they can be permutated)
;algorithm for some basic permutations of function / arg arrangement
;use logic for transformations (in -> out)



(defrel number! n)
(defrel string! n)
(defrel nil! n)
(defrel function! n)
(defrel collection! n)

(defrel type! n)
(defrel literal! n)

;we want to record function relations with the arity count and types
;this can be generated
(defrel arity-type-first! f a)
(defrel arity-type-rest! f a )
(defrel return-type! f a )

(defrel arity-req-more! f)

(defn same-type [a b]
  (conde
   [(number! a)(number! b)(type! b)(literal! a)]
   [(string! a)(string! b)(type! b)(literal! a)]
   [(collection! a)(collection! b)(type! b)(literal! a)]))

(defn not-needs-more [a]
  (fresh [h]
     (arity-req-more! h)
     (!= a h)))

;lets assume functions have first and rest conditions for arity type

(facts arity-type-first! [['+ :number]
                        ['- :number]
                        ['/ :number]
                        ['* :number]
                        ['max :number]['min :number]
                        ['inc :number]
                        ['dec :number]
                        ['str :number] ['str :string]

                        ['interleave :string]['interleave :coll]

                        ['cons :number]['cons :string]['cons :coll]
                        ['conj :coll]

                        ['count :string]['count :coll] ])
(facts arity-type-rest! [['+ :number]
                        ['- :number]
                        ['/ :number]
                        ['* :number]
                        ['max :number]['min :number]
                        ['str :number] ['str :string]
                        ['max :number]
                        ['min :number]
                        ['interleave :string]['interleave :coll]

                        ['cons :coll]
                        ['conj :number]['conj :string]['conj :coll]])

(facts return-type!    [['+ :number]
                        ['- :number]
                        ['/ :number]
                        ['* :number]
                        ['str :string]
                        ['inc :number]
                        ['dec :number]
                        ['count :number]
                        ['interleave :coll]
                        ['cons :coll]
                        ['conj :coll]])


(facts arity-req-more! [['interleave]['cons]['conj]])


(run 20 [q] (arity-type-first! q :string)(not-needs-more q))


(fact number! :number)
(fact string! :string)
(fact nil! :nil)
(fact collection! :coll)


(facts type! [[:number][:string][:nil][:coll]])

(def in-vars ['in-a 'in-b 'in-c 'in-d 'in-e 'in-f 'in-g 'in-h])

(defn get-arity
  ([f1 a]
  (fresh [v]
      (same-type a v)
      (arity-type-first! f1 v)
      (not-needs-more f1)))
  ([f1 a & more]
  (fresh [v k]
      (same-type a k)
      (arity-type-first! f1 k)
      (same-type (first more) v)
      (arity-type-rest! f1 v))))


(defn conf-head [vars]
  (str "(run 300 [q] (fresh [f1 " vars "] ") )
(defn conf-tail [vars in]
  (str "(get-arity f1 " (string/join " " vars ) ") "
       (apply str (distincto vars) )
       "(== q `(~f1 ~" (string/join " ~" vars ) ")) ))" ))

(defn conf-in-facts [two]
  "makes type facts from the in arg literals"
  (let [in (first two)
        var (last two)
        literal `(~'fact ~'literal! ~in)]
  (cond
   (number? in) (str `(~'fact ~'number! ~in) literal)
   (string? in) (str `(~'fact ~'string! ~in) literal)
   (coll? in) (str `(~'fact ~'collection! ~in) literal)
   :else (str `(~'fact ~'nil! ~in) literal))))

(defn conf-in-rel [two]
  "makes goals from the in arg literals"
  (let [in (first two)
        var (last two)
        literal `(~'literal! ~var)]
  (cond
   (number? in) (str `(~'number! ~var)`(~'literal! ~var))
   (string? in) (str `(~'string! ~var)`(~'literal! ~var))
   (coll? in) (str `(~'collection! ~var)`(~'literal! ~var))
   :else (str `(~'nil! ~var)`(~'literal! ~var)))))

(defn conflibulate [in out]
  "returns a logic run for the in and out data"
  (let [vars (take (count in) in-vars)]
  (str (apply str (map conf-in-facts (reverse (zipmap in vars))))
       (conf-head (string/join " " vars ))
       (apply str (map conf-in-rel (reverse (zipmap in vars))))
       (conf-tail vars (string/join " " in )))
  ))




(def flib (conflibulate [1 2 3] nil))
flib

(into #{} (load-string flib))
(map eval (into #{} (load-string flib)))






