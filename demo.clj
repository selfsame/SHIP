;Lets make some data to play with; a bunch of names
;permutate all possible combinations
(def names
  (shuffle (apply combo/cartesian-product [['Ron 'Mark 'Margarite 'Al 'Larry 'Bill 'Sarah 'Alice]
                                 ['John 'May 'Cassandra 'Elliot 'Eugene]
                                 ['Robinson 'Parker 'Mills 'Twain 'Einstein]] )))
;quite a lot of data
(count names)

(take 2 names)

;use a high order anon function to filter the set
(filter #(= 'Parker (last %)) names)

;the predicate of a list is always applied as a function, all data is a function, all code is data
('Mills (group-by last names))

;what is the longest name?

;cast as string and sort by count
(def names-len
  (sort-by (fn[n] (count (apply str n)) ) names))

;shortest permutation
(first names-len)
;longest permutation
(last names-len)

;lets sort by which names have the most 'l's
(defn char-count [chr string]
  (count (filter #(= chr %) string)))

(char-count \s "Mississippi")
                                                   ;and the winner is :
(first (reverse
 (sort-by #(char-count \l (apply str %)) names )))


;map data with some structure
(def mapped-names
  (into [] (map
    (fn[n]
        {:name (string/join " " n)
         :first (first n)
         :middle (first(rest n))
         :last (last n)
         :age (inc (int (rand 95)))
         :sex (cond
                (nil? (#{'Sarah 'Alice 'Margarite} (first n))) :male
                :else :female)
         }) names)))

(first mapped-names)

(def minors (filter #(< (:age %) 18) mapped-names))

(map :age minors)
(count (:female (group-by :sex minors)))

;oldest person
(last (sort-by :age mapped-names))