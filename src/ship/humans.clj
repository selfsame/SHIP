(ns ship.humans
  (:use [ship.util :only [DATA get-UID dissoc-in get-prefix rand-in get-thing record-thing symbolize-keyword]]))

(defn make-genotype [gene-data]
  (reduce conj {} (for [gene gene-data
    :let [k (first gene)
          v (last gene)]]
    {k (rand-nth v)})))










(defn describe-human [h]
  (let [human (get-thing h)]
  (format "%s, a %s %s with %s hair."
    (:name human)
    (:height (:dna human))
    (cond
      (= :m (:sex human)) "man"
      (= :f (:sex human)) "woman")
    (:hair-color (:dna human))
    )))

(def racial
  {:mongolian     {:name {:first {:m '("Nergüi" "Narantsetseg" "Enkhtuyaa" "Nergüi")
                                  :f '("Bolormaa" "Bat-Erdene" "Erdenechimeg" "Narantsetseg") }
                          :last '("Mönkhbat" "Mönkh-Erdene" "Gantulga" "Batbayar") }}

  :mexican        {:name {:first {:m '("Pedro" "Juan" "Horge" "Jesus")
                                  :f '("Anita" "Marcela" "Estefania" "Maria") }
                          :last '("Martinez" "Juan" "Ruiz" "Garcias") }}

  :inuit         {:name {:first {:m '("Yutu" "Toskolo" "Adlartok" "Maniitok")
                                  :f '("Kirima" "Koko") }
                          :last '("") }}

  :ethiopian      {:name {:first {:m '("Yonatan" "Ermiyas" "Sintayehu" "Abiy")
                                  :f '("Frehiwot" "Dershaye" "Eyerusalem" "Geni") }
                          :last '("Alemseged" "Asfaw" "Dibaba" "Kibebe") }}

  :portugeuse     {:name {:first {:m '("Donny" "Frederico" "Martin")
                                  :f '("Anita" "Marcela" "Estefania" "Maria") }
                          :last '("Martinez" "Juan" "Ruiz" "Garcias") }}

  :persian        {:name {:first {:m '("Mahmoud" "Mazdak" "Mazdan" "Maziar" "Sassan" "Sepehr" "Ardashir" "Aria")
                                  :f '("Anousheh" "Arezu" "Arian" "Mahshid" "Maryam" "Mehregan" "Mina" "Mithra") }
                          :last '("Homayoun" "Hooshang"  "Mazandarani" "Mokri" "Mohsen" "Ebrahimi" "Esfahani") }}

  :finnish        {:name {:first {:m '("Yorgi" "Olaf")
                                  :f '("Anita" "Marcela" "Estefania" "Maria") }
                          :last '("Martinez" "Juan" "Ruiz" "Garcias") }}

  :russian        {:name {:first {:m '("Vladimir" "Alexander" "Constantine")
                                  :f '("Sasha" "Katrina" "Anya" "Mashenyka") }
                          :last '("Venediktov" "Dezhnyov" "Kozlovsky") }}
  })

(def genetic
  { :height '(:short :average :tall)
    :hair-color '(:black :blonde :brown :red)
    :hair-texture '(:thin :normal :thick) ;'(:straight :normal :wavy :curly)
    })





(defn make-human []
  (let [ my-race (rand-nth '(:mongolian :mexican :inuit :ethiopian :portugeuse :persian :finnish :russian))
         my-sex (cond (> (rand 1.0) 0.52) :m :else :f)
         my-dna (make-genotype genetic)
         my-uid (get-UID :h)
         human {
          :role :human
          :UID my-uid
          :sex my-sex
          :dna my-dna
          :race my-race
          :room :false
          :name (rand-nth (get (:first (:name (get racial my-race)) ) my-sex))
          :lastname (rand-nth (:last (:name (get racial my-race))))
          :state (atom {})
          } ]
         (record-thing human)
          my-uid))















