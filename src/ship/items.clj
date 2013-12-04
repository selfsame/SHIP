(ns ship.items
  (:use [ship.types]
        [ship.util]))



(defn modify [thing k v]
  (swap! (:state thing) assoc-in [k] v ))








(defn record-item [& {:keys [tag name weight desc flags data state] :or {tag :item name "item" weight 1 desc "generic item" flags #{} data {} state{}}} ]
  (let [ t (new ship.types.Item (get-UID :x) tag name weight desc flags data)]
    (do
      (record-thing t)
  )))

(defn spawn-instance [item]
  (let [ my-uid (get-UID :i)
         item {
          :UID my-uid
          :is (:UID item)
          :state (atom {})}]
         (record-thing item)
          my-uid))

(defn init []

  (record-item :tag :book-actions :name "book of actions" :flags #{:readable :basic} :data{
     :relations [ ['is-concept :c :x] ['action-req :a :set]]
     :facts [['is-concept :action #{:goto :wait :notice :think :plan-path :pick-random :tell :ask :eat :drink :sleep :sit :stand :lay :poop :pee}]
             ['action-req [:goto [:place :adjacent]] [:plan-path [:place]]
                          [:pick-random [:set]]
                          [:tell [:human :present]] [:ask [:human :present]]
                          [:drink [:thing :present :edible :liquid :portable ]]
                          [:drink [:thing :present :edible :solid :portable ]]


              ]]})

  (record-item :tag :book-concepts :name "book of concepts" :flags #{:readable :basic} :data{
     :relations [ ['concept :c] ['sub-concept :c :c] ['state-of :a :p] ]
     :facts [['concept :action :place :thing :human :information :posture :state :time :good :bad :property]
             ['sub-concept [:thing :human]]
             ['state-of [:thing :place]
                        [:human :posture]
                        [:human :action]]]})



  (record-item :tag :book-humans :name "book of humans" :flags #{:readable :basic} :data{
    :relations [ ['property-of :a :p] ['property :p] ['value :v] ]
    :facts [['property :name :last-name :sex :race :age :height :hair-color :job :state]
            ['property-of :human #{:name :last-name :sex :race :age :height :hair-color :job :state}]]})


  (record-item :tag :book-structures :name "book of structure" :flags #{:uplink :basic} :data{ :code
  '(do



     (defrel structure s)
     (defrel node n)
     (defrel link n n)
     (defn adjacent [n1 n2]
       (link n1 n2))

     (fact node :r)


     )})


)




















