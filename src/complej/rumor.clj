(ns complej.rumor
  (use [complej.core]))

(defn epidemics [g initial-state transition terminate?]
  (loop [coll '(), state initial-state]
    (if (terminate? g state)
      coll
      (let [next-state (transition g state)]
        (recur (cons next-state coll) next-state)))))

(def daley-kendall-table
  {:ignorant [:spreader :spreader]
   :spreader [:stifler  :stifler]
   :stifler  [:stifler  :stifler]})

(defn daley-kendall-transition [g state]
  (let [is-spreader? #(= :spreader (val (first %)))
        spreaders    (map #(key (first %)) (filter is-spreader? state))
        edges        (for [v spreaders] [v (rand-nth (adjacents g v))])]))
    
 
