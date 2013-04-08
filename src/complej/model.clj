(ns complej.model
  (use [complej.core]))

(defn clique [n & args]
  "Complete graph (clique) with n vertices."
  (reduce add-edge (apply new-graph args)
          (for [i (range n), j (range n)] [i j])))

(defn erdos-renyi [n k & args]
  "Erdos-Renyi graph with n vertices with k average degree."
  (reduce add-edge (apply new-graph args) 
          (for [i (range n), j (range n) 
                :when (< (rand-int n) k)] 
            [i j])))

