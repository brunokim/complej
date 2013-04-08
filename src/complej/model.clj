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

#_(defn lattice [n k d & args]
  "Regular d-dimensional lattice with n vertices per dimension and k neighbors")

#_(defn barabasi-albert [n k & args]
  "Barabási-Albert graph with n vertices and k edges per new vertex")

#_(defn watts-strogatz [n k beta]
  "Watts-Strogatz undirected graph with n vertices and k neighbors each, 
  with beta probability to redirect an edge")

#_(defn watts-strogatz-2 [n k beta]
  "Watts-Strogatz undirected graph with n vertices and k neighbors each, added to 
  random edges created with probability beta")
