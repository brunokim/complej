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

(defn- preferential-vertices [edges]
  (let [vertex (rand-nth (rand-nth edges))]
    (cons vertex 
          (lazy-seq (preferential-vertices (filter #(not (some #{vertex} %)) edges))))))

(defn- preferential-edges [k vertex edges]
  (map vector (repeat vertex) (take k (preferential-vertices edges))))

(defn preferential-growth [k vertex edges]
  "Returns a lazy sequence of edges, each time conjoined with k edges from a new
  vertex to, preferentially, the most connected vertices in the iteration. The
  vertex count starts from vertex and is incremented by 1 each step."
  (cons edges (lazy-seq
                (preferential-growth k (inc vertex) (concat edges (preferential-edges k vertex edges))))))

(defn- clique-edges [n]
  (for [i (range n), j (range n) :when (< i j)] [i j]))

(defn barabasi-albert [n k & args]
  "Barabási-Albert graph with n vertices and k edges per new vertex"
  (let [k+1 (inc k)]
    (reduce add-edge (apply clique k+1 args) 
            (last (take (- n k) (preferential-growth k k+1 (clique-edges k+1)))))))

#_(defn watts-strogatz [n k beta]
  "Watts-Strogatz undirected graph with n vertices and k neighbors each, 
  with beta probability to redirect an edge")

#_(defn watts-strogatz-2 [n k beta]
  "Watts-Strogatz undirected graph with n vertices and k neighbors each, added to 
  random edges created with probability beta")
