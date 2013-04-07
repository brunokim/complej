(ns complej.model
  (use [complej.core]))

(defn- pairs-erdos-renyi [n k]
  (for [i (range n), j (range n)
        :when (< (rand-int n) k)]
   [i j]))

(defn erdos-renyi 
  "Returns an Erdos-Renyi graph with n vertices with k average (out-)degree.
  looped?   If true, allows self-references, ie, edges like [v1 v1]. Default is false.
  directed? If true, creates in average k outgoing edges per vertex. Else, add all
            reversed edges to the graph, making it undirected. Note that in this case
            the total average degree is 2*k. Default is false."
  ([n k] (erdos-renyi n k false false))
  ([n k looped? directed?] 
    (reduce add-edge 
            (assoc empty-graph :looped? looped? :directed? directed?)
            (pairs-erdos-renyi n k))))

(defn clique
  "Returns a complete graph (clique) with n vertices."
  ([n] (clique n false false))
  ([n looped? directed?]
    (reduce add-edge
            (assoc empty-graph :looped? looped? :directed? directed?)
            (for [i (range n), j (range n)] [i j]))))

(defn preferential-edges [v k edges]
  (loop [i 0, new-edges #{}, available-edges (seq edges)]
    (if (= i k)
      new-edges
      (let [selected-vertex (rand-nth (rand-nth available-edges))
            edge-has-selected? (partial has-vertex? selected-vertex)]
        (recur (inc i)
               (conj new-edges [v selected-vertex])
               (filter (complement edge-has-selected?) available-edges))))))

(defn barabasi
  ([n k] (barabasi n k false false))
  ([n k looped? directed?]
    (loop [i (inc k), g (clique (inc k) looped? directed?)]
      (if (= i n)
        g
        (recur (inc i)
               (reduce add-edge g (preferential-edges i k (:e g))))))))

