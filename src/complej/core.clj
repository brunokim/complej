(ns complej.core
  (:use [clojure.set]
        [incanter core stats charts]))

(def empty-graph {:v #{} :e #{}})

(defn- add-e [g v1 v2] 
  (if (and (= v1 v2) (not (:looped? g)))
    (:e g)
    (if (:directed? g)
      (conj (:e g) [v1 v2])
      (conj (:e g) [v1 v2] [v2 v1]))))

(defn add-edge
  ([g [v1 v2]] (add-edge g v1 v2))
  ([g v1 v2]   (assoc g :v (conj (:v g) v1 v2) :e (add-e g v1 v2))))

;; Some useful predicates, testing whether the vertex is in some position 
;; within the edge.
(defn has-vertex? [v e] (some #{v} e))
(defn out-vertex? [v e] (= v (first e)))
(defn in-vertex?  [v e] (= v (second e)))

(defn select-edges
  "Select edges that satisfy the position predicate of the specified 
  vertex v in g"
  [pred v g]
  (select (partial pred v) (:e g)))

(defn degree 
  "Returns the degree of every vertex in the graph, considering their
  edge position."
  ([g] (degree has-vertex? g))
  ([pred g]
    (for [v (:v g)] [v (count (select-edges pred v g))])))

(defn out-degree [g] (degree out-vertex? g))
(defn in-degree  [g] (degree in-vertex?  g))

(defn neighbours
  "Returns a sequence of neighbouring vertices of v in g"
  ([g v] (neighbours has-vertex? g v))
  ([pred g v]
    (map (fn [[v1 v2]] (if (= v v1) v2 v1))
         (select (partial pred v) (:e g)))))

(def adjacents (partial neighbours out-vertex?))
(def incidents (partial neighbours in-vertex?))

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

(defn hist [pairs loglog?]
  (let [data (map (fn [[v d]] d) pairs)
        h    (histogram data :nbins (count data))]
    (if loglog?
      (let [plot (.getXYPlot h)
            x-axis (org.jfree.chart.axis.LogarithmicAxis. "deg")
            y-axis (org.jfree.chart.axis.LogarithmicAxis. "freq")]
        (do
          (.setRange x-axis (org.jfree.data.Range. 1 (count data)))
          (.setRange y-axis (org.jfree.data.Range. 1 (count data)))
          (.setRangeAxis plot y-axis)
          (.setDomainAxis plot x-axis)
          h))
      h)))

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
    
    
