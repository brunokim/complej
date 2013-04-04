(ns complej.mwe
  (use clojure.set))

(def empty-graph {:vertices #{} :edges #{}})

(defn add-edge [g [v1 v2]]
  (assoc g :vertices (conj (:vertices g) v1 v2)
           :edges    (conj (:edges    g) [v1 v2])))

(defn clique [n]
  (reduce add-edge empty-graph 
          (for [i (range n), j (range n) :when (< i j)] [i j])))

(defn preference [g v]
  (let [available-edges (filter (complement (partial some #{v})) (:edges g))]
    (rand-nth (rand-nth available-edges))))

(defn barabasi [n k]
  (loop [g (clique (inc k)), i (inc k)]
    (if (= i n)
      g
      (recur (reduce add-edge g (for [_ (range k)] [i (preference g i)]))
             (inc i)))))



