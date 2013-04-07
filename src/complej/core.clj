(ns complej.core
  (:use [clojure.set]
        [incanter core stats]))

(def empty-graph {:edges #{} :directed? true :looped? false})

(defmulti add-edge 
  "Add the specified edge to the graph. The following syntaxes are supported:
   Directed graph:
     (g v1 v2), (g [v1 v2]): inserts edge from v1 to v2
     (g v1 v1), (g [v1 v1]): inserts loop in vertex v1, if graph is looped
   Undirected graph:
     (g v1), (g [v1]): inserts loop in vertex v1 (looped graph only). The vertex can't be a collection
     (g v1 v2), (g [v1 v2]): inserts edge between v1 and v2
     (g v1 v2 v3..), (g [v1 v2 v3...]): insert multi-edge between specified vertices"
 (fn [g & args] (:directed? g)))

(defmulti degree 
  "Maps vertex to its degree.
   Directed graph: 
     {:in {v1 <in-deg-v1>, v2 <in-deg-v2>, ...}
      :out {v1 <out-deg-v1>, v2 <out-deg-v2>, ...}}
   Undirected graph:
     {v1 <deg-v1>, v2 <deg-v2>}"
  (fn [g] (:directed? g)))

;; Directed graph methods

(defmethod add-edge true 
  ([g v1 v2] (add-edge g [v1 v2]))
  ([g [v1 v2]]
    (cond 
      (or (:looped? g) (not= v1 v2)) 
        {:edges (conj (:edges g) [v1 v2])
         :directed? true
         :looped? (:looped? g)}
      :else g)))

(defmethod degree true 
  ([g]
  (let [m [apply map vector (:edges g)]
        vertices (reduce #(apply conj %1 %2) #{} (:edges g))]
    (println (:edges g) m vertices)
    {:in (frequencies (second m))
     :out (frequencies (first m))})))

;; Undirected graph methods

(defmethod add-edge false
  ([g v1 v2 & vs] (add-edge g (conj (set vs) v1 v2)))
  ([g arg1]
    (if (coll? arg1)
     (let [edge (set arg1)] 
       (if (or (> (count edge) 1) (:looped? g))
         {:edges (conj (:edges g) edge) :directed? false :looped? (:looped? g)}
         g))
     (add-edge g #{arg1}))))

(defmethod degree false [g]
  (frequencies (reduce concat [] (:edges g))))

#_(defn neighbours
  "Returns a sequence of neighbouring vertices of v in g"
  ([g v] (neighbours has-vertex? g v))
  ([pred g v]
    (map (fn [[v1 v2]] (if (= v v1) v2 v1))
         (select (partial pred v) (:e g)))))

#_(def adjacents (partial neighbours out-vertex?))
#_(def incidents (partial neighbours in-vertex?))

#_(defn hist [pairs loglog?]
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

