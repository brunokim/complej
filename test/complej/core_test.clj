(ns complej.core-test
  (:use clojure.test
        complej.core))

(deftest new-graph-test
  (testing "new-graph"
    (let [dir-unloop (new-graph :directed :unlooped)]
      (is (:directed? dir-unloop))
      (is (not (:looped? dir-unloop))))))

(deftest dir-unlooped-test
  (testing "directed unlooped graph"
    (let [dir-unloop (new-graph :directed :unlooped)]
      (is (= 0 (count (:edges dir-unloop))))
      (is (= 0 (count (:edges (add-edge dir-unloop [1 1])))))
      (is (= 1 (count (:edges (add-edge dir-unloop [1 2])))))
      (is (= 1 (count (:edges (reduce add-edge dir-unloop (take 10 (repeat [1 2]))))))))))
 
