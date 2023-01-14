(ns dd
  "a naive non-optimized implementation of decision diagrams")

(defrecord Node [var-index children])

(def node? (partial instance? Node))

(def ^:dynamic *domains* nil)

(defn node [var-index & children]
  (when (not (integer? var-index))
    (throw (ex-info "node var-index must be an integer" {:var-index var-index})))
  (when (neg? var-index)
    (throw (ex-info "node var-index must be greater than or equal to zero" {:var-index var-index})))
  (when (empty? children)
    (throw (ex-info "node must have at least one child" {})))
  (when-not (= (*domains* var-index) (count children))
    (throw (ex-info "node must have the same number of children as the size of variable domain"
                    {:var-index var-index
                     :children-count (count children)
                     :expected-children-count (*domains* var-index)})))
  (doseq [child children]
    (when (and (node? child) (<= (:var-index child) var-index))
      (throw (ex-info "node children must have higher var-index than parent" {:child child}))))
  (if (apply = children)
    (first children)
    (Node. var-index (into [] children))))

(defn eval-dd [env node]
  (loop [node node]
    (if (node? node)
      (recur
       (nth (:children node) (env (:var-index node))))
      node)))

(defn all-where
  [value node]
  (if (node? node)
    (let [var-index (:var-index node)]
      (apply
       concat
       (map-indexed
        (fn [value-index child]
          (map #(assoc % var-index value-index)
               (all-where value child)))
        (:children node))))
    (if (= value node)
      [{}]
      [])))

(defn apply1 [f dd]
  (if (node? dd)
    (apply
     node
     (:var-index dd)
     (map (partial apply1 f) (:children dd)))
    (f dd)))

(defn apply2 [f dd1 dd2]
  (cond
    (and (node? dd1) (node? dd2))
    (let [v1 (:var-index dd1)
          v2 (:var-index dd2)]
      (cond
        (= v1 v2)
        (apply
         node
         v1
         (map (partial apply2 f) (:children dd1) (:children dd2)))

        (< v1 v2)
        (apply
         node
         v1
         (map (fn [c1] (apply2 f c1 dd2)) (:children dd1)))

        (< v2 v1)
        (apply
         node
         v2
         (map (fn [c2] (apply2 f dd1 c2)) (:children dd2)))))

    (node? dd1)
    (apply1 (fn [d1] (f d1 dd2)) dd1)

    (node? dd2)
    (apply1 (fn [d2] (f dd1 d2)) dd2)

    :else
    (f dd1 dd2)))

(defn bvar [var-index] (node var-index 0 1))

(defn dd-not [dd] (apply1 #(if (= % 0) 1 0) dd))

(def not-bvar (comp dd-not bvar))

(defn dd-and
  ([dd] dd)
  ([dd1 dd2] (apply2 (fn [a b] (if (and (= a 1) (= b 1)) 1 0)) dd1 dd2))
  ([dd1 dd2 & more] (dd-and dd1 (apply dd-and dd2 more))))

(defn dd-or
  ([dd] dd)
  ([dd1 dd2] (apply2 (fn [a b] (if (or (= a 1) (= b 1)) 1 0)) dd1 dd2))
  ([dd1 dd2 & more] (dd-or dd1 (apply dd-or dd2 more))))

(comment
  (def wiki-bdd
    (binding [*domains* [2 2 2]]
      (let [x3-1 (node 2 1 0)
            x3-2 (node 2 0 1)
            x2-1 (node 1 0 1)
            x2-2 (node 1 x3-1 x3-2)]
        (node 0 x2-2 x2-1))))

  (eval-dd [0 0 2] wiki-bdd) ;; fails as var is out of range
  (eval-dd [0 0] wiki-bdd) ;; fails as a variable is missing
  (eval-dd [0 0 0 0] wiki-bdd) ;; extra vars are given but they are ignored
  (eval-dd [2] (node 0 0 1 2)) ;; vars dont have to be binary nor do we have only two terminals

  (def built-wiki-bdd
    (binding [*domains* [2 2 2]]
      (dd-or
       (dd-and
        (not-bvar 0)
        (not-bvar 1)
        (not-bvar 2))

       (dd-and
        (bvar 0)
        (bvar 1))

       (dd-and
        (bvar 1)
        (bvar 2)))))

  (let [x3-1 (node 2 1 0)
        x3-2 (node 2 0 1)
        x2-1 (node 1 0 1)
        x2-2 (node 1 x3-1 x3-2)
        x1 (node 0 x2-2 x2-1)]
    (doseq [v0 [0 1]
            v1 [0 1]
            v2 [0 1]]
      (println [v0 v1 v2] (eval-dd [v0 v1 v2] x1))))

  (def not-wiki-bdd
    (binding [*domains* [2 2 2]]
      (apply1 #(if (= % 0) 1 0) wiki-bdd)))

  (doseq [v0 [0 1]
          v1 [0 1]
          v2 [0 1]]
    (println [v0 v1 v2] (eval-dd [v0 v1 v2] wiki-bdd) (eval-dd [v0 v1 v2] not-wiki-bdd)))
  (all-where
   1
   (binding [*domains* [2 3]]
     (let [n (node 1 0 0 1)]
       (node 0 n 1))))
  (all-where 0 wiki-bdd)
  (all-where 1 wiki-bdd)
  ;
  )
