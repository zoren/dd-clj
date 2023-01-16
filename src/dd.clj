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

(defmacro with-domains [domains & body]
  `(binding [*domains* ~domains] ~@body))

(defn eval-dd [node & env]
  (loop [node node]
    (if (node? node)
      (recur
       (nth (:children node) (nth env (:var-index node))))
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

;; utility definitions for binary decision diagrams

(defn bvar [var-index] (node var-index false true))

(defn dd-not [dd] (apply1 false? dd))

(def not-bvar (comp dd-not bvar))

(defn dd-and
  ([] true)
  ([dd] dd)
  ([dd1 dd2] (apply2 (fn [a b] (and a b)) dd1 dd2))
  ([dd1 dd2 & more] (dd-and dd1 (apply dd-and dd2 more))))

(defn dd-or
  ([] false)
  ([dd] dd)
  ([dd1 dd2] (apply2 (fn [a b] (or a b)) dd1 dd2))
  ([dd1 dd2 & more] (dd-or dd1 (apply dd-or dd2 more))))

(defmacro with-binary [& body]
  `(with-domains (constantly 2) ~@body))
