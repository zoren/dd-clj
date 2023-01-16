(ns dd
  "a naive non-optimized implementation of decision diagrams"
  (:require
   [clojure.string :as str]))

(defrecord Node [var-index children])

(def node? (partial instance? Node))

(def ^:dynamic *domains* nil)
(def ^:dynamic *mem-nodes* nil)

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
    (if-some [mem-atom *mem-nodes*]
      (let [k [var-index children]]
        (if-some [mem-node (get @mem-atom k)]
          mem-node
          (let [node (Node. var-index (into [] children))]
            (swap! mem-atom assoc k node)
            node)))
      (Node. var-index (into [] children)))))

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

(defn eval-boolean-exp [env outer-expression]
  ((fn go [expression]
     (if (symbol? expression)
       (env expression)
       (case (first expression)
         :not
         (not (go (second expression)))

         :and
         (apply (fn [& args]
                  (reduce (fn [_ b]
                            (if b b (reduced b))) true args))
                (map go (rest expression)))

         :or
         (apply (fn [& args]
                  (reduce (fn [_ b]
                            (if b (reduced b) b)) false args))
                (map go (rest expression)))

         (throw (ex-info "unknown expression" {:expression expression})))))
   outer-expression))

(defn boolean-exp->bdd [vars-in-order outer-expression]
  (let [var-order (into {} (map-indexed (fn [i v] [v i]) vars-in-order))]
    (binding [*mem-nodes* (atom {})]
      (with-binary
        ((fn go [expression]
           (if (symbol? expression)
             (bvar (var-order expression))
             (case (first expression)
               :not
               (dd-not (go (second expression)))

               :and
               (apply dd-and (map go (rest expression)))

               :or
               (apply dd-or (map go (rest expression)))

               (throw (ex-info "unknown expression" {:expression expression})))))
         outer-expression)))))

(defn get-node-id [d]
  (str "n" (System/identityHashCode d)))

(defn build-graphviz [dd]
  (let [nodes-atom (atom {})
        edges-atom (atom #{})]
    ((fn go [d]
       (let [node-id (get-node-id d)]
         (swap! nodes-atom assoc node-id d)
         (doseq [[i child] (map-indexed vector (:children d))]
           (swap! edges-atom conj
                  {:from node-id
                   :to (get-node-id child)
                   :label (str i)})
           (go child))))
     dd)
    {:nodes @nodes-atom
     :edges @edges-atom}))

(defn gen-dot [{:keys [nodes edges]}]
  (str "digraph G {
        ordering=\"out\";\n"
       (str/join
        (map
         (fn [[node-id node]]
           (if (node? node)
             (str "  " node-id " [label=\"" (:var-index node) "\"];\n")
             (str "  " node-id " [label=\"" node "\" shape=\"box\"];\n")))
         nodes))
       \newline
       (str/join
        (map
         (fn [{:keys [from to label]}]
           (if (= label "0")
             (str "  " from " -> " to " [label=\"" label "\" style=\"dashed\"];\n")
             (str "  " from " -> " to " [label=\"" label "\"];\n")))
         (sort-by :label edges)))
       \newline
       (str/join
        (for [[_ n] (group-by :var-index (vals nodes))]
          (str "  { rank=same; " (clojure.string/join " " (map get-node-id n)) " };\n")))
       "}"))

(comment
  (defn mk-exp [n]
    (when (odd? n)
      (throw (ex-info "n must be even" {:n n})))
    (let [vars (for [i (range n)]
                 (symbol (str "x" i)))]
      (into [:or]
            (map (fn [vs] (into [:and] vs))
                 (partition 2 vars)))))
  (eval-boolean-exp
   '{x0 false x1 true x2 false x3 true}
   (mk-exp 4))
  (mk-exp 8)
  (let [n 8
        vars (for [i (range n)] (symbol (str "x" i)))
        dd
        (boolean-exp->bdd
         vars
         (mk-exp n))
        var-order (into {} (map-indexed (fn [i v] [v i]) vars))]
    (spit "test.dot" (gen-dot (build-graphviz dd)))
    var-order)
  *e
  (defn build-envs [n]
    (if (= n 0)
      [[]]
      (let [ss (build-envs (dec n))]
        (concat (map #(into [0] %) ss) (map #(into [1] %) ss)))))
  (let [n 3
        exp (mk-exp n)
        vars (for [i (range n)]
               (symbol (str "x" i)))
        dd
        (boolean-exp->bdd
         vars
         exp)]
    (doseq [env (build-envs n)]
      (println
       env
       (eval-boolean-exp (into {} (map-indexed (fn [i v] [(symbol (str \x i)) (= 1 v)]) env)) exp)
       (apply eval-dd dd env))))
  (def g *1)
  g
  (println (gen-dot g))
  (group-by :var-index (vals (:nodes g)))

  (let [n 3
        is (range n)
        vars
        (for [i is]
          (symbol (str "x" i)))
        bad-ordering
        (for [i (concat (filter even? is) (filter odd? is))]
          (symbol (str "x" i)))
        dd
        (boolean-exp->bdd
         bad-ordering
         (mk-exp n))]
    (println bad-ordering)
    (count (build-graphviz dd))
    (spit "test_bad.dot" (gen-dot (build-graphviz dd))))

  (def wiki-bdd
    (with-binary
      (let [x3-1 (node 2 true false)
            x3-2 (node 2 false true)
            x2-1 (node 1 false true)
            x2-2 (node 1 x3-1 x3-2)]
        (node 0 x2-2 x2-1))))
  (spit "wiki.dot" (gen-dot (build-graphviz wiki-bdd)))
  ;
  )
