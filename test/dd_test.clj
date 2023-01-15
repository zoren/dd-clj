(ns dd-test
  (:require
   [clojure.test :refer [deftest is]]
   [dd :refer [node eval-dd all-where apply1 apply2
               with-domains with-binary
               bvar not-bvar dd-not dd-or dd-and]]))

(deftest eval-dd-test
  (with-domains [3]
    (is (= \c (eval-dd (node 0 \a \b \c) 2))) ;; vars dont have to be binary nor do we have only two terminals
    )
  (let [wiki-bdd
        (with-binary
          (let [x3-1 (node 2 true false)
                x3-2 (node 2 false true)
                x2-1 (node 1 false true)
                x2-2 (node 1 x3-1 x3-2)]
            (node 0 x2-2 x2-1)))]
    (is (= [{}] (all-where false false)))
    (is (= [] (all-where true false)))
    (is (= [{2 1, 1 0, 0 0}
            {2 0, 1 1, 0 0}
            {1 0, 0 1}] 
           (all-where false wiki-bdd)))
    (is (= [{2 0, 1 0, 0 0} 
            {2 1, 1 1, 0 0} 
            {1 1, 0 1}]
           (all-where true wiki-bdd)))

    (is (= wiki-bdd
           (with-binary
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
               (bvar 2))))))
    (is (eval-dd wiki-bdd 0 0 0 0)) ;; extra vars are given but they are ignored
    (doseq [[input expected]
            [[[0 0 0] 1]
             [[0 0 1] 0]
             [[0 1 0] 0]
             [[0 1 1] 1]
             [[1 0 0] 0]
             [[1 0 1] 0]
             [[1 1 0] 1]
             [[1 1 1] 1]]]
      (is (= (= 1 expected) (apply eval-dd wiki-bdd input))))
    (let [not-wiki-bdd (with-binary (dd-not wiki-bdd))]
      (doseq [[input expected]
              [[[0 0 0] 0]
               [[0 0 1] 1]
               [[0 1 0] 1]
               [[0 1 1] 0]
               [[1 0 0] 1]
               [[1 0 1] 1]
               [[1 1 0] 0]
               [[1 1 1] 0]]]
        (is (= (= 1 expected) (apply eval-dd not-wiki-bdd input)))))))
