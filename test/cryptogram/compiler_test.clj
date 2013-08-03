(ns cryptogram.compiler-test
  (:use cryptogram.compiler
        clojure.test))

;;;; Helpers

(deftest helpers-test
  (testing "render-value"
    (are [v res] (= (render-value v) res)
      [1 2 3] "[1, 2, 3]"
      "foo"   "\"foo\""
      :foo    "\"foo\""
      1/2     "0.5"
      nil     "NULL" 
      5       "5")))

(defmacro defclause= [name clause-fn key]
  ;; Helper to mitigate redundancy in some of the tests.
  `(defn ~name [& xs#]
     (let [res# (last xs#)]
       (= (~clause-fn {~key (vec (butlast xs#))})
          res#))))

;;;; START

(deftest start-cluase-test
  (let [clause (start-clause
                {:start {:n "node(1)" :r "rel(2)"}})]
    (is (re-find #"n = node\(1\)" clause))
    (is (re-find #"r = rel\(2\)" clause))))

;;;; MATCH

(defclause= match= match-clause :match)

(deftest match-clause-test
  (are [pattern result] (match= pattern result)
    [:n [:K] :p]            "MATCH n-[:K]-p"
    [:n '- [:K] "->" :p]    "MATCH n-[:K]->p"
    [:n :<- [:K] :p]        "MATCH n<-[:K]-p"
    [:n [:r :K] :p]         "MATCH n-[r:K]-p"
    [:n [:r :L :D] :p]      "MATCH n-[r:L|D]-p"
    [:n [:Y] [:w {:x "J"}]] "MATCH n-[:Y]-(w {x: \"J\"})"
    [:n ["r"] :p]           "MATCH n-[r]-p"
    [:n "-->" () "<--" :p]  "MATCH n-->()<--p"
    [:n [:?] :p]            "MATCH n-[?]-p"
    [:n [:?*] :p]           "MATCH n-[?*]-p")

  (is (match= [:n ["r"] :m] [:m ["r"] :l]
              "MATCH n-[r]-m, m-[r]-l"))

  (is (match= [] nil)))

;;;; WHERE

(defclause= where= where-clause :where)

(deftest where-clause-test
  (is (where= "n.age! = 21"
              "WHERE n.age! = 21"))

  (is (where= "n.age! >= 21" "n.age! =< 65"
              "WHERE n.age! >= 21 and n.age! =< 65")))

;;;; LIMIT

(deftest limit-clause-test
  (is (= (limit-clause {:limit 1})
         "LIMIT 1")))

;;;; SKIP

(deftest skip-clause-test
  (is (= (skip-clause {:skip 1})
         "SKIP 1")))

;;;; DELETE

(defclause= delete= delete-clause :delete)

(deftest delete-clause-test
  (is (delete= :n
               "DELETE n"))

  (is (delete= :n :p
               "DELETE n, p")))

;;;; ORDER BY

(defclause= order-by= order-by-clause :order-by)

(deftest order-by-clause-test
  (is (order-by= :n
                 "ORDER BY n"))

  (is (order-by= :n :p
                 "ORDER BY n, p"))

  (is (order-by= :n :p :DESC
                 "ORDER BY n, p DESC"))

  (is (nil? (order-by-clause {:order [:DESC]}))))

;;;; RETURN

(defclause= return= return-clause :return)

(deftest return-clause-test
  (let [q {:delete [:n] :return []}]
    (is (nil? (return-clause q))))

  (is (return= :n
               "RETURN n"))

  (is (return= :n :p
               "RETURN n, p"))

  (is (return= :n "COUNT(n)"
               "RETURN n, COUNT(n)"))

  (is (return= :n [:count :n]
               "RETURN n, count(n)"))

  (is (return= [:max :n.age!]
               "RETURN max(n.age!)"))

  (is (return= [:percentile_disc :n.property 0.5]
               "RETURN percentile_disc(n.property, 0.5)")))
