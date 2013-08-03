(ns cryptogram.compiler-test
  (:use cryptogram.compiler
        clojure.test))

(defmacro defclause= [name clause-fn key]
  ;; Helper to mitigate redundancy in some of the tests.
  `(defn ~name [& xs#]
     {:pre [(string? (last xs#))]}
     (let [res# (last xs#)]
       (= (~clause-fn {~key (vec (butlast xs#))})
          res#))))

;;;; START

(deftest start-cluase-test
  (is (= (start-clause {:start {:n "node(1)"}})
         "START n = node(1)"))
  
  (let [clause (start-clause {:start {:n "node(1)" :r "rel(2)"}})]
    (is (and (re-find #"n = node\(1\)" clause)
             (re-find #"r = rel\(2\)" clause)))))

;;;; MATCH

(defclause= match= match-clause :match)

(deftest match-clause-test
  (is (match= [:n [:KNOWS] :person]
              "MATCH n-[:KNOWS]-person"))

  (is (match= [:n '- [:KNOWS] "->" :person]
              "MATCH n-[:KNOWS]->person"))

  (is (match= [:n :<- [:KNOWS] :person]
              "MATCH n<-[:KNOWS]-person"))

  (is (match= [:n [:r :KNOWS] :person]
              "MATCH n-[r:KNOWS]-person"))

  (is (match= [:n [:r :LIKES :DISLIKES] :person]
              "MATCH n-[r:LIKES|DISLIKES]-person"))

  (is (match= [:n [:LOVES] [:wife {:name "Jenny"}]]
              "MATCH n-[:LOVES]-(wife {name: \"Jenny\"})"))

  (is (match= [:n ["r"] :p]
              "MATCH n-[r]-p"))

  (is (match= [:n "-->" () "<--" :p]
              "MATCH n-->()<--p"))

  (is (match= [:n [:?] :p]
              "MATCH n-[?]-p"))

  (is (match= [:n [:?*] :p]
              "MATCH n-[?*]-p"))

  (is (match= [:n ["r"] :m]
              [:m ["r"] :l]
              "MATCH n-[r]-m, m-[r]-l")))

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
