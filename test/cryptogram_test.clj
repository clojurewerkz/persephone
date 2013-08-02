(ns cryptogram-test
  (:use clojure.test
        cryptogram))

(def empty-query (deref #'cryptogram/empty-query))

;;;; API tests

(deftest api-test
  (testing "starting points"
    (is (= (node *)
           "node(*)"))

    (is (= (node 1)
           "node(1)"))

    (is (= (node [1 2 3])
           "node(1, 2, 3)"))

    (is (= (node "foo")
           "node(\"foo\")"))

    (is (= (node :foo)
           "node(\"foo\")"))

    (is (= (node {:foo "bar"})
           "node(foo = \"bar\")"))

    (is (= (node {:foo [1 2 3]})
           "node(foo = [1, 2, 3])"))

    (is (= (node :nodes "name:A")
           "node:nodes(\"name:A\")"))

    (is (= (node :nodes {:foo "bar"})
           "node:nodes(foo = \"bar\")")))

  (testing "start*"
    (is (= (:start (start* {:n (node 1)}))
           {:n "node(1)"})))

  (testing "limit"
    (is (thrown? AssertionError
                 (limit empty-query "foo")))))

;;;; Rendering tests
(def start-clause    #'cryptogram/start-clause)
(def where-clause    #'cryptogram/where-clause)
(def match-clause    #'cryptogram/match-clause)
(def return-clause   #'cryptogram/return-clause)
(def limit-clause    #'cryptogram/limit-clause)
(def skip-clause     #'cryptogram/skip-clause)
(def delete-clause   #'cryptogram/delete-clause)
(def order-by-clause #'cryptogram/order-by-clause)

;; Helpers

(defmacro where= [& constraints]
  (assert (string? (last constraints)))
  `(let [q# (start* {:n (node 1)}
              (where ~@(butlast constraints)))
         res# ~(last constraints)]
     (= (where-clause q#)
        res#)))

(defn match= [& patterns]
  {:pre [(string? (last patterns))]}
  (let [result (last patterns)]
    (= (match-clause (apply match empty-query (butlast patterns)))
       result)))

(defn return= [& returns]
  {:pre [(string? (last returns))]}
  (let [result (last returns)]
    (= (return-clause (apply return empty-query (butlast returns)))
       result)))

(defn limit= [amt result]
  {:pre [(string? result)]}
  (= (limit-clause (limit empty-query amt))
     result))

(defn skip= [amt result]
  {:pre [(string? result)]}
  (= (skip-clause (skip empty-query amt))
     result))

(defn delete= [& rows]
  {:pre [(string? (last rows))]}
  (let [result (last rows)]
    (= (delete-clause (apply delete empty-query (butlast rows)))
       result)))

(defn order-by= [& rows]
  {:pre [(string? (last rows))]}
  (let [result (last rows)]
    (= (order-by-clause (apply order-by empty-query (butlast rows)))
       result)))

(deftest render-test
  (testing "start-clause"
    (is (= (start-clause (start* {:n (node 1)}))
           "START n = node(1)"))

    (let [clause (start-clause
                  (start* {:n "node(1)" :r "rel(2)"}))]
      (is (and (re-find #"r = rel\(2\)" clause)
               (re-find #"n = node\(1\)" clause)))))

  (testing "where-clause"
    (is (where= (= :n.age! 21)
                "WHERE n.age! = 21"))

    (is (where= (= :n.name! "Tobias")
                "WHERE n.name! = \"Tobias\""))

    (is (where= (= :n 1)
                "WHERE n = 1"))

    (is (where= (<> :n 1)
                "WHERE n <> 1"))

    (is (where= (not= :n 1)
                "WHERE n <> 1"))

    (is (where= (not (= :n 1))
                "WHERE not(n = 1)"))

    (is (where= (> :n 1)
                "WHERE n > 1"))

    (is (where= (>= :n 1)
                "WHERE n >= 1"))

    (is (where= (< :n 1)
                "WHERE n < 1"))

    (is (where= (<= :n 1)
                "WHERE n <= 1"))

    (is (where= (and (= :n 1) (= :m 2))
                "WHERE (n = 1 and m = 2)"))

    (is (where= (and (or (= :n 1) (= :m 2))
                     (or (= :i 1) (= :j 2)))
                "WHERE ((n = 1 or m = 2) and (i = 1 or j = 2))"))

    (is (where= (or (= :n 1) (= :m 2))
                "WHERE (n = 1 or m = 2)"))

    (is (where= (or (= :n 1) (= :m 2))
                (or (= :i 1) (= :j 2))
                "WHERE (n = 1 or m = 2) and (i = 1 or j = 2)")))

  (testing "match-clause"
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

  (testing "return-clause"
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

  (testing "limit-clause"
    (is (limit= 5
                "LIMIT 5"))

    (is (limit= 1.7
                "LIMIT 2")))

  (testing "skip-clause"
    (is (skip= 5
                "SKIP 5"))

    (is (skip= 1.7
                "SKIP 2")))

  (testing "delete-clause"
    (is (delete= :n
                 "DELETE n"))

    (is (delete= :n :p
                 "DELETE n, p"))))
