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

;;;; Clause testing

(defmacro defclause= [name clause-fn key]
  ;; Helper to mitigate redundancy in some of the tests.
  `(defn ~name [& xs#]
     (let [res# (last xs#)
           input# (~clause-fn {~key (vec (butlast xs#))})]
       (= input# res#))))

(def base {:start {:n "node(1)" :r "rel(2)"}})

;;;; START

(deftest start-cluase-test
  (let [clause (start-clause base)]
    (is (re-find #"n = node\(1\)" clause))
    (is (re-find #"r = rel\(2\)" clause))))

;;;; MATCH

(defn substring? [^String s ^String substring]
  (.contains s substring))

(defn match [q m]
  (update-in q [:match] conj m))

(deftest match-clause-test
  (are [pattern result] (-> (match base pattern) match-clause (substring? result))
    "n-[:K]-p"              "n-[:K]-p"
    [:n [:K] :p]            "n-[:K]-p"
    [:n [:K] :p]            "n-[:K]-p"
    [:n '- [:K] "->" :p]    "n-[:K]->p"
    [:n :<- [:K] :p]        "n<-[:K]-p"
    [:n [:r :K] :p]         "n-[r:K]-p"
    [:n [:r :L :D] :p]      "n-[r:L|D]-p"
    [:n [:Y] [:w {:x "J"}]] "n-[:Y]-(w {x: \"J\"})"
    [:n ["r"] :p]           "n-[r]-p"
    [:n "-->" () "<--" :p]  "n-->()<--p"
    [:n [:?] :p]            "n-[?]-p"
    [:n [:?*] :p]           "n-[?*]-p"
    {:p [:n [:K] :p]}       "p = n-[:K]-p")

  (let [res (-> base
                (match [:n ["r"] :m])
                (match [:m ["r"] :l])
                match-clause)]
    (is (substring? res "n-[r]-m"))
    (is (substring? res "m-[r]-l"))
    (is (re-find #"[A-Z][0-9]+ = n-\[r\]-m" res))
    (is (re-find #"[A-Z][0-9]+ = m-\[r\]-l" res)))

  (is (nil? (-> base (match []) match-clause)))

  (is (thrown? RuntimeException
               (-> (match base {:p {:q "x"}}) match-clause))))

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
               "RETURN percentile_disc(n.property, 0.5)"))

  (is (return= [:timestamp]
               "RETURN timestamp()")))
