(ns cryptogram-test
  (:use clojure.test
        cryptogram))

(def empty-query @#'cryptogram/empty-query)

;;;; API tests

(deftest node-test
  (testing "starting points"
    (are [x y] (= (node x) y)
      *              "node(*)"
      "*"            "node(*)"
      1              "node(1)"
      [1 2 3]        "node(1, 2, 3)"
      "foo"          "node(\"foo\")"
      :foo           "node(\"foo\")"
      {:foo "bar"}   "node(foo = \"bar\")"
      {:foo [1 2 3]} "node(foo = [1, 2, 3])")

    (is (= (node :nodes "name:A")
           "node:nodes(\"name:A\")"))

    (is (= (node :nodes {:foo "bar"})
           "node:nodes(foo = \"bar\")"))))

(defmacro where-query [& constraints]
  `(:where (where empty-query ~@constraints)))

(deftest api-test
  (testing "where"
    (are [expr res] (= (-> empty-query (where expr) :where first) res)
      (= :n.age! 21)          "n.age! = 21"
      (= :n.name! "Tobias")   "n.name! = \"Tobias\""
      (= :n 1)                "n = 1"
      (<> :n 1)               "n <> 1"
      (not= :n 1)             "n <> 1"
      (not (= :n 1))          "not(n = 1)"
      (> :n 1)                "n > 1"
      (>= :n 1)               "n >= 1"
      (< :n 1)                "n < 1"
      (<= :n 1)               "n <= 1"
      (and (= :n 1) (= :m 2)) "(n = 1 and m = 2)"
      (or (= :n 1) (= :m 2))  "(n = 1 or m = 2)")

    (let [q (-> empty-query
                (where (or (= :n 1) (= :m 2))
                       (or (= :i 1) (= :j 2)))
                :where)]
      (is (= (first q)
             "(n = 1 or m = 2)"))

      (is (= (second q)
             "(i = 1 or j = 2)"))))

  (testing "delete"
    (is (= (-> empty-query (delete :n :p) :delete)
           [:n :p]))

    (is (= (-> empty-query (delete :n) (delete :p) :delete)
           [:n :p])))

  (testing "limit"
    (is (thrown? AssertionError (limit empty-query "foo"))))

  (testing "skip"
    (is (thrown? AssertionError (skip empty-query "foo"))))

  (testing "return"
    (is (= (-> empty-query (return :n) :return)
           [:n]))

    (is (= (-> empty-query (return :n) (return :p) :return)
           [:n :p]))))
