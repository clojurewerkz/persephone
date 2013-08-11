(ns clojurewerkz.persephone.util-test
  (:use clojurewerkz.persephone.util
        clojure.test))

(deftest util-test
  (is (= (str* :foo)
         "foo"))

  (is (= (str* "foo")
         "foo"))

  (is (= (escape "foo")
         "\"foo\""))

  (is (= (escape #"foo")
         "\"foo\""))

  (is (= (escape :foo)
         :foo)))
