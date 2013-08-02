(ns cryptogram.util-test
  (:use cryptogram.util
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
