(ns cryptogram.syntax
  "Functions for rendering Cypher query syntax."
  (:require [clojure.string :as str]
            [cryptogram.util :refer [str* escape]]))

;; Operators

(defn infix-op [op lhs rhs]
  (let [lhs (str* lhs)
        rhs (escape rhs)]
    (format "%s %s %s" lhs op rhs)))

;; Comparision operators

(def comp-=  (partial infix-op "="))
(def comp->  (partial infix-op ">"))
(def comp-<  (partial infix-op "<"))
(def comp-<= (partial infix-op "<="))
(def comp->= (partial infix-op ">="))
(def comp-<> (partial infix-op "<>"))

;; Boolean operators

(defn bool-op [op]
  (fn [& tests]
    (->> tests
         (str/join (format " %s " op))
         (format "(%s)"))))

(defn bool-not [args]
  (format "not(%s)" (str* args)))

(def bool-and (bool-op "and"))
(def bool-or  (bool-op "or"))

;; Other operators

(def op-is (partial infix-op "IS"))

;; Keywords

(defn kw-distinct [field]
  (str* "DISTINCT " field))

;; Functions

(defn func [name args]
  (format "%s(%s)" name (str* args)))

(def has (partial func "HAS"))
