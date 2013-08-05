(ns cryptogram.compiler
  (:require [clojure.string :as str]
            [cryptogram.util :refer [str* escape comma-join]]))

;;;; Helpers

(declare render-array)

(defn render-value [v]
  (cond
   (sequential? v) (render-array v)
   (or (string? v) (keyword? v)) (escape (str* v))
   (ratio? v) (str (float v))
   (nil? v) "NULL"
   :else (str v)))

(defn render-array [xs]
  (format "[%s]" (comma-join render-value xs)))

(defn- render-aggregate
  "Render an aggregate.

   Ex.
     (render-aggregate :count [:n])
     => \"count(n)\"
     (render-aggregate :percentile_disc [:n.property 0.5])
     => \"percentile_disc(n, 0.5)\""
  [name args]
  (format "%s(%s)" (str* name) (comma-join args)))

;;;; START

(defn start-clause
  "Render the START clause of Cypher query."
  [query]
  {:pre [(seq (:start query))]}
  (->> (:start query)
       (map (fn [[k v]] (str* k " = " v)))
       (str/join ", ")
       (format "START %s")))

;;;; MATCH

(defn- render-property [[k v]]
  (str* k ": " (render-value v)))

(defn- render-properties [m]
  (->> (map render-property m)
       (str/join ", ")
       (format "{%s}")))

(defn- rel-strategy [[x y & _]]
  (when (and x y)
    (if (map? y) ::property ::alias)))

(defmulti ^:private render-rel rel-strategy)

;; Render patterns containing properties.
;;
;; Examples:
;;
;;   (render-rel [:wife {:name "Gunhild"}])
;;   => "(wife {name: \"Gunhild\"})"
;;
(defmethod render-rel ::property [[ident props]]
  (format "(%s %s)" (str* ident) (render-properties props)))

;; Render patterns containing an alias and one or more named
;; relationships.
;;
;; Examples:
;;
;;   (render-rel [:r :KNOWS])
;;   => "[r:KNOWS]"
;;   (render-rel [:r :LIKES :DISLIKES])
;;   => "[r:LIKES|DISLIKES]"
;;
(defmethod render-rel ::alias [[alias rel & more]]
  (let [[alias rel] (map str* [alias rel])
        rel (format "%s:%s" alias rel)
        rel (if (seq more)
              (->> (map str* more)
                   (cons rel)
                   (str/join "|"))
              rel)]
    (str "[" rel "]")))

;; Render patterns containing only a single relationship. Pattern
;; values such as :?, :?*, :*n, :*n..m (where n and m are integers),
;; and named relationships (ie. :r:REL_TYPE) are rendered as strings.
;; All other values are rendered as is.
;;
;; Examples:
;;
;;   (render-rel [:?])
;;   => "[?]"
;;   (render-rel [:r:KNOWS])
;;   => "[r:KNOWS]"
;;   (render-rel ["r"])
;;   => "[r]"
;;   (render-rel [:LOVES])
;;   => "[:LOVES]"
;;
(defmethod render-rel :default [[x]]
  (let [rel
        (if (-> #"(?:\?\*?|\*\d+(?:\.\.[1-9]\d*)?|[a-zA-Z_]+:[a-zA-Z_]+)"
                (re-matches (str* x)))
          (str* x)
          x)]
    (str "[" rel "]")))

;; Render an individual pattern part.
(defn- render-pattern-part [p]
  (cond
   (vector? p) (render-rel p)
   (and (list? p) (empty? p)) "()"
   :else (str* p)))

;; Render a complete Cypher pattern. Patterns may be interleaved with
;; or without path symbols (ie. `-`, `->`, `-->`, etc.). When a path
;; symbol is omitted between elements of the pattern a `-`
;; relationship is implied.
;;
;;   (render-pattern [:me "-->" :friend [:?] "->" :friend_of_friend])
;;   => "me-->friend-[?]->friend_of_friend
;;   (render-pattern [:me [:MARRIED_TO] [:wife {:name "Gunhild"}]])
;;   => "me-[:MARRIED_TO]-(wife {name: \"Gunhild\"})
;;
(defn- render-pattern [pat]
  (loop [ps (rest pat)
         sb (StringBuilder. (render-pattern-part (first pat)))]
    (if-let [p (first ps)]
      (let [p1 (render-pattern-part p)]
        (if (#{"-" "--" "->" "-->" "<-" "<--"} p1)
          (let [p2 (render-pattern-part (second ps))]
            (recur (nthnext ps 2) (.. sb (append p1) (append p2))))
          (recur (cons "-" ps) sb)))
      (str sb))))

(defn match-clause
  "Render the MATCH clause of a Cypher query."
  [query]
  (let [patterns (remove empty? (:match query))]
    (when (seq patterns)
      (->> (comma-join render-pattern patterns)
           (format "MATCH %s")))))

;;;; WHERE

(defn where-clause
  "Render the WHERE clause of Cypher query."
  [query]
  (let [exprs (:where query)]
    (when (seq exprs)
      (str "WHERE " (str/join " and " exprs)))))

;;;; LIMIT

(defn limit-clause
  "Render the LIMIT clause of a Cypher query."
  [query]
  (when-let [n (:limit query)]
    (str "LIMIT " (Math/round (float n)))))
;;;; SKIP

(defn skip-clause
  "Render the SKIP clause of a Cypher query."
  [query]
  (when-let [n (:skip query)]
    (str "SKIP " (Math/round (float n)))))

;;;; DELETE

(defn delete-clause
  "Render the DELETE clause of a Cypher query."
  [query]
  (when (seq (:delete query))
    (->> (:delete query)
         comma-join
         (format "DELETE %s"))))

;;;; ORDER BY

(def ^:private descending-flags #{:desc :DESC "desc" "DESC"})

(defn- descending-order? [query]
  (boolean (some descending-flags (:order-by query))))

(defn order-by-clause
  "Render the ORDER BY clause of a Cypher query."
  [query]
  (when (seq (:order-by query))
    (let [order-bys (:order-by query)
          desc? (descending-order? query)
          order-bys (if desc? 
                      (remove descending-flags order-bys)
                      order-bys)
          fmt (str "ORDER BY %s" (when desc? " DESC"))]
      (when (seq order-bys)
        (format fmt (comma-join order-bys))))))

;;;; RETURN

(defn- render-return-clause-part [part]
  (if-let [[name & args] (and (vector? part) part)]
    (render-aggregate name args)
    (str* part)))

(defn- omit-return-allowed?
  "Verify whether or not a RETURN clause may be omitted from a Cypher
   query."
  [query]
  (seq (:delete query)))

(defn return-clause
  "Render the RETURN clause of a Cypher query. This may be omitted under
   certain circumstances, such as the presence of a DELETE clause."
  [query]
  (if (and (not (seq (:return query)))
           (omit-return-allowed? query))
    nil
    (->> (:return query)
         (map render-return-clause-part)
         (str/join ", ")
         (format "RETURN %s"))))

(defn compile-query
  "Render a full Cypher query."
  [query]
  (->> query
       ((juxt start-clause
              match-clause
              where-clause
              delete-clause
              limit-clause
              skip-clause
              return-clause
              order-by-clause))
       (remove nil?)
       (str/join "\n")))
