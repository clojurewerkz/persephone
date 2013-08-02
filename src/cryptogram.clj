(ns cryptogram
  "DSL for authoring Cypher queries."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [cryptogram.util :refer [str* escape]]
            [cryptogram.syntax :as syntax])
  (:import java.lang.StringBuilder))

;; Emacs indentation:
(comment
  (define-clojure-indent
    (start* 'defun)
    (start 'defun)))

;; Render helpers

(defn- comma-join [xs]
  (str/join ", " (map str* xs)))

(defn- render-array [xs]
  (format "[%s]" (comma-join xs)))

(defn- render-value [v]
  (cond
   (sequential? v) (render-array v)
   (or (string? v) (keyword? v)) (escape (str* v))
   :else (str v)))

(defn- aggregate [name args]
  (format "%s(%s)" (str* name) (comma-join args)))

;;;; Symbol expansion

(def ^:private sym-map
  {;; Comparision operators
   '=        #'syntax/comp-=
   '>        #'syntax/comp->
   '<        #'syntax/comp-<
   '>=       #'syntax/comp->=
   '<=       #'syntax/comp-<=
   '<>       #'syntax/comp-<>
   'not=     #'syntax/comp-<>
   ;; Boolean operators
   'not      #'syntax/bool-not
   'and      #'syntax/bool-and
   'or       #'syntax/bool-or
   ;; Other operators
   'is       #'syntax/op-is
   ;; Keywords
   'distinct #'syntax/kw-distinct})

(defn- expand-form [form]
  (walk/prewalk
   (fn [val]
     (if-let [f (and (seq? val) (sym-map (first val)))]
       (cons f (rest val))
       val))
   form))

(def ^:private empty-query
  {:start {}
   :match []
   :where []
   :return []
   :delete []
   :order-by []
   :limit nil
   :skip nil})

;;;; Query rendering

;; START

(defn- start-clause
  "Render the START clause of Cypher query."
  [query]
  {:pre [(seq (:start query))]}
  (->> (:start query)
       (map (fn [[k v]] (str* k " = " v)))
       (str/join ", ")
       (format "START %s")))

;; WHERE

(defn- where-clause
  "Render the WHERE clause of Cypher query."
  [query]
  (let [exprs (:where query)]
    (when (seq exprs)
      (str "WHERE " (str/join " and " exprs)))))

;; MATCH

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

(defn- match-clause
  "Render the MATCH clause of a Cypher query."
  [query]
  (when (seq (:match query))
    (->> (:match query)
         (map render-pattern)
         (str/join ", ")
         (format "MATCH %s"))))

;; RETURN

(defn- render-return-clause-part [part]
  (if-let [[aggname & args] (and (vector? part) part)]
    (aggregate aggname args)
    (str* part)))

(defn- omit-return-allowed?
  "Verify whether or not a RETURN clause may be omitted from a Cypher
   query."
  [query]
  (:delete query))

(defn- return-clause
  "Render the RETURN clause of a Cypher query. This may be omitted under
   certain circumstances, such as the presence of a DELETE clause."
  [query]
  (if (and (not (:return query))
           (omit-return-allowed? query))
    ""
    (->> (:return query)
         (map render-return-clause-part)
         (str/join ", ")
         (format "RETURN %s"))))

;; LIMIT

(defn- limit-clause
  "Render the LIMIT clause of a Cypher query."
  [query]
  (when-let [n (:limit query)]
    (str "LIMIT " n)))

;; SKIP

(defn- skip-clause
  "Render the SKIP clause of a Cypher query."
  [query]
  (when-let [n (:skip query)]
    (str "SKIP " n)))

;; DELETE

(defn- delete-clause
  "Render the DELETE clause of a Cypher query."
  [query]
  (when (seq (:delete query))
    (->> (:delete query)
         (map str*)
         (str/join ", ")
         (format "DELETE %s"))))

;; ORDER BY

(defn- order-by-clause
  "Render the ORDER BY clause of a Cypher query."
  [query]
  (when (seq (:order-by query))
    (->> (:order-by query)
         (map str*)
         (str/join ", ")
         (format "ORDER BY %s"))))

;;;; API

(defn render-query
  "Render a full Cypher query."
  [query]
  (->> query
       ((juxt start-clause
              match-clause
              where-clause
              delete-clause
              return-clause
              limit-clause
              skip-clause))
       (remove nil?)
       (str/join "\n")))

(defn- index-lookup [m]
  {:pre [(map? m)]}
  (let [[k v] (first m)]
    (format "%s = %s" (str* k) (render-value v))))

(defn- starting-point [v]
  (cond
   (or (= * v) (= "*" (str* v))) "*"
   (integer? v) v
   (sequential? v) (str/join ", " (map str* v))
   (map? v) (index-lookup v)
   :else (render-value v)))

(defn- query-start-fn [type]
  {:pre [(string? type)]}
  (fn
    ([v]
       (let [v (starting-point v)]
         (format "%s(%s)" type v)))
    ([index v]
       (let [v (if (map? v)
                 (index-lookup v)
                 (render-value v))]
         (format "%s:%s(%s)" type (str* index) v)))))

(def ^:private query-start-doc
  "Return a {x} starting point, optionally with an index.

   Ex.
     ;; Equivalent to `{x}(*)`.
     ({x} *)
     ;; Equivalent to `{x}(1)`.
     ({x} 1)
     ;; Equivalent to `{x}(1, 2, 3)`.
     ({x} [1, 2, 3])
     ;; Both equivalent to `{x}(\"foo\")`.
     ({x} :foo)
     ({x} \"foo\")
     ;; Equivalent to `{x}(foo = \"bar\")`.
     ({x} {:foo \"bar\"})
     ;; Equivalent to `{x}:index(\"name:A\")`.
     ({x} :index \"name:A\")")

(def ^:private query-start-arglists
  '(list '[v] '[index v]))

(defmacro ^:private defquerystartfn [name]
  (let [doc (str/replace query-start-doc #"\{x}" (str name))]
    `(def ~(vary-meta name assoc
                      :doc doc
                      :arglists query-start-arglists)
       (query-start-fn ~(str name)))))

(defquerystartfn node)
(defquerystartfn rel)

;; Used by `where` to update the query.
(defn- where*
  ([query constraint]
     (update-in query [:where] conj constraint))
  ([query constraint & more]
     (reduce where* (where* query constraint) more)))

(defmacro where
  "Add one or more contraints to a Cypher query WHERE clause.

   Ex.
     ;; Equivalent to `START n = node(*) WHERE n.p! = 1 RETURN n`
     (start* {:n (node *)}
       (where (= :n.p! 1))
       (return :n))"
  [query & constraints]
  `(#'where* ~query ~@(expand-form constraints)))

(defn match
  "Add one or more patterns to a Cypher query MATCH clause.

   Ex.
     ;; Equivalent to `START n = node(1) MATCH n-[:KNOWS]-p RETURN n, p`
     (start* {:n (node 1)}
       (match [:n [:KNOWS] :p])
       (return :n :p))"
  ([query pattern]
     (update-in query [:match] conj pattern))
  ([query pattern & more]
     (reduce match (match query pattern) more)))

(defn return
  "Add one or more expression to a Cypher query RETURN clause.

   Ex.
     ;; Equivalent to `START n = node(1) RETURN n, m`
     (start* {:n (node 1) :m (node 2)}
       (return :n :m))"
  ([query expr]
     (update-in query [:return] conj expr))
  ([query expr & more]
     (reduce return (return query expr) more)))

(defn limit
  "Add a LIMIT clause to a Cypher query.

   Ex.
     ;; Equivalent to `START n = node(*) RETURN n LIMIT 5`
     (start* {:n (node *)}
       (return :n)
       (limit 5))"
  [query n]
  {:pre [(number? n)]}
  (assoc query :limit (Math/round (float n))))

(defn skip
  "Add a SKIP clause to a Cypher query.

   Ex.
     ;; Equivalent to `START n = node(*) RETURN n SKIP 5`
     (start* {:n (node *)}
       (return :n)
       (skip 5))"
  [query n]
  {:pre [(number? n)]}
  (assoc query :skip (Math/round (float n))))

(defn delete
  "Add one or more rows to a Cypher query DELETE clause.

   Ex.
     ;; Equivalent to `START n = node(1) DELETE n`
     (start* {:n (node 1)}
       (delete :n))"
  ([query row]
     (update-in query [:delete] conj row))
  ([query row & more]
     (reduce delete (delete query row) more)))

(defn order-by
  "Add one or more rows to a Cypher query ORDER BY clause.

   Ex.
     ;; Equivalent to `START n = node(1) RETURN n ORDER BY n.age`
     (start* {:n (node *)}
       (order-by :n.name)
       (return n))"
  ([query row]
     (update-in query [:order] conj row))
  ([query row & more]
     (reduce order-by (order-by query row) more)))

;; This is named `begin` instead of `start` because `start` is the
;; name of the public macro.
(defn- begin [query start-map]
  {:pre [(and (map? start-map) (seq start-map))]}
  (update-in query [:start] merge start-map))

(defmacro start*
  "Start a Cypher query to but do not render it."
  [start-map & body]
  (let [init (partial begin empty-query)]
    `(-> ~(init start-map) ~@(expand-form body))))

(defmacro start
  "Construct and render a Cypher query."
  [start-map & body]
  `(render-query (start* ~start-map ~@body)))
