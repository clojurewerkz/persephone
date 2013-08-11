(ns clojurewerkz.persephone
  "DSL for authoring Cypher queries."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojurewerkz.persephone.util :refer [str* escape comma-join]]
            [clojurewerkz.persephone.syntax :as syntax]
            [clojurewerkz.persephone.compiler :as compiler]))

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

;;;; Node and relationship helpers

(defn- index-lookup [m]
  {:pre [(map? m)]}
  (let [[k v] (first m)]
    (format "%s = %s" (str* k) (compiler/render-value v))))

(defn- starting-point [v]
  (cond
   (or (= * v) (= "*" (str* v))) "*"
   (integer? v) v
   (sequential? v) (comma-join v)
   (map? v) (index-lookup v)
   :else (compiler/render-value v)))

(defn- query-start-fn [type]
  {:pre [(string? type)]}
  (fn
    ([v]
       (let [v (starting-point v)]
         (format "%s(%s)" type v)))
    ([index v]
       (let [v (if (map? v)
                 (index-lookup v)
                 (compiler/render-value v))]
         (format "%s:%s(%s)" type (str* index) v)))))

;; Rather than place all of this in the documenation in the macro
;; below and make a mess, `query-start-doc` is kept separate for the
;; sake of cleanliness.
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

(defmacro ^:private defquerystartfn
  "Used to define `node` and `rel`."
  [name]
  (let [doc (str/replace query-start-doc #"\{x}" (str name))]
    `(def ~(vary-meta name assoc
                      :doc doc
                      :arglists '(list '[v] '[index v]))
       (query-start-fn ~(str name)))))

;;;; API

(declare render-query)

;; This is named `begin` instead of `start` because `start` is the
;; name of the public macro.
(defn- begin [query start-map]
  (update-in query [:start] merge start-map))

(defmacro start*
  "Start a Cypher query to but do not render it."
  [start-map & body]
  (assert (and (map? start-map) (seq start-map)))
  (let [init (partial begin empty-query)]
    `(-> ~(init start-map) ~@(expand-form body))))

(defmacro start
  "Construct and render a Cypher query."
  [start-map & body]
  (assert (and (map? start-map) (seq start-map)))
  `(render-query (start* ~start-map ~@body)))

(defquerystartfn node)

(defquerystartfn rel)

(defn- where*
  ;; Used by `where` to update the query.
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

(defn limit
  "Add a LIMIT clause to a Cypher query.

   Ex.
     ;; Equivalent to `START n = node(*) RETURN n LIMIT 5`
     (start* {:n (node *)}
       (return :n)
       (limit 5))"
  [query n]
  {:pre [(number? n)]}
  (assoc query :limit n))

(defn skip
  "Add a SKIP clause to a Cypher query.

   Ex.
     ;; Equivalent to `START n = node(*) RETURN n SKIP 5`
     (start* {:n (node *)}
       (return :n)
       (skip 5))"
  [query n]
  {:pre [(number? n)]}
  (assoc query :skip n))

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

(def ^{:doc "Render a full Cypher query."
       :arglists '([query])}
  render-query compiler/compile-query)
