(ns cryptogram
  (:require [clojure.string :as str]
            [clojure.walk :as walk])
  (:import java.lang.StringBuilder))

;;;; Cypher Query DSL

;; Example usage:
(comment
  ;; Query:
  (start {:n (node [3 1])}
    (where (or (and (< :n.age 30)
                    (= :n.name "Tobias"))
               (not (= :n.name "Tobias"))))
    (return :n))

  ;; Result:
  ;; START n=node(3, 1)
  ;; WHERE ((n.age < 30 and n.name = "Tobias") or not(n.name = "Tobias"))
  ;; RETURN n

  ;; Query:
  (start {:a (node 1) :b (node [3 2])}
    (match [:a "<-" [:r?] "-" :b])
    (where (is r null))
    (return :b))

  ;; Result:
  ;; START a = node(1), b = node(3,2)
  ;; MATCH a<-[r?]-b
  ;; WHERE r is null
  ;; RETURN b

  ;; Queries are fully composable.
  (let [base (start* {:a (node 1) :b (node [3 2])})]
    (-> base
        (match [:a "<-" [:r?] "-" :b])
        (where (is r null))
        (return :b)))

  ;; Emacs indentation:
  (define-clojure-indent
    (start* 'defun)
    (start 'defun)))

;;;; Utilities

(defn- ^String str*
  ([] "")
  ([x] (if (keyword? x)
         (name x)
         (str x)))
  ([x & more]
     (let [sb (StringBuilder. ^String (str* x))]
       (str (reduce #(.append % (str* %2)) sb more)))))

(defn- escape [x]
  (if (or (string? x)
          (instance? java.util.regex.Pattern x))
    (format "\"%s\"" x)
    x))

;;;; Operators and functions

(defn- boolean-op [op & tests]
  (->> tests
       (str/join (format " %s " op))
       (format "(%s)")))

(defn- infix-op [op lhs rhs]
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

(defn bool-not [args]
  (format "not(%s)" (str* args)))

(def bool-and (partial boolean-op "and"))
(def bool-or  (partial boolean-op "or"))

;; Other operators
(def ^:private op-is (partial infix-op "IS"))

;; Keywords

(defn ^:private k-distinct [field]
  (str* "DISTINCT " field))

;; Functions

(defn- aggregate [name args]
  (->> (map str* args)
       (str/join ", ")
       (format "%s(%s)" (str* name))))

(defn- func [name args]
  (format "%s(%s)" name (str* args)))

(def has (partial func "HAS"))

;; Render helpers

(defn- render-array [v]
  (->> (map str* v)
       (str/join ", ")
       (format "[%s]")))

(defn- render-value [v]
  (cond
   (sequential? v) (render-array v)
   (or (string? v) (keyword? v)) (escape (str* v))
   :else (str v)))

;;;; Symbol expansion

(def ^:private sym-map
  {;; Comparision operators
   '=        #'comp-=
   '>        #'comp->
   '<        #'comp-<
   '>=       #'comp->=
   '<=       #'comp-<=
   '<>       #'comp-<>
   'not=     #'comp-<>
   ;; Boolean operators
   'not      #'bool-not
   'and      #'bool-and
   'or       #'bool-or
   ;; Other operators
   'is       #'op-is
   ;; Keywords
   'distinct #'k-distinct})

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
   :limit nil})

;; START

;; This is named `begin` instead of `start` because `start` is the
;; name of the public macro.
(defn- begin [query start-map]
  {:pre [(and (map? start-map) (seq start-map))]}
  (update-in query [:start] merge start-map))

(defn- start-clause [query]
  {:pre [(seq (:start query))]}
  (->> (:start query)
       (map (fn [[k v]] (str* k " = " v)))
       (str/join ", ")
       (format "START %s")))

;; WHERE

(defn- where-clause [query]
  (let [exprs (:where query)]
    (when (seq exprs)
      (str "WHERE " (str/join " and " exprs)))))

;; MATCH

;; Pattern rendering.

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

(defn- match-clause [query]
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

(defn- omit-return-allowed? [query]
  (:delete query))

(defn- return-clause [query]
  (if (and (not (:return query))
           (omit-return-allowed? query))
    ""
    (->> (:return query)
         (map render-return-clause-part)
         (str/join ", ")
         (format "RETURN %s"))))

;; LIMIT

(defn- limit-clause [query]
  (when-let [n (:limit query)]
    (str "LIMIT " n)))

;; DELETE

(defn- delete-clause [query]
  (when (seq (:delete query))
    (->> (:delete query)
         (map str*)
         (str/join ", ")
         (format "DELETE %s"))))

;;;; Query rendering and executing

(defn render-query [query]
  (->> query
       ((juxt start-clause
              match-clause
              where-clause
              delete-clause
              return-clause
              limit-clause))
       (remove nil?)
       (str/join "\n")))

;;;; API

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
    `(def ~(vary-meta name assoc :doc doc :arglists query-start-arglists)
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

(defn delete
  "Add one or more rows to a Cypher query DELETE clause."
  ([query row]
     (update-in query [:delete] conj row))
  ([query row & more]
     (reduce delete (delete query row) more)))

(defmacro start*
  "Start a Cypher query to but do not render it."
  [start-map & body]
  (let [init (partial begin empty-query)]
    `(-> ~(init start-map) ~@(expand-form body))))

(defmacro start [start-map & body]
  `(render-query (start* ~start-map ~@body)))
