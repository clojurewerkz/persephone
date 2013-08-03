# Cryptogram

Cryptogram is a DSL for authoring Cypher queries in Clojure.

Here's an example:

```clojure
(require '[cryptogram :refer [start node where return]])

(start {:n (node [3 1])}
  (where (or (and (< :n.age 30) (= :n.name "Tobias"))
             (not (= :n.name "Tobias"))))
  (return :n))
```

and here's the query it generates:

```
START n = node(3, 1)
WHERE ((n.age < 30 and n.name = "Tobias") or not(n.name = "Tobias"))
RETURN n
```

## Usage

For the sake of demonstration the examples will assume the following
frowned upon practice is in play:

```clojure
(use 'cryptogram)
```

But you're a better Clojure programmer than that.

### Starting and returning 

The most basic Cypher query begins with `start` and ends with
`return`.

```clojure
(start {:n "node(1)"}
  (return :n))
```

`start` is a macro that accepts a map of starting points, such as
nodes or relationships, and the body of the query. In this case our
starting point is the node with the id of `1` which is bound to `n`.

The body of our query consists soley of the `return` clause, which is
simply a function which accepts any number of columns, or aggregates
of, to return. With the execption of `start`, query clauses may be
provided in any order.

The return value of `start` is a rendered Cypher query. A string.
If we `print` the result of the above query we get:

```
START n = node(1)
RETURN n
```

#### Nodes and relationships

You may have noticed in our start map we used the string `"node(1)"`
for our starting point. While strings are a nice "backdoor",
Cryptogram provides the `node` and `rel` functions to make starting
points more Clojure friendly.

`node` and `rel`, for all intents and purposes, are identical
functions. The only difference is the strings they produce. Both
accept a value, such as an integer or string, and optionally an index.
The table below demonstrates the use of the `node` function and it's
Cypher analogue.

Expression                   | Result                    
-----------------------------|---------------------------
`(node 1)`                   | `node(1)`                 
`(node [1 2 3])`             | `node(1, 2, 3)`           
`(node "foo")`               | `node("foo")`             
`(node :foo)`                | `node("foo")`             
`(node {:foo "bar"}`         | `node(foo = "bar")`       
`(node :nodes "name:A")`     | `node:nodes("name:A")`    
`(node :nodes {:foo "bar"})` | `node:nodes(foo = "bar")` 

With this we can now author queries like 

```
START a = node(1), b = node(2)
RETURN a, b
```

completely with Clojure as

```clojure
(start {:a (node 1) :b (node 2)}
  (return :a :b))
```

all while avoiding tedious and error prone string templating.

### Match

One of the coolest features of the Cypher query language is `MATCH`.
The `MATCH` clause allows you to (literally) draw out the
relationships between nodes of interest in the graph.

Suppose we want to write the query

```
START bob = node(1)
MATCH bob-[:KNOWS]-alice
RETURN alice
```

with our DSL. Cryptogram provides the `match` function which allows
you to compose `MATCH` patterns with one or more vectors. The example
below demonstrates the use of `match` to create the query above. 

```clojure
(start {:bob (node 1)}
  (match [:bob [:KNOWS] :alice])
  (return :alice))
```

As you can see the syntax is very similar to the "real thing". However
there are some differences.

The first thing that stands out about the above match pattern is the
lack of `-` path symbols. When path symbols are omitted between
elements a `-` symbol is assumed. So the basic "grammar" of a
Cryptogram match pattern is

```clojure
[:node-x path-symbol? :node-y ...]
[:node-x path-symbol? [:REL] path-symbol? :node-y ...]
```

where the string form of `path-symbol` is an element of the set
`#{"-", "--", "->", "<-", "-->", "<--"}`. 

Patterns may be of arbitrary length as in Cypher. 

## Emacs indention

If you're using Emacs with `clojure-mode` enabled you can place this
in your `init.el` or evaluate it in the `*scratch*` buffer.

```lisp
(define-clojure-indent
  (start* 'defun)
  (start 'defun))
```

## License

Copyright © 2013 Joel Holdbrooks

Distributed under the Eclipse Public License, the same as Clojure.
