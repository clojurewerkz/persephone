# Persephone, Clojure DSL for Neo4J Cypher

Persephone is a Clojure DSL that generates [Neo4J Cypher](http://docs.neo4j.org/chunked/stable/cypher-query-lang.html) queries and is supposed
to augment Clojure Neo4J clients such as [Neocons](http://clojureneo4j.info).


## Documentation & Examples

TBD when the project matures and the API stabilizes.


## Community

Persephone uses [Neocons' mailing list](https://groups.google.com/group/clojure-neo4j). Feel free to join it and ask any questions you may have.

To subscribe for announcements of releases, important changes and so on, please follow [@ClojureWerkz](https://twitter.com/#!/clojurewerkz) on Twitter.


## Project Maturity

Persephone is a *very young project*. The API may radically change in the near future.
We will updated this section once the library matures a bit.



## Maven Artifacts

It is *very* early days of the project, so no releases have been published
to Clojars and the API may radically change in the near future.

Persephone artifacts are [released to Clojars](https://clojars.org/clojurewerkz/persephone). If you are using Maven, add the following repository
definition to your `pom.xml`:

``` xml
<repository>
  <id>clojars.org</id>
  <url>http://clojars.org/repo</url>
</repository>
```

### Snapshots

With Leiningen:

    [clojurewerkz/persephone "0.1.1"]

With Maven:

    <dependency>
      <groupId>clojurewerkz</groupId>
      <artifactId>persephone</artifactId>
      <version>0.1.1</version>
    </dependency>


## Continuous Integration

[![Continuous Integration status](https://secure.travis-ci.org/clojurewerkz/persephone.png)](http://travis-ci.org/clojurewerkz/persephone)

CI is hosted by [travis-ci.org](http://travis-ci.org)


## Supported Clojure versions

The project requires Clojure 1.5 or a later version.
The most recent stable Clojure release is highly recommended.


## Supported Neo4J Server versions

Persephone targets Neo4J 1.8 and later versions.



## Persephone Is a ClojureWerkz Project

Persephone is part of the [group of libraries known as
ClojureWerkz](http://clojurewerkz.org), together with
[Monger](http://clojuremongodb.info),
[Langohr](http://clojurerabbitmq.info),
[Neocons](http://clojureneo4j.info),
[Quartzite](http://clojurequartz.info),
[Elastisch](http://clojureelasticsearch.info) and several others.


## Development

The project uses [Leiningen 2](http://leiningen.org). Make sure you have it installed and then run tests against
all supported Clojure versions using

    lein2 all test

Then create a branch and make your changes on it. Once you are done with your changes and all tests pass, submit
a pull request on Github.


## License

Copyright (C) 2013 Joel Holdbrooks and the ClojureWerkz Team.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html) (the same as Clojure).
