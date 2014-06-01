(defproject clojurewerkz/persephone "0.1.2-SNAPSHOT"
  :description "Clojure DSL that generates [Neo4J] Cypher queries"
  :url "http://github.com/clojurewerkz/persephone"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:plugins      [[codox "0.8.5"]]
                   :dependencies [[com.novemberain/monger "1.5.0"]
                                  [cheshire               "5.1.1"]
                                  [hiccup                 "1.0.3"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.7.0-master-SNAPSHOT"]]}
             :master {:dependencies [[org.clojure/clojure "1.7.0-master-SNAPSHOT"]]}}
  :aliases  {"all" ["with-profile" "+dev:+1.4:+1.6:+master"]}
  :repositories {"sonatype" {:url "http://oss.sonatype.org/content/repositories/releases"
                             :snapshots false
                             :releases {:checksum :fail :update :always}}
                 "sonatype-snapshots" {:url "http://oss.sonatype.org/content/repositories/snapshots"
                                       :snapshots true
                                       :releases {:checksum :fail :update :always}}})
