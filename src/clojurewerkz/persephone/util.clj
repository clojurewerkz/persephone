(ns clojurewerkz.persephone.util
  (:require [clojure.string :as str])
  (:import java.lang.StringBuilder))

(defn ^String str*
  ([] "")
  ([x] (if (keyword? x)
         (name x)
         (str x)))
  ([x & more]
     (let [sb (StringBuilder. ^String (str* x))]
       (str (reduce #(.append % (str* %2)) sb more)))))

(defn escape [x]
  (if (or (string? x)
          (instance? java.util.regex.Pattern x))
    (format "\"%s\"" x)
    x))

(defn comma-join
  ([xs] (comma-join str* xs))
  ([f xs] (str/join ", " (map f xs))))
