(ns task-8.core
  (:require [task-8.class_declaration :refer :all])
  (:gen-class))

;; This is for pretty printing of ::keywords.
(import 'clojure.lang.Keyword)
(import 'java.io.Writer)
(defmethod print-method
  Keyword [^Keyword k, ^Writer w]
  (if (.getNamespace k)
    (.write w (str "::" (name k)))
    (.write w (str k))))

(defn -main []
  (println "-main."))
