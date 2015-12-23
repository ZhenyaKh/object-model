(ns task-8.core
  (:require [task-8.class_declaration :refer :all])
  (:gen-class))

(use 'clojure.set)
(use 'clojure.repl)

;; This is for pretty printing of ::keywords.
(import 'clojure.lang.Keyword)
(import 'java.io.Writer)
(defmethod print-method
  Keyword [^Keyword k, ^Writer w]
  (if (.getNamespace k)
    (.write w (str "::" (name k)))
    (.write w (str k))))

(defn -main []
  ;(def-class :A () ())
  ;(def-class :B (:A) ())
  ;(def-class :C (:A) ())
  ;(def-class :D (:A) ())
  ;(def-class :E (:B :C) ())
  ;(def-class :F (:D) ())
  ;(def-class :G (:D) ())
  ;(def-class :H (:E :F :G) ())
  ; BFS = ({:H} {:E :F :G} {:B :C :D} {:A}), {..} means a common tree level.
  ;(def e (new-instance :H))

  ;(def-generic classes-names)
  ;(def-method classes-names :A [obj] (println :A)) 
  ;(def-method classes-names :B [obj] (call_next_method) (println :B))
  ;(def-method classes-names :C [obj] (println :C) (call_next_method))
  ;(def-method classes-names :D [obj] (println :D) (call_next_method))
  ;(def-method classes-names :E [obj] (println :E) (call_next_method))
  ;(def-method classes-names :F [obj] (println :F) (call_next_method))
  ;(def-method classes-names :G [obj] (println :G) (call_next_method))
  ;(def-method classes-names :H [obj] (println :H) (call_next_method))
                           ; [(:H obj1) (:J obj2) arg1 arg2 ... argN]  
  ;(classes-names e)


  (println "\nThe End."))
