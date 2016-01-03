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
  (println "-main.")

  (def-class :A1 () ())
  (def-class :B1 (:A1) ())
  (def-class :C1 (:A1) ())
  (def-class :D1 (:B1 :C1) ())
  (def-class :A2 () ())
  (def-class :B2 (:A2) ())
  (def-class :C2 (:A2) ())
  (def-class :D2 (:B2 :C2) ())
  ; BFS1 = ({:D1} {:B1 :C1} {:A1}), BFS2 = ({:D2} {:B2 :C2} {:A2}).
  ;(dorun (println @classes-hierarchy)
  ;(println "!!!!!!!!!!"))

  (def e1 (new-instance :A1))
  (def e2 (new-instance :A2))

  (def-generic classes-names)
  (def-method classes-names [(:A1 obj) & args] (println :A1 " " obj args))
  (def-method classes-names [(:A2 obj) & args] (println :A2 " " obj args))
  (def-method classes-names [(:A1 obj1) (:A2 obj2) & args] (println :A1 :A2 " " obj1 obj2 args))
  (def-method classes-names [(:A2 obj1) (:A1 obj2) & args] (println :A2 :A1 " " obj1 obj2 args))
 ; (def-method classes-names :B1 [obj arg] (println :B1 " " arg) (call-next-method arg))
 ; (def-method classes-names :C1 [obj arg] (println :C1 " " arg) (call-next-method arg))
 ; (def-method classes-names :D1 [obj arg] (println :D1 " " arg) (call-next-method arg))

 ; (def-method classes-names :A2 [obj arg] (println :A2 " " arg))
 ; (def-method classes-names :B2 [obj arg] (println :B2 " " arg) (call-next-method arg))
 ; (def-method classes-names :C2 [obj arg] (println :C2 " " arg) (call-next-method arg))
 ; (def-method classes-names :D2 [obj arg] (println :D2 " " arg) (call-next-method arg))
  ; [(:A1 obj1) (:A2 obj2) arg1 arg2 ... argN]
  ; [(:D1 obj1) arg]


  ;(classes-names e1 "arg1")
  ;(println)
  ;(classes-names e2 "arg2")


  (println "\nThe End."))
