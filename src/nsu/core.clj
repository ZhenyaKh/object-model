(ns nsu.core
  (:require [nsu.objectmodel :refer :all])
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
  (def-class :A2 () ())
  (def-class :B2 (:A2) ())
  (def-class :A3 () ())
  (def-class :B3 (:A3) ())

  (def e1 (new-instance :B1))
  (def e2 (new-instance :B2))
  (def e3 (new-instance :B3))

  (def-generic ride)
  (def-method ride [(:A1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] (println :A1 :A2 :A3 arg1 arg2))
  (def-method ride [(:A1 obj1) (:A2 obj2) (:B3 obj3) arg1 & arg2] (println :A1 :A2 :B3 arg1 arg2) (call-next-method arg1 arg2))
  (def-method ride [(:A1 obj1) (:B2 obj2) (:A3 obj3) arg1 & arg2] (println :A1 :B2 :A3 arg1 arg2) (call-next-method arg1 arg2))
  (def-method ride [(:A1 obj1) (:B2 obj2) (:B3 obj3) arg1 & arg2] (println :A1 :B2 :B3 arg1 arg2) (call-next-method arg1 arg2))
  (def-method ride [(:B1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] (println :B1 :A2 :A3 arg1 arg2) (call-next-method arg1 arg2))
  (def-method ride [(:B1 obj1) (:A2 obj2) (:B3 obj3) arg1 & arg2] (println :B1 :A2 :B3 arg1 arg2) (call-next-method arg1 arg2))
  (def-method ride [(:B1 obj1) (:B2 obj2) (:A3 obj3) arg1 & arg2] (println :B1 :B2 :A3 arg1 arg2) (call-next-method arg1 arg2))
  (def-method ride [(:B1 obj1) (:B2 obj2) (:B3 obj3) arg1 & arg2] (println :B1 :B2 :B3 arg1 arg2) (call-next-method arg1 arg2))

  (def-class :A () ())
  (def-class :B (:A) ())
  (def-class :C (:A) ())
  (def-class :D (:B :C) ())
  (def-class :E () ())
  (def-class :X (:E) ())
  (def-class :F (:X) ())
  (def-class :G (:X) ())
  (def-class :H (:F :G) ())

  (def inst1 (new-instance :D))
  (def inst2 (new-instance :H))

  (def-method ride [(:A obj1) (:E obj) arg1 arg2] (println :A :E arg1 arg2 obj1 obj))
  (def-method ride [(:A obj1) (:F obj) arg1 arg2] (println :A :F arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
  (def-method ride [(:A obj1) (:G obj) arg1 arg2] (println :A :G arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
  (def-method ride [(:A obj1) (:H obj) arg1 arg2] (println :A :H arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
  (def-method ride [(:B obj1) (:E obj) arg1 arg2] (println :B :E arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
  (def-method ride [(:B obj1) (:F obj) arg1 arg2] (println :B :F arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
  (def-method ride [(:B obj1) (:G obj2) arg1 arg2] (println :B :G arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:B obj1) (:H obj2) arg1 arg2] (println :B :H arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:C obj1) (:E obj2) arg1 arg2] (println :C :E arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:C obj1) (:F obj2) arg1 arg2] (println :C :F arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:C obj1) (:G obj2) arg1 arg2] (println :C :G arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:C obj1) (:H obj2) arg1 arg2] (println :C :H arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:D obj1) (:E obj2) arg1 arg2] (println :D :E arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:D obj1) (:F obj2) arg1 arg2] (println :D :F arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:D obj1) (:G obj2) arg1 arg2] (println :D :G arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:D obj1) (:H obj2) arg1 arg2] (println :D :H arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:A obj1) (:X obj2) arg1 arg2] (println :A :X arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:B obj1) (:X obj2) arg1 arg2] (println :B :X arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:C obj1) (:X obj2) arg1 arg2] (println :C :X arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
  (def-method ride [(:D obj1) (:X obj2) arg1 arg2] (println :D :X arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))


  (def-class :I () ())
  (def-class :J (:I) ())
  (def-class :K (:I) ())
  (def-class :L (:I) ())
  (def-class :M (:J :K) ())
  (def-class :N (:L) ())
  (def-class :O (:L) ())
  (def-class :P (:M :N :O) ())
  ; BFS = ({:P} {:M :N :O} {:J :K :L} {:I}), {..} means a common tree level.

  (def e (new-instance :P))

  (def-method ride [(:I obj)] (println :I))
  (def-method ride [(:J obj)] (call-next-method) (println :J))
  (def-method ride [(:K obj)] (println :K) (call-next-method))
  (def-method ride [(:L obj)] (println :L) (call-next-method))
  (def-method ride [(:M obj)] (println :M) (call-next-method))
  (def-method ride [(:N obj)] (println :N) (call-next-method))
  (def-method ride [(:O obj)] (println :O) (call-next-method))
  (def-method ride [(:P obj)] (println :P) (call-next-method))

  (ride [e])

  (ride [inst1 inst2] "arg_1" "arg_2")
  (ride [e1 e2 e3] "arg_1" "arg_2")

  (println "\nThe End."))
