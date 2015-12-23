(ns task-8.core-test
  (:require [clojure.test :refer :all]
            [task-8.core :refer :all]
            [task-8.class_declaration :refer :all]))

(def-class :A ()
  (:a))
(def-class :B (:A)
  (:b))
(def-class :C (:A)
  (:c))
(def-class :D (:A)
  (:d))
(def-class :E (:B :C)
  (:e))
(def-class :F (:D)
  (:f))
(def-class :G (:D)
  (:g))
(def-class :H (:E :F :G)
  (:h))

(def-generic classes-names)
(def-method classes-names :A [obj]
  (println :A))
(def-method classes-names :B [obj]
  (call_next_method)
  (println :B))
(def-method classes-names :C [obj]
  (println :C))
(def-method classes-names :D [obj]
  (println :D))
(def-method classes-names :E [obj]
  (println :E))
(def-method classes-names :F [obj]
  (println :F))
(def-method classes-names :G [obj]
  (println :G))
(def-method classes-names :H [obj]
  (println :H))

; Notes how it should be rewritten to support dispatcherization
; on many arguments
; 1 parapeter
;   (def-generic classes-names)
;   (def-method classes-names[:A obj]
;     (println :A))
; 2 parameters
;   (def-generic multi)
;   (def-method multi[:A obj1 :B obj2]
;     (println :A))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
