(ns nsu.core-test
  (:require [clojure.test :refer :all]
            [nsu.objectmodel :refer :all]))

; BFS = ({:D} {:B :C} {:A :E}), {..} means a common tree level.
(def-class :A ()
  (:a1 :a2 :a3)
  (init :a3 42))
(def-class :B (:A :E)
  (:b))
(def-class :C (:A)
  (:c))
(def-class :D (:B :C)
  (:d1 :d2 ))
(def-class :E ()
  (:e))

(def d (new-instance :D :d1 1 :d2 2 :b 3 :c 4 :a1 5 :a2 7 :e 8))

(deftest field-access-test
  (testing "Field access test"
    (is (do
          (setf d :a3 24)
          (= 24 (getf d :a3))))
    (is (= 7 (getf d :a2)))))
