(ns task-8.core-test
  (:require [clojure.test :refer :all]
;            [task-8.core :refer :all]
            [task-8.class_declaration :refer :all]))

(def-class :A ()
  (:a1 :a2 :a3)
  (attr-accessor :a3)
  (attr-reader :a2)
  (attr-writer :a1)
  (init :a3 42))
  
(def-class :B (:E :A)
  (:b))
(def-class :C (:A)
  (:c))
(def-class :D (:B :C)
  (:d1 :d2 ))
(def-class :E ()
  (:e))
; TODO make like this (def-generic m1 [obj])
; argument list is determined here and error will be occured
; if def-method doesn't use exactly it
(def-generic m1)

(def-method m1 :A [obj]
  `(:A))

(def-method m1 :B [obj]
  (cons :B (call-next-method)))

(def-method m1 :C [obj]
  (cons :C (call-next-method)))

(def-method m1 :D [obj]
  (cons :D (call-next-method)))

; TODO make like this (def-generic m2 [obj msg])
(def-generic m2)

(def-method m2 :A [obj msg]
  (println (list :A msg))
  (list :A msg))

(def-method m2 :C [obj msg]
  (println (cons (list :C msg) (call-next-method (str msg "(after C)"))))
  (cons (list :C msg) (call-next-method (str msg "(after C)"))))

(def-method m2 :D [obj msg]
  (println (conj (call-next-method msg) (list :D msg)))
  (conj (call-next-method msg) (list :D msg)))

(def-method m2 :E [obj msg]
 (println (list :E msg))
  (list :E msg))

; Notes how it should be rewritten to support dispatcherization
; on many arguments
; 1 parapeter
;   (def-generic m)
;   (def-method m[(:A obj)]
;     (println :A))
; 2 parameters
;   (def-generic m)
;   (def-method m[(:A obj1) ... (:B obj2) ...]
;     (println :A))

(def d (new-instance :D :d1 1 :d2 2 :b 3 :c 4 :a1 5 :a2 7 :e 8))

(deftest a-test
  (testing "TODO write a description"
    (is (= (m1 d) `(:D :B :C :A)))
    (is (= (m2 d "test") `((:C "test")
                            (:E "test(after C)")
                            (:A "test(after C)")
                            (:D "test"))))
	(is (= 42 (get-a3 d)))
	(is (do
		  (set-a3 d 24)
		  (= 24 (get-a3 d))))
	(is (= 7 (get-a2 d)))))
