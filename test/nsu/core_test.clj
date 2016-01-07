(ns nsu.core-test
  (:require [clojure.test :refer :all]
            [nsu.objectmodel :refer :all]))

;; This is for pretty printing of ::keywords.
(import 'clojure.lang.Keyword)
(import 'java.io.Writer)
(defmethod print-method
  Keyword [^Keyword k, ^Writer w]
  (if (.getNamespace k)
    (.write w (str "::" (name k)))
    (.write w (str k))))

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

(def-generic m1)

(def-method m1 [(:A obj)]
  `(:A))

(def-method m1 [(:B obj)]
  (cons :B (call-next-method)))

(def-method m1 [(:C obj)]
  (cons :C (call-next-method)))

(def-method m1 [(:D obj)]
  (cons :D (call-next-method)))

(def-generic m2)

(def-method m2 [(:A obj) msg]
  (list :A msg))

(def-method m2 [(:C obj) msg]
  (cons (list :C msg) (call-next-method (str msg "(after C)"))))

(def-method m2 [(:D obj) msg]
  (conj (call-next-method msg) (list :D msg)))

(def-method m2 [(:E obj) msg]
  (list :E msg))

(def d (new-instance :D :d1 1 :d2 2 :b 3 :c 4 :a1 5 :a2 7 :e 8))

(deftest test-1
  (testing "test-1"
    (is (= (m1 d) `(:D :B :C :A)))
    (is (= (m2 d "test") `((:D "test") (:C "test") :A "test(after C)")))
    (is (= 42 (getf d :a3)))
    (is (do
          (setf d :a3 24)
          (= 24 (getf d :a3))))
    (is (= 7 (getf d :a2)))))

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

; test-2
(def-class :F () ())
(def-class :G (:F) ())
(def-class :H (:F) ())
(def-class :I (:F) ())
(def-class :J (:G :H) ())
(def-class :K (:I) ())
(def-class :L (:I) ())
(def-class :M (:J :K :L) ())
; BFS = ({:M} {:J :K :L} {:G :H :I} {:F}), {..} means a common tree level.

(def instance (new-instance :M))

(def-generic classes-names)
(def-method classes-names [(:F obj) nc] (dosync (alter nc concat '(:F))))
(def-method classes-names [(:G obj) nc] (call-next-method nc) (dosync (alter nc concat '(:G))))
(def-method classes-names [(:H obj) nc] (dosync (alter nc concat '(:H)) (call-next-method nc)))
(def-method classes-names [(:I obj) nc] (dosync (alter nc concat '(:I))) (call-next-method nc))
(def-method classes-names [(:J obj) nc] (dosync (alter nc concat '(:J))) (call-next-method nc))
(def-method classes-names [(:K obj) nc] (dosync (alter nc concat '(:K))) (call-next-method nc))
(def-method classes-names [(:L obj) nc] (dosync (alter nc concat '(:L))) (call-next-method nc))
(def-method classes-names [(:M obj) nc] (dosync (alter nc concat '(:M))) (call-next-method nc))
                           ; [(:M obj1) (:J obj2) arg1 arg2 ... argN]
(deftest test-2
  (testing "test-2"
    (def names_collector (ref '()))
    (classes-names instance names_collector)
    (println @names_collector)
    (is (= @names_collector '(:M :J :K :L :H :I :F :G)))))
