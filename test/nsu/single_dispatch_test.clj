(ns nsu.single-dispatch-test
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
    (is (= (m1 [d]) `(:D :B :C :A)))
    (is (= (m2 [d] "test") `((:D "test") (:C "test") :A "test(after C)")))))

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
    (classes-names [instance] names_collector)
    (is (= @names_collector '(:M :J :K :L :H :I :F :G)))))

; test-5

(def-class :pI () ())
(def-class :pJ (:pI) ())
(def-class :pK (:pI) ())
(def-class :pL (:pI) ())
(def-class :pM (:pJ :pK) ())
(def-class :pN (:pL) ())
(def-class :pO (:pL) ())
(def-class :pP (:pM :pN :pO) ())
; BFS = ({:pP} {:pM :pN :pO} {:pJ :pK :pL} {:pI}), {..} means a common tree level.

(def e (new-instance :pP))

(def-generic ride)
(def-method ride [(:pI obj)] (def s (str s ":pI ")))
(def-method ride [(:pJ obj)] (call-next-method) (def s (str s ":pJ ")))
(def-method ride [(:pK obj)] (def s (str s ":pK ")) (call-next-method))
(def-method ride [(:pL obj)] (def s (str s ":pL ")) (call-next-method))
(def-method ride [(:pM obj)] (def s (str s ":pM ")) (call-next-method))
(def-method ride [(:pN obj)] (def s (str s ":pN ")) (call-next-method))
(def-method ride [(:pO obj)] (def s (str s ":pO ")) (call-next-method))
(def-method ride [(:pP obj)] (def s (str s ":pP ")) (call-next-method))

(deftest test-5
  (testing "test-5"
    (def s "")
    (ride [e])
    (is (= s ":pP :pM :pN :pO :pK :pL :pI :pJ "))))
