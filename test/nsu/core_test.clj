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
    (is (= (m1 [d]) `(:D :B :C :A)))
    (is (= (m2 [d] "test") `((:D "test") (:C "test") :A "test(after C)")))
    (is (= 42 (getf d :a3)))
    (is (do
          (setf d :a3 24)
          (= 24 (getf d :a3))))
    (is (= 7 (getf d :a2)))))

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
    (println @names_collector)
    (is (= @names_collector '(:M :J :K :L :H :I :F :G)))))

; test-3

(def-class :A1 () ())
(def-class :B1 (:A1) ())
(def-class :A2 () ())
(def-class :B2 (:A2) ())
(def-class :A3 () ())
(def-class :B3 (:A3) ())

(def-generic ride)
(def-method ride [(:A1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] (println :A1 :A2 :A3 arg1 arg2))
(def-method ride [(:A1 obj1) (:A2 obj2) (:B3 obj3) arg1 & arg2] (println :A1 :A2 :B3 arg1 arg2) (call-next-method arg1 arg2))
(def-method ride [(:A1 obj1) (:B2 obj2) (:A3 obj3) arg1 & arg2] (println :A1 :B2 :A3 arg1 arg2) (call-next-method arg1 arg2))
(def-method ride [(:A1 obj1) (:B2 obj2) (:B3 obj3) arg1 & arg2] (println :A1 :B2 :B3 arg1 arg2) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] (println :B1 :A2 :A3 arg1 arg2) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:A2 obj2) (:B3 obj3) arg1 & arg2] (println :B1 :A2 :B3 arg1 arg2) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:B2 obj2) (:A3 obj3) arg1 & arg2] (println :B1 :B2 :A3 arg1 arg2) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:B2 obj2) (:B3 obj3) arg1 & arg2] (println :B1 :B2 :B3 arg1 arg2) (call-next-method arg1 arg2))

(def-support :before ride [(:A1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] (println "support method was called"))

(def e1 (new-instance :B1))
(def e2 (new-instance :B2))
(def e3 (new-instance :B3))

; test-4

(def-class :pA () ())
(def-class :pB (:pA) ())
(def-class :pC (:pA) ())
(def-class :pD (:pB :pC) ())
(def-class :pE () ())
(def-class :pX (:pE) ())
(def-class :pF (:pX) ())
(def-class :pG (:pX) ())
(def-class :pH (:pF :pG) ())

(def-method ride [(:pA obj1) (:pE obj) arg1 arg2] (println :pA :pE arg1 arg2 obj1 obj))
(def-method ride [(:pA obj1) (:pF obj) arg1 arg2] (println :pA :pF arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
(def-method ride [(:pA obj1) (:pG obj) arg1 arg2] (println :pA :pG arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
(def-method ride [(:pA obj1) (:pH obj) arg1 arg2] (println :pA :pH arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
(def-method ride [(:pB obj1) (:pE obj) arg1 arg2] (println :pB :pE arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
(def-method ride [(:pB obj1) (:pF obj) arg1 arg2] (println :pB :pF arg1 arg2 obj1 obj) (call-next-method arg1 arg2))
(def-method ride [(:pB obj1) (:pG obj2) arg1 arg2] (println :pB :pG arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pB obj1) (:pH obj2) arg1 arg2] (println :pB :pH arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pC obj1) (:pE obj2) arg1 arg2] (println :pC :pE arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pC obj1) (:pF obj2) arg1 arg2] (println :pC :pF arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pC obj1) (:pG obj2) arg1 arg2] (println :pC :pG arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pC obj1) (:pH obj2) arg1 arg2] (println :pC :pH arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pD obj1) (:pE obj2) arg1 arg2] (println :pD :pE arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pD obj1) (:pF obj2) arg1 arg2] (println :pD :pF arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pD obj1) (:pG obj2) arg1 arg2] (println :pD :pG arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pD obj1) (:pH obj2) arg1 arg2] (println :pD :pH arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pA obj1) (:pX obj2) arg1 arg2] (println :pA :pX arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pB obj1) (:pX obj2) arg1 arg2] (println :pB :pX arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pC obj1) (:pX obj2) arg1 arg2] (println :pC :pX arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))
(def-method ride [(:pD obj1) (:pX obj2) arg1 arg2] (println :pD :pX arg1 arg2 obj1 obj2) (call-next-method arg1 arg2))

(def inst1 (new-instance :pD))
(def inst2 (new-instance :pH))

; test-5

(def-class :pI () ())
(def-class :pJ (:pI) ())
(def-class :pK (:pI) ())
(def-class :pL (:pI) ())
(def-class :pM (:pJ :pK) ())
(def-class :pN (:pL) ())
(def-class :pO (:pL) ())
(def-class :pP (:pM :pN :pO) ())
; BFS = ({:P} {:M :N :O} {:J :K :L} {:I}), {..} means a common tree level.

(def e (new-instance :pP))

(def-method ride [(:pI obj)] (println :pI))
(def-method ride [(:pJ obj)] (call-next-method) (println :pJ))
(def-method ride [(:pK obj)] (println :pK) (call-next-method))
(def-method ride [(:pL obj)] (println :pL) (call-next-method))
(def-method ride [(:pM obj)] (println :pM) (call-next-method))
(def-method ride [(:pN obj)] (println :pN) (call-next-method))
(def-method ride [(:pO obj)] (println :pO) (call-next-method))
(def-method ride [(:pP obj)] (println :pP) (call-next-method))

(deftest test-3
  (testing "test-3"
           (ride [e])
           (ride [inst1 inst2] "arg_1" "arg_2")
           (ride [e1 e2 e3] "arg_1" "arg_2")))
