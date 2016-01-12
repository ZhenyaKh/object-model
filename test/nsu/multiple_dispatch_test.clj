(ns nsu.multiple-dispatch-test
  (:require [clojure.test :refer :all]
            [nsu.objectmodel :refer :all]))

; test-3

(def-class :A1 () ())
(def-class :B1 (:A1) ())
(def-class :A2 () ())
(def-class :B2 (:A2) ())
(def-class :A3 () ())
(def-class :B3 (:A3) ())

(def-generic ride)
(def-method ride [(:A1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] 
            (def s (str s ":A1 :A2 :A3" arg1 " " arg2 " \n")))
(def-method ride [(:A1 obj1) (:A2 obj2) (:B3 obj3) arg1 & arg2] 
            (def s (str s ":A1 :A2 :B3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))
(def-method ride [(:A1 obj1) (:B2 obj2) (:A3 obj3) arg1 & arg2] 
            (def s (str s ":A1 :B2 :A3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))
(def-method ride [(:A1 obj1) (:B2 obj2) (:B3 obj3) arg1 & arg2] 
            (def s (str s ":A1 :B2 :B3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:A2 obj2) (:A3 obj3) arg1 & arg2] 
            (def s (str s ":B1 :A2 :A3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:A2 obj2) (:B3 obj3) arg1 & arg2] 
            (def s (str s ":B1 :A2 :B3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:B2 obj2) (:A3 obj3) arg1 & arg2] 
            (def s (str s ":B1 :B2 :A3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))
(def-method ride [(:B1 obj1) (:B2 obj2) (:B3 obj3) arg1 & arg2] 
            (def s (str s ":B1 :B2 :B3" arg1 " " arg2 " \n")) (call-next-method arg1 arg2))

(def e1 (new-instance :B1))
(def e2 (new-instance :B2))
(def e3 (new-instance :B3))

(deftest test-3
  (testing "test-3"
    (def s "")
    (ride [e1 e2 e3] "arg_1" "arg_2")
    (is (= s (str ":B1 :B2 :B3arg_1 (\"arg_2\") \n"
                  ":B1 :B2 :A3arg_1 ((\"arg_2\")) \n"
                  ":B1 :A2 :B3arg_1 (((\"arg_2\"))) \n"
                  ":B1 :A2 :A3arg_1 ((((\"arg_2\")))) \n"
                  ":A1 :B2 :B3arg_1 (((((\"arg_2\"))))) \n"
                  ":A1 :B2 :A3arg_1 ((((((\"arg_2\")))))) \n"
                  ":A1 :A2 :B3arg_1 (((((((\"arg_2\"))))))) \n"
                  ":A1 :A2 :A3arg_1 ((((((((\"arg_2\")))))))) \n")))))

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

(def-method ride [(:pA obj1) (:pE obj) arg1 arg2] 
    (def s (str s ":pA :pE " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj) " \n")))

(def-method ride [(:pA obj1) (:pF obj) arg1 arg2] 
    (def s (str s ":pA :pF " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pA obj1) (:pG obj) arg1 arg2] 
    (def s (str s ":pA :pG " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pA obj1) (:pH obj) arg1 arg2] 
    (def s (str s ":pA :pH " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj) " \n")) 
    (call-next-method arg1 arg2))

(def-method ride [(:pB obj1) (:pE obj) arg1 arg2] 
    (def s (str s ":pB :pE " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj) " \n")) 
    (call-next-method arg1 arg2))

(def-method ride [(:pB obj1) (:pF obj) arg1 arg2] 
    (def s (str s ":pB :pF " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pB obj1) (:pG obj2) arg1 arg2]
    (def s (str s ":pB :pG " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pB obj1) (:pH obj2) arg1 arg2] 
    (def s (str s ":pB :pH " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pC obj1) (:pE obj2) arg1 arg2] 
    (def s (str s ":pC :pE " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pC obj1) (:pF obj2) arg1 arg2] 
    (def s (str s ":pC :pF " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pC obj1) (:pG obj2) arg1 arg2] 
    (def s (str s ":pC :pG " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pC obj1) (:pH obj2) arg1 arg2] 
    (def s (str s ":pC :pH " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pD obj1) (:pE obj2) arg1 arg2] 
    (def s (str s ":pD :pE " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pD obj1) (:pF obj2) arg1 arg2] 
    (def s (str s ":pD :pF " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pD obj1) (:pG obj2) arg1 arg2] 
    (def s (str s ":pD :pG " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pD obj1) (:pH obj2) arg1 arg2] 
    (def s (str s ":pD :pH " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pA obj1) (:pX obj2) arg1 arg2] 
    (def s (str s ":pA :pX " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pB obj1) (:pX obj2) arg1 arg2] 
    (def s (str s ":pB :pX " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pC obj1) (:pX obj2) arg1 arg2] 
    (def s (str s ":pC :pX " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def-method ride [(:pD obj1) (:pX obj2) arg1 arg2] 
    (def s (str s ":pD :pX " arg1 " " arg2 " " (instance-class obj1) " " (instance-class obj2) " \n"))
    (call-next-method arg1 arg2))

(def inst1 (new-instance :pD))
(def inst2 (new-instance :pH))

(deftest test-4
  (testing "test-4"
    (def s "")
    (ride [inst1 inst2] "arg_1" "arg_2")
    (is (= s (str ":pD :pH arg_1 arg_2 :pD :pH \n"
                  ":pD :pF arg_1 arg_2 :pD :pH \n"
                  ":pD :pG arg_1 arg_2 :pD :pH \n"
                  ":pD :pX arg_1 arg_2 :pD :pH \n"
                  ":pD :pE arg_1 arg_2 :pD :pH \n"
                  ":pB :pH arg_1 arg_2 :pD :pH \n"
                  ":pB :pF arg_1 arg_2 :pD :pH \n"
                  ":pB :pG arg_1 arg_2 :pD :pH \n"
                  ":pB :pX arg_1 arg_2 :pD :pH \n"
                  ":pB :pE arg_1 arg_2 :pD :pH \n"
                  ":pC :pH arg_1 arg_2 :pD :pH \n"
                  ":pC :pF arg_1 arg_2 :pD :pH \n"
                  ":pC :pG arg_1 arg_2 :pD :pH \n"
                  ":pC :pX arg_1 arg_2 :pD :pH \n"
                  ":pC :pE arg_1 arg_2 :pD :pH \n"
                  ":pA :pH arg_1 arg_2 :pD :pH \n"
                  ":pA :pF arg_1 arg_2 :pD :pH \n"
                  ":pA :pG arg_1 arg_2 :pD :pH \n"
                  ":pA :pX arg_1 arg_2 :pD :pH \n"
                  ":pA :pE arg_1 arg_2 :pD :pH \n")))))
