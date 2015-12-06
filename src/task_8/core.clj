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

;  (def-class :Base (:fields (:cnt1 :cnt2 :cnt3)))

;  (def-class :Derived (:super :Base) (:fields (:cnt4)))

;  (def-class :Derived_from_Derived (:super :Derived) (:fields (:cnt5)))

;  (def q (create-instance :Base :cnt1 "1" :cnt2 '(2) :cnt3 3))
;  (println q)
;  (println (instance-class q))

;  (println (is-instance? q))
;  (println (is-instance? [1 2]))
;  (println (getf q :cnt2))
;  (setf q :cnt2 '(3))
;  (println (getf q :cnt2))
;
;  (def-command increment-cnt)
;  (def-method increment-cnt :Base [obj amount amount2]
;    (setf obj :cnt3 (+ amount (getf obj :cnt3))))

;  (increment-cnt q 3 5)
;  (println q "\n")

;  (def w (create-instance :Derived :cnt1 "1" :cnt2 '(2) :cnt4 4 :cnt3 3))
;  (println w)
;  (def-method increment-cnt :Derived [obj amount amount2]
;    (super amount amount2)
;    (setf obj :cnt4 (+ amount (getf obj :cnt4))))

;  (println)
;  (println w)
;  (increment-cnt w 5 7)
;  (println w)

;  (def e (create-instance :Derived_from_Derived :cnt1 "1" :cnt2 '(2) :cnt4 4 :cnt3 3 :cnt5 10))
;
;  (def-method increment-cnt :Derived_from_Derived [obj amount amount2]
;    (super amount amount2)
;    (setf obj :cnt5 (+ amount2 (getf obj :cnt5))))

;  (println)
;  (println e)
;  (increment-cnt e 5 7)
;  (println e)

;  (println)
;  (println @classes-hierarchy)



 ; (Thread/sleep 1000)
  ;(dotimes [i 10] (.start (Thread. (fn [] (mac i) (Thread/sleep (* 1000 i))  ))))

  ;(Thread/sleep 1000)

  (println "\nThe End."))





