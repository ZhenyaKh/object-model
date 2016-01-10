(defclass A1 () ())

(defclass B1 () ())

(defclass C1 (B1 A1) ())

(defclass A2 () ())

(defclass B2 () ())

(defclass C2 (B2 A2) ())

(defgeneric ride (obj1 obj2))

;(defmethod ride :around ((obj1 A1) (obj2 A2))
;(format t "start A1 & A2~%")
;(call-next-method)
;(format t "finish A1 & A2~%"))

(defmethod ride :before ((obj1 A1) (obj2 A2))
(format t "before A1 & A2~%"))

;(defmethod ride ((obj1 A1) (obj2 A2))
;(format t "A1 & A2~%"))

(defmethod ride :after ((obj1 A1) (obj2 A2))
(format t "after A1 & A2~%"))

;;

;(defmethod ride :around ((obj1 A1) (obj2 B2))
;(format t "start A1 & B2~%")
;(call-next-method)
;(format t "finish A1 & B2~%"))

(defmethod ride :before ((obj1 A1) (obj2 B2))
(format t "before A1 & B2~%"))

(defmethod ride ((obj1 A1) (obj2 B2))
(format t "A1 & B2~%"))

(defmethod ride :after ((obj1 A1) (obj2 B2))
(format t "after A1 & B2~%"))

;;

;(defmethod ride :around ((obj1 A1) (obj2 C2))
;(format t "start A1 & C2~%")
;(call-next-method)
;(format t "finish A1 & C2~%"))

(defmethod ride :before ((obj1 A1) (obj2 C2))
(format t "before A1 & C2~%"))

;(defmethod ride ((obj1 A1) (obj2 C2))
;(format t "A1 & C2~%")
;(call-next-method))

(defmethod ride :after ((obj1 A1) (obj2 C2))
(format t "after A1 & C2~%"))

;;

;(defmethod ride :around ((obj1 B1) (obj2 A2))
;(format t "start B1 & A2~%")
;(call-next-method)
;(format t "finish B1 & A2~%"))

(defmethod ride :before ((obj1 B1) (obj2 A2))
(format t "before B1 & A2~%"))

;(defmethod ride ((obj1 B1) (obj2 A2))
;(format t "B1 & A2~%")
;(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 A2))
(format t "after B1 & A2~%"))

;;

;(defmethod ride :around ((obj1 B1) (obj2 B2))
;(format t "start B1 & B2~%")
;(call-next-method)
;(format t "finish B1 & B2~%"))

(defmethod ride :before ((obj1 B1) (obj2 B2))
(format t "before B1 & B2~%"))

;(defmethod ride ((obj1 B1) (obj2 B2))
;(format t "B1 & B2~%")
;(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 B2))
(format t "after B1 & B2~%"))

;;

;(defmethod ride :around ((obj1 B1) (obj2 C2))
;(format t "start B1 & C2~%")
;(call-next-method)
;(format t "finish B1 & C2~%"))

(defmethod ride :before ((obj1 B1) (obj2 C2))
(format t "before B1 & C2~%"))

;(defmethod ride ((obj1 B1) (obj2 C2))
;(format t "B1 & C2~%")
;(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 C2))
(format t "after B1 & C2~%"))

;;

;(defmethod ride :around ((obj1 C1) (obj2 A2))
;(format t "start C1 & A2~%")
;(call-next-method)
;(format t "finish C1 & A2~%"))

(defmethod ride :before ((obj1 C1) (obj2 A2))
(format t "before C1 & A2~%"))

;(defmethod ride ((obj1 C1) (obj2 A2))
;(format t "C1 & A2~%")
;(call-next-method))

(defmethod ride :after ((obj1 C1) (obj2 A2))
(format t "after C1 & A2~%"))

;;

;(defmethod ride :around ((obj1 C1) (obj2 B2))
;(format t "start C1 & B2~%")
;(call-next-method)
;(format t "finish C1 & B2~%"))

(defmethod ride :before ((obj1 C1) (obj2 B2))
(format t "before C1 & B2~%"))

;(defmethod ride ((obj1 C1) (obj2 B2))
;(format t "C1 & B2~%")
;(call-next-method))

(defmethod ride :after ((obj1 C1) (obj2 B2))
(format t "after C1 & B2~%"))

;;

;(defmethod ride :around ((obj1 C1) (obj2 C2))
;(format t "start C1 & C2~%")
;(call-next-method)
;(format t "finish C1 & C2~%"))

(defmethod ride :before ((obj1 C1) (obj2 C2))
(format t "before C1 & C2~%"))

;(defmethod ride ((obj1 C1) (obj2 C2))
;(format t "C1 & C2~%")
;(call-next-method))

(defmethod ride :after ((obj1 C1) (obj2 C2))
(format t "after C1 & C2~%"))

(defparameter inst1 (make-instance 'C1))

(defparameter inst2 (make-instance 'C2))


(ride inst1 inst2)




