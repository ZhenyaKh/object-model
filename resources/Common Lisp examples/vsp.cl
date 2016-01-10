(defclass A1 () ())

(defclass B1 (A1) ())

(defclass A2 () ())

(defclass B2 (A2) ())

(defclass A3 () ())

(defclass B3 (A3) ())

(defgeneric ride (obj1 obj2 obj3))

(defmethod ride :around ((obj1 A1) (obj2 A2) (obj3 A3))
(format t "start A1 & A2 & A3~%")
(call-next-method)
(format t "finish A1 & A2 & A3~%"))

(defmethod ride :before ((obj1 A1) (obj2 A2) (obj3 A3))
(format t "before A1 & A2 & A3~%"))

(defmethod ride ((obj1 A1) (obj2 A2) (obj3 A3))
(format t "A1 & A2 & A3~%"))

(defmethod ride :after ((obj1 A1) (obj2 A2) (obj3 A3))
(format t "after A1 & A2 & A3~%"))

;;

(defmethod ride :around ((obj1 A1) (obj2 A2) (obj3 B3))
(format t "start A1 & A2 & B3~%")
(call-next-method)
(format t "finish A1 & A2 & B3~%"))

(defmethod ride :before ((obj1 A1) (obj2 A2) (obj3 B3))
(format t "before A1 & A2 & B3~%"))

(defmethod ride ((obj1 A1) (obj2 A2) (obj3 B3))
(format t "A1 & A2 & B3~%")
(call-next-method))

(defmethod ride :after ((obj1 A1) (obj2 A2) (obj3 B3))
(format t "after A1 & A2 & B3~%"))

;;

(defmethod ride :around ((obj1 A1) (obj2 B2) (obj3 A3))
(format t "start A1 & B2 & A3~%")
(call-next-method)
(format t "finish A1 & B2 & A3~%"))

(defmethod ride :before ((obj1 A1) (obj2 B2) (obj3 A3))
(format t "before A1 & B2 & A3~%"))

(defmethod ride ((obj1 A1) (obj2 B2) (obj3 A3))
(format t "A1 & B2 & A3~%")
(call-next-method))

(defmethod ride :after ((obj1 A1) (obj2 B2) (obj3 A3))
(format t "after A1 & B2 & A3~%"))

;;

(defmethod ride :around ((obj1 A1) (obj2 B2) (obj3 B3))
(format t "start A1 & B2 & B3~%")
(call-next-method)
(format t "finish A1 & B2 & B3~%"))

(defmethod ride :before ((obj1 A1) (obj2 B2) (obj3 B3))
(format t "before A1 & B2 & B3~%"))

(defmethod ride ((obj1 A1) (obj2 B2) (obj3 B3))
(format t "A1 & B2 & B3~%")
(call-next-method))

(defmethod ride :after ((obj1 A1) (obj2 B2) (obj3 B3))
(format t "after A1 & B2 & B3~%"))

;;

(defmethod ride :around ((obj1 B1) (obj2 A2) (obj3 A3))
(format t "start B1 & A2 & A3~%")
(call-next-method)
(format t "finish B1 & A2 & A3~%"))

(defmethod ride :before ((obj1 B1) (obj2 A2) (obj3 A3))
(format t "before B1 & A2 & A3~%"))

(defmethod ride ((obj1 B1) (obj2 A2) (obj3 A3))
(format t "B1 & A2 & A3~%")
(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 A2) (obj3 A3))
(format t "after B1 & A2 & A3~%"))

;;

(defmethod ride :around ((obj1 B1) (obj2 A2) (obj3 B3))
(format t "start B1 & A2 & B3~%")
(call-next-method)
(format t "finish B1 & A2 & B3~%"))

(defmethod ride :before ((obj1 B1) (obj2 A2) (obj3 B3))
(format t "before B1 & A2 & B3~%"))

(defmethod ride ((obj1 B1) (obj2 A2) (obj3 B3))
(format t "B1 & A2 & B3~%")
(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 A2) (obj3 B3))
(format t "after B1 & A2 & B3~%"))

;;

(defmethod ride :around ((obj1 B1) (obj2 B2) (obj3 A3))
(format t "start B1 & B2 & A3~%")
(call-next-method)
(format t "finish B1 & B2 & A3~%"))

(defmethod ride :before ((obj1 B1) (obj2 B2) (obj3 A3))
(format t "before B1 & B2 & A3~%"))

(defmethod ride ((obj1 B1) (obj2 B2) (obj3 A3))
(format t "B1 & B2 & A3~%")
(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 B2) (obj3 A3))
(format t "after B1 & B2 & A3~%"))

;;

(defmethod ride :around ((obj1 B1) (obj2 B2) (obj3 B3))
(format t "start B1 & B2 & B3~%")
(call-next-method)
(format t "finish B1 & B2 & B3~%"))

(defmethod ride :before ((obj1 B1) (obj2 B2) (obj3 B3))
(format t "before B1 & B2 & B3~%"))

(defmethod ride ((obj1 B1) (obj2 B2) (obj3 B3))
(format t "B1 & B2 & B3~%")
(call-next-method))

(defmethod ride :after ((obj1 B1) (obj2 B2) (obj3 B3))
(format t "after B1 & B2 & B3~%"))

;;; let's check:

(defparameter inst1 (make-instance 'B1))

(defparameter inst2 (make-instance 'B2))

(defparameter inst3 (make-instance 'B3))

(ride inst1 inst2 inst3)




