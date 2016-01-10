(defclass vehicle () ())

(defclass fairy-vehicle (vehicle) ())
(defclass plane-vehicle (vehicle) ())

(defclass fairy-plane-vehicle (fairy-vehicle plane-vehicle) ())

(defclass driver () ())

(defclass pilot-driver (driver) ())
(defclass fairy-driver (driver) ())

(defclass fairy-pilot-driver (fairy-driver pilot-driver) ())

(defgeneric ride (dr vh))

;;1
(defmethod ride ((dr driver) (vh vehicle))
(format t "DRIVER & VEHICLE~%"))

;;2
(defmethod ride ((dr driver) (vh fairy-vehicle))
(format t "DRIVER & FAIRY-VEHICLE~%")
(call-next-method))

;;3
(defmethod ride ((dr driver) (vh plane-vehicle))
(format t "DRIVER & PLANE-VEHICLE~%")
(call-next-method))

;;4
(defmethod ride ((dr driver) (vh fairy-plane-vehicle))
(format t "DRIVER & FAIRY-PLANE-VEHICLE~%")
(call-next-method))

;;5
(defmethod ride ((dr pilot-driver) (vh vehicle))
(format t "PILOT-DRIVER & VEHICLE~%")
(call-next-method))

;;6
(defmethod ride ((dr pilot-driver) (vh fairy-vehicle ))
(format t "PILOT-DRIVER & FAIRY-VEHICLE~%")
(call-next-method))

;;7
(defmethod ride ((dr pilot-driver) (vh plane-vehicle))
(format t "PILOT-DRIVER & PLANE-VEHICLE~%")
(call-next-method))

;;8
(defmethod ride ((dr pilot-driver) (vh fairy-plane-vehicle))
(format t "PILOT-DRIVER & FAIRY-PLANE-VEHICLE~%")
(call-next-method))

;;9
(defmethod ride ((dr fairy-driver) (vh vehicle))
(format t "FAIRY-DRIVER & VEHICLE~%")
(call-next-method))

;;10
(defmethod ride ((dr fairy-driver) (vh fairy-vehicle ))
(format t "FAIRY-DRIVER & FAIRY-VEHICLE~%")
(call-next-method))

;;11
(defmethod ride ((dr fairy-driver) (vh plane-vehicle))
(format t "FAIRY-DRIVER & PLANE-VEHICLE~%")
(call-next-method))

;;12
(defmethod ride ((dr fairy-driver) (vh fairy-plane-vehicle))
(format t "FAIRY-DRIVER & FAIRY-PLANE-VEHICLE~%")
(call-next-method))

;;13
(defmethod ride ((dr fairy-pilot-driver) (vh vehicle))
(format t "FAIRY-PILOT-DRIVER & VEHICLE~%")
(call-next-method))

;;14
(defmethod ride ((dr fairy-pilot-driver) (vh fairy-vehicle ))
(format t "FAIRY-PILOT-DRIVER & FAIRY-VEHICLE~%")
(call-next-method))

;;15
(defmethod ride ((dr fairy-pilot-driver) (vh plane-vehicle))
(format t "FAIRY-PILOT-DRIVER & PLANE-VEHICLE~%")
(call-next-method))

;;16
(defmethod ride ((dr fairy-pilot-driver) (vh fairy-plane-vehicle))
(format t "FAIRY-PILOT-DRIVER & FAIRY-PLANE-VEHICLE~%")
(call-next-method))

;;; let's check:

(defparameter fairy-pirx (make-instance 'fairy-pilot-driver))

(defparameter fairy-il-86 (make-instance 'fairy-plane-vehicle))

(ride fairy-pirx fairy-il-86)




