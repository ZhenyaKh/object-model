(defclass vehicle () ())

(defclass flying-vehicle (vehicle) ())

(defclass plane-vehicle (flying-vehicle) ())

(defclass driver () ())

(defclass human-driver (driver) ())

(defclass pilot-driver (human-driver) ())

(defgeneric ride (dr vh))

(defmethod ride ((dr driver) (vh vehicle))
(format t "DRIVER & VEHICLE~%"))

(defmethod ride ((dr driver) (vh flying-vehicle ))
(format t "DRIVER & FLYING-VEHICLE~%")
(call-next-method))

(defmethod ride ((dr driver) (vh plane-vehicle))
(format t "DRIVER & PLANE-VEHICLE~%")
(call-next-method))

(defmethod ride ((dr human-driver) (vh vehicle))
(format t "HUMAN-DRIVER & VEHICLE~%")
(call-next-method))

(defmethod ride ((dr pilot-driver) (vh vehicle))
(format t "PILOT-DRIVER & VEHICLE~%")
(call-next-method))

(defmethod ride ((dr human-driver) (vh flying-vehicle))
(format t "HUMAN-DRIVER & FLYING-VEHICLE~%")
(call-next-method))

(defmethod ride ((dr human-driver) (vh plane-vehicle))
(format t "HUMAN-DRIVER & PLANE-VEHICLE~%")
(call-next-method))

(defmethod ride ((dr pilot-driver) (vh flying-vehicle))
(format t "PILOT-DRIVER & FLYING-VEHICLE~%")
(call-next-method))

(defmethod ride ((dr pilot-driver) (vh plane-vehicle))
(format t "PILOT-DRIVER & PLANE-VEHICLE~%")
(call-next-method))

;;; let's check:

(defparameter pirx (make-instance 'pilot-driver))

(defparameter il-86 (make-instance 'plane-vehicle))

(ride pirx il-86)




