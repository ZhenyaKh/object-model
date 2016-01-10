(defclass vehicle ()

((name :initarg :name

:accessor vehicle-name)))

;;;наследник vehicle

(defclass land-vehicle (vehicle) ())
(defclass floating-vehicle (vehicle) ())
(defclass simple-vehicle (vehicle) ())
(defclass flying-vehicle (vehicle) ())

;;;наследник land-vehicle и floating-vehicle

(defclass amphibian-vehicle (land-vehicle floating-vehicle) ())

(defclass armour (amphibian-vehicle) ())
(defclass bicycle (land-vehicle simple-vehicle) ())
(defclass plane (flying-vehicle) ())
;;;остальные классы аналогично

;;;...

;;;обобщенная функция, не привязанная к классу

(defgeneric capabilities (vh))

(defparameter t-90 (make-instance 'armour :name "T-90"))

(defparameter my-bicycle (make-instance 'bicycle :name "Bicycle"))

(defparameter il-86 (make-instance 'plane :name "Il-86"))

(defmethod capabilities ((vh vehicle))
	(format t "~a allows to:~%" (vehicle-name vh)))

(defmethod capabilities ((vh flying-vehicle))
	(call-next-method)
	(format t "fly~%"))

(defmethod capabilities ((vh land-vehicle))
	(call-next-method)
	(format t "move on land~%"))

(defmethod capabilities ((vh floating-vehicle))
	(call-next-method)
	(format t "sail~%"))

(defmethod capabilities ((vh simple-vehicle))
	(call-next-method)
	(format t "be driven by anyone~%"))

;;что напечатается?



