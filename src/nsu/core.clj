(ns nsu.core
  (:require [nsu.objectmodel :refer :all])
  (:gen-class))

(def-class :Vehicle ()
  (:name)
  (init :name "Unnamed"))

(def-class :LandVehicle (:Vehicle) ())
(def-class :FloatingVehicle (:Vehicle) ())
(def-class :FlyingVehicle (:Vehicle) ())
(def-class :SimpleVehicle (:Vehicle) ())
(def-class :AmphibianVehicle (:LandVehicle :FloatingVehicle) ())
(def-class :Armour (:AmphibianVehicle) ())
(def-class :Bicycle (:LandVehicle :SimpleVehicle) ())
(def-class :Plane (:FlyingVehicle) ())

;; Drivers
(def-class :Driver ()
  (:name)
  (init :name "Unnamed"))

(def-class :AnimalDriver (:Driver) ())
(def-class :HumanDriver (:Driver) ())
(def-class :Pilot (:HumanDriver) ())

;; Methods

(def-generic capabilities)

(def-method capabilities [(:Vehicle v)]
	(println (getf v :name) "allows to:"))
(def-method capabilities [(:FlyingVehicle v)]
	(call-next-method)
	(println "fly"))
(def-method capabilities [(:LandVehicle v)]
	(call-next-method)
	(println "move on land"))
(def-method capabilities [(:FloatingVehicle v)]
	(call-next-method)
	(println "sail"))
(def-method capabilities [(:SimpleVehicle v)]
	(call-next-method)
	(println "be driven by anyone"))

(def-generic ride)

(def-method ride [(:Driver d) (:Vehicle v)]
  (println (getf d :name) "rides" (getf v :name)))

(def-method ride [(:AnimalDriver d) (:SimpleVehicle v)]
  (println (getf d :name) "is smart and rides" (getf v :name)))

(def-method ride [(:AnimalDriver d) (:Vehicle v)]
  (println (getf d :name) "is not smart enough to ride" (getf v :name)))

(def-method ride [(:Driver d) (:FlyingVehicle v)]
  (println (getf d :name) "requires special training to fly" (getf v :name)))

(def-method ride [(:Pilot d) (:FlyingVehicle v)]
  (println (getf d :name) "flies on" (getf v :name)))

(def-support :before ride [(:Driver d) (:LandVehicle v)]
  (println "Fuel the tank"))

(def-support :after ride [(:Driver d) (:LandVehicle v)]
  (println "Turn on alarm"))

(def-support :before ride [(:Driver d) (:FloatingVehicle v)]
  (println "Set sails"))

(def-support :after ride [(:Driver d) (:FloatingVehicle v)]
  (println "Take in sails"))

(def-support :before ride [(:Driver d) (:FlyingVehicle v)]
  (println "Check parachute"))

(def-support :after ride [(:Driver d) (:FlyingVehicle v)]
  (println "Be happy with successful landing"))

(def-support :before ride [(:Driver d) (:Armour v)]
  (println "Load ammunition"))

(def-support :after ride [(:Driver d) (:Armour v)]
  (println "Leave vehicle without hurts"))

(def t-90 (new-instance :Armour :name "T-90"))
(def my-bicycle (new-instance :Bicycle :name "My bicycle"))
(def il-86 (new-instance :Plane :name "Il-86"))

(def monkey (new-instance :AnimalDriver :name "Monkey"))
(def anonymous (new-instance :HumanDriver :name "Anonymous"))
(def pirx (new-instance :Pilot :name "Pirx"))

(defn -main []
  (println "-main")

  (capabilities [t-90])
  (println)
  
  (ride [monkey my-bicycle])
  (println)
  (ride [monkey t-90])
  (println)
  (ride [anonymous il-86])
  (println)
  (ride [pirx il-86])
  (println)
; Expected output:

; T-90 allows to:
; move on land
; sail
;
; Monkey is smart and rides Bicycle
; Monkey is not smart enough to ride T-90
; Anonymous requires special training to fly Il-86
; Pirxflies on Il-86

  (println "\nThe End"))
