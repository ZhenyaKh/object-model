(ns task-8.util
  (:require [clojure.set :refer :all])
  (:require [task-8.class_declaration :refer :all]))

(defn init [field value & map]
  {:init (apply hash-map field value map)})

(defn attr-accessor [field & fields]
  {:accessor (apply list field fields)})

(defn attr-reader [field & fields]
  {:reader (apply list field fields)})

(defn attr-writer [field & fields]
  {:writer (apply list field fields)})

(defn get-all-fields [сlass]
  (let [class-def (get @classes-hierarchy сlass)
        class-fields (get class-def :task-8.class_declaration/fields)
        supers (super-class сlass)]
    (if (not (nil? supers))
      (union
        class-fields
        (apply union (map
                       (fn [super] (get-all-fields super))
                       supers)))
      class-fields)))

(defn get-all-inits [сlass]
  (let [class-def (get @classes-hierarchy сlass)
        class-init (get class-def :task-8.class_declaration/init)
        supers (super-class сlass)]
    (if (not (nil? supers))
      (merge
        (apply merge (map
                       (fn [super] (get-all-inits super))
                       supers))
        class-init)
      class-init)))
