(in-ns 'nsu.objectmodel)

(defn get-all-fields [сlass]
  "Extracts all fields of the class including all predecessor"
  (let [class-def (get @classes-hierarchy сlass)
        class-fields (get class-def ::fields)
        supers (super-class сlass)]
    (if (not (nil? supers))
      (union
        class-fields
        (apply union (map
                       (fn [super] (get-all-fields super))
                       supers)))
      class-fields)))

(defn get-all-inits [сlass]
  "Extracts all default values of the class including all predecessor.
  Successor overrides values set up in predecessor"
  (let [class-def (get @classes-hierarchy сlass)
        class-init (get class-def ::init)
        supers (super-class сlass)]
    (if (not (nil? supers))
      (merge
        (apply merge (map
                       (fn [super] (get-all-inits super))
                       supers))
        class-init)
      class-init)))