(in-ns 'nsu.objectmodel)

(defn init [field value & map]
  "init section for class-def"
  {:init (apply hash-map field value map)})
