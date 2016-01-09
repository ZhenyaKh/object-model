(in-ns 'nsu.objectmodel)

(defn init [field value & map]
  "init section for def-class"
  {:init (apply hash-map field value map)})
