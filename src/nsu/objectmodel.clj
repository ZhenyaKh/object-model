(ns nsu.objectmodel
  "In this namespace we implemented all methods and macroses to create
  the multiple inheritance object model resenbling on of Common Lisp"
  (:gen-class))

(use 'clojure.set)

(def classes-hierarchy
  "This is a hierachy of classes."
  (ref {::Object {::super nil
                    ::fields {}
                    ::init nil}}))

(defn super-class
  "Indicates a super class of the class."
  [class]
  (get (get @classes-hierarchy class) ::super))

(defn init [field value & map]
  "init section for class-def"
  {:init (apply hash-map field value map)})

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

(defn def-getter [field]
  "defines a piece of code for getter"
  {:pre [(keyword? field)]}
  `(defn ~(symbol (str "get-" (name field))) [object#]
   (let [state# (get object# ::state)]
     (assert (contains? state# ~field) "getf: no such field.")
     @(get state# ~field))))


(defn def-setter [field]
  "defines a piece of code for setter"
  {:pre [(keyword? field)]}
  `(defn ~(symbol (str "set-" (name field))) [object# new-value#]
     (let [state# (get object# ::state)]
       (assert (contains? state# ~field) "setf: no such field.")
       (dosync (ref-set (get state# ~field) new-value#)))))

(defmacro def-class [name supers fields & sections]
  "This macro creates a class declaration."
  (let [sections (apply merge (map eval sections))
        init (get sections :init)]
    `(let [super# (if (empty? '~supers)
                    (list ::Object)
                    '~supers)
           fields# (set '~fields)]
       ; registering a new class in classes-hierarchy
       (dosync
         (assert (not (contains? @classes-hierarchy ~name)) (format "Forbidden new class name %s." ~name))
         (alter classes-hierarchy assoc ~name {::super super#
                                              ::fields fields#
                                              ::init '~init})))))
(defn new-instance
  "Creates an instance of the class."
  [class & fields_values]
  {:pre [(contains? @classes-hierarchy class)
         (or (nil? fields_values) (even? (.size fields_values)))]}
  (let [fields_values (if (nil? fields_values) '() fields_values)
        all_fields (get-all-fields class)
        all_inits (get-all-inits class)
        fields_values_map (merge all_inits (apply hash-map fields_values))
        state (into
                {}
                (map
                  (fn [pair] [(first pair) (ref (second pair))])
                  fields_values_map))] 
    (assert (= (apply hash-set (keys state)) all_fields) "new-instance: wrong fields.")
    {::class class, ::state state}))

(defn getf
  "This is the getter (common for all classes)."
  [obj field]
  (let [state (obj ::state)]
    (assert (contains? state field) "getf: no such field.")
    (deref (state field))))

(defn setf
  "This is the setter (common for all classes)."
  [obj field new_value]
  (let [state (obj ::state)]
    (assert (contains? state field))
    (dosync (ref-set (state field) new_value))))

(load "objectmodel_method_def")
(load "objectmodel_introspection")
