(ns task-8.class_declaration
  "In this namespace we implemented all methods and macroses to create classes

   And so on..."
  (:gen-class))

(use 'clojure.set)

(def classes-hierarchy
  "This is a hierachy of classes."
  (ref {::Object {::super nil
                    ::fields {}
                    ::init nil
                    ::accessor nil
                    ::reader nil
                    ::writer nil}}))

(defn super-class
  "Indicates a super class of the class."
  [class]
  (get (get @classes-hierarchy class) ::super))

(defn init [field value & map]
  "init section for class-def"
  {:init (apply hash-map field value map)})

(defn attr-accessor [field & fields]
  "attr-accessor section for class-def"
  {:accessor (apply list field fields)})

(defn attr-reader [field & fields]
  "attr-reader section for class-def"
  {:reader (apply list field fields)})

(defn attr-writer [field & fields]
  "attr-writer section for class-def"
  {:writer (apply list field fields)})

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
        init (get sections :init)
        accessor (get sections :accessor)
        reader (get sections :reader)
        writer (get sections :writer)]
    ; actually defining class consists of...
    (concat
      `(let [super# (if (empty? '~supers)
                      (list ::Object)
                      '~supers)
             fields# (set '~fields)]
         ; registering a new class in classes-hierarchy
        (dosync
         (assert (not (contains? @classes-hierarchy ~name)) (format "Forbidden new class name %s." ~name))
         (alter classes-hierarchy assoc ~name {::super super#
                                              ::fields fields#
                                              ::init '~init
                                              ::accessor '~accessor
                                              ::reader '~reader
                                              ::writer '~writer})))
      ; processing list of fields need setters and getters and providing them
      (for [i accessor]
        (def-getter i))
      (for [i accessor]
        (def-setter i))
      (for [i reader]
        (def-getter i))
      (for [i writer]
        (def-setter i)))))

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

(defn instance-class
  "Returns the class name of the instance."
  [instance]
  (instance ::class))

(defn is-instance?
  "Returns true iff obj is an instance of some class."
  [obj]
  (and (map? obj) (contains? obj ::class)))

(defmacro def-generic
  "This macro creates the command ~name that either adds a new virtual version of method ~name to
  the command virtual table or calls the already added version of ~name using perform-effective-command."
  [name]
  `(let [vtable# (ref {})]
     (defn ~name [obj# & args#]
       (if (is-instance? obj#)
         (let [class# (instance-class obj#)
               BFS_classes# (loop [acc# (list class#) ; BFS = Breadth-first search
                                  queue# acc#]
                             (let [head# (first queue#)
                                   fathers# (super-class head#)]
                               (if (empty? queue#) acc#
                                 (recur (concat acc# fathers#) (concat (rest queue#) fathers#)))))
               eff_classes# (distinct (filter #(contains? @vtable# %) BFS_classes#))]
           (println eff_classes#)
           (apply perform-effective-command
                  (concat (list @vtable# eff_classes# 0 obj#) args#)))
         (dosync (alter vtable# assoc (first obj#) (second obj#)))))))

(defmacro def-method
  "This calls the command ~name to add a new virtual version of method ~name to
  the command virtual table. The virtual version is of ~class class."
  [name class args & body]
  `(~name [~class (fn ~args ~@body)]))

;; заглушка для super, который определяется каждый раз в perform-effective-command с помощью binding
(def ^:dynamic call_next_method nil)

(defn perform-effective-command
  "Peforms the command whose virtual versions are all kept in vtable. The performance can go down
  the classes hierarchy from the base class to the last derived one if (super ...) is called."
  [vtable eff_classes eff_index obj & args]
  {:pre [(< eff_index (.size eff_classes))]}
  (let [eff_class (nth eff_classes eff_index)
        eff_method (vtable eff_class)]
    (binding [call_next_method 
              (partial perform-effective-command vtable eff_classes (inc eff_index) obj)]
      (dosync (apply eff_method (concat (list obj) args))))))
    

