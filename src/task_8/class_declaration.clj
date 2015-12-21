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
  {:init (apply hash-map field value map)})

(defn attr-accessor [field & fields]
  {:accessor (apply list field fields)})

(defn attr-reader [field & fields]
  {:reader (apply list field fields)})

(defn attr-writer [field & fields]
  {:writer (apply list field fields)})

(defn get-all-fields [сlass]
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

; defines a piece of code for getter
(defn def-getter [field]
  {:pre [(keyword? field)]}
  `(defn ~(symbol (str "get-" (name field))) [object#]
     (let [state# (get object# ::state)]
       (assert (contains? state# ~field) "getf: no such field.")
       @(get state# ~field))))

; defines a piece of code for setter
(defn def-setter [field]
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
    (concat
     `(let [super# (if (empty? '~supers)
                     (list ::Object)
                     '~supers)
            fields# (set '~fields)]
       (dosync
        ;(assert (not (contains? @classes-hierarchy ~name)) (format "Forbidden new class name %s." ~name))
        (alter classes-hierarchy assoc ~name {::super super#
                                             ::fields fields#
                                             ::init '~init
                                             ::accessor '~accessor
                                             ::reader '~reader
                                             ::writer '~writer})))
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
         (even? (.size fields_values))]}
  (let [all_fields (get-all-fields class)
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
  [name args]
  `(let [vtable# (ref {})]
     (defn ~name [obj# & args#]
       (if (is-instance? obj#)
         (apply perform-effective-command
           (concat (list @vtable# (instance-class obj#) obj#) args#))
         (dosync (alter vtable# assoc (first obj#) (second obj#)))))))

(defmacro def-method
  "This calls the command ~name to add a new virtual version of method ~name to
  the command virtual table. The virtual version is of ~class class."
  [name class args & body]
  `(~name [~class (fn ~args ~@body)]))

;; заглушка для super, который определяется каждый раз в perform-effective-command с помощью binding
(def ^:dynamic super nil)

(defn perform-effective-command
  "Peforms the command whose virtual versions are all kept in vtable. The performance can go down
  the classes hierarchy from the base class to the last derived one if (super ...) is called."
  [vtable class obj & args]
  (let [eff_class (loop [x class]
                    (assert (not (nil? x)) "No such method.")
                    (if (contains? vtable x) x (recur (super-class x))))
        eff_method (vtable eff_class)]
    (binding [super (partial perform-effective-command vtable (super-class class) obj)]
      (dosync (apply eff_method (concat (list obj) args))))))
