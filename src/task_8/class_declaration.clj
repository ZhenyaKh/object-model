(ns task-8.class_declaration
  "In this namespace we implemented all methods and macroses to create classes

   And so on..."
  (:gen-class))

(use 'clojure.set)

(import 'clojure.lang.Keyword)
(import 'java.io.Writer)

(defmethod print-method
  "This is a class hierachy"
  Keyword [^Keyword k, ^Writer w]
  (if (.getNamespace k)
    (.write w (str "::" (name k)))
    (.write w (str k))))

(def classes-hierarchy
  "This is a class hierachy"
  (ref {}))

(dosync (alter classes-hierarchy assoc
          :Object {::super nil
                   ::fields {}}))

(defmacro def-class [name & sections]
  "This macro creates a class declaration"
  `(do
     (assert (not (contains? @classes-hierarchy ~name)) "Forbidden new class name.")
     (let [sections_map# (apply hash-map (mapcat (fn [section#] section#) '~sections))
         super# (or (sections_map# :super) :Object)
         fields# (or (apply hash-set (sections_map# :fields)) {})]
     (dosync (alter classes-hierarchy assoc
               ~name {::super super#
                      ::fields fields#})))))

(defn own-class
  "This function indicates a super"
  [class]
  ((classes-hierarchy class) ::class))

(defn super-class
  "This function indicates a super"
  [class]
  ((classes-hierarchy class) ::super))

(defn create-instance
  "This function creates an instance"
  [class & fields_values]
  {:pre [(contains? @classes-hierarchy class)]}
  (let [get_all_fields (fn [c fields]
                         (if (not (nil? c))
                           (recur (super-class c) (union fields ((@classes-hierarchy c) ::fields)))
                           fields))
        all_fields (get_all_fields class {})
        fields_values_map (apply hash-map fields_values)
        state (ref {})]
    (assert (= (apply hash-set (keys fields_values_map)) all_fields) "create-instance: wrong fields.")
    (doseq [field_value_pair fields_values_map]
      (dosync (alter state assoc (first field_value_pair) (second field_value_pair))))
    {::class class, ::state state}))

(defn getf
  "This... TODO"
  [obj field]
  (let [state (obj ::state)]
    (assert (contains? @state field) "getf: no such field.")
    (state field)))

(defn setf
  "This... TODO"
  [obj field new_value]
  (let [state (obj ::state)]
    (assert (contains? @state field))
    (dosync (alter state assoc field new_value))))

(defn instance-class
  "This... TODO"
  [instance]
  (instance ::class))

(defn is-instance?
  "This... TODO"
  [obj]
  (and (map? obj) (contains? obj ::class)))

(defmacro def-command
  "This macro defines a command... TODO"
  [name]
  `(let [vtable# (ref {})]
     (defn ~name [obj# & args#]
       (if (is-instance? obj#)
         (apply perform-effective-command
           (concat (list @vtable# (instance-class obj#) obj#) args#))
         (dosync (alter vtable# assoc (first obj#) (second obj#)))))))

(defmacro def-method
  "This macro defines a method... TODO"
  [method_name class method_arguments & method_body]
  `(~method_name [~class (fn ~method_arguments ~@method_body)]))

;; what is it?
(def ^:dynamic super nil)

(defn perform-effective-command
  "This... TODO"
  [vtable class obj & args]
  (let [eff_class (loop [x class]
                    (assert (not (nil? x)) "No such method.")
                    (if (contains? vtable x) x (recur (super-class x))))
        eff_method (vtable eff_class)]
    (binding [super (partial perform-effective-command vtable (super-class class) obj)]
      (dosync (apply eff_method (concat (list obj) args))))))
