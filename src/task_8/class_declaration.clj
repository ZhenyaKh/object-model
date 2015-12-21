(ns task-8.class_declaration
  "In this namespace we implemented all methods and macroses to create classes

   And so on..."
  (:require [task-8.util :refer :all])
  (:gen-class))

(use 'clojure.set)

(def classes-hierarchy
  "This is a hierachy of classes."
  (ref {::Object {::super nil
                    ::fields {}
                    :init nil
                    :accessor nil
                    :reader nil
                    :writer nil}}))

(defmacro def-class [name supers fields & sections]
  "This macro creates a class declaration."
  `(let [super# (if (empty? '~supers)
                  (list ::Object)
                  '~supers)
         fields# (set '~fields)
         sections# (apply merge (map eval '~sections))
         init# (get sections# :init)
         accessor# (get sections# :accessor)
         reader# (get sections# :reader)
         writer# (get sections# :writer)]
     (dosync
       (assert (not (contains? @classes-hierarchy ~name)) (format "Forbidden new class name %s." ~name))
       (alter classes-hierarchy assoc ~name {::super super#
                                             ::fields fields#
                                             ::init init#
                                             ::accessor accessor#
                                             ::reader reader#
                                             ::writer writer#}))))

(defn super-class
  "Indicates a super class of the class."
  [class]
  (get (get @classes-hierarchy class) ::super))

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
                               (do 
                                 (println "fields_values_map: " fields_values_map)
    (assert (= (apply hash-set (keys state)) all_fields) "new-instance: wrong fields.")
    {::class class, ::state state})))

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
