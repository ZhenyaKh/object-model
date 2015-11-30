(ns task-8.core
  (:gen-class))

(use 'clojure.set)

(import 'clojure.lang.Keyword)
(import 'java.io.Writer)

(defmethod print-method Keyword [^Keyword k, ^Writer w]
  (if (.getNamespace k)
    (.write w (str "::" (name k)))
    (.write w (str k))))

(def classes_hierarchy (ref {}))

(dosync (alter classes_hierarchy assoc
          :Object {::super nil
                   ::fields {}}))

(defmacro def-class [name & sections]
  `(do
     (assert (not (contains? @classes_hierarchy ~name)) "Forbidden new class name.")
     (let [sections_map# (apply hash-map (mapcat (fn [section#] section#) '~sections))
         super# (or (sections_map# :super) :Object)
         fields# (or (apply hash-set (sections_map# :fields)) {})]
     (dosync (alter classes_hierarchy assoc
               ~name {::super super#
                      ::fields fields#})))))

(defn super-class [class] ((classes_hierarchy class) ::super))

(defn create-instance [class & fields_values]
  {:pre [(contains? @classes_hierarchy class)]}
  (let [get_all_fields (fn [c fields]
                         (if (not (nil? c))
                           (recur (super-class c) (union fields ((@classes_hierarchy c) ::fields)))
                           fields))
        all_fields (get_all_fields class {})
        fields_values_map (apply hash-map fields_values)
        state (ref {})]
    (assert (= (apply hash-set (keys fields_values_map)) all_fields) "create-instance: wrong fields.")
    (doseq [field_value_pair fields_values_map]
      (dosync (alter state assoc (first field_value_pair) (second field_value_pair))))
    {::class class, ::state state}))

(defn getf [obj field] (let [state (obj ::state)]
                         (assert (contains? @state field) "getf: no such field.")
                         (state field)))

(defn setf [obj field new_value] (let [state (obj ::state)]
                         (assert (contains? @state field))
                         (dosync (alter state assoc field new_value))))

(defn instance-class [instance] (instance ::class))
(defn is_instance? [obj] (and (map? obj) (contains? obj ::class)))

(defmacro def-command [name]
  `(let [vtable# (ref {})]
     (defn ~name [obj# & args#]
       (if (is_instance? obj#)
         (apply perform-effective-command
           (concat (list @vtable# (instance-class obj#) obj#) args#))
         (dosync (alter vtable# assoc (first obj#) (second obj#)))))))

(defmacro def-method [method_name class method_arguments & method_body]
  `(~method_name [~class (fn ~method_arguments ~@method_body)]))

(def ^:dynamic super nil)

(defn perform-effective-command [vtable class obj & args]
  (let [eff_class (loop [x class]
                    (assert (not (nil? x)) "No such method.")
                    (if (contains? vtable x) x (recur (super-class x))))
        eff_method (vtable eff_class)]
    (binding [super (partial perform-effective-command vtable (super-class class) obj)]
      (dosync (apply eff_method (concat (list obj) args))))))

(defn -main []

  (def-class :Base (:fields (:cnt1 :cnt2 :cnt3)))
  (def-class :Derived (:super :Base) (:fields (:cnt4)))
  (def-class :Derived_from_Derived (:super :Derived) (:fields (:cnt5)))

  (def q (create-instance :Base :cnt1 "1" :cnt2 '(2) :cnt3 3))
  (println q)
;  (println (instance-class q))

;  (println (is_instance? q))
;  (println (is_instance? [1 2]))
;  (println (getf q :cnt2))
;  (setf q :cnt2 '(3))
;  (println (getf q :cnt2))

  (def-command increment-cnt)
  (def-method increment-cnt :Base [obj amount amount2]
    (setf obj :cnt3 (+ amount (getf obj :cnt3))))

  (increment-cnt q 3 5)
  (println q "\n")

  (def w (create-instance :Derived :cnt1 "1" :cnt2 '(2) :cnt4 4 :cnt3 3))
  (println w)
  (def-method increment-cnt :Derived [obj amount amount2]
    (super amount amount2)
    (setf obj :cnt4 (+ amount (getf obj :cnt4))))

  (println)
  (println w)
  (increment-cnt w 5 7)
  (println w)

  (def e (create-instance :Derived_from_Derived :cnt1 "1" :cnt2 '(2) :cnt4 4 :cnt3 3 :cnt5 10))
  (println w)
  (def-method increment-cnt :Derived_from_Derived [obj amount amount2]
    (super amount amount2)
    (setf obj :cnt5 (+ amount2 (getf obj :cnt5))))

  (println)
  (println e)
  (increment-cnt e 5 7)
  (println e)

  (println "The End."))
