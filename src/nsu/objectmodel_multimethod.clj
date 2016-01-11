(in-ns 'nsu.objectmodel)

(declare get-classes-from-graphs)
(declare inc-inds)
(declare dec-inds)

(defmacro def-generic
  "This macro declares a multimethod ~name."
  [name]
  `(let [primarytable# (ref {})
         aroundtable# (ref {})
         beforetable# (ref {})
         aftertable# (ref {})]
     (defn ~name [objs# & args#]
       ;; When defining method name objs# is [(:A1 A2) (fn[] (...))] and args# 
       ;; is empty or consists of one keyword.
       ;; When performing name objs# is [inst1 inst2] and args# is (arg1 arg_2 ...).
       (if (is-instance? (first objs#))
         (let [classes# (map instance-class objs#)
               ;; For each parameter class we build a Breadth-first search graph of its predecessors.
               ;; So we have a list of graphs.
               BFS_graphs_not_uniq# (map #(loop [acc# (list %) queue# acc#]
                                            (let [head# (first queue#) fathers# (super-class head#)]
                                              (if (empty? queue#) acc#
                                                (recur (concat acc# fathers#) (concat (rest queue#) fathers#)))))
                                      classes#)
               ;; For each graph we make all its classes-vertices distinct and 
               ;; remove all ::Object entries.
               BFS_graphs# (map (fn [graph#] (distinct (remove #(= % ::Object) graph#)))
                             BFS_graphs_not_uniq#)
               max_inds# (map #(dec (.size %)) BFS_graphs#)
               graphs_number# (.size BFS_graphs#)]
           (when (not (empty? @beforetable#))
             (apply perform-effective-before-after (concat (list @beforetable# BFS_graphs#
                                                             (repeat graphs_number# 0) max_inds# inc-inds objs#) args#)))
           (let [result#
                  (apply perform-effective-command (concat (list @primarytable# BFS_graphs#
                                                             (repeat graphs_number# 0) max_inds# inc-inds objs#) args#))]
           (when (not (empty? @aftertable#))
             (apply perform-effective-before-after (concat (list @aftertable# BFS_graphs#
                                                             max_inds# max_inds# dec-inds objs#) args#)))
           result#))
         (if (not (empty? args#))
           (let [support-type# (first args#)]
             (cond
               (= support-type# :around) (dosync (alter aroundtable# assoc (first objs#) (second objs#)))
               (= support-type# :before) (dosync (alter beforetable# assoc (first objs#) (second objs#)))
               (= support-type# :after)  (dosync (alter aftertable# assoc (first objs#) (second objs#)))
               true (assert false "Incorrect type of support.")))
           ;; we add a new version of ~name multimethod to its virtual table.
           (dosync (alter primarytable# assoc (first objs#) (second objs#))))))))

(defmacro def-method
  "This macro defines a particular version of the multimethod ~name."
  [name objs_and_args & body]
  (let [classes_objs (filter #(seq? %) objs_and_args)
        objs (map #(second %) classes_objs)
        classes (map #(first %) classes_objs)
        args (vec (map #(if (seq? %) (second %) %) objs_and_args))]
    (assert (= objs (distinct objs)) (str "Identical names of the arguments of method " name "."))
    `(~name ['~classes (fn ~args ~@body)])))

(defmacro def-support [type name objs_and_args & body]
  "This macro defines support method to corresponding multimethod ~name with specified type: around, before or after"
  (let [classes_objs (filter #(seq? %) objs_and_args)
        objs (map #(second %) classes_objs)
        classes (map #(first %) classes_objs)
        args (vec (map #(if (seq? %) (second %) %) objs_and_args))]
    (assert (= objs (distinct objs)) (str "Identical names of the arguments of method " name "."))
    `(~name ['~classes (fn ~args ~@body)] '~type)))

;; dummy for call-next-method that is defined each time in perform-effective-command using 'binding.
(def ^:dynamic call-next-method nil)

(defn perform-effective-command
  "perform-effective-command performs the multimethod whose virtual versions are all kept in vtable.
   It is recursive and can be called explicitly by a user with (call-next-method ...) construction."
  [vtable BFS_graphs indices indices_to inds-changer objs & args]
  (let [classes (get-classes-from-graphs BFS_graphs indices)]
    (if (contains? vtable classes)
      (binding [call-next-method
                (if (= indices indices_to)
                  (fn [& x] (assert nil (str "(call-next-method) can not be called from a method if"
                                             "the classes of ALL its arguments are base.")))
                  (partial perform-effective-command vtable BFS_graphs
                    (inds-changer indices indices_to) indices_to inds-changer objs))]
        (dosync (apply (vtable classes) (concat objs args))))
      (apply perform-effective-command (concat (list vtable BFS_graphs
                    (inds-changer indices indices_to) indices_to inds-changer objs) args)))))

(defn perform-effective-before-after
  ;; TODO: some docs here.
  [vtable BFS_graphs indices indices_to inds-changer objs & args]
  (let [graphs_number (.size BFS_graphs)
        classes (get-classes-from-graphs BFS_graphs indices)]
    (when (contains? vtable classes)
      (dosync (apply (vtable classes) (concat objs args))))
    (when (not (= indices (if (= inds-changer inc-inds) indices_to (repeat graphs_number 0))))
      (apply perform-effective-before-after (concat (list vtable BFS_graphs
                                                      (inds-changer indices indices_to) indices_to inds-changer objs) args)))))

(defn get-classes-from-graphs
  ;; TODO: some docs here.
  [BFS_graphs indices]
  (let [graphs_number (.size BFS_graphs)]
    (loop [i (dec graphs_number)
           classes '()]
      (if (< i 0)
        classes
        (let [graph (nth BFS_graphs i)
              index (nth indices i)]
          (recur (dec i) (conj classes (nth graph index))))))))

(defn inc-inds
  ;; TODO: some docs here.
  [sub_inds sub_max_inds]
  (let [index (last sub_inds)
        max_index (last sub_max_inds)]
    (if (empty? sub_inds) '()
      (if (< index  max_index)
        (concat (drop-last sub_inds) [(inc index)])
        (concat (inc-inds (drop-last sub_inds) (drop-last sub_max_inds)) [0])))))

(defn dec-inds
  ;; TODO: some docs here.
  [sub_inds sub_max_inds]
  (let [index (last sub_inds)
        max_index (last sub_max_inds)]
    (if (empty? sub_inds) '()
      (if (> index  0)
        (concat (drop-last sub_inds) [(dec index)])
        (concat (dec-inds (drop-last sub_inds) (drop-last sub_max_inds)) [max_index])))))
