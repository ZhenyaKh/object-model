(in-ns 'nsu.objectmodel)

(defmacro def-generic
  "This macro creates the command ~name that either adds a new virtual version of method ~name to
  the command virtual table or calls the already added version of ~name using perform-effective-command.
  For the latter a multiple inheritance queue 'eff_classes' of classes having the ~name method implemented is created."
  [name]
  `(let [vtable# (ref {})]
     (defn ~name [objs# & args#]
       ;; when defining method name objs# is [(:A1 A2) (fn[] (...))] and args# is empty
       ;; when performing name objs# is [inst1 inst2] and args# is (arg1 arg_2 ...)
       (if (is-instance? (first objs#))
         (let [classes# (map instance-class objs#)
               BFS_graphs# (map #(loop [acc# (list %) queue# acc#]
                                  (let [head# (first queue#) fathers# (super-class head#)]
                                    (if (empty? queue#) acc#
                                      (recur (concat acc# fathers#) (concat (rest queue#) fathers#)))))
                             classes#)
               BFS_graphs# (map (fn [graph#] (distinct (remove #(= % ::Object) graph#))) BFS_graphs#)]
           (println "\n" BFS_graphs#)
           (apply perform-effective-command
             (concat (list @vtable# BFS_graphs# (repeat (.size BFS_graphs#) 0) objs#) args#)))
         (dosync (alter vtable# assoc (first objs#) (second objs#)))))))


(defmacro def-method [name objs_and_args & body]
  (let [classes_objs (filter #(seq? %) objs_and_args)
        objs (map #(second %) classes_objs)
        classes (map #(first %) classes_objs)
        args (vec (map #(if (seq? %) (second %) %) objs_and_args))]
    (assert (= objs (distinct objs)) (str "Identical names of the arguments of method " name "."))
    `(~name ['~classes (fn ~args ~@body)])))

;; dummy for call-next-method that is defined each time in perform-effective-command using 'binding.
(def ^:dynamic call-next-method nil)

(defn perform-effective-command
  "Performs the command whose virtual versions are all kept in vtable. The performance can go along all the classes
  of 'eff_classes' multiple inheritance queue created by def-generic if (call-next-method ...) is called."
  [vtable BFS_graphs indices objs & args]
  (let [classes (loop [i (dec (.size BFS_graphs))
                       classes '()]
                  (if (< i 0)
                    classes
                    (let [graph (nth BFS_graphs i)
                          index (nth indices i)]
                      (recur (dec i) (conj classes (nth graph index))))))
        max_inds (map #(dec (.size %)) BFS_graphs)
        inc-inds (fn self [sub_inds sub_max_inds]
                      (let [index (last sub_inds)
                            max_index (last sub_max_inds)]
                        (if (empty? sub_inds) '()
                          (if (< index  max_index)
                            (concat (drop-last sub_inds) [(inc index)])
                            (concat (self (drop-last sub_inds) (drop-last sub_max_inds)) [0])))))]
   ; (println BFS_graphs indices objs)
    (if (contains? vtable classes)
      (let [eff_classes classes]
        (binding [call-next-method
                  (if (= indices max_inds)
                    (fn [& x] (assert nil (str "(call-next-method) can not be called from a method if "
                                            "the classes of ALL its arguments are base.")))
                    (partial perform-effective-command vtable BFS_graphs (inc-inds indices max_inds) objs))]
          (dosync (apply (vtable eff_classes) (concat objs args)))))
      (apply perform-effective-command (concat (list vtable BFS_graphs (inc-inds indices max_inds) objs) args)))))
