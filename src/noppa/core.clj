(ns noppa.core)

(defn- remove-first-n [pred n coll]
  (let [indices (filter some? (map-indexed #(if (pred %2) %1) coll))
        indices-to-remove (set (take n indices))]
    (vec
     (keep-indexed
      (fn [idx item] (if (not (contains? indices-to-remove idx)) item))
      coll))))

(defn- straight [{:keys [score faces]}]
  (if (= faces '[1 2 3 4 5 6])
    {:score 500 :faces '[]}
    {:score score :faces faces}))

(defn- three-pairs [{:keys [score faces]}]
  (if (= 3 (count (filter #(= % 2) (vals (frequencies faces)))))
    {:score 500 :faces '[]}
    {:score score :faces faces}))

(defn- three-ones [{:keys [score faces]}]
  (cond
    (= (count (filter #(= % 1) faces)) 6)  {:score (+ score 2000) :faces '[]}
    (>= (count (filter #(= % 1) faces)) 3) {:score (+ score 1000)
                                            :faces (remove-first-n #(= % 1) 3 faces)}
    :else                                  {:score score :faces faces}))

(defn- three-or-six-same [{:keys [score faces number]}]
  (let [number-count (count (filter some? (map-indexed #(if (= %2 number) %1) faces)))]
    (cond
      (= number-count 6)  {:score (+ score (* 2 (* 100 number))) :faces '[]}
      (>= number-count 3) {:score (+ score (* 100 number))
                           :faces (remove-first-n #(= % number) 3 faces)}
      :else               {:score score :faces faces})))

(defn- three-same [{:keys [score faces]}]
  (-> {:score score :faces faces}
      (#(three-or-six-same (assoc % :number 2)))
      (#(three-or-six-same (assoc % :number 3)))
      (#(three-or-six-same (assoc % :number 4)))
      (#(three-or-six-same (assoc % :number 5)))
      (#(three-or-six-same (assoc % :number 6)))
      (dissoc :number)))

(defn- ones [{:keys [score faces]}]
  {:score (+ score (* 100 (count (filter #(= % 1) faces))))
   :faces (vec (remove #(= % 1) faces))})

(defn- fives [{:keys [score faces]}]
  {:score (+ score (* 50 (count (filter #(= % 5) faces))))
   :faces (vec (remove #(= % 5) faces))})

(defn score
  "Calculate score for a sequence of dice faces."
  [faces]
  {:pre  [(or (seq? faces) (vector? faces))
          (every? int? faces)]}
  (-> {:score 0 :faces (vec (sort faces))}
      straight
      three-ones
      three-pairs
      three-same
      ones
      fives))
