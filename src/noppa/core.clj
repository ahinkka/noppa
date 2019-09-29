(ns noppa.core
  (:require [clojure.set :refer [union subset?]]))


(defn- remove-first-n [pred n coll]
  (let [indices (filter some? (map-indexed #(if (pred %2) %1 nil) coll))
        indices-to-remove (set (take n indices))]
    (vec
     (keep-indexed
      (fn [idx item] (if (not (contains? indices-to-remove idx)) item nil))
      coll))))

(defn- score-increase-consumes-faces?
  "Helper post-condition function that can be used for sanity checking scoring."
  [old-score new-score old-faces new-faces]
  (cond
    (and (= old-score new-score) (= (count old-faces) (count new-faces))) true
    (and (> new-score old-score) (> (count old-faces) (count new-faces))) true
    :else false))

(defn- straight [{:keys [score faces]}]
  (if (= faces '[1 2 3 4 5 6])
    {:score 500 :faces '[]}
    {:score score :faces faces}))

(defn- three-pairs [{:keys [score faces]}]
  (if (= 3 (count (filter #(= % 2) (vals (frequencies faces)))))
    {:score 500 :faces '[]}
    {:score score :faces faces}))

(defn- multiple-ones [{:keys [score faces]}]
  (cond
    (= (count (filter #(= % 1) faces)) 6)  {:score (+ score 2000) :faces '[]}
    (>= (count (filter #(= % 1) faces)) 3) {:score (+ score 1000)
                                            :faces (remove-first-n #(= % 1) 3 faces)}
    :else                                  {:score score :faces faces}))

(defn- three-or-six-same [{:keys [score faces number]}]
  (let [number-count (count (filter some? (map-indexed #(if (= %2 number) %1 nil) faces)))]
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

(defn- one [{:keys [score faces]}]
  (if (not-empty (filter #(= % 1) faces))
    {:score (+ score 100)
     :faces (vec (remove-first-n #(= % 1) 1 faces))}
    {:score score :faces faces}))

(defn- five [{:keys [score faces]}]
  (if (not-empty (filter #(= % 5) faces))
    {:score (+ score 50)
     :faces (vec (remove-first-n #(= % 5) 1 faces))}
    {:score score :faces faces}))

(defn- score* [fns previous-results]
  {:pre  [(set? previous-results)
          (every? map? previous-results)
          (coll? fns)
          (every? fn? fns)]
   :post [(set? %)
          (every? #(contains? % :score) %)
          (every? #(contains? % :faces) %)]}
  (let [new-results
        (reduce union
                (map (fn [previous-result]
                       (set (map #(% previous-result) fns)))
                     previous-results))]
    (if (subset? new-results previous-results)
      previous-results
      (score* fns (union previous-results new-results)))))

(defn score
  "Calculate scoring possibilities and leftover dice faces for a collection of dice faces."
  [faces]
  {:pre  [(coll? faces)
          (every? int? faces)
          (every? #(>= % 1) faces)
          (every? #(<= % 6) faces)]
   :post [(set? %)
          (every? map? %)
          (every? #(contains? % :score) %)
          (every? #(contains? % :faces) %)]}
  (score* (list straight
                three-pairs
                multiple-ones
                three-same
                one
                five)
          #{{:score 0 :faces (vec (sort faces))}}))
