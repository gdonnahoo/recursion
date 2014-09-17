(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (seq coll) (= (rest coll) '()))
    true
    false))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   :else (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (cond
   (<= (count seq-1) (count seq-2)) seq-2
   :else seq-1))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) (first a-seq)
   (singleton? a-seq) (first a-seq)
   :else (longest-sequence (cons (seq-max (first a-seq) (second a-seq)) (drop 2 a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (cond
     (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
     :else (my-filter pred? (rest a-seq))
     )))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (cond
     (=  (first a-seq) elem) true
     :else (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (cond
     (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (cond
     (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
     :else a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq)
           (empty? b-seq))
    true
    (cond
     (or (empty? a-seq) (empty? b-seq)) false
     (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
     :else false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (= up-to 0) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (drop 1 a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (butlast a-seq)))))

(defn rotation-helper [nmbr-rotations coll]
  (if (empty? coll)
    '(())
    (cond
     (== nmbr-rotations (count coll)) '()
     :else (cons coll (rotation-helper (inc nmbr-rotations) (concat (rest coll) (list (first coll))))))))

(defn rotations [a-seq]
  (rotation-helper 0 a-seq))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (cond
      (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (cons (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (> n 0) (my-drop (dec n) (rest coll))
   :else (cons (first coll) (my-drop n (rest coll)))))


(defn halve-helper [half-1 half-2 cnt-1 cnt-2 a-seq]
  (cond
     (> cnt-1 0) (halve-helper (cons (first a-seq) half-1) half-2 (dec cnt-1) cnt-2  (rest a-seq))
     (> cnt-2 0) (halve-helper half-1 (cons (first a-seq) half-2) cnt-1 (dec cnt-2) (rest a-seq))
     :else (vector (reverse half-1) (reverse half-2))))

(defn halve [a-seq]
  (halve-helper '() '() (int (/ (count a-seq) 2)) (- (count a-seq) (/ (count a-seq) 2)) a-seq))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
   (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
 (cond
  (empty? a-seq) '()
  (= (count a-seq) 1) (list (first a-seq))
  :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort(last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

