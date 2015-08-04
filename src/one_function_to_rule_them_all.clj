(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (seq (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [counter _] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) '() a-seq))

(defn min-max-element [a-seq]
  (let [min (reduce (fn [a b] (if (< a b) a b)) a-seq)
        max (reduce (fn [a b] (if (> a b) a b)) a-seq)]
    [min max]))

(defn insert [sorted-seq n]
  (loop [s sorted-seq
         result '()]
    (cond 
      (and (empty? s) (empty? result)) (conj result n)
      (and (empty? result) (< n (first s))) (reverse (into (conj result n) s))
      (empty? s) (reverse result)
      :else (recur (rest s) 
                   (if (>= n (first s))
                     (conj result (first s) n)
                     (conj result (first s)))))))

(defn insertion-sort [a-seq]
  (reduce (fn [s x] (insert s x)) '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
							  (if (contains? a-set elem)
							    (disj a-set elem)
							    (conj a-set elem)))]
    (reduce (fn [s e] (toggle s e)) #{} a-seq)))

(defn minus 
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& all]
  (count all))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [a] true))
  ([x] x)
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))
  

(defn my-map [f a-seq]
  [:-])