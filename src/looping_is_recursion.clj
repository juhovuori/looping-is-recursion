(ns looping-is-recursion)

(defn power [base exp]
  (let [h (fn [a b e]
            (if (zero? e)
                a
                (recur (* b a) b (dec e))))]
    (h 1 base exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (loop [s1 seq1 s2 seq2]
    (cond
        (and (empty? s1) (empty? s2)) true
        (or (empty? s1) (empty? s2)) false
        (not (= (first s1) (first s2))) false
        :else (recur (rest s1) (rest s2)))))

(defn find-first-index [pred a-seq]
  (loop [i 0 a-s a-seq]
    (cond
        (empty? a-s) nil
        (pred (first a-s)) i
        :else (recur (inc i) (rest a-s)))))

(defn avg [a-seq]
  (loop [c 0 avg-so-far nil a-seq-to-go a-seq]
    (if (empty? a-seq-to-go)
      avg-so-far
      (let [new-average-so-far (if (zero? c)
                                 (first a-seq-to-go)
                                 (/ (+ (* c avg-so-far) (first a-seq-to-go))
                                    (inc c)))]
        (recur (inc c)
                new-average-so-far
                (rest a-seq-to-go))))))

(defn toggle [a-set elem]
  ((if
    (contains? a-set elem)
    disj
    conj) a-set elem))

(defn parity [a-seq]
  (loop [my-set #{} a-s a-seq]
    (if (empty? a-s)
      my-set
      (recur (toggle my-set (first a-s)) (rest a-s)))))

(defn fast-fibo [n]
  (cond 
    (= 0 n) 0
    (= 1 n) 1
    :else (loop [f-nn-2 0 f-nn-1 1 nn 2]
      (let [f-nn (+ f-nn-2 f-nn-1)] 
        (if (= n nn)
          f-nn
          (recur f-nn-1 f-nn (inc nn)))))))

(defn cut-at-repetition [a-seq]
  (loop [seq-so-far [] a-s a-seq]
    (if (or (empty? a-s)
            (some (fn [x] (= x (first a-s))) seq-so-far))
      seq-so-far
      (recur (conj seq-so-far (first a-s)) (rest a-s)))))

