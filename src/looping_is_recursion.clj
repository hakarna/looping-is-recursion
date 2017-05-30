(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
                 (if (zero? e)
                   acc
                   (recur (* acc b) b (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc]
                 (if (empty? (rest acc))
                   (first acc)
                   (recur (rest acc))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]

  (let [helper (fn [acc1 acc2]
                 (if (empty? acc1)
                   true
                   (if (not (= (first acc1) (first acc2)))
                     false
                   (recur (rest acc1) (rest acc2)))))]
    (if (not (== (count seq1) (count seq2)))
    false
    (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq
         p pred]
    (if (empty? s)
      nil
    (if (p (first s))
      acc
      (recur (inc acc) (rest s) p)))))

(defn avg [a-seq]
  (loop [sum 0
         s a-seq
         n 0]
    (if (empty? s)
      (/ sum n)
      (recur (+ sum (first s)) (rest s) (inc n)))))

(defn parity [a-seq]
  (loop [a-set #{}
         s a-seq]
    (if (empty? s)
      a-set
      (recur (if (contains? a-set (first s))
       (disj a-set (first s))
       (conj a-set (first s)))
             (rest s)))))

(defn fast-fibo [n]
  (if (== 0 n)
    0
    (if (== 1 n)
      1
      (loop [f1 0
             f2 1
             k (dec n)]
        (if (== 0 k)
          f2
          (recur f2 (+ f1 f2) (dec k)))))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         a-set #{}
         new-seq []]
    (if (or (empty? s) (contains? a-set (first s)))
      new-seq
      (recur (rest s) (conj a-set (first s)) (conj new-seq (first s))))))

