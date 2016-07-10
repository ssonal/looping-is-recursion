(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                  (if (zero? k)
                    acc
                    (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2) (not= (first seq1) (first seq2))) false 
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq
         index 0]
    (cond 
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [a-seq a-seq
         sum 0
         l 0]
    (if (empty? a-seq)
      (/ sum l)
      (recur (rest a-seq) (+ sum (first a-seq)) (inc l)))))

(defn parity [a-seq]
 (loop [acc   #{}
         a-seq a-seq]
    (if (empty? a-seq)
      acc
      (recur (toggle acc (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [n n
         a 0
         b 1]
    (if (zero? n)
      a
      (recur (dec n) b (+ a b)))))

(defn cut-at-repetition [a-seq]
  (loop [acc   []
         seen  #{}
         a-seq a-seq]
    (if (or (contains? seen (first a-seq)) (empty? a-seq))
      acc
      (recur (conj acc (first a-seq)) (conj seen (first a-seq)) (rest a-seq)))))

