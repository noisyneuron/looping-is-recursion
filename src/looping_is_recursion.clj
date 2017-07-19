(ns looping-is-recursion)


(defn power [base exp]
  (let [acc-power (fn [acc b e]
                    (if (= 0 e)
                      acc
                      (recur (* b acc) b (dec e))))]
    (acc-power 1 base exp)))


(defn last-element [a-seq]
  (let [get-last (fn [el s]
                   (if (empty? s)
                     el
                     (recur (first s) (rest s))))]
    (get-last nil a-seq)))


(defn seq= [seq1 seq2]
  (let [helper (fn [same s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) true
                   (and (empty? s1) ((complement empty?) s2)) false
                   (and (empty? s2) ((complement empty?) s1)) false
                   ((complement =) (first s1) (first s2)) false
                   :else (recur true (rest s1) (rest s2))))]
    (helper true seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [idx 0
         p pred
         s a-seq]
    (cond
      (empty? s) nil
      (p (first s)) idx
      :else (recur (inc idx) p (rest s)))))


(defn avg [a-seq]
  (loop [cnt 0
         sum 0
         s a-seq]
    (if (empty? s)
      (/ sum cnt)
      (recur (inc cnt) (+ sum (first s)) (rest s)))))


(defn toggle [el a-set]
  (if (contains? a-set el)
    (set (remove #{el} a-set))
    (set (cons el a-set))))


(defn parity [a-seq]
  (loop [odds #{}
         s a-seq]
    (if (empty? s)
      odds
      (recur (toggle (first s) odds) (rest s)))))


(defn fast-fibo [n]
  (loop [cnt n
         nm0 1
         nm1 0]
    (cond
      (= cnt 0) nm1
      (= cnt 1) nm0
      :else (recur (dec cnt) (+ nm0 nm1) nm0))))


(defn cut-at-repetition [a-seq]
  (loop [norep []
         s a-seq]
    (cond
      (contains? (set norep) (first s)) norep
      (empty? s) norep
      :else (recur (conj norep (first s)) (rest s)))))

