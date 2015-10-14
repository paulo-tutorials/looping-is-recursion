(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base' exp']
                 (if (zero? exp')
                   acc
                   (recur (* acc base') base' (dec exp'))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (== 1 (count a-seq)) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or
    (not (= (count seq1) (count seq2)))
    (not (= (first seq1) (first seq2)))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred' pred
         a-seq' a-seq]
    (cond
     (empty? a-seq') nil
     (pred (first a-seq')) index
     :else (recur (inc index) pred' (rest a-seq')))))


(defn avg [a-seq]
  (loop [acc 0
         length (count a-seq)
         a-seq' a-seq]
    (if (empty? a-seq') (/ acc length)
      (recur (+ acc (first a-seq')) length (rest a-seq')))))

(defn parity [a-seq]
  (loop [a-set #{}
         a-seq' a-seq
         toggle (fn [a-set' elem]
                 (if (contains? a-set' elem)
                   (disj a-set' elem)
                   (conj a-set' elem)))]
    (if
     (empty? a-seq') a-set
     (recur (toggle a-set (first a-seq')) (rest a-seq') toggle))))


(defn fast-fibo [n]
  (loop [fibn-1 0
         fibn 1
         remaing n]
    (cond
     (zero? remaing) fibn-1
     (= 1 remaing) fibn
     :else (recur fibn (+ fibn fibn-1) (dec remaing)))))

(defn cut-at-repetition [a-seq]
  (loop [a-vector []
         a-seq' a-seq]
    (cond
     (empty? a-seq') a-vector
     (contains? (set a-vector) (first a-seq')) (recur a-vector (rest a-seq'))
     :else (recur (conj a-vector (first a-seq')) (rest a-seq')))))




