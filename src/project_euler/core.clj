(ns project-euler.core)

(defn multiple-of?
  [x y]
  (zero? (rem x y)))

(defn multiple-of-some?
  [x divisors]
  (some (partial multiple-of? x) divisors))

;; problem 1
(defn multiples-sum
  [limit & divisors]
  (reduce + (filter #(multiple-of-some? % divisors) (range limit))))

;; mike's
(reduce + (distinct (concat (range 0 1000 3) (range 0 1000 5))))

;; mark
(defn findSum 
  [limit]
  (loop [i 0
         sum 0]
    (println (str sum))
    (println (str "i " i))
    (if (> i 3)
      (println sum)
      (recur (inc i) (inc sum)))))

;; problem 2
(def fib
  (map first
       (iterate (fn [[a b]] [b (+ a b)])
                [0 1])))

(defn fib 
  [num1 num2 sum limit times-run]
  (let [updated-sum (if (= sum 0)
                      (+ sum num2)
                      (if (= (rem num2 2) 0)  
                        (+ sum num2)))]
    (if (< num2 limit)
      (fib num2 (+ num1 num2) updated-sum limit (inc times-run))
      (println (str "and the final sum is: " updated-sum)))))




(defn even-fib-sum
  [limit]
  (->> fib
       (filter even?)
       (take-while #(< % limit))
       (reduce +)))

(loop [sum 0
       n1 1
       n2 1]
  (if (< n1 4000000)
    (if (even? n1)
      (recur (+ sum n1) n2 (+ n1 n2))
      (recur sum n2 (+ n1 n2)))
    sum))


;; problem 3
(def primes
  (map first
       (iterate (fn [primes-acc]
                  (let [largest (first primes-acc)]
                    (conj primes-acc
                          (first (drop-while
                                  #(multiple-of-some? % primes-acc)
                                  (iterate inc (inc largest)))))))
                '(2))))


;; mike
(loop [n 600851475143 factor 2]
  (if (= n 1)
    factor
    (if (zero? (mod n factor))
      (recur (/ n factor) factor)
      (recur n (+ factor 1)))))


;; problem 4
(defn symmetrical?
  [seq]
  (loop [seq seq]
    (if (empty? seq)
      true
      (let [first (first seq)
            last (last seq)]
        (if (= first last)
          (recur (drop 1 (butlast seq))))))))

(defn palindrome?
  [number]
  (symmetrical? (str number)))

(defn largest-palindrome
  [upper-limit lower-limit]
  (loop [outer-level upper-limit]
    (if-let [inner-level (loop [inner-level upper-limit]
                           (let [x (* outer-level inner-level)]
                             (if (palindrome? x)
                               inner-level
                               (if (= inner-level outer-level)
                                 nil
                                 (recur (dec inner-level))))))]
      [outer-level inner-level]
      (recur (dec outer-level)))))

;; problem 5
(apply *  (take-while #(< % 20) primes))
(take-while #(< % 20) primes)

;; problem 6
(defn sqr
  [x]
  (* x x))

(defn sum-diff
  [limit]
  (let [nums (range (inc limit))]
    (- (sqr (reduce + nums))
       (reduce + (map sqr nums)))))

;; problem 7
(last (take 10001 primes))

;; problem 8

;; problem 10

;; problem 11
