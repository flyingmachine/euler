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

;; problem 2
(def fib (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))
(defn even-fib-sum
  [limit]
  (->> fib
       (filter even?)
       (take-while #(< % limit))
       (reduce +)))


;; problem 3
(def primes
  (map first
       (iterate (fn [primes-acc]
                  (let [largest (first primes-acc)]
                    (take 1 (drop-while #(some (partial multiple-of? %) primes-acc)
                                       (iterate inc (inc largest))))))
                '(2))))
