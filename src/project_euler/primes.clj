(ns project-euler.primes
  (:require [clojure.math.numeric-tower :as math]))

(defn multiple-of?
  [x y]
  (zero? (rem x y)))

(defn multiple-of-some?
  [x divisors]
  (some (partial multiple-of? x) divisors))

(def primes
  (map first
       (iterate (fn [primes-acc]
                  (let [largest (first primes-acc)]
                    (conj primes-acc
                          (first (drop-while
                                  #(multiple-of-some? % primes-acc)
                                  (iterate inc (inc largest)))))))
                '(2))))

(defn mike-primes
  []
  (loop [n 600851475143 factor 2]
    (if (= n 1)
      factor
      (if (zero? (mod n factor))
        (recur (/ n factor) factor)
        (recur n (+ factor 1))))))


'(false false true false)

(defn primep?
  [primes n]
  (some identity
        (pmap
         (fn [prime-group]
           (not (some #(zero? (mod n %)) prime-group)))
         (partition-all 100 primes))))

(defn get-primes
  [limit-fn return-fn]
  (loop [primes []
         n 2]
    (if (limit-fn primes n)
      (return-fn primes n)
      (if (some #(zero? (mod n %)) primes)
          (recur primes (inc n))
          (recur (conj primes n) (inc n))))))

(defn mike-7
  [collection-size]
  (get-primes (fn [primes n] (= (count primes) collection-size))
              (fn [primes n] primes)))

(defn mike-7b
  [collection-size]
  (loop [primes []
         n 2]
    (if (= (count primes) collection-size)
      primes
      (let [max-prime (math/ceil (/ n 2))
            relevant-primes (take-while #(< % max-prime) primes)]
        (if (some #(zero? (mod n %)) relevant-primes)
          (recur primes (inc n))
          (recur (conj primes n) (inc n)))))))

(defn mike-10
  [max-prime]
  (get-primes (fn [primes n] (> n max-prime))
              (fn [primes n] (reduce + primes))))
