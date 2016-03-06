(ns genetic-programming-in-clojure.core
  (:gen-class))

;;; Toy problem

;; we could use standard functions
( even? (count (filter identity [true false true])))
;; ... but we are limited to and, or, nand, not, nor
(def target-data
  [[false false false true]
   [false false true false]
   [false true false false]
   [false true true true]
   [true false false false]
   [true false true true]
   [true true false true]
   [true true true false]])

(def function-table (zipmap '(and or nand nor not)
                            '(2   2   2    2  1)))

(defn random-function []
  (rand-nth (keys function-table)))

;; random terminal in genetic programming is simply one of the leaves of program tree
(defn random-terminal []
  (rand-nth '[in1 in2 in3]))

;; nand and nor are not built-in
(defn nand [a b] (not (and a b)))
(defn nor [a b] (not (or a b)))

;; random tree generation
(defn random-code [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))

(defn codesize [p]
  (if (seq? p) (count (flatten p))) 1)

(defn random-subtree [p]
  (if (zero? (rand-int (codesize p)))
  p
  (random-subtree
    (rand-nth (apply concat
                     (map #(repeat (codesize %) %))
                     (rest p))))))

(defn replace-random-subtree [p replacement]
  "Returns program with a random subexpression replaced with replacement.

  This implementation tries to make probability of targeting some subexpression proportional
  to the size of this subexpression."
  (if (zero? (rand-int (codesize p)))
    replacement
    (let [position-to-change
          (rand-nth (apply concat
                           (map #(repeat (codesize %1) %2)
                                (rest p)
                                (iterate inc 1))))]
      (map #(if %1 (replace-random-subtree %2 replacement) %2)
           (for [n (iterate inc 0)] (= n position-to-change))
           p)
      )))

;; Mutation & crossover
(defn mutate [p]
  (replace-random-subtree p (random-code 2)))

(defn crossover [p q]
  (replace-random-subtree p (random-subtree q)))

(defn error [p]
  "error or 'fitness' function tells us how bad the input program is - zero means it's perfect."
  (let [value-function (eval (list 'fn '[in1 in2 in3] p))]
    (reduce + (map (fn [[in1 in2 in3 correct_output]]
                     (if (= (value-function in1 in2 in3)
                            correct_output)
                       0
                       1))
            target-data))))


(defn sort-by-error [population]
  "Sort the population of programs by error"
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]]
                    (< err1 err2))
                  (map #(vector (error %) %)
                       population)))))

;; how do we select better parents?
(defn select [sorted-population tournament-size]
  (let [size (count sorted-population)]
    (nth sorted-population
         (apply min (repeatedly tournament-size
                                #(rand-int size))))))

;;; Evolution
;; verbose version
(defn evolve [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 2)))]
    (let [best (first population)
          best-error (error best)]
      (println "========================")
      (println "Generation: " generation)
      (println "Best error: " best-error)
      (println "Best program: " best)
      (println "     Median error: " (error (nth population
                                                 (int (/ popsize 2)))))
      (println "     Average program size: "
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      (if (< best-error 0.1)                                ;; good enough to count as success
        (println "Success: " best)
        (recur
          (inc generation)
          (sort-by-error
            (concat
              (repeatedly (* 1/10 popsize) #(mutate (select population 5)))
              (repeatedly (* 8/10 popsize) #(crossover (select population 5)
                                                       (select population 5)))
              (repeatedly (* 1/10 popsize) #(select population 5))
              ))))))

  (evolve 1000)


;;; Lexical selection (with code) mentioned in presentation



  (defn -main
    "Starts evolution."
    [& args]
    (evolve 1000)))
