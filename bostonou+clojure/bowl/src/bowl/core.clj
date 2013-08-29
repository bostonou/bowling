(ns bowl.core
  (:require [clojure.core.async :refer [<!! >!! chan thread close!]]))

(defn ball
  [rolls score future-rolls]
  (thread
   (loop []
     (when-let [pins (<!! rolls)]
       (condp = pins
         :strike (>!! score (+ 10 (<!! future-rolls) (<!! future-rolls)))
         :spare (>!! score (+ 10 (<!! future-rolls)))
         (>!! score pins))
       (recur)))))

(defn frame
  [frames rolls future-rolls]
  (thread
   (loop []
     (when-let [[[b1 b2 b3] [b4 b5] [b6]] (<!! frames)]
       (cond
        (= 10 b1) (do (>!! rolls :strike)
                      (>!! future-rolls (or b2 b4 0))
                      (>!! future-rolls (or b3 b5 b6 0)))
        (and b2 (= 10 (+ b1 b2))) (do (>!! rolls :spare)
                                      (>!! future-rolls (or b3 b4 0)))
        :else (>!! rolls (+ b1 (or b2 0))))
       (recur)))))

(defn score-game
  [frames]
  (let [frames-chan (chan)
        rolls (chan)
        future-rolls (chan)
        score (chan)]
    (thread
     (doseq [f (partition-all 3 1 frames)] (>!! frames-chan f))
     (close! frames-chan))
    (frame frames-chan rolls future-rolls)
    (ball rolls score future-rolls)
    (let [final-score (loop [n (count frames)
                             s 0]
                        (if (> n 0)
                          (recur (dec n) (+ s (<!! score)))
                          s))]
      (close! rolls)
      (close! future-rolls)
      (close! score)
      final-score)))

(defn test-bowling [b-func]
  (assert (= 300 (b-func [[10] [10] [10] [10] [10] [10] [10] [10] [10] [10 10 10]])))
  (assert (= 0 (b-func (repeat 10 [0 0]))))
  (assert (= 1 (b-func [[1]])))
  (assert (= 5 (b-func [[2 3]])))
  (assert (= 10 (b-func [[10]])))
  (assert (= 10 (b-func [[6 4]])))
  (assert (= 30 (b-func [[10] [10]])))
  (assert (= 60 (b-func [[10] [10] [10]])))
  (assert (= 38 (b-func [[10] [7 3] [3 2]])))
  (assert (= 96 (b-func [[7 3] [6 4] [10] [10] [10]])))
  (assert (= 299 (b-func [[10] [10] [10] [10] [10] [10] [10] [10] [10] [10 10 9]]))))

(defn run-tests
  []
  (test-bowling score-game))
