(ns algorithmuss.melody
  (:use [leipzig melody scale live]
    [overtone.inst.piano])
  (:require [overtone.live :as overtone]))

;; Minimal example of how to play a melody using Clojure/Overtone/Leipzig, based on code
;; at https://github.com/ctford/leipzig

;; by Lee Spector, lspector@hampshire.edu, 20140204

;; Add the following to your dependencies in project.cl, and do "lein deps" if your environment requires it:
;; [leipzig "0.7.0"]


(defmethod play-note :default [{midi :pitch}] (piano midi))

(defn play-melody
  [pitches durations]
  (->> (phrase durations pitches)
       (where :part (is :melody))
       (where :time (bpm 60))
       (where :pitch (comp C major))
       play))

;; C major scale
(play-melody [0 1 2 3 4 5 6 7]
               [1 1 1 1 1 1 1 1])

;; C chromatic scale
(play-melody [0 0.5 1 1.5 2 3 3.5 4 4.5 5 5.5 6 7]
               [1 1   1 1   1 1 1   1 1   1 1   1 1])

;(stop)

(defn random-beats [n]
  (if (or (<= n 1/8)
          (zero? (rand-int 12)))
    [n]
    (concat (random-beats (/ n 2))
            (random-beats (/ n 2)))))

;(random-beats 4)

;; here's a non-recursive version... uglier but safer:

(defn random-beats [n]
  (loop [result []
         remaining [n]]
    (if (empty? remaining)
      result
      (let [first-thing (first remaining)]
        (if (or (<= first-thing 1/8)
                (zero? (rand-int 12)))
          (recur (conj result first-thing)
                 (rest remaining))
          (recur result
                 (concat [(/ first-thing 2) (/ first-thing 2)]
                         (rest remaining))))))))

;(random-beats 4)

(defn random-pitch []
  (if (zero? (rand-int 8))
    nil
    (- (rand-int 48) 24)))

; (random-pitch)

(play-melody (repeatedly random-pitch) (random-beats 4))

#_(let [rhy1 (random-beats 2)
        p1 (repeatedly (count rhy1) random-pitch)
        rhy2 (random-beats 2)
        p2 (repeatedly (count rhy2) random-pitch)
        durations (concat rhy1 rhy1 rhy1 rhy1
                          rhy2 rhy2 rhy1 rhy1)
        pitches (concat p1 p1 p2 p2
                        (reverse p1) (reverse p1) p2 p1)]
    (println "durations:" durations)
    (println "pitches:" pitches)
    (play-melody pitches durations))


;; This will allow you to use a better sounding piano, but the first time you
;; do it it'll take a lot of time to download samples.

;(use 'overtone.inst.sampled-piano)

;(defmethod play-note :default [{midi :pitch}] (sampled-piano midi))