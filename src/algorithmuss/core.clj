;; core.clj
;; (c) 2017 by Milan Gruner
;; Part of project "Algorithmuss"

(ns algorithmuss.core)
    ;(:use [leipzig melody scale live]
    ;  [overtone.inst.piano])
    ;(:require [overtone.live :as overtone]))

(use 'overtone.live)
(use 'algorithmuss.instruments)
(require 'algorithmuss.drums)

; Settings
(def metro (metronome 140)) ; BPM

; melody
(def scale-degrees [:i :ii :vi :vii :i+ :_ :vii# :_ :i+ :viib :vi :_ :vii :_])
(def scale-pitches (degrees->pitches scale-degrees :dorian :E4))

(defn play [beat notes sep]
  (let [note (first notes)]
    (when note
      (at (metro beat)
        ;(trem (midi->hz note) 10 3 1)))
        (saw (midi->hz note))))
    (let [next-time (metro (+ beat sep))]
      (apply-at next-time play [next-time (rest notes) sep] []))))

; beat
(defn swinger [beat]
  (at (metro beat) (hihat 0.8 0.5))
	(at (metro (inc beat)) (hihat 0.8 0.08))
	(at (metro (+ 1.65 beat)) (hihat 0.8 0.08))
  (apply-at (metro (+ 2 beat)) swinger (+ 2 beat) [])

(defn looper [nome sound]
  (let [beat (nome)]
    (at (nome beat) (sound))
    (apply-at (nome (inc beat)) looper [nome sound] [])))

(defn -main []
  (println "Hello overtone!"))

(swinger (metro))
(play (metro) scale-pitches 1)
;(looper metro kick)
;(stop)
