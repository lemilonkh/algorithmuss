;; funky.clj
;; (c) 2017 by Milan Gruner
;; Part of project "AlgorithMuss"

(ns algorithmuss.funky
	(:use	[leipzig melody]
		[overtone.live]
		[overtone.inst.piano]
		[algorithmuss.instruments])
	(:require
		[leipzig.scale :as scale]
		[leipzig.live :as live]
		[leipzig.temperament :as temperament]))

(def funky
  (->> (phrase [2 1/2 1/2 1/2 2.5 1/2 1/2 1/2 2.5 1/2 1/2 1/2 2.5 1 1]
               [0 -1 0 2 -3 -4 -3 -1 -5 -6 -5 -3 -7 -6 -5])
       (where :pitch (comp scale/G scale/minor))
       (all :part :funky)
       (all :amp 1)))

(defmethod live/play-note :funky [{hertz :pitch seconds :duration amp :amp}]
	(when hertz (funky :freq hertz :dur seconds :amp (or amp 1))))

(->> funky
	(wherever :pitch, :pitch temperament/equal)
	(tempo (bpm 120))
	live/play) 


