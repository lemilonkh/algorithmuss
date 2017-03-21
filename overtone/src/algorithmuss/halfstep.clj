;; halfstep.clj
;; (c) 2017 by Milan Gruner
;; Half-step dubstep rhythm
;; Original idea by Darkraqqen

(ns algorithmuss.halfstep
  (:require
    [overtone.live :refer :all]
    [leipzig.live :as live]
    [leipzig.melody :refer :all]
    [overtone.inst.drum :as drums]))

; Load drum kit
(def kit {
  :kick drums/kick2
  :snare drums/snare
  :closed-hat drums/closed-hat
  :open-hat drums/open-hat })

; Drum kit player
(defmethod live/play-note :beat [note]
  (when-let [fn (-> (get kit (:drum note)) :sound)]
    (fn :amp (:amp note))))

; Helper for programming drum sequences
(defn drum [drum times length & {:keys [amp] :or {amp 1}}]
  (map #(zipmap [:time :duration :drum :amp]
    [%1 (- length %1) drum amp]) times))

; Half step beat
(def halfstep
  (->>
    (reduce with [
      (drum :kick [0/8 3/8 8/8] 2)
      (drum :snare [4/8 12/8] 2)])
      ;(drum :closed-hat (sort (concat [3.75 7.75] (range 1/2 8 1))) 8)
    (all :part :beat)))

; Play it!
(->> halfstep
     (tempo (bpm 110))
     live/play)

;(live/jam (var halfstep))