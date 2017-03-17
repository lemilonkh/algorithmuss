;; drums.clj
;; (c) 2017 by Milan Gruner
;; Part of project "Algorithmuss"

(ns algorithmuss.drums)

(use 'overtone.live)

; Settings
(def metro (metronome 165)) ; BPM

; Instruments
(definst kick "Kick drum" [amp 0.7 t 0.01 env-len 0.01]
  (let [
    env (env-gen (perc env-len t) 1 1 0 1 FREE)
    kick-sample (sample (freesound-path 2086))
    sqr (* (env-gen (perc env-len 0.04)) (pulse 420 0.2))
    filt (lpf (+ sqr kick-sample) 600)]
  (* amp env filt)))

(definst hihat [amp 0.8 t 0.04]
  (let [
    env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
    noise (white-noise)
    sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
    filt (bpf (+ sqr noise) 9000 0.5)]
  (* amp env filt)))

; beat
(defn rhythm [beat]
  (at (metro beat) (hihat 0.8 0.5))
        (at (metro (inc beat)) (kick 0.8 0.08))
        (at (metro (+ 1.65 beat)) (hihat 0.8 0.08))
  (apply-at (metro (+ 2 beat)) rhythm (+ 2 beat) []))

;(rhythm (metro))
