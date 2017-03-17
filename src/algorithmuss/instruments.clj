;; instruments.clj
;; (c) 2017 by Milan Gruner
;; Part of project "AlgorithMuss"

(ns algorithmuss.instruments)
(use 'overtone.live)

(definst funky [freq 440 dur 1.0 amp 1.0 cutoff 2200 boost 12 dist-level 0.015]
  (let [env (env-gen (adsr 0.3 0.7 0.5 0.3) (line:kr 1.0 0.0 dur) :action FREE)
        level (+ (* freq 0.25) 
                (env-gen (adsr 0.5 0.3 1 0.5) (line:kr 1.0 0.0 (/ dur 2)) :level-scale cutoff)) 
        osc (mix [ 
                (saw freq) 
                (saw (* freq 0.7491535384383409))])]
        (-> osc
                (bpf level 0.6)
                (* env amp)
                pan2
                (clip2 dist-level)
                (* boost)
                distort)))

(definst trem "Tremolo saw" [freq 440 depth 10 rate 6 length 3]
  (* 0.3
    (line:kr 0 1 length FREE)
    (saw (+ freq (* depth (sin-osc:kr rate 0 1 0))))))


