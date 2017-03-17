;; blood.clj
;; (c) 2017 by Milan Gruner
;; Part of project "AlgorithMuss"

(ns algorithmuss.blood)
(use 'overtone.live)
(use 'overtone.inst.piano)

(load "midi/midifile")
(load "midi/midi_player")

(def midi (song-from-file "midi/blood.mid"))
(def song (song-with-instruments midi {"Track 1" piano})

; Show available MIDI tracks
; (map :name (:tracks midi))
(def piano-play (midi-poly-player piano))


