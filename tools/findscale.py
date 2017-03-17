# findscale.py
# (c) 2017 by Milan Gruner
# Part of project "AlgorithMuss"

import music21
import sys

if len(sys.argv) < 1:
	print("Usage: python3 findscale.py MIDI_FILENAME")
	sys.exit(1)

score = music21.converter.parse(sys.argv[1])

methods = ['key', 'Krumhansl', 'AardenEssen']

for method in methods:
	key = score.analyze(method)
	print("Using method:", method)
	print("Key found:", key.tonic.name, key.mode)
