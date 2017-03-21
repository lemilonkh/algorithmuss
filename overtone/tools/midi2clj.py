# midi2clj.py
# (c) 2017 by Milan Gruner
# Part of project "AlgorithMuss"
# Translate MIDI files into data structures for Overtone and Leipzig

import sys
from subprocess import check_output

def main():
	if len(sys.argv) < 2:
		print("Usage: python3 midi2clj.py MIDI_FILE")
		sys.exit(1)
	
	ascmidi = check_output(["./midiconverter/mid2asc", sys.argv[1]])
	
	# TODO feed into ascmidi2notes.py
	# refactor that to allow direct string input instead of filename
	# => refactor ascmidi2notes.py into module

if __name__ == "__main__":
	main()
