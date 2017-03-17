#!/bin/python3

# ascmidi2notes.py
# (c) 2017 by Milan Gruner
# Part of project "AlgorithMuss"
# Parses the output of mid2asc.c
# See: http://www.archduke.org/midi/

import csv
import sys
import re
from itertools import groupby
from fractions import *

default_note = {
	"bar": 1,
	"crotchet": 0,
	"track": 0,
	"channel": 1,
	"text": "",
	"note": ""
}

def parse_line(line):
	note = default_note

	if len(line) < 9:
		note["text"] = " ".join(line)
		return note

	note["bar"] = int(line[1])
	note["crotchet"] = parse_crotchet(line[3])
	note["track"] = int(line[5])
	note["channel"] = int(line[7])

	if line[8] == "NT":
		note["note"] = line[9].replace("'", "+")
	else:
		note["text"] = ' '.join(line[9:])

	return note

def parse_crotchet(crotchetStr):
	resultValue = Fraction(0, 1)
	fracList = crotchetStr.split("+")
	for frac in fracList:
		if '/' in frac:
			fracParts = frac.split("/")
			numerator = int(fracParts[0])
			denominator = int(fracParts[1])
			
			resultFrac = Fraction(numerator, denominator)
			resultValue += resultFrac
		else:
			resultFrac = Fraction(int(frac), 1)
			resultValue += resultFrac
	
	return resultValue

def main(args):
	if len(args) < 1:
		print("Usage: python ascmidi2notes.py FILENAME")
		return 1

	filename = args[1]
	midiLines = []
	with open(filename, 'r') as f:
		for line in f:
			line = re.sub(' +', '\t', line)
			line = line.strip().split('\t')
			midiLines.append(line)

	midiEvents = map(parse_line, midiLines)

	tracks = [[], []]
	for trackDict in midiEvents:
		key = int(trackDict["track"])
		if len(tracks) <= key:
			tracks[key] = []
		note = dict(trackDict)
		tracks[key].append(note)
		#print(note)

	trackNum = 0
	for track in tracks:
		print("Track #", trackNum)
		trackNum += 1
		noteStr = "["
		lenStr = "["
		
		# TODO group by note["bar"] and make pretty bar lines

		for note in track:
			if note["note"] != "":
				noteStr += note["note"] + " "
				# TODO calculate note length fractions etc.
				lenStr += str(Fraction(1,4) * note["crotchet"]) + " "

		# remove last space before bracket
		noteStr = noteStr[:-1] + "]"
		lenStr = lenStr[:-1] + "]"

		print("pitches", noteStr)
		print("times", lenStr)

	#for note in notes:
	#	print("Note: ", note["crotchet"], " ", note["note"], "\n")
	print("End...")

if __name__ == "__main__":
	main(sys.argv)
