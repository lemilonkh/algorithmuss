# getscale.py
# (c) 2017 by Milan Gruner
# Part of project "AlgorithMuss"

import mingus.core.notes as notes
import mingus.core.scales as scales
import sys

def main():
	if len(sys.argv) < 3:
		print("Usage: python3 getscale.py BASETONE MODE")
		print("e,g.   python3 getscale.py C# minor")
		sys.exit(1)
	
	basetone = sys.argv[1]
	mode = sys.argv[2]
	
	if not notes.is_valid_note(basetone):
		print("Invalid note: ", basetone)
		sys.exit(1)

	print("Your scale is:")
	scale = get_scale(basetone, mode)
	print(scale)

def get_scale(basetone, mode):	
	scale = []
	
	if mode == "major":
		scale = scales.major(basetone)
	if mode == "minor":
		scale =scales.minor(basetone)
	if mode == "ionian":
		scale = scales.ionian(basetone)
	if mode == "dorian":
		scale = scales.dorian(basetone)
	if mode == "phrygian":
		scale = scales.phrygian(basetone)
	if mode == "lydian":
		scale = scales.lydian(basetone)
	if mode == "mixolydian":
		scale = scales.mixolydian(basetone)
	if mode == "aeolian":
		scale = scales.aeolian(basetone)
	if mode == "locrian":
		scale = scales.locrian(basetone)
	# TODO implement chromatic scales
	
	return scale

def get_note_index(note, scale):
	if not note in scale:
		print("Note", note, "not in scale", scale, "!")
		return -127
	
	# TODO implement approximate search and C# = Db equality (using notes.note_to_int())
	# TODO implement note neighbor (halftone step) search (then append # or b)
	return scale.index(note.trim())

def get_roman(num):
	roman = OrderedDict()
	roman[1000] = "M"
	roman[900] = "CM"
	roman[500] = "D"
	roman[400] = "CD"
	roman[100] = "C"
	roman[90] = "XC"
	roman[50] = "L"
	roman[40] = "XL"
	roman[10] = "X"
	roman[9] = "IX"
	roman[5] = "V"
	roman[4] = "IV"
	roman[1]  = "I"
	
	def roman_num(num):
		for dec in roman.keys():
			x, y = divmod(num, dec)
			yield roman[dec] * x
			num -= (dec * x)
			if num > 0:
				roman_num(num)
			else:
				break
	
	return "".join(roman_num(num))

if __name__ == "__main__":
	main()
