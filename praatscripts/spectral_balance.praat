﻿# This script goes through sound and TextGrid files in a directory. For
# each TextGrid two tiers are required: one interval tier for which all
# labels are printed (main tier) and another interval tier on which the
# acoustically stable parts of vowels are indicated (sub tier). Then, for
# each non-empty label at the main tier, the script searches for a
# subinterval at the sub tier. If this subinterval is found, several
# measures in the spectrum are made, being: both frequency (Hz) and
# intensity (dB) of the first harmonic (H1) and the first to the fourth
# formant (F1, F2, F3 and F4).
#
# Spectral balance is calculated by subtracting the intensity of the
# formant (A1-A4) from the intensity of H1. This method is based on
# Sluijter and Van Heuven (1995) and Stevens and Hanson (1995) and is
# shown to correlate with stress.
#
# Usage notes: (negative) dB values are to be interpreted with regard to 
# the standard dBA reference value of 20 µPa. Negative intensity values
# for the spectral tilt measures indicate sound below the hearing threshold
# and should be interpreted with caution. If this happens structurally,
# check whether the subinterval is large enough and whether the speech is
# loud enough.
#
# With regard to this script:
#
#	- all measures mentioned above are printed in the result file
# 	- both tiers must be interval tiers
#	- all labels are printed for ALL non-empty intervals at the main tier
#	- only labels of non-empty intervals at the sub tier are printed
#	- all non-existing values/labels are printed as "0"
#	- N.B. bandwith errors when creating Ltas are ignored! (nocheck)
#	- gender of speaker is taken into account to set frequency ceiling for formant measurements
#
# This script is created by Constantijn Kaland and based on "collect pitch
# data from files" by Mietta Lennes and "spectral_profile" by Jörg Mayer.
# 09-2017.

form Get spectral balance from labeled segments in files
	comment Directory of sound files
	text sound_directory 
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile output.txt
	comment Take all non-empty intervals from this tier:
	sentence Tierall syllable
	comment ... and analyze the stable vowel parts from this tier (if they exist):
	sentence Tier intpeaks
	comment Pitch analysis parameters
	positive Time_step 0.01
	positive Minimum_pitch_(Hz) 75
	positive Maximum_pitch_(Hz) 500
	choice Speaker_gender 1
		button male
		button female
endform

# select maximum frequency for formant calculation based on speaker gender
if speaker_gender = 1
	maxFormant = 5000
else
	maxFormant = 5500
endif

# Here, you make a listing of all the sound files in a directory.
# The example gets file names ending with ".wav" from directory

Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings

# Check if the result file exists:
if fileReadable (resultfile$)
	pause The result file 'resultfile$' already exists! Do you want to overwrite it?
	filedelete 'resultfile$'
endif

# Write a row with column titles to the result file:
# (remember to edit this if you add or change the analyses!)

titleline$ = "Filename	Main tier label	Sub tier label	H1 (Hz)	F1 (Hz)	F2 (Hz)	F3 (Hz)	F4 (Hz)	H1 (dB)	A1 (dB)	A2 (dB)	A3 (dB)	A4 (dB)	H1-A1 (dB)	H1-A2 (dB)	H1-A3 (dB)	H1-A4 (dB)'newline$'"
fileappend "'resultfile$'" 'titleline$'

# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'sound_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every sound file that was opened:
	soundname$ = selected$ ("Sound", 1)
	select Sound 'soundname$'
	To Pitch... time_step minimum_pitch maximum_pitch
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Find the tier number that has the label given in the form:
		call GetTier 'tier$' tier
		call GetTier 'tierall$' tierall
		numberOfIntervals = Get number of intervals... tierall
		# Pass through all intervals in the main tier:
		for interval to numberOfIntervals
			label$ = Get label of interval... tierall interval
			if label$ <> ""
				# reset values
				h1 = 0
				medp = 0
				a1 = 0
				a2 = 0
				a3 = 0
				a4 = 0
				f1 = 0
				f2 = 0
				f3 = 0
				f4 = 0
				h1a1$ = "0"
				h1a2$ = "0"
				h1a3$ = "0"
				h1a4$ = "0"
				subintlabel$ = "0"
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tierall interval
				end = Get end point... tierall interval
				lowsubint = Get low interval at time: tier, start
				highsubint = Get high interval at time: tier, end
				if lowsubint = highsubint - 2
					subint = lowsubint + 1
					subintstart = Get starting point... tier subint
					subintend = Get end point... tier subint
					subintlabel$ = Get label of interval: tier, subint
					# extract s(ound) Hanning windowed, then get pitch, get f(ormants) and get l(ong term average spectrum)
					select Sound 'soundname$'
					s = Extract part: subintstart, subintend, "Hanning", 1, "no"
					selectObject: s
					f = To Formant (burg): 0, 5, maxFormant, 0.025, 50
					selectObject: s
					l = nocheck To Ltas: 100
					# calculate frequencies of H1 (F0) and A3 (F3)
					select Pitch 'soundname$'
					medp = Get quantile: subintstart, subintend, 0.5, "Hertz"
					selectObject: f
					f1 = Get quantile: 1, 0, 0, "Hertz", 0.5
					f2 = Get quantile: 2, 0, 0, "Hertz", 0.5
					f3 = Get quantile: 3, 0.0, 0.0, "Hertz", 0.5
					f4 = Get quantile: 4, 0.0, 0.0, "Hertz", 0.5
					# get intensities at H1 and A3
					nocheck selectObject: l
					h1 = nocheck Get value at frequency: medp, "Nearest"
					a1 = nocheck Get value at frequency: f1, "Nearest"
					a2 = nocheck Get value at frequency: f2, "Nearest"
					a3 = nocheck Get value at frequency: f3, "Nearest"
					a4 = nocheck Get value at frequency: f4, "Nearest"
					h1a1$ = fixed$ (h1-a1,3)
					h1a2$ = fixed$ (h1-a2,3)
					h1a3$ = fixed$ (h1-a3,3)
					h1a4$ = fixed$ (h1-a4,3)
				endif
				# Save result to text file:
				resultline$ = "'newline$''soundname$'	'label$'	'subintlabel$'	'medp'	'f1'	'f2'	'f3'	'f4'	'h1'	'a1'	'a2'	'a3'	'a4'	'h1a1$'	'h1a2$'	'h1a3$'	'h1a4$'"
				fileappend "'resultfile$'" 'resultline$'
				select TextGrid 'soundname$'
				nocheck removeObject (s,f,l)
			endif
		clearinfo
		perc$ = fixed$((100/numberOfIntervals * interval), 2)
		appendInfo: "'perc$'% completed of file 'soundname$'. Latest interval: 'interval'	'label$'"
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
	# Remove the temporary objects from the object list
	select Pitch 'soundname$'
	Remove
	select Sound 'soundname$'
	Remove
	select Strings list
	# and go on with the next sound file!
endfor

Remove


#-------------
# This procedure finds the number of a tier that has a given label.

procedure GetTier name$ variable$
        numberOfTiers = Get number of tiers
        itier = 1
        repeat
                tier$ = Get tier name... itier
                itier = itier + 1
        until tier$ = name$ or itier > numberOfTiers
        if tier$ <> name$
                'variable$' = 0
        else
                'variable$' = itier - 1
        endif

	if 'variable$' = 0
		exit The tier called 'name$' is missing from the file 'soundname$'!
	endif

endproc