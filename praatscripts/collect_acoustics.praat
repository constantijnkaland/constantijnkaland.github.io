# This script goes through sound and TextGrid files in a directory,
# opens each pair of Sound and TextGrid, calculates the following measures
# of each labeled interval, and saves results to a text file:
# mean f0 (pitchmean)
# minimum f0 (pitchmin)
# maximum f0 (pitchmax)
# timestamp of maximum f0 (pitchmaxT)
# timestamp interval start (start)
# timestamp interval end (end)
# duration of interval (dur)
# mean amplitude (dbmean)
# minimum amplitude (dbmin)
# maximum amplitude (dbmax)
# timestamp of minimum amplitude (dbminT)
# timestamp of maximum amplitude (dbmaxT)
#
# This script is created by Constantijn Kaland and based on "collect pitch
# data from files" by Mietta Lennes.
# 05-2021.

form Analyze pitch maxima from labeled segments in files
	comment Directory of sound files
	text sound_directory 
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile output.txt
	comment Which tier do you want to analyze?
	sentence Tier syllable
	comment Pitch analysis parameters
	positive Time_step 0.01
	positive Minimum_pitch_(Hz) 75
	positive Maximum_pitch_(Hz) 600
endform

# Here, you make a listing of all the sound files in a directory.
Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings

# Check if the result file exists:
if fileReadable (resultfile$)
	pause The result file 'resultfile$' already exists! Do you want to overwrite it?
	filedelete 'resultfile$'
endif

# Write a row with column titles to the result file:
# (remember to edit this if you add or change the analyses!)

titleline$ = "Filename	Segment label	Mean pitch (ST)	Min pitch (ST)	Max pitch (ST)	Max pitch (s)	Start (s)	End (s)	Duration (s)	Intensity (dB)	Int_min	Int_max	Int_min_T	Int_max_T	'newline$'"
fileappend "'resultfile$'" 'titleline$'

# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'sound_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every sound file that was opened:
	soundname$ = selected$ ("Sound", 1)
	To Pitch... time_step minimum_pitch maximum_pitch
	select Sound 'soundname$'
	To Intensity: minimum_pitch, 0, "yes"
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Find the tier number that has the label given in the form:
		call GetTier 'tier$' tier
		numberOfIntervals = Get number of intervals... tier
		# Pass through all intervals in the selected tier:
		for interval to numberOfIntervals
			label$ = Get label of interval... tier interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tier interval
				end = Get end point... tier interval
				dur = end - start
				# get the Pitch maximum at that interval
				select Pitch 'soundname$'
				pitchmean = Get mean... start end semitones re 1 Hz
				pitchmin = Get minimum: start, end, "semitones re 1 Hz", "Parabolic"
				pitchmax = Get maximum: start, end, "semitones re 1 Hz", "Parabolic"
				pitchmaxT = Get time of maximum... start end "semitones re 1 Hz" Parabolic
				select Intensity 'soundname$'
				dbmean = Get mean... start end dB
				dbmin = Get minimum... start end Parabolic
				dbminT = Get time of minimum... start end Parabolic
				dbmax = Get maximum... start end Parabolic
				dbmaxT = Get time of maximum... start end Parabolic
				# Save result to text file:
				resultline$ = "'soundname$'	'label$'	'pitchmean'	'pitchmin'	'pitchmax'	'pitchmaxT'	'start'	'end'	'dur'	'dbmean'	'dbmin'	'dbmax'	'dbminT'	'dbmaxT''newline$'"
				fileappend "'resultfile$'" 'resultline$'
				select TextGrid 'soundname$'
			endif
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
	# Remove the temporary objects from the object list
	select Sound 'soundname$'
	plus Pitch 'soundname$'
	plus Intensity 'soundname$'
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
