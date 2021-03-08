# This script goes through sound and TextGrid files in a directory,
# opens each pair of Sound and TextGrid, extracts stylized F0 contours from 
# intervals of specified length, takes a specified number of F0 measures equally 
# distributed over the contour, and saves results to a comma-separated csv-file.
# Octave jumps can be handled, in which case mean F0 per interval is measured 
# before and after handling. The difference between both means (m1/m2) is then
# given in an additional column "jumpkilleffect". Stylization resolution (ST)
# can be specified by user.
#
# The generated output serves time-series analyses of F0, such as GAMMs or cluster analysis.
#
# The core structure of this script is based on "collect_pitch_data_from_files.praat"
# by Mietta Lennes (https://lennes.github.io/spect/).
#
# Constantijn Kaland, March 2021.
# https://constantijnkaland.github.io/contourclustering/

form Get time-series F0 data
	comment Directory of sound files
	text sound_directory /home/x/Desktop/tmp/
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory /home/c/Desktop/tmp/
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile /home/x/Desktop/tmp/output.csv
# Manual: 2.1.1. Tier
	comment Which tier defines the intervals corresponding to the contours? (provide tier name)
	sentence Tier tiername
# Manual: 2.1.2. Duration
	comment What is the desired duration range of the contour length (seconds)?
	positive Minimum_duration 0.0001
	positive Maximum_duration 100
# Manual: 2.1.3. Number of measures
	comment How many measures should be taken per contour?
	positive Number_of_measures_(steps) 20
	comment Pitch analysis parameters
# Manual: 2.1.4. Time-step
	positive Time_step 0.01
# Manual: 2.1.5. Pitch minimum and maximum
	positive Minimum_pitch_(Hz) 75
	positive Maximum_pitch_(Hz) 600
# Manual: 2.1.6. Stylization resolution
	positive Stylization_resolution_(ST) 2
# Manual: 2.1.7. Kill octave jumps
	boolean Kill_octave_jumps 1
endform

# Here, you make a listing of all the sound files in a directory.
# The example gets file names ending with ".wav" from the specified directory

Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings

# Check if the result file exists:
if fileReadable (resultfile$)
	pause The result file 'resultfile$' already exists! Do you want to overwrite it?
	filedelete 'resultfile$'
endif

# Write a row with column titles to the result file:
# (remember to edit this if you add or change the analyses!)

titleline$ = "filename,interval_label,start,end,step,stepnumber,f0,jumpkilleffect'newline$'"
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
				if dur > minimum_duration and dur < maximum_duration
					# extract sound at interval
					select Sound 'soundname$'
					Extract part: start, end, "rectangular", 1, "no"
					# to pitch
					selectObject: "Sound 'soundname$'_part"
					To Pitch: time_step, minimum_pitch, maximum_pitch
					selectObject: "Pitch 'soundname$'_part"
					# kill octave jumps and retrieve difference between F0 before and after jump handling
					mchange = 0
					if kill_octave_jumps = 1
						morg = Get mean: 0, 0, "Hertz"
						Kill octave jumps
						moct = Get mean: 0, 0, "Hertz"						
						mchange = morg/moct													
					endif
					#stylize pitchtier
					Down to PitchTier
					Stylize: stylization_resolution, "Semitones"
					# get specified number of measures equally distributed over interval
					measurestep = dur/(number_of_measures+1)
					step = measurestep
					stepnr = 1
					while stepnr <= number_of_measures
						selectObject: "PitchTier 'soundname$'_part"
						value = Get value at time: step
						resultline$ = "'soundname$','label$','start','end','step','stepnr','value','mchange''newline$'"
						fileappend "'resultfile$'" 'resultline$'
						step = step + measurestep
						stepnr = stepnr + 1
					endwhile
					# remove used objects
					selectObject: "Pitch 'soundname$'_part"
					Remove
					if kill_octave_jumps = 1
						selectObject: "Pitch 'soundname$'_part"
						Remove
					endif
					selectObject: "PitchTier 'soundname$'_part"
					Remove
					selectObject: "Sound 'soundname$'_part"
					Remove
				endif
				select TextGrid 'soundname$'
			endif
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
	# Remove the temporary objects from the object list
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
