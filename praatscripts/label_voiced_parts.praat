# AUTOMATIC SEGMENTATION AND LABELLING OF VOICED PARTS OF SPEECH
#
# This script is meant for automatic creation and labelling of 
# intervals where pitch is measured. This can be useful for analyses
# that require voiced segments. 
#
# This script goes through sound and TextGrid files in a directory,
# opens each pair of Sound and TextGrid and goes through all intervals
# of the specified TextGrid tier. For each labelled interval on the
# original tier a subinterval will be taken for which pitch is tracked
# (voiced part). Pitch tracking is done on a pitch object, created
# from the sound, in Herz using linear interpolation. The time-step
# and the pitch range can be specified by the user. If there are more
# than one voiced subintervals (maximum is three), the longest is taken.
# For accurate results, syllable-level intervals are recommended on the
# original tier. Subintervals are created and labelled on an additional
# tier named by the user. The interval labels on the new tier are copied
# from the original tier.
#
# The Praat infowindow prints essential information about the ongoing 
# pitch tracking (pitch level, boundary times, labels) and can be used 
# for debugging. Upon successful run of the script, the Praat Info window
# can be ignored.
#
# This script is created by Constantijn Kaland and adapted from the 
# original script "collect pitch data from files" by Mietta Lennes.
# 09-2017.

form Automatically extract voiced subintervals from labeled segments in files
	comment Directory of sound files
	text sound_directory 
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Which tier do you want to analyze?
	sentence Tier syllable
	comment Which tier do you want to add (subintervals)?
	sentence NewTier svoiced
	comment Overwrite existing textgrids? 
	comment (if unchecked name of new tier will be added to filename)
	boolean overwrite 0
	comment Pitch analysis parameters
	positive Time_step 0.01
	positive Minimum_pitch_(Hz) 75
	positive Maximum_pitch_(Hz) 500
endform

# Here, you make a listing of all the sound files in a directory.

Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings

# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'sound_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every sound file that was opened:
	soundname$ = selected$ ("Sound", 1)
	To Pitch... time_step minimum_pitch maximum_pitch
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Get number of tiers and add new tier
		numberOfTiers = Get number of tiers
		newtiernumber = numberOfTiers + 1
		Insert interval tier... newtiernumber 'newTier$'
		# Find the tier number that has the label given in the form:
		select TextGrid 'soundname$'
		call GetTier 'tier$' tier
		numberOfIntervals = Get number of intervals... tier
		# newintervalnumber for first check (avoid double boundary check)
		newintervalnumber = 1
		# step_time for precise time measures, calculated on the basis of the pitch analysis time_step
		step_time = time_step / 10
		# go through all intervals on the tier
		for interval to numberOfIntervals
			select TextGrid 'soundname$'
			label$ = Get label of interval... tier interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tier interval
				end = Get end point... tier interval
				# check whether pitch is present
				select Pitch 'soundname$'
				pitchavg = Get mean... start end Hertz
				if not(pitchavg = undefined)
clearinfo
appendInfo: "Interval 'interval' of 'numberOfIntervals''newline$'"
					timevar = start
					#find start of first voiced interval
					repeat
						p_start = Get value at time... timevar Hertz Linear
						timevar = timevar + step_time
appendInfo: "'newline$''p_start'"						
					until not(p_start = undefined) or timevar >= end
					if timevar >= end
							v_start = start
						else
							timevar = timevar - step_time
							v_start = timevar
							p_start = Get value at time... v_start Hertz Linear
						endif
appendInfo: "'newline$''p_start'"
appendInfo: "'newline$''v_start'"
					#find end of first voiced interval				
					repeat
						p_end = Get value at time... timevar Hertz Linear
						timevar = timevar + step_time
appendInfo: "'newline$''p_end'"
					until p_end = undefined or timevar >= end
					if timevar >= end
						v_end = end
appendInfo: "'newline$''p_end' timelimit"
appendInfo: "'newline$''v_end'"
					else
						timevar = timevar - step_time
						v_end = timevar
						p_end = Get value at time... v_end Hertz Linear
appendInfo: "'newline$''p_end' pitchlimit"
appendInfo: "'newline$''v_end'"
					endif
					# look for possible second interval
					if timevar < end
						repeat
							p_start2 = Get value at time... timevar Hertz Linear
							timevar = timevar + step_time
appendInfo: "'newline$''p_start2'"
						until not(p_start2 = undefined) or timevar >= end
						if timevar >= end
							v_start2 = end
						else
							timevar = timevar - step_time
							v_start2 = timevar
							p_start2 = Get value at time... v_start2 Hertz Linear
						endif
appendInfo: "'newline$''p_start2'"
appendInfo: "'newline$''v_start2'"
						repeat
							p_end2 = Get value at time... timevar Hertz Linear
							timevar = timevar + step_time
appendInfo: "'newline$''p_end2'"
						until p_end2 = undefined or timevar >= end
						if timevar >= end
							v_end2 = end
appendInfo: "'newline$''p_end2' timelimit"
appendInfo: "'newline$''v_end2'"
						else
							timevar = timevar - step_time
							v_end2 = timevar
							p_end2 = Get value at time... v_end2 Hertz Linear
appendInfo: "'newline$''p_end2' pitchlimit"
appendInfo: "'newline$''v_end2'"
						endif
						# compare previously found interval, take the longest
						if (v_end - v_start) < (v_end2 - v_start2)
							v_start = v_start2
							v_end = v_end2
						endif
					endif
					# look for possible third interval
					if timevar < end
						repeat
							p_start3 = Get value at time... timevar Hertz Linear
							timevar = timevar + step_time
appendInfo: "'newline$''p_start3'"
						until not(p_start3 = undefined) or timevar >= end
						if timevar >= end
							v_start3 = end
						else
							timevar = timevar - step_time
							v_start3 = timevar
							p_start3 = Get value at time... v_start3 Hertz Linear
						endif
appendInfo: "'newline$''p_start3'"
appendInfo: "'newline$''v_start3'"
						repeat
							p_end3 = Get value at time... timevar Hertz Linear
							timevar = timevar + step_time
appendInfo: "'newline$''p_end3'"
						until p_end3 = undefined or timevar >= end
						if timevar >= end
							v_end3 = end
appendInfo: "'newline$''p_end3' timelimit"
appendInfo: "'newline$''v_end3'"
						else
							timevar = timevar - step_time
							v_end3 = timevar
							p_end3 = Get value at time... v_end3 Hertz Linear
appendInfo: "'newline$''p_end3' pitchlimit"
appendInfo: "'newline$''v_end3'"
						endif
						# compare previously found interval, take the longest
						if (v_end - v_start) < (v_end3 - v_start3)
							v_start = v_start3
							v_end = v_end3
						endif
					endif
appendInfo: "'newline$''newline$''v_start'"
appendInfo: "'newline$''newline$''v_end'"
					# select Textgrid and add boundaries at new tier
					select TextGrid 'soundname$'
					if newintervalnumber > 1
						doubleboundary = Get end point... newtiernumber newintervalnumber
						if not(v_start = doubleboundary)
							Insert boundary... newtiernumber v_start
						endif
					else
						Insert boundary... newtiernumber v_start
					endif
					Insert boundary... newtiernumber v_end
					intervalnumber = Get interval at time... tier v_start
					originallabel$ = Get label of interval... tier intervalnumber
					newintervalnumber = Get interval at time... newtiernumber v_start
					Set interval text... newtiernumber newintervalnumber 'originallabel$'
				# endif because of pitchavg check
				endif
			# endif because of empty label check
			endif
appendInfo: "'newline$''label$'"
		endfor
		# save textgrid
		if overwrite = 0
		gridfile$ = "'textGrid_directory$''soundname$'_'newTier$''textGrid_file_extension$'" 		
		endif
		select TextGrid 'soundname$'
		Save as text file... 'gridfile$'
		# Remove the TextGrid object from the object list
		Remove
	endif
	# Remove the temporary objects from the object list
	select Sound 'soundname$'
	plus Pitch 'soundname$'
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