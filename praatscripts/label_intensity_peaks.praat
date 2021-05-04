# AUTOMATIC SEGMENTATION AND LABELLING OF INTENSITY PEAKS
#
# This script is meant for automatic creation and labelling of 
# intervals around intensity peaks. This can be useful for analyses
# of vowels in syllable nuclei. The resulting intervals around
# peaks tend to show the part of the syllable where the formant are
# the most stable. This can be useful to measure spectral
# characteristics of vowels: formants, spectrals tilt, etc.
#
# This script goes through sound and TextGrid files in a directory,
# opens each pair of Sound and TextGrid and goes through all
# intervals of the specified TextGrid tier. For each labelled
# interval on the original tier an intensity peak will be detected.
# From the time of the intensity peak the script searches to the
# left and to the right until it reaches a given intensity loss
# treshold (i.e. 5% intensity loss compared to the peak intensity;
# this threshold can be specified by the user) or until it
# reaches the boundary of the interval on the tier (i.e. when the
# intensity doesn't drop until the threshold within the interval).
# The newly detected boundaries around the peak are then written
# into a new tier in the textgrid.
#
# Usage notes:
# 	- for accurate results, syllable-level intervals are
#	  recommended on the original tier due to the small amount of
#	  intensity peaks
#	- interval labels on the new tier are copied from the
#	  original tier
#	- use a small time-step for the most accurate results
#	  (especially for steep intensity curves), however too small
#	  time-step values slow down the running of the script
#
# The Praat infowindow prints the percentage completed (per file) and
# the latest interval number and label.
#
# This script is created by Constantijn Kaland and adapted from the 
# original script "collect pitch data from files" by Mietta Lennes.
# 09-2017.

form Automatically extract intensity peak subintervals from labeled segments in files
	comment Directory of sound files
	text sound_directory 
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Which tier do you want to analyze?
	sentence Tier syllable
	comment Which tier do you want to add (intensity subintervals)?
	sentence NewTier intpeaks
	comment Overwrite existing textgrids?
	comment (existing tiers will never be overwritten) 
	comment (if unchecked name of new tier will be added to filename)
	boolean overwrite 0
	comment Intensity peak analysis parameters:
	positive Boundary_treshold_(%) 5
	positive Time_step 0.00001
endform

# Here, you make a listing of all the sound files in a directory.

Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings
clearinfo
# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'sound_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every sound file that was opened:
	soundname$ = selected$ ("Sound", 1)
	To Intensity... 100 0 "yes"
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
		# step_time for precise time measures, calculated on the basis of the pitch analysis time_step
		# step_time = time_step / 10
		# go through all intervals on the tier
		for interval to numberOfIntervals
			select TextGrid 'soundname$'
			label$ = Get label of interval... tier interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tier interval
				end = Get end point... tier interval
				# get intensity and time measures from peak
				select Intensity 'soundname$'
				peak = Get time of maximum... start end Parabolic
				peak_int = Get value at time... peak Cubic
				# detect boundary left of peak depending on treshold, starting from peak searching leftwards. If treshold cannot be reached within interval take boundary of original tier
				timevar = peak
				repeat
					left_int = Get value at time... timevar Cubic
					timevar = timevar - time_step
				until left_int <= peak_int*(1-(boundary_treshold/100)) or timevar <= start
				if timevar <= start
					peak_left = start
				else
					peak_left = timevar
				endif	
				# same for right boundary detection
				timevar = peak
				repeat
					right_int = Get value at time... timevar Cubic
					timevar = timevar + time_step
				until right_int <= peak_int*(1-(boundary_treshold/100)) or timevar >= end
				if timevar >= end
					peak_right = end
				else
					peak_right = timevar
				endif
				# write boundaries to textgrid
				select TextGrid 'soundname$'
				exist = Get interval boundary from time... newtiernumber peak_left
				if exist = 0
					Insert boundary... newtiernumber peak_left
				endif
				exist = Get interval boundary from time... newtiernumber peak_right
				if exist = 0
					Insert boundary... newtiernumber peak_right
				endif
				newintervalnumber = Get interval at time... newtiernumber peak
				Set interval text... newtiernumber newintervalnumber 'label$'
			endif
		clearinfo
		perc$ = fixed$((100/numberOfIntervals * interval), 2)
		appendInfo: "'perc$'% completed of file 'soundname$'. Latest interval: 'interval'	'label$'"
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