# INTERVAL OVERLAP
#
# This script is meant to compare two interval tiers to find out where
# labeled (i.e. non-empty) intervals on both tiers overlap.
# For example, you have labelled all syllables on a given tier. On 
# another tier you have marked where there is background noise in the
# recording. This script will mark all non-empty intervals on your
# syllable tier (tiermark) that fully or partially overlap with
# non-empty intervals on the noise tier (tiercomp). In this way, you
# can easily identify the syllables that are bothered by background
# noise (i.e. to exclude from further analysis), but this script might
# suit other purposes as well. Results are written in a textfile.
#
# Usage notes:
#	- textgrids should have at least two interval tiers
#  	- overlap is detected when the following conditions are met:
#	  1) interval X at tiermark is non-empty
#	  2) within the boundaries of interval X (part of) a non-empty
#	     interval at tiercomp can be found.
#	- interval at tiermark could be bigger, smaller or identical to
#	  the interval at tiercomp.
#	- intervals at tiermark for which overlap is found are marked with "X"
#	- if overlap is found, also the label of the comparison tier is
#	  included in the resultfile
#	- all empty intervals are ignored on either tier
#	- running the script on several files might take while
#	- script halts on double quotes occurring on a tier (consider
#	  removing double quotes before running this script)
#
# Upon running the script, the user defines:
#	- path to textgrid(s) and output file
#	- name of the tier for which labels should be marked if there is 
# 	  overlap (tiermark)
#	- name of the tier to look for intervals that possibly overlap with the 
#       intervals on tiermark (tiercomp)
#
# This script is created by Constantijn Kaland and partially based on the 
# original "collect pitch data from files" by Mietta Lennes. 09-2017.

form Interval overlap
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile 
	comment Mark all non-empty intervals on tier... (enter name)
	sentence Tiermark syllable
	comment ... that overlap with non-empty intervals on tier... (enter name)
	sentence Tiercomp word
endform

# Here, you make a listing of all the textgrid files in a directory.

Create Strings as file list... list 'textGrid_directory$'*'textGrid_file_extension$'
numberOfFiles = Get number of strings

# Check if the result file exists:
if fileReadable (resultfile$)
	pause The result file 'resultfile$' already exists! Do you want to overwrite it?
	filedelete 'resultfile$'
endif

# Write a row with column titles to the result file:
# (remember to edit this if you add or change the analyses!)

titleline$ = "Filename	Label	Start	End	Overlap	Overlaplabel	'newline$'"
fileappend "'resultfile$'" 'titleline$'

# Go through all the textgrid files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'textGrid_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every textgrid file that was opened:
	soundname$ = selected$ ("TextGrid", 1)
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Find the tier number that has the label given in the form:
		call GetTier 'tiermark$' tiermark
		call GetTier 'tiercomp$' tiercomp
		numberOfIntervals = Get number of intervals... tiermark
		# Pass through all intervals in the selected tier:
		for interval to numberOfIntervals
			select TextGrid 'soundname$'
			label$ = Get label of interval... tiermark interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tiermark interval
				end = Get end point... tiermark interval
				# empty labels for comparison tier
				mark$ = ""
				labelcomp$ = ""
				labelcompstart$ = ""
				labelcompend$ = ""
				# compare start and end with tiercomp and add labels if overlap
				compstart = Get high interval at time... tiercomp start
				labelcompstart$ = Get label of interval... tiercomp compstart
				if labelcompstart$ <> ""
					mark$ = "X"
					labelcomp$ = labelcompstart$
				endif
				compend = Get low interval at time... tiercomp end
				labelcompend$ = Get label of interval... tiercomp compend
				if labelcompend$ <> ""
					mark$ = "X"
					labelcomp$ = labelcompend$
				endif
				numberintcomp = Get number of intervals... tiercomp
				if compstart <= numberintcomp-1 and mark$ = ""
					compwithin = compstart + 1
					compwithinstart = Get start time of interval... tiercomp compwithin
					labelcompwithin$ = Get label of interval... tiercomp compwithin
					if compwithinstart < end
						mark$ = "X"
						labelcomp$ = labelcompwithin$
					endif
				endif
				# write results
				resultline$ = "'soundname$'	'label$'	'start'	'end'	'mark$'	'labelcomp$'	'newline$'"
				fileappend "'resultfile$'" 'resultline$'
			endif
		clearinfo
		perc$ = fixed$((100/numberOfIntervals * interval), 2)
		appendInfo: "'perc$'% completed. Latest interval: 'interval'	'label$'"
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
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
