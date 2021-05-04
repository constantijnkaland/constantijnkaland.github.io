# COUNT SUBINTERVAL POSITIONS WITHIN INTERVALS
#
# This script is meant to count syllable positions within a larger
# domain (word, phrase, sentence). The script goes through TextGrid
# files in a directory, opens each TextGrid, and goes through all
# non-empty intervals on the tier containing the interval. For each
# non-empty interval, all non-empty subintervals are counted on the
# tier that contains subintervals. The output file shows the name
# of the soundfile, the (sub)interval label, the number of the subinterval
# in the interval, and the total number of subintervals in the interval.
#
# Usage notes:
#	- textgrids should have at least two interval tiers with
#		- one tier annotated for intervals (word, phrase, sentence)
#		- one tier annotated for subintervals within the interval
#	- instances where the interval consists of just one subinterval can be handled by the script
#	- interval boundaries are taken to count subintervals 
# 	  that fall within these boundaries; ideally the interval 
#	  boundaries coincide with the left boundary of the first subinterval
#	  and the right boundary of the last subinterval.
#	- all empty (sub)intervals are ignored in the output counts
#	- running the script on several files might take while; the Praat
#	  info window shows the progress during running.
#
# Upon running the script, the user defines:
#	- path to textgrid(s) and output file
#	- name of the tier consisting of the intervals (I)
#	- name of the tier consisting of the subintervals (Subs)
#
# This script is created by Constantijn Kaland and based on the 
# original "collect pitch data from files" by Mietta Lennes. 09-2017.

form Count subintervals within intervals
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile 
	comment Which interval tier marks intervals?
	sentence Tiername word
	comment Which interval tier marks subintervals?
	sentence Tiersyllname syllable
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

titleline$ = "Filename	Interval	Subinterval	Position Sub in I	Total Subs in I'newline$'"
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
		call GetTier 'tiername$' tier
		call GetTier 'tiersyllname$' tiersyll
		numberOfIntervals = Get number of intervals... tier
		# Pass through all intervals in the selected tier:
		for interval to numberOfIntervals
			select TextGrid 'soundname$'
			label$ = Get label of interval... tier interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tier interval
				end = Get end point... tier interval
				# extract interval part from textgrid
				select TextGrid 'soundname$'
				Extract part: start, end, "no"
				select TextGrid 'soundname$'_part
				# get number of intervals on syllable tier 
				total = Get number of intervals: tiersyll
				# set counts for empty intervals to zero
				foundempty = 0
				emptycount = 0
				# count total number of empty intervals on syllable tier
				for subcheckinterval to total
					labelsyll$ = Get label of interval: tiersyll, subcheckinterval
					if not(labelsyll$ <> "")
						emptycount = emptycount + 1
					endif
				totalnew = total - emptycount	
				endfor
				# get the counts, while ignoring empty intervals in the output file
				for subinterval to total
					output = 1
					labelsyll$ = Get label of interval: tiersyll, subinterval
					if not(labelsyll$ <> "")
						foundempty = foundempty + 1
						output = 0
					endif
					if foundempty = 0
						resultline$ = "'soundname$'	'label$'	'labelsyll$'	'subinterval'	'totalnew''newline$'"
					else
						subintervalnew = subinterval - foundempty
						resultline$ = "'soundname$'	'label$'	'labelsyll$'	'subintervalnew'	'totalnew''newline$'"
					endif
					# add to output file when not empty
					if output = 1
						fileappend "'resultfile$'" 'resultline$'
					endif
				endfor
				# remove textgrid part
				select TextGrid 'soundname$'_part
				Remove
			endif
		clearinfo
		perc$ = fixed$((100/numberOfIntervals * interval), 2)
		appendInfo: "'perc$'% completed of file 'soundname$'. Latest interval: 'interval'	'label$'"
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
	select Strings list
	# and go on with the next file!
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
