# This script is meant to align boundaries on two different tiers in a
# textgrid. This is especially useful if boundaries on one tier have
# been created manually and therefore might not always align perfectly
# with boundaries on another tier.
#
# The script goes through TextGrid files in a directory, opens each
# TextGrid, and goes through all non-empty intervals on a specified
# tier (tierorg). Each left and right boundary of a non-empty interval
# are compared to a second tier (tiercomp). The script checks whether a
# boundary exists at tiercomp at the exact timestamp where a boundary
# was found for tierorg. If not, a boundary on tiercomp is created at
# the timestamp and in addition, the nearest boundary within a
# specified time-window (margin) will be removed. In this way, you can
# get rid of misaligned boundaries between, for example, a word level
# tier and a syllable level tier. The assumption is that all left and
# right word boundaries coincide exactly with the left boundary of the
# first syllable of that word and with the right boundary of the last
# syllable in that word respectively.
#
# Usage notes:
#	- textgrids should have at least two interval tiers (tierorg
#	  and tiercomp)
#	- labels of intervals at tiercomp preserved (also for interval
#	  with misaligned boundaries)
#	- Praat infowindow shows amount of corrected boundaries for each
#	  file
#	- N.B. original textgrid file will be overwritten! (either run
#	  script on a copy or change filename in the script below)
#
# Upon running the script, the user defines:
#	- path to textgrid(s)
#	- name of the tiers (tierorg and tiercomp)
#	- margin within misaligned boundaries should be removed (0 = no removal)
#
# This script is created by Constantijn Kaland and based on the 
# original "collect pitch data from files" by Mietta Lennes. 09-2017

form Align boundaries
	comment Directory of TextGrid files
	text textGrid_directory 
	sentence TextGrid_file_extension .TextGrid
	comment Which interval tier marks intervals?
	sentence Tierorg word
	comment Which interval tier marks subintervals?
	sentence Tiercomp syllable
	comment What is the margin within subinterval boundaries can be removed?
	positive Margin 0.01

endform

# Here, you make a listing of all the textgrid files in a directory.

Create Strings as file list... list 'textGrid_directory$'*'textGrid_file_extension$'
numberOfFiles = Get number of strings

# Go through all the textgrid files, one by one:
clearinfo
for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A file is opened from the listing:
	Read from file... 'textGrid_directory$''filename$'
	# Starting from here, you can add everything that should be 
	# repeated for every textgrid file that was opened:
	soundname$ = selected$ ("TextGrid", 1)
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Find the tier number that has the label given in the form:
		call GetTier 'tierorg$' tierorg
		call GetTier 'tiercomp$' tiercomp
		numberOfIntervals = Get number of intervals... tierorg
		# Pass through all intervals in the selected tier:
		count = 0
		for interval to numberOfIntervals
			select TextGrid 'soundname$'
			label$ = Get label of interval... tierorg interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tierorg interval
				end = Get end point... tierorg interval
				# align start
				startbnd = Get interval boundary from time... tiercomp start
				endbnd = Get interval boundary from time... tiercomp end
				if startbnd = 0
					count = count + 1
					Insert boundary... tiercomp start
					intlow = Get low interval at time... tiercomp start
					intlowstart = Get starting point... tiercomp intlow
					if (start - intlowstart) < margin
						intlabel$ = Get label of interval... tiercomp intlow
						Set interval text... tiercomp intlow 
						Remove boundary at time... tiercomp intlowstart
						Set interval text... tiercomp intlow 'intlabel$'
					endif
					inthigh = Get high interval at time... tiercomp start
					inthighend = Get end point... tiercomp inthigh
					if (inthighend - start) < margin
						Remove boundary at time... tiercomp inthighend
					endif
				endif
				# align end 
				if endbnd = 0
					count = count + 1
					Insert boundary... tiercomp end
					intlow = Get low interval at time... tiercomp end
					intlowstart = Get starting point... tiercomp intlow
					if (end - intlowstart) < margin
						intlabel$ = Get label of interval... tiercomp intlow
						Set interval text... tiercomp intlow 
						Remove boundary at time... tiercomp intlowstart
						Set interval text... tiercomp intlow 'intlabel$'
					endif
					inthigh = Get high interval at time... tiercomp end
					inthighend = Get end point... tiercomp inthigh
					if (inthighend - end) < margin
						Remove boundary at time... tiercomp inthighend
					endif
				endif
			endif
		endfor
		# show summary, save textgrid and remove from object list
		appendInfo: "'count' boundaries corrected for 'soundname$'.'newline$'"
		select TextGrid 'soundname$'
		# CHANGE FILENAME HERE IF DESIRED
		Save as text file... 'textGrid_directory$''soundname$'.TextGrid
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
