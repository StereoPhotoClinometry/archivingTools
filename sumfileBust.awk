# Eric E. Palmer - 23 NOv 21
# Dumps out the list of landmarks that are assigned to the SUMFILES
BEGIN {
	count = 0
	found = 0	# This tracks when we are in the landmark phase
	min = 9999999		# Tracks the lowest pixel location for the maplets
}

# Turn off printing when LIMB is found
/LIMB/ {
	found = 0
	exit
}

# Scan every line.  If we ran across the keyword LARNMARKS
#		until LIMBS is found, then quit

// {
	if (found) {
		print $1
      if ( $3 < min)
			min = $3
	}# if
}

# Once LANDMARKS is found, print everything after than
/LANDMARKS/ {
	found = 1
}

END {
	print min
}
