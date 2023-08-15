START {
		active=0
	}

/LIMB FITS/ {
	active=0
	}

// {
	if (active)
		print $1
	}

/MAP OVERLAPS/ {
	active=1
	}
