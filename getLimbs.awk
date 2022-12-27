START {
		active=0
	}

/END/ {
	active=0
	}

// {
	if (active)
		print $1
	}

/LIMB FITS/ {
	active=1
	}
