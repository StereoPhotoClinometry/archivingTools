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
/LANDMARKS/ {
	active=1
	}
