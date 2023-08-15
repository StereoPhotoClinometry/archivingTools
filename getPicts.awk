START {
		active=0
	}
/MAP OVERLAPS/ {
	active=0
	}
// {
	if (active)
		print $1
	}
/PICTURES/ {
	active=1
	}
