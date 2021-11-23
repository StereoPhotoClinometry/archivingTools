// {
	#print doNext, ">", $0
	if (doNext == 1) {
		#print $1, $4, $5, $3 
		name = $1
		x=$4
		y=$5
		res=$3
		#print $0
		doNext = -1
	}
	else if ($1 == file) {
		#print "Found",  $0
		offX = $2
		offY = $3
		totalOff = sqrt (offX*offX + offY*offY)
		if ( res < limit)
			print name, x, y, offX, offY, totalOff, res
		#else
			#print "fart", res, limit
		}
}

/\.\.\.\.\./  {
	doNext++
	#print "#####", $0
}

