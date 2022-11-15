# Eric E Palmer - 15 Nov 2022
# Reformats OBJ to be fixed width table

/v/ { 
		count += 1; 
		printf ("%4s %18.9f %18.9f %18.9f\n", $1, $2, $3, $4) 
	}

/f/ {
		fCount += 1; 
		printf ("%4s %18d %18d %18d\n", $1, $2, $3, $4) 
	}
END {
	#print "V", count
	#print "F", fCount;
}
