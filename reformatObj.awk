# Eric E Palmer - 15 Nov 2022
# Reformats OBJ to be fixed width table

/v/ { 
		count += 1; 
		printf ("%3s %13.9f %13.9f %13.9f\n", $1, $2, $3, $4) 
	}

/f/ {
		fCount += 1; 
		printf ("%3s %13d %13d %13d\n", $1, $2, $3, $4) 
	}
END {
	#print "V", count
	#print "F", fCount;
}
