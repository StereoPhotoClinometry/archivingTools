// Eric E. Palmer
//	25 August 2020
//	Takes a set of cartisian points (x, y, z) and converts 
//		them into lat/lon/radius.  
//	There is a hard-coded option to change to a 4th value
//		which replaces radius
//  Version 1.1 -- Changes output to be x, y (lon, then lat)
//	 Version 1.2
//		Allows 2nd argument to be a value -- that will replace the radius
float	version = 1.2;

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

#define pi 3.1415926

int main (int argc, char *argv[]) {
	char	*inFile;
	char	*extra = 0;
	float	x, y, z, val;

	// Check for at least a filename
	if (argc <= 1) {
		fprintf (stderr, "Usage %s <filename> [parameter]\n", argv [0]);
		exit (-1);
	}//if


	// Check for version, assign filename
	// Argc will be at leats 2
	if (! strcmp (argv[1], "-v" ) ) {
		printf ("Version %3.1f\n", version);
		exit (-1);
	}//strcmp
	
	inFile = argv[1];



	// Flag for having a 4th value
	if (argc == 3) {
		extra = argv [2];
	}//if3


	FILE *in = fopen (inFile, "r");
	if (! in) {
		fprintf (stderr, "Cannot open %s\n", inFile);
		exit (-1);
	}//if



	// Loop until it is done
	while ( feof (in) == 0) {
		fscanf (in, "%f", &x);
		fscanf (in, "%f", &y);
		fscanf (in, "%f", &z);
		if (extra) fscanf (in, "%f", &val);
	
		// match
		float dist = sqrt (x*x + y*y + z*z);
		float lat = asin (z/dist);
		lat *= 180.0 / pi;
		float lon = atan2 (y, x);
		lon *= 180.0 / pi;
		if (lon < 0) lon += 360;
	
		// Print lon, lat
		printf ("%09.5f	%09.5f", lon, lat);

		// Print 3rd column, either value or radius
		if (extra)
			printf ("	%09.5f", val);
		else
			printf ("	%09.6f", dist);
	
		printf ("\n");
	
	}//while


}//main
