// vector2Grid - Eric E. Palmer - 16 July 2022
//		Converts an ICQ formatted data (x,y,z,value) and put it into
//			a 1x1 degree grid
// Suggest plotting the data:  plot 'grid.txt' u 1:2:3 matrix palette pt 5
// Version 1.0 - First
// Version 1.1 - 25 Aug 2022 - added interpolation

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

////////////////////////////////////////////////////////////
void adjustGrid(float grid[180][360], float difference) {
	int	i, j;
	float	sum=0;

	for (i=0; i<180; i++)
		for (j=0; j<360; j++) {
			if (grid[i][j] == 0) continue;
			grid [i][j] -= difference;
			}
}//meanVal

////////////////////////////////////////////////////////////
float meanVal(float grid[180][360]) {
	int	i, j;
	float	sum=0;

	int count = 0;
	//for (i=0; i<180; i++)
	for (i=20; i<160; i++)
		for (j=0; j<360; j++) {
			if (grid[i][j] == 0) continue;
			count++;
			sum += grid [i][j];
		}//for loop-in

	return (sum/(float)count);
}//meanVal

////////////////////////////////////////////////////////////
void loadGrid(char *filename, float grid[180][360]) {
	int i, j;
	int iLat, iLon;

	for (i=0; i<180; i++)
		for (j=0; j<360; j++)
			grid [i][j] = 0;

	FILE *in = fopen (filename, "r");
	if (! in) {
		printf ("Couldn't open %s\n", filename);
		fprintf (stderr, "Couldn't open %s\n", filename);
		exit (-1);
	}//if
   int num;
   fscanf (in, "%d\n", &num);
   printf ("# num: %d\n", num);

   long max = 6 * (num+1) * (num+1) ;
   fprintf (stderr, "max %ld\n", max);
   float maxVal, maxLat, maxLon;
   maxVal = 0;
  
   for (i=0; i<max; i++) {
      float x, y, z, sig;
      float lat, lon, r;

      fscanf (in, "%f %f %f %f\n", &x, &y, &z, &sig);
      x *= 1000;     // convert to meters
      y *= 1000;     // convert to meters
      z *= 1000;     // convert to meters
      r = sqrt (x*x + y*y + z*z);
      lat =acos (z/r);
      lon = atan2 (y,x);
      lat *= 180/3.1415;
      //lat = 90 - lat;
      lon *= 180/3.1415;
      if (lon < 0) lon += 360;
//      lon = 360 - lon;     // Switching to W Lon

      if (sig > maxVal) {
         maxVal = sig;
         maxLat = lat;
         maxLon = lon;
      }// 
      //printf ("%3.5f %3.5f %3.5f\n", lat, lon, r);
		//lat += 90;		// go from 0-180 rather than -90 to 90
		iLat = (int) lat;
		iLon = (int) lon;
		grid [iLat][iLon] = sig;
	}//for

	fprintf (stdout, "# Max %8.3f (%3.1f Lat %3.1f Lon)\n", maxVal, maxLat, maxLon);

}//

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
int main (int argc, char *argv[])
{
	char *vers = "Version 1.10a";
	char *str1;
	if (argc == 2) {
		str1 = argv [1];
	}
	else {
		printf ("Usage: %s <file1> \n", argv[0]);
		exit (0);
	};

	float grid1[180][360];
	printf ("%s\n", vers);


	// Read the files and load the vectors into a gridded product
	loadGrid (str1, grid1);


	// Check to see how many places is missing data
	int	num=0;
	int 	i, j;
	for (i=0; i<180; i++)
		for (j=0; j<360; j++) 
			if (grid1 [i][j] == 0) num++;
	printf ("# There are %d grid spots without a value\n", num);


	int	di, dj;
	while (num > 0) {
		i = (int) rand()%180;
		j = (int) rand()%360;

		//skip if there is something there
		if (grid1 [i][j] ) continue;	

		// Pick a random direction to interpolate
		di = i + rand()%3-1;		// values will be -1, 0, 1
		dj = j + rand()%3-1;

		// skip if the same spot was selected
		if ((di == i) && (dj == j)) continue;

		// skip if random walk went out of bounds
		if (di < 0) continue;
		if (dj < 0) continue;
		if (di >= 180) continue;
		if (di >= 360) continue;
		

		// skip if neighbor is empty
		if (grid1 [di][dj] == 0)  continue;

		// If random neighbor has a value, assign it
		grid1 [i][j] = grid1 [di][dj];
		num--;
		//printf ("%d %d %d %3.3f\n", num, di, dj, grid1[di][dj]);
	

	}//while





	// Calulcate difference and print
	float max = 0;
	int mLat, mLon;
	FILE *out;
	out = fopen ("grid.txt", "w");
	float sum=0;
	for (i=0; i<180; i++){
		for (j=0; j<360; j++) {

			fprintf (out, "%3.5e	", grid1 [i][j]);
		}//forj
		fprintf (out, "\n");
	}//fori

	fclose (out);



}//main
