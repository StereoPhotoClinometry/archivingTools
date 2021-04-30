// pgm2ascii.c
// Eric E. Palmer
//		29 April 2021
//		Reads in a pgm file.  Based on the headers, it will
//		read in that many rows and columns.  It will output
//		the binary file (in unsigned byte) as an 
//		ascii table (ascii numbers with a tab between data elements)

#include <stdio.h>
#include <math.h>
#include <stdlib.h>


// Reads one line in (until new line) and fills up the string array
int readLine (FILE *in, char line[255]){

	char	ch;
	int	i;

	for (i=0; i<255; i++) {
		ch = fgetc (in);
		if (ch == '\n') break;
		line [i] = ch;

		if (feof (in) ) return 0;

	}//for
	line [i+1] = '\0';
return 1;

}//readline


// Main //
int main (int argc, char *argv []) {

	char *filename = "test.pgm";
	FILE *in;
	
	if (argc == 2)
		filename = argv [1];
	
	in = fopen (filename, "r");
	if (! in) {
		fprintf (stderr, "Cannot open %s\n", filename);
		return 0;
	}//if
	
	long	cnt = 0;
	char	line [256];
	char	ch;
		
	// Dump P5
	readLine (in, line);
	// #.
	readLine (in, line);
	
	// Read in line 3, the rows and columns
	int row, col;
	readLine (in, line);
	sscanf (line,"%d %d", &row, &col);
	printf ("Row %d, Col %d\n", row, col);

	// Read in bit type of the data
	int bits;
	readLine (in, line);
	sscanf (line,"%d", &bits);
	printf ("Bits %d\n", bits);


	// Set up output - used boring default out.txt
	FILE *out = fopen ("out.txt", "w");
	if (! out) {
		fprintf (stderr, "Cannot open %s\n", "out.txt");
		return 0;
	}//if
	


	int	i=0;			// current col
	int	j=0; 		// current row

	// Loop for the data
	while (! feof (in) ) {
		ch = fgetc (in);
		// Read in one byte and print it as an ascii number
		fprintf (out, "%d	", (unsigned short) ch);
		i ++;

		// got to the end of a line
		if (i >= col) {
			fprintf (out, "\n");
			j++;		// increase the row

			// got to the end of the file
			if (j >= row) break;
			i=0;		// reset the column market

		}//if
	

	}//while

	printf ("Done %d %d\n", i, j);



}//main
