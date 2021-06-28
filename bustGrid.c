#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

float version = 1.0;
////////////
// main
////////////
int main (int argc, char *argv[]){
	char *filename;

	if (argc != 2) {
		fprintf (stderr, "Usage: %s <filename>\n", argv[0]);
		return -1;
	}//if argc2

	filename = argv [1];

	if (! strcmp (filename, "-v") ) {
		printf ("Version %3.1f\n", version);
		return 0;
	}//if

	FILE *in=fopen (filename, "r");
	if (! in) {
		fprintf (stderr, "Can't open %s\n", filename);
		return -1;
	}//ifin

	// Loop until end of file.  Test each char
	char ch;
	int	white = 1;			// flag for if we're processing white space
	while (! feof (in) ) {
		ch = fgetc (in);

		if (feof (in)) {
			break;
		}//
		
		if (isspace (ch) ) {
			// found white and already working on whitespace
			// skip
			if (white) 	
				continue;
			
			// found white, but it is the 1st time
			// print newline
			else {
				white = 1;
				printf ("\n");
			}//elsewhite

		} else {
			white = 0;				// easy to just set it
			printf ("%c", ch);
		}//else isspace
	}//while

	// printf ("\n");
}//main
