#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>


int main(int argc, char **argv) {
	if (argc != 2) { 
		fprintf(stderr, "Usage: usleep <usec>\n");
		exit(1);
	}
	
	usleep(atol(argv[1]));
	return 0;
}
