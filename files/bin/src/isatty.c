#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>


int main (int argc, char **argv) {
	int res;
	int fd;

	argc--; argv++;
	if (argc != 1) {
		fprintf(stderr, "Usage: isatty <file>\n");
		exit(2);
	}

	fd=open(argv[0], O_NOCTTY|O_RDONLY);
	if (fd == -1) {
		perror("open");
		exit(3);
	}

	res=isatty(fd);
	
	exit(!res);
	return 0;
}
