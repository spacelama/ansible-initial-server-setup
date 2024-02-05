
#include <stdio.h>

int main(int argc , char ** argv) {
	int len;
	char *str;
	int i;
start:
	argc--; argv++;
	if (argc <= 0) {
		printf("\n");
		exit(0);
	}
	str=argv[0];
	len=strlen(str);
	for (i=0; i < len ; i++) {
		printf("%c\b%c", str[i], str[i]);
	}
	if (argc > 0)
		printf(" ");
	goto start;
}
