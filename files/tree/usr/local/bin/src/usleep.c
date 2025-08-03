#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int isNumber(const char number[])
{
    int i = 0;

    //checking for negative numbers
    //if (number[0] == '-')
    //    i = 1;
    for (; number[i] != 0; i++)
    {
        //if (number[i] > '9' || number[i] < '0')
        if (!isdigit(number[i]))
            return 0;
    }
    return 1;
}

void usage(void) {
    fprintf(stderr, "Usage: usleep <usec>\n");
    exit(1);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        usage();
    }

    if (!isNumber(argv[1])) {
        usage();
    }
    usleep(atol(argv[1]));
    return 0;
}
