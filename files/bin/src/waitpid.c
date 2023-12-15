#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define SZ 256

int main(int argc, char **argv) {
   char buffer[SZ];
   if (argc != 2) {
      fprintf(stderr, "Usage: %s <pid>\n", argv[0]);
      exit(1);
   } 
   printf("Waiting for %s\n", argv[1]);
   strncpy(buffer, "/proc/", SZ);
   strncat(buffer, argv[1], SZ);
   buffer[SZ-1] = '\0';
//   printf("Looking at %s\n", buffer);
   while (1) {
      chdir("/");
      if (chdir(buffer) == -1) {
         exit(1);
      }
      sleep(5);
   }
}
