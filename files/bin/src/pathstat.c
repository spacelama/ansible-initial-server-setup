/* pathstat.c

Copyright (C) 2003 Anthony de Boer

This program is free software; you can redistribute it and/or modify
it under the terms of version 2 of the GNU General Public License as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

I'm not including the usual "either version 2 of the License, or (at your
option) any later version" text since I want to reserve the chance to
read version 3 before using it, just to be sure it doesn't promise free
phone support or sexual favours from the program's author.

---

Usage:

In ~/.profile, add everything plus the kitchen sink to $PATH, then
`eval pathstat`.  It removes duplicates, nondirectories, dot, and such rot.

*/

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

struct spath {
     dev_t	p_dev;
     ino_t	p_ino;
     struct spath *p_nxt;
};

void say(char *blurb) {
     write (1, blurb, strlen(blurb));
}

void kvetch(char *blurb) {
     write (2, blurb, strlen(blurb));
}

void barf(char *excuse) {
     kvetch(excuse);
     exit(1);
}

int main(int argc, char **argv) {
     char *path, *npath;
     struct stat sbuf;
     struct spath *sptop, *spc;
     char *var;
     char saypre[1024];
     int verbose=0;

     var="PATH";

     while (argc > 1) {
          if (!strcmp(argv[1], "-v")) {
               verbose = 1;
          } else {
               var=argv[1];
               break;
          }
          argc--; argv++;
     }
     strncpy(saypre, var, 1024);
     strncat(saypre, "=", 1024);
     sptop = NULL;

     path = getenv(var);
     if (!path || !*path) {
          strcpy(saypre, "No ");
          strncat(saypre, var, 1024);
          strncat(saypre, "?\n", 1024);
          barf(saypre);
     }
     path=strdup(path);
     while(path) {
          npath = strchr(path, ':');
          if (npath) {
               *npath++ = '\0';
          }

          if(!*path || ( *path != '/' && *path != '.')) {
               if (verbose) {
                    kvetch("Tossing \"");
                    kvetch(path);
                    kvetch("\" from ");
                    kvetch(var);
                    kvetch("\n");
               }
               goto LOOP;
          }
          if(stat(path, &sbuf)) {
               if (verbose) {
                    perror(path);
               }
               goto LOOP;
          }
          if (!S_ISDIR(sbuf.st_mode)) {
               if (verbose) {
                    kvetch(path);
                    kvetch(" is not a directory\n");
               }
               goto LOOP;
          }

          spc = sptop;
          while(spc) {
               if (spc->p_dev == sbuf.st_dev && spc->p_ino == sbuf.st_ino) {
                    if (verbose) {
                         kvetch(path);
                         kvetch(" already seen\n");
                    }
                    goto LOOP;
               }
               spc = spc->p_nxt;
          }

          spc = (struct spath *)malloc(sizeof(struct spath));
          if (spc == NULL) {
               barf("malloc failed\n");
          }
          spc->p_dev = sbuf.st_dev;
          spc->p_ino = sbuf.st_ino;
          spc->p_nxt = sptop;
          sptop = spc;

          say(saypre);
          say(path);

          strcpy(saypre, ":");

     LOOP:
          path = npath;
     }
	
     say("\n");
     exit(0);
     return 0;
}

