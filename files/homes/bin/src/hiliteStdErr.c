/*
 * hilite - runs a command, highlighting everything it sends to stderr
 *
 * Copyright (C) 2000  Mike Schiraldi <mgs21@columbia.edu>
 * Made a bit more general 2005-2007 Tim Connors <twc+NO@SPAM+aaocbn.aao.gov.au>
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program, but Mike's a lazy bastard. To get one,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite
 * 330, Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>

#define EHEADER "\033[31m"
#define EFOOTER "\033[0m"
#define SHEADER "\033[32;1m"
#define SFOOTER "\033[0m"

#define BUF_MAX 8192

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

int
main (int argc, char **argv)
{

     char buf[BUF_MAX];
     int pe[2];
     int ps[2];
     int f,fs,fe;
     fd_set rfds;
     struct timeval tv;
     int no_stdout;
     
     int flags;

     no_stdout=0;

     if (argc > 1 && !strcmp(argv[1],"--no-colour-stdout")){
         no_stdout=1;
         argc--;
         argv++;
     }

     if (argc < 2)
     {
          printf ("Specify a command to execute, you blithering idiot.\n");
          return 1;
     }

     if (pipe (pe))
     {
          perror("pipe-1");
          return 1;
     }
     if (pipe (ps))
     {
          perror("pipe-2");
          return 1;
     }

     setbuf(stdout,NULL);
     setbuf(stderr,NULL);
     
     f = fork ();

     if (f == -1)
     {
          perror("fork");
          return 1;
     }

     if (f)
     {
          close (pe[1]);
          close (ps[1]);

          flags = fcntl(pe[0], F_GETFL, 0);
          if (fcntl(pe[0], F_SETFL, flags | O_NONBLOCK)) {
               perror("fcntl-1");
               return 1;
          }
          flags = fcntl(ps[0], F_GETFL, 0);
          if (fcntl(ps[0], F_SETFL, flags | O_NONBLOCK)) {
               perror("fcntl-2");
               return 1;
          }
          FD_ZERO(&rfds);
          FD_SET(pe[0], &rfds);
          FD_SET(ps[0], &rfds);
          do
          {
               tv.tv_sec=0;
               tv.tv_usec=10000;
               select(max(pe[0],ps[0]), &rfds, NULL, NULL, &tv);
               fe = read (pe[0], buf, BUF_MAX - 1);
               if (fe == -1) {
                    if (errno != EINTR && errno != EWOULDBLOCK) {
                         perror("read-1");
                         return 1;
                    }
                    fe=1;//make sure we really do loop again, unless there really is eof
               } else if (fe > 0) {
                    buf[fe] = 0;
                    fprintf (stderr, "%s%s%s", EHEADER, buf, EFOOTER);
               }
               fs = read (ps[0], buf, BUF_MAX - 1);
               if (fs == -1) {
                    if (errno != EINTR && errno != EWOULDBLOCK) {
                         perror("read-2");
                         return 1;
                    }
                    fs=1;//make sure we really do loop again, unless there really is eof
               } else if (fs > 0) {
                    buf[fs] = 0;
                    if (no_stdout) {
                        fprintf (stdout, "%s", buf);
                    } else {
                        fprintf (stdout, "%s%s%s", SHEADER, buf, SFOOTER);
                    }
               }
          } while (fe > 0 || fs > 0);
          wait (&f);
          return WEXITSTATUS (f) | WIFSIGNALED(f);
     }
     else
     {
          close (pe[0]);
          close (ps[0]);
          close (fileno (stderr));
          close (fileno (stdout));
          pe[1] = dup2 (pe[1], fileno (stderr));

          if (pe[1] == -1)
          {
               perror ("dup2-1");
               return 1;
          }
          ps[1] = dup2 (ps[1], fileno (stdout));
          if (ps[1] == -1)
          {
               perror ("dup2-2");
               return 1;
          }

          if (!fdopen (pe[1], "w")) {
               perror("fdopen-1");
               return 1;
          }
          if (!fdopen (ps[1], "w")) {
               perror("fdopen-2");
               return 1;
          }

          execvp (argv[1], &argv[1]);
          // perror ("execvp");
          fprintf (stderr, "%s %s: %s\n", "execvp:", argv[1], strerror(errno));
          return 1;
     }
}
