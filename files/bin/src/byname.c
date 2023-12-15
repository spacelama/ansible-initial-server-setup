/*From: Keith Lewis <keithl@mukluk.its.monash.edu.au>
  To: luv-talk@luv.asn.au
  Subject: Re: Interesting new tactics in spam

  Changed by Tim Connors, Jan 2005, to do reverse lookups, and changed the outputting format

  on solaris: cc -lnsl byname.c -o SunOS/byname
*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>

extern int h_errno;

main (argc, argv)
int argc;
char *argv[];

{
     char* name;
     int i, j, k;
     struct hostent host_entry, *hp, *hp_byname, *gethostbyname (), *gethostbyaddr ();
     for (;;) {
          if (argc <= 1) {
               /*printf ("\n");
               printf ("host to look up ? ");
               if (scanf ("%s", name) == EOF)*/
               printf("Usage: byname <host|ip>\n");
               exit (1);
          } else {
               name=argv[1];
               if (argc > 2)
                    printf ("warning - extra arguments ignored\n");
          }

          if ((hp_byname = gethostbyname (name)) == 0) {
               printf ("error at resolving hostname or IP address: ");
               if (h_errno == 0)
                    printf ("unspecified error");
               if (h_errno == HOST_NOT_FOUND)
                    printf ("host not found");
               if (h_errno == TRY_AGAIN)
                    printf ("try again");
               if (h_errno == NO_RECOVERY)
                    printf ("no rcovery");
               if (h_errno == NO_ADDRESS)
                    printf ("no address");
               printf ("\n");
               return 1;
          } else {
               printf ("canonical name or ip: %s\n", hp_byname->h_name);

               for (i = 0; hp_byname->h_aliases[i] != (char *) 0; i++)
                    printf ("alias: %s\n", hp_byname->h_aliases[i]);
               printf ("address type: %d\n", hp_byname->h_addrtype);
               printf ("address length: %d\n", hp_byname->h_length);
               if (hp_byname->h_addr_list[0] != (char *) 0)
                    printf ("ip addresses: ");
               for (i = 0; hp_byname->h_addr_list[i] != (char *) 0; i++) {
                    printf ("%u", hp_byname->h_addr_list[i][0] & 0377);
                    for (j = 1; j < hp_byname->h_length; j++)
                         printf (".%u", hp_byname->h_addr_list[i][j] & 0377);
                    if (hp_byname->h_addr_list[i + 1] != (char *) 0)
                         printf (" ");
               }
               printf("\n");

               if ((hp = gethostbyaddr (hp_byname->h_addr_list[0],hp_byname->h_length, hp_byname->h_addrtype)) == 0) {
                    printf ("error at reverse name lookup of IP address: ");
                    if (h_errno == 0)
                         printf ("unspecified error");
                    if (h_errno == HOST_NOT_FOUND)
                         printf ("host not found");
                    if (h_errno == TRY_AGAIN)
                         printf ("try again");
                    if (h_errno == NO_RECOVERY)
                         printf ("no rcovery");
                    if (h_errno == NO_ADDRESS)
                         printf ("no address");
                    printf ("\n");
               } else {
                    printf ("canonical name: %s\n", hp->h_name);
               }
          }
          if (argc > 2)
               exit (1);
          exit(0);
     }
}
