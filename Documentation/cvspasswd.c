#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
 
main(argc, argv)
int argc;
char *argv[];
{
    time_t now;
    char salt[64];
 
    if (argc != 3) {
        fprintf(stderr, "Usage: mkpasswd [name] [passwd]\n");         exit(1);
    }
 
    if (time(&now)==-1) {
        fprintf(stderr, "System Error: time not available\n");         exit(1);
    }
 
    /* add a little salt to our crypt */
    sprintf(salt, "%s", ctime(&now));
    salt[0] = salt[17]; salt[1] = salt[18]; salt[2] = '\0';
 
    printf("%s:%s\n", argv[1], crypt(argv[2], salt));
    exit(errno);
}
