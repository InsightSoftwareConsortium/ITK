#include <stdio.h>
#include <stdlib.h> /* for abort() */

void sig_die(register char *s, int kill)
{
  /* print error message, then clear buffers */
  fprintf(stderr, "%s\n", s);
 
  if (kill) {
    abort();
  }
}
