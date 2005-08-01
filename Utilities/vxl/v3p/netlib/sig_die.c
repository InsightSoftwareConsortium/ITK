#include "f2c.h"
#include "netlib.h"
#undef abs
#undef min
#undef max
#include <stdio.h>
#include <stdlib.h> /* for abort() */

void sig_die(register const char *s, int kill)
{
  /* print error message, then clear buffers */
  fprintf(stderr, "%s\n", s);
 
  if (kill) {
    abort();
  }
}
