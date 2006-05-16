/* xermsg.f -- this is a dummy implementation which just calls fprintf
*/

#include "f2c.h"
#include "stdio.h"

/* Table of constant values */

/* DECK XERMSG */
/* Subroutine */ int xermsg_(char *librar, char *subrou, char *messg, integer 
        *nerr, integer *level, ftnlen librar_len, ftnlen subrou_len, ftnlen 
        messg_len)
{

  fprintf(stderr, "%s/%s: %s\n", librar, subrou, messg);
  return 0;
} /* xermsg_ */

