/* xermsg.f -- this is a dummy implementation which just calls fprintf
*/

#define V3P_NETLIB_SRC
#include "v3p_netlib.h"
#include "stdio.h"

/* Table of constant values */

/* DECK XERMSG */
/* Subroutine */ int xermsg_(const char *librar, const char *subrou, const char *messg, integer
        *nerr, integer *level, ftnlen librar_len, ftnlen subrou_len, ftnlen
        messg_len)
{

  fprintf(stderr, "%s/%s: %s\n", librar, subrou, messg);
  return 0;
} /* xermsg_ */

