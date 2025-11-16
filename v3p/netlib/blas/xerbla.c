/* blas/xerbla.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

#include <stdio.h>

/*<       SUBROUTINE XERBLA( SRNAME, INFO ) >*/
/* Subroutine */ int xerbla_(char *srname, integer *info, ftnlen srname_len)
{
/*  -- LAPACK auxiliary routine (preliminary version) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER*6        SRNAME >*/
/*<       INTEGER            INFO >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  XERBLA  is an error handler for the LAPACK routines. */
/*  It is called by an LAPACK routine if an input parameter has an */
/*  invalid value.  A message is printed and execution stops. */

/*  Installers may consider modifying the STOP statement in order to */
/*  call system-specific exception-handling facilities. */

/*  Arguments */
/*  ========= */

/*  SRNAME  (input) CHARACTER*6 */
/*          The name of the routine which called XERBLA. */

/*  INFO    (input) INTEGER */
/*          The position of the invalid parameter in the parameter list */
/*          of the calling routine. */


/*<       WRITE( *, FMT = 9999 )SRNAME, INFO >*/
/*<       STOP >*/
  (void)srname_len;
  printf("** On entry to %6s, parameter number %2ld had an illegal value\n", srname, *info);
/*<  9 >*/

/*     End of XERBLA */

/*<       END >*/
    return 0;
} /* xerbla_ */

#ifdef __cplusplus
        }
#endif
