#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Subroutine */ void dlabad_(small, large)
doublereal *small, *large;
{
/*  -- LAPACK auxiliary routine (version 2.0) --                          */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     October 31, 1992                                                   */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLABAD takes as input the values computed by SLAMCH for underflow and */
/*  overflow, and returns the square root of each of these values if the  */
/*  log of LARGE is sufficiently large.  This subroutine is intended to   */
/*  identify machines with a large exponent range, such as the Crays, and */
/*  redefine the underflow and overflow limits to be the square roots of  */
/*  the values computed by DLAMCH.  This subroutine is needed because     */
/*  DLAMCH does not compensate for poor arithmetic in the upper half of   */
/*  the exponent range, as is found on a Cray.                            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  SMALL   (input/output) DOUBLE PRECISION                               */
/*          On entry, the underflow threshold as computed by DLAMCH.      */
/*          On exit, if LOG10(LARGE) is sufficiently large, the square    */
/*          root of SMALL, otherwise unchanged.                           */
/*                                                                        */
/*  LARGE   (input/output) DOUBLE PRECISION                               */
/*          On entry, the overflow threshold as computed by DLAMCH.       */
/*          On exit, if LOG10(LARGE) is sufficiently large, the square    */
/*          root of LARGE, otherwise unchanged.                           */
/*                                                                        */
/*  ===================================================================== */

/*     If it looks like we're on a Cray, take the square root of */
/*     SMALL and LARGE to avoid overflow and underflow problems. */

    if (d_lg10(large) > 2e3) {
        *small = sqrt(*small);
        *large = sqrt(*large);
    }
} /* dlabad_ */
