#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void drscl_(n, sa, sx, incx)
integer *n;
doublereal *sa, *sx;
integer *incx;
{
    static doublereal cden;
    static logical done;
    static doublereal cnum, cden1, cnum1;
    static doublereal bignum, smlnum, mul;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  Purpose                                                              */
/*  =======                                                              */
/*                                                                       */
/*  DRSCL multiplies an n-element real vector x by the real scalar 1/a.  */
/*  This is done without overflow or underflow as long as                */
/*  the final result x/a does not overflow or underflow.                 */
/*                                                                       */
/*  Arguments                                                            */
/*  =========                                                            */
/*                                                                       */
/*  N       (input) INTEGER                                              */
/*          The number of components of the vector x.                    */
/*                                                                       */
/*  SA      (input) DOUBLE PRECISION                                     */
/*          The scalar a which is used to divide each component of x.    */
/*          SA must be >= 0, or the subroutine will divide by zero.      */
/*                                                                       */
/*  SX      (input/output) DOUBLE PRECISION array, dimension             */
/*                         (1+(N-1)*abs(INCX))                           */
/*          The n-element vector x.                                      */
/*                                                                       */
/*  INCX    (input) INTEGER                                              */
/*          The increment between successive values of the vector SX.    */
/*          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n */
/*                                                                       */
/* ===================================================================== */

/*     Quick return if possible */

    if (*n <= 0) {
        return;
    }

/*     Get machine parameters */

    smlnum = dlamch_("S");
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);

/*     Initialize the denominator to SA and the numerator to 1. */

    cden = *sa;
    cnum = 1.;

L10:
    cden1 = cden * smlnum;
    cnum1 = cnum / bignum;
    if (abs(cden1) > abs(cnum) && cnum != 0.) {

/*        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM. */

        mul = smlnum;
        done = FALSE_;
        cden = cden1;
    } else if (abs(cnum1) > abs(cden)) {

/*        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM. */

        mul = bignum;
        done = FALSE_;
        cnum = cnum1;
    } else {

/*        Multiply X by CNUM / CDEN and return. */

        mul = cnum / cden;
        done = TRUE_;
    }

/*     Scale the vector X by MUL */

    dscal_(n, &mul, sx, incx);

    if (! done) {
        goto L10;
    }
} /* drscl_ */
