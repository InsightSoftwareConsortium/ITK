#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zlacgv_(n, x, incx)
const integer *n;
doublecomplex *x;
const integer *incx;
{
    /* Local variables */
    static integer ioff, i;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/* ===================================================================== */
/*                                                                       */
/*  Purpose                                                              */
/*  =======                                                              */
/*                                                                       */
/*  ZLACGV conjugates a complex vector of length N.                      */
/*                                                                       */
/*  Arguments                                                            */
/*  =========                                                            */
/*                                                                       */
/*  N       (input) INTEGER                                              */
/*          The length of the vector X.  N >= 0.                         */
/*                                                                       */
/*  X       (input/output) COMPLEX*16 array, dimension                   */
/*                         (1+(N-1)*abs(INCX))                           */
/*          On entry, the vector of length N to be conjugated.           */
/*          On exit, X is overwritten with conjg(X).                     */
/*                                                                       */
/*  INCX    (input) INTEGER                                              */
/*          The spacing between successive elements of X.                */
/*                                                                       */
/* ===================================================================== */

    if (*incx == 1) {
        for (i = 0; i < *n; ++i) {
            x[i].i = -x[i].i; /* d_cnjg(&x[i], &x[i]); */
        }
    } else {
        ioff = 0;
        if (*incx < 0) {
            ioff = (1-(*n)) * *incx;
        }
        for (i = 0; i < *n; ++i) {
            x[ioff].i = -x[ioff].i; /* d_cnjg(&x[ioff], &x[ioff]); */
            ioff += *incx;
        }
    }
} /* zlacgv_ */
