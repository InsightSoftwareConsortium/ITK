#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static doublecomplex c_b3 = {0.,0.};
static doublecomplex c_b5 = {1.,0.};
static integer c__1 = 1;

/* Subroutine */ void zlarf_(side, m, n, v, incv, tau, c, ldc, work)
const char *side;
const integer *m, *n;
doublecomplex *v;
const integer *incv;
const doublecomplex *tau;
doublecomplex *c;
const integer *ldc;
doublecomplex *work;
{
    /* System generated locals */
    doublecomplex z__1;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLARF applies a complex elementary reflector H to a complex M-by-N    */
/*  matrix C, from either the left or the right. H is represented in the  */
/*  form                                                                  */
/*                                                                        */
/*        H = I - tau * v * v'                                            */
/*                                                                        */
/*  where tau is a complex scalar and v is a complex vector.              */
/*                                                                        */
/*  If tau = 0, then H is taken to be the unit matrix.                    */
/*                                                                        */
/*  To apply H' (the conjugate transpose of H), supply conjg(tau) instead */
/*  tau.                                                                  */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  SIDE    (input) CHARACTER*1                                           */
/*          = 'L': form  H * C                                            */
/*          = 'R': form  C * H                                            */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix C.                           */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix C.                        */
/*                                                                        */
/*  V       (input) COMPLEX*16 array, dimension                           */
/*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'                */
/*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'                */
/*          The vector v in the representation of H. V is not used if     */
/*          TAU = 0.                                                      */
/*                                                                        */
/*  INCV    (input) INTEGER                                               */
/*          The increment between elements of v. INCV <> 0.               */
/*                                                                        */
/*  TAU     (input) COMPLEX*16                                            */
/*          The value tau in the representation of H.                     */
/*                                                                        */
/*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)            */
/*          On entry, the M-by-N matrix C.                                */
/*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',  */
/*          or C * H if SIDE = 'R'.                                       */
/*                                                                        */
/*  LDC     (input) INTEGER                                               */
/*          The leading dimension of the array C. LDC >= max(1,M).        */
/*                                                                        */
/*  WORK    (workspace) COMPLEX*16 array, dimension                       */
/*                         (N) if SIDE = 'L'                              */
/*                      or (M) if SIDE = 'R'                              */
/*                                                                        */
/*  ===================================================================== */

    if (lsame_(side, "L")) {

/*        Form  H * C */

        if (tau->r != 0. || tau->i != 0.) {

/*           w := C' * v */

            zgemv_("Conjugate transpose", m, n, &c_b5, c, ldc, v, incv, &c_b3, work, &c__1);

/*           C := C - v * w' */

            z__1.r = -tau->r, z__1.i = -tau->i;
            zgerc_(m, n, &z__1, v, incv, work, &c__1, c, ldc);
        }
    } else {

/*        Form  C * H */

        if (tau->r != 0. || tau->i != 0.) {

/*           w := C * v */

            zgemv_("No transpose", m, n, &c_b5, c, ldc, v, incv, &c_b3, work, &c__1);

/*           C := C - w * v' */

            z__1.r = -tau->r, z__1.i = -tau->i;
            zgerc_(m, n, &z__1, work, &c__1, v, incv, c, ldc);
        }
    }
} /* zlarf_ */
