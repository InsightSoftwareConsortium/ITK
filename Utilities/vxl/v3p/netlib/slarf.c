#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static real c_b4 = 1.f;
static real c_b5 = 0.f;
static integer c__1 = 1;

/* Subroutine */ void slarf_(const char *side, const integer *m, const integer *n, real *v, const integer *incv,
                             const real *tau, real *c, const integer *ldc, real *work)
{
    /* System generated locals */
    real r__1;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  SLARF applies a real elementary reflector H to a real m by n matrix   */
/*  C, from either the left or the right. H is represented in the form    */
/*                                                                        */
/*        H = I - tau * v * v'                                            */
/*                                                                        */
/*  where tau is a real scalar and v is a real vector.                    */
/*                                                                        */
/*  If tau = 0, then H is taken to be the unit matrix.                    */
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
/*  V       (input) REAL array, dimension                                 */
/*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'                */
/*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'                */
/*          The vector v in the representation of H. V is not used if     */
/*          TAU = 0.                                                      */
/*                                                                        */
/*  INCV    (input) INTEGER                                               */
/*          The increment between elements of v. INCV <> 0.               */
/*                                                                        */
/*  TAU     (input) REAL                                                  */
/*          The value tau in the representation of H.                     */
/*                                                                        */
/*  C       (input/output) REAL array, dimension (LDC,N)                  */
/*          On entry, the m by n matrix C.                                */
/*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',  */
/*          or C * H if SIDE = 'R'.                                       */
/*                                                                        */
/*  LDC     (input) INTEGER                                               */
/*          The leading dimension of the array C. LDC >= max(1,M).        */
/*                                                                        */
/*  WORK    (workspace) REAL array, dimension                             */
/*                         (N) if SIDE = 'L'                              */
/*                      or (M) if SIDE = 'R'                              */
/*                                                                        */
/*  ===================================================================== */

    if (lsame_(side, "L")) {

/*        Form  H * C */

        if (*tau != 0.f) {

/*           w := C' * v */

            sgemv_("Transpose", m, n, &c_b4, c, ldc, v, incv, &c_b5, work, &c__1);

/*           C := C - v * w' */

            r__1 = -(*tau);
            sger_(m, n, &r__1, v, incv, work, &c__1, c, ldc);
        }
    } else {

/*        Form  C * H */

        if (*tau != 0.f) {

/*           w := C * v */

            sgemv_("No transpose", m, n, &c_b4, c, ldc, v, incv, &c_b5, work, &c__1);

/*           C := C - w * v' */

            r__1 = -(*tau);
            sger_(m, n, &r__1, work, &c__1, v, incv, c, ldc);
        }
    }
} /* slarf_ */
