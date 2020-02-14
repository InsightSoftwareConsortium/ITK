/* lapack/complex16/zladiv.f -- translated by f2c (version 20050501).
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

/*<       DOUBLE COMPLEX   FUNCTION ZLADIV( X, Y ) >*/
/* Double Complex */ VOID zladiv_(doublecomplex * ret_val, doublecomplex *x,
        doublecomplex *y)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1;

    /* Builtin functions */
    double d_imag(doublecomplex *);

    /* Local variables */
    doublereal zi, zr;
    extern /* Subroutine */ int dladiv_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *);


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       COMPLEX*16         X, Y >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y */
/*  will not overflow on an intermediary step unless the results */
/*  overflows. */

/*  Arguments */
/*  ========= */

/*  X       (input) COMPLEX*16 */
/*  Y       (input) COMPLEX*16 */
/*          The complex scalars X and Y. */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   ZI, ZR >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLADIV >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DBLE, DCMPLX, DIMAG >*/
/*     .. */
/*     .. Executable Statements .. */

/*<    >*/
    d__1 = x->r;
    d__2 = d_imag(x);
    d__3 = y->r;
    d__4 = d_imag(y);
    dladiv_(&d__1, &d__2, &d__3, &d__4, &zr, &zi);
/*<       ZLADIV = DCMPLX( ZR, ZI ) >*/
    z__1.r = zr, z__1.i = zi;
     ret_val->r = z__1.r,  ret_val->i = z__1.i;

/*<       RETURN >*/
    return ;

/*     End of ZLADIV */

/*<       END >*/
} /* zladiv_ */

#ifdef __cplusplus
        }
#endif
