/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal scnrm2_(n, x, incx)
integer *n;
complex *x;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real ret_val, r__1;

    /* Builtin functions */
    double r_imag(), sqrt();

    /* Local variables */
    static real temp, norm, scale;
    static integer ix;
    static real ssq;

/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  SCNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     SCNRM2 := sqrt( conjg( x' )*x ) */



/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to CLASSQ. */
/*     Sven Hammarling, Nag Ltd. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1 || *incx < 1) {
        norm = (float)0.;
    } else {
        scale = (float)0.;
        ssq = (float)1.;
/*        The following loop is equivalent to this call to the LAPACK
*/
/*        auxiliary routine: */
/*        CALL CLASSQ( N, X, INCX, SCALE, SSQ ) */

        i__1 = (*n - 1) * *incx + 1;
        i__2 = *incx;
        for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
            i__3 = ix;
            if (x[i__3].r != (float)0.) {
                i__3 = ix;
                temp = (r__1 = x[i__3].r, dabs(r__1));
                if (scale < temp) {
/* Computing 2nd power */
                    r__1 = scale / temp;
                    ssq = ssq * (r__1 * r__1) + (float)1.;
                    scale = temp;
                } else {
/* Computing 2nd power */
                    r__1 = temp / scale;
                    ssq += r__1 * r__1;
                }
            }
            if (r_imag(&x[ix]) != (float)0.) {
                temp = (r__1 = r_imag(&x[ix]), dabs(r__1));
                if (scale < temp) {
/* Computing 2nd power */
                    r__1 = scale / temp;
                    ssq = ssq * (r__1 * r__1) + (float)1.;
                    scale = temp;
                } else {
/* Computing 2nd power */
                    r__1 = temp / scale;
                    ssq += r__1 * r__1;
                }
            }
/* L10: */
        }
        norm = scale * sqrt(ssq);
    }

    ret_val = norm;
    return ret_val;

/*     End of SCNRM2. */

} /* scnrm2_ */

