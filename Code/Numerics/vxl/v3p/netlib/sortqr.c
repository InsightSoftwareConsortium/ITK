/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


/* ------------------------------------------------------------------ */

/* Subroutine */ int sortqr_(nz, n, nblock, z, b)
integer *nz, *n, *nblock;
real *z, *b;
{
    /* System generated locals */
    integer z_dim1, z_offset, b_dim1, b_offset, i__1, i__2;
    real r__1;

    /* Builtin functions */
    double r_sign();

    /* Local variables */
    static real temp;
    extern doublereal sdot_(), snrm2_();
    static integer i, j, k, m;
    static real sigma;
    extern /* Subroutine */ int sscal_(), saxpy_();
    static integer length;
    static real tau;



/* THIS SUBROUTINE COMPUTES THE QR FACTORIZATION OF THE N X NBLOCK */
/* MATRIX Z.  Q IS FORMED IN PLACE AND RETURNED IN Z.  R IS */
/* RETURNED IN B. */


/* THIS SECTION REDUCES Z TO TRIANGULAR FORM. */

    /* Parameter adjustments */
    b_dim1 = *nblock;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    z_dim1 = *nz;
    z_offset = z_dim1 + 1;
    z -= z_offset;

    /* Function Body */
    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {

/* THIS FORMS THE ITH REFLECTION. */

        length = *n - i + 1;
        r__1 = snrm2_(&length, &z[i + i * z_dim1], &c__1);
        sigma = r_sign(&r__1, &z[i + i * z_dim1]);
        b[i + i * b_dim1] = -(doublereal)sigma;
        z[i + i * z_dim1] += sigma;
        tau = sigma * z[i + i * z_dim1];
        if (i == *nblock) {
            goto L30;
        }
        j = i + 1;

/* THIS APPLIES THE ROTATION TO THE REST OF THE COLUMNS. */

        i__2 = *nblock;
        for (k = j; k <= i__2; ++k) {
            if (tau == (float)0.) {
                goto L10;
            }
            temp = -(doublereal)sdot_(&length, &z[i + i * z_dim1], &c__1, &z[
                    i + k * z_dim1], &c__1) / tau;
            saxpy_(&length, &temp, &z[i + i * z_dim1], &c__1, &z[i + k *
                    z_dim1], &c__1);
L10:
            b[i + k * b_dim1] = z[i + k * z_dim1];
            z[i + k * z_dim1] = (float)0.;
/* L20: */
        }
L30:
        ;
    }

/* THIS ACCUMULATES THE REFLECTIONS IN REVERSE ORDER. */

    i__1 = *nblock;
    for (m = 1; m <= i__1; ++m) {

/* THIS RECREATES THE ITH = NBLOCK-M+1)TH REFLECTION. */

        i = *nblock + 1 - m;
        sigma = -(doublereal)b[i + i * b_dim1];
        tau = z[i + i * z_dim1] * sigma;
        if (tau == (float)0.) {
            goto L60;
        }
        length = *n - *nblock + m;
        if (i == *nblock) {
            goto L50;
        }
        j = i + 1;

/* THIS APPLIES IT TO THE LATER COLUMNS. */

        i__2 = *nblock;
        for (k = j; k <= i__2; ++k) {
            temp = -(doublereal)sdot_(&length, &z[i + i * z_dim1], &c__1, &z[
                    i + k * z_dim1], &c__1) / tau;
            saxpy_(&length, &temp, &z[i + i * z_dim1], &c__1, &z[i + k *
                    z_dim1], &c__1);
/* L40: */
        }
L50:
        r__1 = (float)-1. / sigma;
        sscal_(&length, &r__1, &z[i + i * z_dim1], &c__1);
L60:
        z[i + i * z_dim1] += (float)1.;
/* L70: */
    }
    return 0;
} /* sortqr_ */

