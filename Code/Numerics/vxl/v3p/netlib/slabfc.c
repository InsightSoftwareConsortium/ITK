/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;


/* *********************************************************************** */

/* Subroutine */ int slabfc_(n, nband, a, sigma, number, lde, eigvec, numl,
        ldad, atemp, d, atol)
integer *n, *nband;
real *a, *sigma;
integer *number, *lde;
real *eigvec;
integer *numl, *ldad;
real *atemp, *d, *atol;
{
    /* System generated locals */
    integer a_dim1, a_offset, eigvec_dim1, eigvec_offset, atemp_dim1,
            atemp_offset, d_dim1, d_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign();

    /* Local variables */
    static real zero[1];
    static integer i, j, k, l, m;
    extern /* Subroutine */ int scopy_(), sswap_(), saxpy_();
    static integer la, ld, kk, nb1, lpm;


/*  THIS SUBROUTINE FACTORS (A-SIGMA*I) WHERE A IS A GIVEN BAND */
/*  MATRIX AND SIGMA IS AN INPUT PARAMETER.  IT ALSO SOLVES ZERO */
/*  OR MORE SYSTEMS OF LINEAR EQUATIONS.  IT RETURNS THE NUMBER */
/*  OF EIGENVALUES OF A LESS THAN SIGMA BY COUNTING THE STURM */
/*  SEQUENCE DURING THE FACTORIZATION.  TO OBTAIN THE STURM */
/*  SEQUENCE COUNT WHILE ALLOWING NON-SYMMETRIC PIVOTING FOR */
/*  STABILITY, THE CODE USES A GUPTA'S MULTIPLE PIVOTING */
/*  ALGORITHM. */

/*  FORMAL PARAMETERS */


/*  LOCAL VARIABLES */


/*  FUNCTIONS CALLED */


/*  SUBROUTINES CALLED */

/*     SAXPY, SCOPY, SSWAP */


/*  INITIALIZE */

    /* Parameter adjustments */
    d_dim1 = *ldad;
    d_offset = d_dim1 + 1;
    d -= d_offset;
    atemp_dim1 = *ldad;
    atemp_offset = atemp_dim1 + 1;
    atemp -= atemp_offset;
    eigvec_dim1 = *lde;
    eigvec_offset = eigvec_dim1 + 1;
    eigvec -= eigvec_offset;
    a_dim1 = *nband;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    zero[0] = (float)0.;
    nb1 = *nband - 1;
    *numl = 0;
    i__1 = *ldad * *nband;
    scopy_(&i__1, zero, &c__0, &d[d_offset], &c__1);

/*   LOOP OVER COLUMNS OF A */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

/*   ADD A COLUMN OF A TO D */

        d[*nband + *nband * d_dim1] = a[k * a_dim1 + 1] - *sigma;
        m = min(k,*nband) - 1;
        if (m == 0) {
            goto L20;
        }
        i__2 = m;
        for (i = 1; i <= i__2; ++i) {
            la = k - i;
            ld = *nband - i;
            d[ld + *nband * d_dim1] = a[i + 1 + la * a_dim1];
/* L10: */
        }

L20:
/* Computing MIN */
        i__2 = *n - k;
        m = min(i__2,nb1);
        if (m == 0) {
            goto L40;
        }
        i__2 = m;
        for (i = 1; i <= i__2; ++i) {
            ld = *nband + i;
            d[ld + *nband * d_dim1] = a[i + 1 + k * a_dim1];
/* L30: */
        }

/*   TERMINATE */

L40:
        lpm = 1;
        if (nb1 == 0) {
            goto L70;
        }
        i__2 = nb1;
        for (i = 1; i <= i__2; ++i) {
            l = k - *nband + i;
            if (d[i + *nband * d_dim1] == (float)0.) {
                goto L60;
            }
            if ((r__1 = d[i + i * d_dim1], dabs(r__1)) >= (r__2 = d[i + *
                    nband * d_dim1], dabs(r__2))) {
                goto L50;
            }
            if (d[i + *nband * d_dim1] < (float)0. && d[i + i * d_dim1] < (
                    float)0. || d[i + *nband * d_dim1] > (float)0. && d[i + i
                    * d_dim1] >= (float)0.) {
                lpm = -lpm;
            }
            i__3 = *ldad - i + 1;
            sswap_(&i__3, &d[i + i * d_dim1], &c__1, &d[i + *nband * d_dim1],
                    &c__1);
            sswap_(number, &eigvec[l + eigvec_dim1], lde, &eigvec[k +
                    eigvec_dim1], lde);
L50:
            i__3 = *ldad - i;
            r__1 = -(doublereal)d[i + *nband * d_dim1] / d[i + i * d_dim1];
            saxpy_(&i__3, &r__1, &d[i + 1 + i * d_dim1], &c__1, &d[i + 1 + *
                    nband * d_dim1], &c__1);
            r__1 = -(doublereal)d[i + *nband * d_dim1] / d[i + i * d_dim1];
            saxpy_(number, &r__1, &eigvec[l + eigvec_dim1], lde, &eigvec[k +
                    eigvec_dim1], lde);
L60:
            ;
        }

/*  UPDATE STURM SEQUENCE COUNT */

L70:
        if (d[*nband + *nband * d_dim1] < (float)0.) {
            lpm = -lpm;
        }
        if (lpm < 0) {
            ++(*numl);
        }
        if (k == *n) {
            goto L110;
        }

/*   COPY FIRST COLUMN OF D INTO ATEMP */
        if (k < *nband) {
            goto L80;
        }
        l = k - nb1;
        scopy_(ldad, &d[d_offset], &c__1, &atemp[l * atemp_dim1 + 1], &c__1);

/*   SHIFT THE COLUMNS OF D OVER AND UP */

        if (nb1 == 0) {
            goto L100;
        }
L80:
        i__2 = nb1;
        for (i = 1; i <= i__2; ++i) {
            i__3 = *ldad - i;
            scopy_(&i__3, &d[i + 1 + (i + 1) * d_dim1], &c__1, &d[i + i *
                    d_dim1], &c__1);
            d[*ldad + i * d_dim1] = (float)0.;
/* L90: */
        }
L100:
        ;
    }

/*  TRANSFER D TO ATEMP */

L110:
    i__1 = *nband;
    for (i = 1; i <= i__1; ++i) {
        l = *n - *nband + i;
        i__2 = *nband - i + 1;
        scopy_(&i__2, &d[i + i * d_dim1], &c__1, &atemp[l * atemp_dim1 + 1], &
                c__1);
/* L120: */
    }

/*   BACK SUBSTITUTION */

    if (*number == 0) {
        return 0;
    }
    i__1 = *n;
    for (kk = 1; kk <= i__1; ++kk) {
        k = *n - kk + 1;
        if ((r__1 = atemp[k * atemp_dim1 + 1], dabs(r__1)) <= *atol) {
            atemp[k * atemp_dim1 + 1] = r_sign(atol, &atemp[k * atemp_dim1 +
                    1]);
        }

/* L130: */
        i__2 = *number;
        for (i = 1; i <= i__2; ++i) {
            eigvec[k + i * eigvec_dim1] /= atemp[k * atemp_dim1 + 1];
            m = min(*ldad,k) - 1;
            if (m == 0) {
                goto L150;
            }
            i__3 = m;
            for (j = 1; j <= i__3; ++j) {
                l = k - j;
                eigvec[l + i * eigvec_dim1] -= atemp[j + 1 + l * atemp_dim1] *
                         eigvec[k + i * eigvec_dim1];
/* L140: */
            }
L150:
            ;
        }
/* L160: */
    }
    return 0;
} /* slabfc_ */

