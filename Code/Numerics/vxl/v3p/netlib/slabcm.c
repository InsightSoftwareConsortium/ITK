/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


/* *********************************************************************** */

/* Subroutine */ int slabcm_(n, nband, nl, nr, a, eigval, lde, eigvec, atol,
        artol, bound, atemp, d, vtemp)
integer *n, *nband, *nl, *nr;
real *a, *eigval;
integer *lde;
real *eigvec, *atol, *artol, *bound, *atemp, *d, *vtemp;
{
    /* System generated locals */
    integer a_dim1, a_offset, eigvec_dim1, eigvec_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Local variables */
    static logical flag_;
    static real errb;
    static integer nval;
    extern doublereal sdot_();
    static integer numl;
    extern doublereal snrm2_();
    static integer i, j, l, m;
    static real sigma;
    extern /* Subroutine */ int sscal_();
    static real resid;
    extern /* Subroutine */ int scopy_();
    static real vnorm;
    extern /* Subroutine */ int saxpy_(), slabfc_();
    static real rq;
    extern /* Subroutine */ int slabax_(), slaran_();
    static integer numvec;
    static real gap;


/*  THIS SUBROUTINE ORGANIZES THE CALCULATION OF THE EIGENVALUES */
/*  FOR THE BNDEIG PACKAGE.  EIGENVALUES ARE COMPUTED BY */
/*  A MODIFIED RAYLEIGH QUOTIENT ITERATION.  THE EIGENVALUE COUNT */
/*  OBTAINED BY EACH FACTORIZATION IS USED TO OCCASIONALLY OVERRIDE */
/*  THE COMPUTED RAYLEIGH QUOTIENT WITH A DIFFERENT SHIFT TO */
/*  INSURE CONVERGENCE TO THE DESIRED EIGENVALUES. */

/*  FORMAL PARAMETERS. */



/*  LOCAL VARIABLES */



/*  FUNCTIONS CALLED */


/*  SUBROUTINES CALLED */

/*     SLABAX, SLABFC, SLARAN, SAXPY, SCOPY, SSCAL */

/*  REPLACE ZERO VECTORS BY RANDOM */

    /* Parameter adjustments */
    --vtemp;
    --d;
    --atemp;
    bound -= 3;
    eigvec_dim1 = *lde;
    eigvec_offset = eigvec_dim1 + 1;
    eigvec -= eigvec_offset;
    --eigval;
    a_dim1 = *nband;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    nval = *nr - *nl + 1;
    flag_ = FALSE_;
    i__1 = nval;
    for (i = 1; i <= i__1; ++i) {
        if (sdot_(n, &eigvec[i * eigvec_dim1 + 1], &c__1, &eigvec[i *
                eigvec_dim1 + 1], &c__1) == (float)0.) {
            slaran_(n, &eigvec[i * eigvec_dim1 + 1]);
        }
/* L5: */
    }

/*  LOOP OVER EIGENVALUES */

    sigma = bound[(nval + 1 << 1) + 2];
    i__1 = nval;
    for (j = 1; j <= i__1; ++j) {
        numl = j;

/*  PREPARE TO COMPUTE FIRST RAYLEIGH QUOTIENT */

L10:
        slabax_(n, nband, &a[a_offset], &eigvec[j * eigvec_dim1 + 1], &vtemp[
                1]);
        vnorm = snrm2_(n, &vtemp[1], &c__1);
        if (vnorm == (float)0.) {
            goto L20;
        }
        r__1 = (float)1. / vnorm;
        sscal_(n, &r__1, &vtemp[1], &c__1);
        r__1 = (float)1. / vnorm;
        sscal_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1);
        r__1 = -(doublereal)sigma;
        saxpy_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1], &
                c__1);

/*  LOOP OVER SHIFTS */

/*  COMPUTE RAYLEIGH QUOTIENT, RESIDUAL NORM, AND CURRENT TOLERANCE */

L20:
        vnorm = snrm2_(n, &eigvec[j * eigvec_dim1 + 1], &c__1);
        if (vnorm != (float)0.) {
            goto L30;
        }
        slaran_(n, &eigvec[j * eigvec_dim1 + 1]);
        goto L10;

L30:
        rq = sigma + sdot_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1],
                &c__1) / vnorm / vnorm;
        r__1 = sigma - rq;
        saxpy_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1], &
                c__1);
/* Computing MAX */
        r__1 = *atol, r__2 = snrm2_(n, &vtemp[1], &c__1) / vnorm;
        resid = dmax(r__1,r__2);
        r__1 = (float)1. / vnorm;
        sscal_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1);

/*  ACCEPT EIGENVALUE IF THE INTERVAL IS SMALL ENOUGH */

        if (bound[(j + 1 << 1) + 2] - bound[(j + 1 << 1) + 1] < *atol * (
                float)3.) {
            goto L300;
        }

/*  COMPUTE MINIMAL ERROR BOUND */

        errb = resid;
/* Computing MIN */
        r__1 = bound[(j + 2 << 1) + 1] - rq, r__2 = rq - bound[(j << 1) + 2];
        gap = dmin(r__1,r__2);
        if (gap > resid) {
/* Computing MAX */
            r__1 = *atol, r__2 = resid * resid / gap;
            errb = dmax(r__1,r__2);
        }

/*  TENTATIVE NEW SHIFT */

        sigma = (bound[(j + 1 << 1) + 1] + bound[(j + 1 << 1) + 2]) * (float)
                .5;

/*  CHECK FOR TERMINALTION */

        if (resid > *atol * (float)2.) {
            goto L40;
        }
        if (rq - errb > bound[(j << 1) + 2] && rq + errb < bound[(j + 2 << 1)
                + 1]) {
            goto L310;
        }

/*  RQ IS TO THE LEFT OF THE INTERVAL */

L40:
        if (rq >= bound[(j + 1 << 1) + 1]) {
            goto L50;
        }
        if (rq - errb > bound[(j << 1) + 2]) {
            goto L100;
        }
        if (rq + errb < bound[(j + 1 << 1) + 1]) {
            slaran_(n, &eigvec[j * eigvec_dim1 + 1]);
        }
        goto L200;

/*  RQ IS TO THE RIGHT OF THE INTERVAL */

L50:
        if (rq <= bound[(j + 1 << 1) + 2]) {
            goto L100;
        }
        if (rq + errb < bound[(j + 2 << 1) + 1]) {
            goto L100;
        }

/*  SAVE THE REJECTED VECTOR IF INDICATED */

        if (rq - errb <= bound[(j + 1 << 1) + 2]) {
            goto L200;
        }
        i__2 = nval;
        for (i = j; i <= i__2; ++i) {
            if (bound[(i + 1 << 1) + 2] > rq) {
                goto L70;
            }
/* L60: */
        }
        goto L80;

L70:
        scopy_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &eigvec[i *
                eigvec_dim1 + 1], &c__1);

L80:
        slaran_(n, &eigvec[j * eigvec_dim1 + 1]);
        goto L200;

/*  PERTURB RQ TOWARD THE MIDDLE */

L100:
        if (sigma < rq) {
/* Computing MAX */
            r__1 = sigma, r__2 = rq - errb;
            sigma = dmax(r__1,r__2);
        }
        if (sigma > rq) {
/* Computing MIN */
            r__1 = sigma, r__2 = rq + errb;
            sigma = dmin(r__1,r__2);
        }

/*  FACTOR AND SOLVE */

L200:
        i__2 = nval;
        for (i = j; i <= i__2; ++i) {
            if (sigma < bound[(i + 1 << 1) + 1]) {
                goto L220;
            }
/* L210: */
        }
        i = nval + 1;
L220:
        numvec = i - j;
/* Computing MIN */
        i__2 = numvec, i__3 = *nband + 2;
        numvec = min(i__2,i__3);
        if (resid < *artol) {
            numvec = min(1,numvec);
        }
        scopy_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1], &c__1);
        i__2 = (*nband << 1) - 1;
        slabfc_(n, nband, &a[a_offset], &sigma, &numvec, lde, &eigvec[j *
                eigvec_dim1 + 1], &numl, &i__2, &atemp[1], &d[1], atol);

/*  PARTIALLY SCALE EXTRA VECTORS TO PREVENT UNDERFLOW OR OVERFLOW */

        if (numvec == 1) {
            goto L227;
        }
        l = numvec - 1;
        i__2 = l;
        for (i = 1; i <= i__2; ++i) {
            m = j + i;
            r__1 = (float)1. / vnorm;
            sscal_(n, &r__1, &eigvec[m * eigvec_dim1 + 1], &c__1);
/* L225: */
        }

/*  UPDATE INTERVALS */

L227:
        numl = numl - *nl + 1;
        if (numl >= 0) {
            bound[4] = dmin(bound[4],sigma);
        }
        i__2 = nval;
        for (i = j; i <= i__2; ++i) {
            if (sigma < bound[(i + 1 << 1) + 1]) {
                goto L20;
            }
            if (numl < i) {
                bound[(i + 1 << 1) + 1] = sigma;
            }
            if (numl >= i) {
                bound[(i + 1 << 1) + 2] = sigma;
            }
/* L230: */
        }
        if (numl < nval + 1) {
/* Computing MAX */
            r__1 = sigma, r__2 = bound[(nval + 2 << 1) + 1];
            bound[(nval + 2 << 1) + 1] = dmax(r__1,r__2);
        }
        goto L20;

/*  ACCEPT AN EIGENPAIR */

L300:
        slaran_(n, &eigvec[j * eigvec_dim1 + 1]);
        flag_ = TRUE_;
        goto L310;

L305:
        flag_ = FALSE_;
        rq = (bound[(j + 1 << 1) + 1] + bound[(j + 1 << 1) + 2]) * (float).5;
        i__2 = (*nband << 1) - 1;
        slabfc_(n, nband, &a[a_offset], &rq, &numvec, lde, &eigvec[j *
                eigvec_dim1 + 1], &numl, &i__2, &atemp[1], &d[1], atol);
        vnorm = snrm2_(n, &eigvec[j * eigvec_dim1 + 1], &c__1);
        if (vnorm != (float)0.) {
            r__1 = (float)1. / vnorm;
            sscal_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1);
        }

/*  ORTHOGONALIZE THE NEW EIGENVECTOR AGAINST THE OLD ONES */

L310:
        eigval[j] = rq;
        if (j == 1) {
            goto L330;
        }
        m = j - 1;
        i__2 = m;
        for (i = 1; i <= i__2; ++i) {
            r__1 = -(doublereal)sdot_(n, &eigvec[i * eigvec_dim1 + 1], &c__1,
                    &eigvec[j * eigvec_dim1 + 1], &c__1);
            saxpy_(n, &r__1, &eigvec[i * eigvec_dim1 + 1], &c__1, &eigvec[j *
                    eigvec_dim1 + 1], &c__1);
/* L320: */
        }
L330:
        vnorm = snrm2_(n, &eigvec[j * eigvec_dim1 + 1], &c__1);
        if (vnorm == (float)0.) {
            goto L305;
        }
        r__1 = (float)1. / vnorm;
        sscal_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1);

/*   ORTHOGONALIZE LATER VECTORS AGAINST THE CONVERGED ONE */

        if (flag_) {
            goto L305;
        }
        if (j == nval) {
            return 0;
        }
        m = j + 1;
        i__2 = nval;
        for (i = m; i <= i__2; ++i) {
            r__1 = -(doublereal)sdot_(n, &eigvec[j * eigvec_dim1 + 1], &c__1,
                    &eigvec[i * eigvec_dim1 + 1], &c__1);
            saxpy_(n, &r__1, &eigvec[j * eigvec_dim1 + 1], &c__1, &eigvec[i *
                    eigvec_dim1 + 1], &c__1);
/* L340: */
        }
/* L400: */
    }
    return 0;

/* L500: */
} /* slabcm_ */

