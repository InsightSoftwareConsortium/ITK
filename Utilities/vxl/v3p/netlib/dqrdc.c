#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void dqrdc_(x, ldx, n, p, qraux, jpvt, work, job)
doublereal *x;
const integer *ldx, *n, *p;
doublereal *qraux;
integer *jpvt;
doublereal *work;
const integer *job;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static logical negj;
    static integer maxj;
    static integer j, l;
    static doublereal t;
    static logical swapj;
    static doublereal nrmxl;
    static integer jp, pl, pu;
    static doublereal tt, maxnrm;

/*     dqrdc uses householder transformations to compute the qr         */
/*     factorization of an n by p matrix x.  column pivoting            */
/*     based on the 2-norms of the reduced columns may be               */
/*     performed at the users option.                                   */
/*                                                                      */
/*     on entry                                                         */
/*                                                                      */
/*        x       double precision(ldx,p), where ldx .ge. n.            */
/*                x contains the matrix whose decomposition is to be    */
/*                computed.                                             */
/*                                                                      */
/*        ldx     integer.                                              */
/*                ldx is the leading dimension of the array x.          */
/*                                                                      */
/*        n       integer.                                              */
/*                n is the number of rows of the matrix x.              */
/*                                                                      */
/*        p       integer.                                              */
/*                p is the number of columns of the matrix x.           */
/*                                                                      */
/*        jpvt    integer(p).                                           */
/*                jpvt contains integers that control the selection     */
/*                of the pivot columns.  the k-th column x(k) of x      */
/*                is placed in one of three classes according to the    */
/*                value of jpvt(k).                                     */
/*                                                                      */
/*                   if jpvt(k) .gt. 0, then x(k) is an initial         */
/*                                      column.                         */
/*                                                                      */
/*                   if jpvt(k) .eq. 0, then x(k) is a free column.     */
/*                                                                      */
/*                   if jpvt(k) .lt. 0, then x(k) is a final column.    */
/*                                                                      */
/*                before the decomposition is computed, initial columns */
/*                are moved to the beginning of the array x and final   */
/*                columns to the end.  both initial and final columns   */
/*                are frozen in place during the computation and only   */
/*                free columns are moved.  at the k-th stage of the     */
/*                reduction, if x(k) is occupied by a free column       */
/*                it is interchanged with the free column of largest    */
/*                reduced norm.  jpvt is not referenced if              */
/*                job .eq. 0.                                           */
/*                                                                      */
/*        work    double precision(p).                                  */
/*                work is a work array.  work is not referenced if      */
/*                job .eq. 0.                                           */
/*                                                                      */
/*        job     integer.                                              */
/*                job is an integer that initiates column pivoting.     */
/*                if job .eq. 0, no pivoting is done.                   */
/*                if job .ne. 0, pivoting is done.                      */
/*                                                                      */
/*     on return                                                        */
/*                                                                      */
/*        x       x contains in its upper triangle the upper            */
/*                triangular matrix r of the qr factorization.          */
/*                below its diagonal x contains information from        */
/*                which the orthogonal part of the decomposition        */
/*                can be recovered.  note that if pivoting has          */
/*                been requested, the decomposition is not that         */
/*                of the original matrix x but that of x                */
/*                with its columns permuted as described by jpvt.       */
/*                                                                      */
/*        qraux   double precision(p).                                  */
/*               qraux contains further information required to recover */
/*                the orthogonal part of the decomposition.             */
/*                                                                      */
/*        jpvt    jpvt(k) contains the index of the column of the       */
/*                original matrix that has been interchanged into       */
/*                the k-th column, if pivoting was requested.           */
/*                                                                      */
/*     linpack. this version dated 08/14/78 .                           */
/*     g.w. stewart, university of maryland, argonne national lab.      */

/*     dqrdc uses the following functions and subprograms. */
/*                                                         */
/*     blas daxpy,ddot,dscal,dswap,dnrm2                   */
/*     fortran dabs,dmax1,min0,dsqrt                       */

/*     internal variables */

    pl = 0;
    pu = -1;
    if (*job == 0) {
        goto L60;
    }

/*        pivoting has been requested.  rearrange the columns */
/*        according to jpvt. */

    for (j = 0; j < *p; ++j) {
        swapj = jpvt[j] > 0;
        negj = jpvt[j] < 0;
        jpvt[j] = j+1;
        if (negj) {
            jpvt[j] = -j-1;
        }
        if (! swapj) {
            continue;
        }
        if (j != pl) {
            dswap_(n, &x[pl * *ldx], &c__1, &x[j * *ldx], &c__1);
        }
        jpvt[j] = jpvt[pl];
        jpvt[pl] = j+1;
        ++pl;
    }
    pu = *p - 1;
    for (j = pu; j >= 0; --j) {
        if (jpvt[j] >= 0) {
            continue;
        }
        jpvt[j] = -jpvt[j];
        if (j != pu) {
            dswap_(n, &x[pu * *ldx], &c__1, &x[j * *ldx], &c__1);
            jp = jpvt[pu];
            jpvt[pu] = jpvt[j];
            jpvt[j] = jp;
        }
        --pu;
    }
L60:

/*     compute the norms of the free columns. */

    for (j = pl; j <= pu; ++j) {
        qraux[j] = dnrm2_(n, &x[j * *ldx], &c__1);
        work[j] = qraux[j];
    }

/*     perform the householder reduction of x. */

    for (l = 0; l < *n && l < *p; ++l) {
        if (l < pl || l >= pu) {
            goto L120;
        }

/*           locate the column of largest norm and bring it */
/*           into the pivot position. */

        maxnrm = 0.;
        maxj = l;
        for (j = l; j <= pu; ++j) {
            if (qraux[j] <= maxnrm) {
                continue;
            }
            maxnrm = qraux[j];
            maxj = j;
        }
        if (maxj != l) {
            dswap_(n, &x[l * *ldx], &c__1, &x[maxj * *ldx], &c__1);
            qraux[maxj] = qraux[l];
            work[maxj] = work[l];
            jp = jpvt[maxj]; jpvt[maxj] = jpvt[l]; jpvt[l] = jp;
        }
L120:
        qraux[l] = 0.;
        if (l+1 == *n) {
            continue;
        }

/*           compute the householder transformation for column l. */

        i__1 = *n - l;
        nrmxl = dnrm2_(&i__1, &x[l + l * *ldx], &c__1);
        if (nrmxl == 0.) {
            continue;
        }
        if (x[l + l * *ldx] != 0.) {
            nrmxl = d_sign(&nrmxl, &x[l + l * *ldx]);
        }
        i__1 = *n - l;
        d__1 = 1. / nrmxl;
        dscal_(&i__1, &d__1, &x[l + l * *ldx], &c__1);
        x[l + l * *ldx] += 1.;

/*              apply the transformation to the remaining columns, */
/*              updating the norms. */

        for (j = l+1; j < *p; ++j) {
            i__1 = *n - l;
            t = -ddot_(&i__1, &x[l + l * *ldx], &c__1,
                              &x[l + j * *ldx], &c__1) / x[l + l * *ldx];
            daxpy_(&i__1, &t, &x[l + l * *ldx], &c__1, &x[l + j * *ldx], &c__1);
            if (j < pl || j > pu) {
                continue;
            }
            if (qraux[j] == 0.) {
                continue;
            }
            tt = abs(x[l + j * *ldx]) / qraux[j];
            tt = 1. - tt * tt;
            tt = max(tt,0.);
            t = tt;
            d__1 = qraux[j] / work[j];
            tt = tt * .05 * d__1 * d__1 + 1.;
            if (tt != 1.) {
                qraux[j] *= sqrt(t);
                continue;
            }
            i__1 = *n - l - 1;
            qraux[j] = dnrm2_(&i__1, &x[l + 1 + j * *ldx], &c__1);
            work[j] = qraux[j];
        }

/*              save the transformation. */

        qraux[l] = x[l + l * *ldx];
        x[l + l * *ldx] = -nrmxl;
    }
} /* dqrdc_ */
