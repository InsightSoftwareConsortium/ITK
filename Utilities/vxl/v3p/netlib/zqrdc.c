#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static doublecomplex c_1 = {1.,0.};

/* Subroutine */ void zqrdc_(x, ldx, n, p, qraux, jpvt, work, job)
doublecomplex *x;
const integer *ldx, *n, *p;
doublecomplex *qraux;
integer *jpvt;
doublecomplex *work;
const integer *job;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;
    doublecomplex z__1;

    /* Local variables */
    static logical negj;
    static integer maxj, j, l;
    static doublecomplex t;
    static logical swapj;
    static doublecomplex nrmxl;
    static integer jp, pl, pu;
    static doublereal tt, maxnrm;

/************************************************************************/
/*                                                                      */
/*     zqrdc uses householder transformations to compute the qr         */
/*     factorization of an n by p matrix x.  column pivoting            */
/*     based on the 2-norms of the reduced columns may be               */
/*     performed at the users option.                                   */
/*                                                                      */
/*     on entry                                                         */
/*                                                                      */
/*        x       complex*16(ldx,p), where ldx .ge. n.                  */
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
/*        work    complex*16(p).                                        */
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
/*                which the unitary part of the decomposition           */
/*                can be recovered.  note that if pivoting has          */
/*                been requested, the decomposition is not that         */
/*                of the original matrix x but that of x                */
/*                with its columns permuted as described by jpvt.       */
/*                                                                      */
/*        qraux   complex*16(p).                                        */
/*                qraux contains further information required to recover*/
/*                the unitary part of the decomposition.                */
/*                                                                      */
/*        jpvt    jpvt(k) contains the index of the column of the       */
/*                original matrix that has been interchanged into       */
/*                the k-th column, if pivoting was requested.           */
/*                                                                      */
/*     linpack. this version dated 08/14/78 .                           */
/*     g.w. stewart, university of maryland, argonne national lab.      */
/*                                                                      */
/*     zqrdc uses the following functions and subprograms.              */
/*                                                                      */
/*     blas zaxpy,zdotc,zscal,zswap,dznrm2                              */
/*     fortran dmax1,cdabs,dcmplx,cdsqrt,min0                           */
/*                                                                      */
/************************************************************************/

    pl = 0;
    pu = -1;

    if (*job != 0) {

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
                continue; /* next j */
            }
            if (j != pl) {
                zswap_(n, &x[pl* *ldx], &c__1, &x[j* *ldx], &c__1);
            }
            jpvt[j] = jpvt[pl];
            jpvt[pl] = j+1;
            ++pl;
        }
        pu = *p - 1;
        for (j = pu; j >= 0; --j) {
            if (jpvt[j] >= 0) {
                continue; /* next j */
            }
            jpvt[j] = -jpvt[j];
            if (j == pu) {
                --pu; continue; /* next j */
            }
            zswap_(n, &x[pu* *ldx], &c__1, &x[j* *ldx], &c__1);
            jp = jpvt[pu];
            jpvt[pu] = jpvt[j];
            jpvt[j] = jp;
            --pu;
        }
    }

/*     compute the norms of the free columns. */

    for (j = pl; j <= pu; ++j) {
        work[j].r = qraux[j].r = dznrm2_(n, &x[j* *ldx], &c__1),
        work[j].i = qraux[j].i = 0.;
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
            if (qraux[j].r > maxnrm) {
                maxnrm = qraux[j].r;
                maxj = j;
            }
        }
        if (maxj != l) {
            zswap_(n, &x[l* *ldx], &c__1, &x[maxj* *ldx], &c__1);
            qraux[maxj].r = qraux[l].r, qraux[maxj].i = qraux[l].i;
            work[maxj].r = work[l].r, work[maxj].i = work[l].i;
            jp = jpvt[maxj];
            jpvt[maxj] = jpvt[l];
            jpvt[l] = jp;
        }
L120:
        qraux[l].r = 0., qraux[l].i = 0.;
        if (l+1 == *n) {
            continue; /* next l */
        }

/*           compute the householder transformation for column l. */

        i__1 = *n - l;
        i__2 = l + l * *ldx;
        nrmxl.r = dznrm2_(&i__1, &x[i__2], &c__1), nrmxl.i = 0.;
        if (nrmxl.r == 0.) {
            continue; /* next l */
        }
        if (x[i__2].r != 0. || x[i__2].i != 0.) {
            d__1 = z_abs(&nrmxl);
            d__2 = z_abs(&x[i__2]);
            nrmxl.r = d__1 * x[i__2].r / d__2,
            nrmxl.i = d__1 * x[i__2].i / d__2;
        }
        z_div(&z__1, &c_1, &nrmxl);
        zscal_(&i__1, &z__1, &x[i__2], &c__1);
        x[i__2].r += 1.;

/*              apply the transformation to the remaining columns, */
/*              updating the norms. */

        for (j = l+1; j < *p; ++j) {
            i__1 = *n - l;
            i__2 = l + l * *ldx;
            zdotc_(&z__1, &i__1, &x[i__2], &c__1, &x[l+j* *ldx], &c__1);
            z__1.r = -z__1.r, z__1.i = -z__1.i;
            z_div(&t, &z__1, &x[i__2]);
            zaxpy_(&i__1, &t, &x[i__2], &c__1, &x[l+j* *ldx], &c__1);
            if (j < pl || j > pu) {
                continue; /* next j */
            }
            if (qraux[j].r == 0. && qraux[j].i == 0.) {
                continue; /* next j */
            }
            d__1 = z_abs(&x[l+j* *ldx]) / qraux[j].r;
            tt = 1. - d__1 * d__1;
            if (tt < 0.) tt = 0.;
            t.r = tt, t.i = 0.;
            d__1 = qraux[j].r / work[j].r;
            tt = tt * .05 * (d__1 * d__1) + 1.;
            if (tt == 1.) {
                i__1 = *n - l - 1;
                i__2 = l + 1 + j * *ldx;
                work[j].r = qraux[j].r = dznrm2_(&i__1, &x[i__2], &c__1),
                work[j].i = qraux[j].i = 0.;
            }
            else {
                d__1 = sqrt(t.r);
                qraux[j].r *= d__1, qraux[j].i *= d__1;
            }
        }

/*              save the transformation. */

        i__1 = l + l * *ldx;
        qraux[l].r = x[i__1].r, qraux[l].i = x[i__1].i;
        x[i__1].r = -nrmxl.r, x[i__1].i = -nrmxl.i;
    }
} /* zqrdc_ */
