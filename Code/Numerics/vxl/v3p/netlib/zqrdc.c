/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublecomplex c_b28 = {1.,0.};

/* Subroutine */ int zqrdc_(x, ldx, n, p, qraux, jpvt, work, job)
doublecomplex *x;
integer *ldx, *n, *p;
doublecomplex *qraux;
integer *jpvt;
doublecomplex *work;
integer *job;
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void z_div();

    /* Local variables */
    static logical negj;
    static integer maxj, j, l;
    extern doublereal cdabs_();
    static doublecomplex t;
    extern /* Subroutine */ int zscal_();
    extern /* Double Complex */ int zdotc_();
    static logical swapj;
    static doublecomplex nrmxl;
    extern /* Subroutine */ int zswap_(), zaxpy_();
    extern doublereal dznrm2_();
    static integer jj, jp, pl, pu;
    static doublereal tt;
    extern doublereal cdsqrt_();
    static doublereal maxnrm;
    static integer lp1, lup;


/*     zqrdc uses householder transformations to compute the qr */
/*     factorization of an n by p matrix x.  column pivoting */
/*     based on the 2-norms of the reduced columns may be */
/*     performed at the users option. */

/*     on entry */

/*        x       complex*16(ldx,p), where ldx .ge. n. */
/*                x contains the matrix whose decomposition is to be */
/*                computed. */

/*        ldx     integer. */
/*                ldx is the leading dimension of the array x. */

/*        n       integer. */
/*                n is the number of rows of the matrix x. */

/*        p       integer. */
/*                p is the number of columns of the matrix x. */

/*        jpvt    integer(p). */
/*                jpvt contains integers that control the selection */
/*                of the pivot columns.  the k-th column x(k) of x */
/*                is placed in one of three classes according to the */
/*                value of jpvt(k). */

/*                   if jpvt(k) .gt. 0, then x(k) is an initial */
/*                                      column. */

/*                   if jpvt(k) .eq. 0, then x(k) is a free column. */

/*                   if jpvt(k) .lt. 0, then x(k) is a final column. */

/*                before the decomposition is computed, initial columns */
/*                are moved to the beginning of the array x and final */
/*                columns to the end.  both initial and final columns */
/*                are frozen in place during the computation and only */
/*                free columns are moved.  at the k-th stage of the */
/*                reduction, if x(k) is occupied by a free column */
/*                it is interchanged with the free column of largest */
/*                reduced norm.  jpvt is not referenced if */
/*                job .eq. 0. */

/*        work    complex*16(p). */
/*                work is a work array.  work is not referenced if */
/*                job .eq. 0. */

/*        job     integer. */
/*                job is an integer that initiates column pivoting. */
/*                if job .eq. 0, no pivoting is done. */
/*                if job .ne. 0, pivoting is done. */

/*     on return */

/*        x       x contains in its upper triangle the upper */
/*                triangular matrix r of the qr factorization. */
/*                below its diagonal x contains information from */
/*                which the unitary part of the decomposition */
/*                can be recovered.  note that if pivoting has */
/*                been requested, the decomposition is not that */
/*                of the original matrix x but that of x */
/*                with its columns permuted as described by jpvt. */

/*        qraux   complex*16(p). */
/*                qraux contains further information required to recover */
/*                the unitary part of the decomposition. */

/*        jpvt    jpvt(k) contains the index of the column of the */
/*                original matrix that has been interchanged into */
/*                the k-th column, if pivoting was requested. */

/*     linpack. this version dated 08/14/78 . */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     zqrdc uses the following functions and subprograms. */

/*     blas zaxpy,zdotc,zscal,zswap,dznrm2 */
/*     fortran dabs,dmax1,cdabs,dcmplx,cdsqrt,min0 */

/*     internal variables */



    /* Parameter adjustments */
    --work;
    --jpvt;
    --qraux;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    pl = 1;
    pu = 0;
    if (*job == 0) {
        goto L60;
    }

/*        pivoting has been requested.  rearrange the columns */
/*        according to jpvt. */

    i__1 = *p;
    for (j = 1; j <= i__1; ++j) {
        swapj = jpvt[j] > 0;
        negj = jpvt[j] < 0;
        jpvt[j] = j;
        if (negj) {
            jpvt[j] = -j;
        }
        if (! swapj) {
            goto L10;
        }
        if (j != pl) {
            zswap_(n, &x[pl * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
        }
        jpvt[j] = jpvt[pl];
        jpvt[pl] = j;
        ++pl;
L10:
/* L20: */
        ;
    }
    pu = *p;
    i__1 = *p;
    for (jj = 1; jj <= i__1; ++jj) {
        j = *p - jj + 1;
        if (jpvt[j] >= 0) {
            goto L40;
        }
        jpvt[j] = -jpvt[j];
        if (j == pu) {
            goto L30;
        }
        zswap_(n, &x[pu * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
        jp = jpvt[pu];
        jpvt[pu] = jpvt[j];
        jpvt[j] = jp;
L30:
        --pu;
L40:
/* L50: */
        ;
    }
L60:

/*     compute the norms of the free columns. */

    if (pu < pl) {
        goto L80;
    }
    i__1 = pu;
    for (j = pl; j <= i__1; ++j) {
        i__2 = j;
        d__1 = dznrm2_(n, &x[j * x_dim1 + 1], &c__1);
        z__1.r = d__1, z__1.i = 0.;
        qraux[i__2].r = z__1.r, qraux[i__2].i = z__1.i;
        i__2 = j;
        i__3 = j;
        work[i__2].r = qraux[i__3].r, work[i__2].i = qraux[i__3].i;
/* L70: */
    }
L80:

/*     perform the householder reduction of x. */

    lup = min(*n,*p);
    i__1 = lup;
    for (l = 1; l <= i__1; ++l) {
        if (l < pl || l >= pu) {
            goto L120;
        }

/*           locate the column of largest norm and bring it */
/*           into the pivot position. */

        maxnrm = 0.;
        maxj = l;
        i__2 = pu;
        for (j = l; j <= i__2; ++j) {
            i__3 = j;
            if (qraux[i__3].r <= maxnrm) {
                goto L90;
            }
            i__3 = j;
            maxnrm = qraux[i__3].r;
            maxj = j;
L90:
/* L100: */
            ;
        }
        if (maxj == l) {
            goto L110;
        }
        zswap_(n, &x[l * x_dim1 + 1], &c__1, &x[maxj * x_dim1 + 1], &c__1);
        i__2 = maxj;
        i__3 = l;
        qraux[i__2].r = qraux[i__3].r, qraux[i__2].i = qraux[i__3].i;
        i__2 = maxj;
        i__3 = l;
        work[i__2].r = work[i__3].r, work[i__2].i = work[i__3].i;
        jp = jpvt[maxj];
        jpvt[maxj] = jpvt[l];
        jpvt[l] = jp;
L110:
L120:
        i__2 = l;
        qraux[i__2].r = 0., qraux[i__2].i = 0.;
        if (l == *n) {
            goto L190;
        }

/*           compute the householder transformation for column l. */

        i__2 = *n - l + 1;
        d__1 = dznrm2_(&i__2, &x[l + l * x_dim1], &c__1);
        z__1.r = d__1, z__1.i = 0.;
        nrmxl.r = z__1.r, nrmxl.i = z__1.i;
        z__1.r = nrmxl.r * 0. - nrmxl.i * -1., z__1.i = nrmxl.r * -1. +
                nrmxl.i * 0.;
        if ((d__1 = nrmxl.r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
            goto L180;
        }
        i__2 = l + l * x_dim1;
        i__3 = l + l * x_dim1;
        z__1.r = x[i__3].r * 0. - x[i__3].i * -1., z__1.i = x[i__3].r * -1. +
                x[i__3].i * 0.;
        if ((d__1 = x[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.)
                {
            d__3 = cdabs_(&nrmxl);
            i__4 = l + l * x_dim1;
            d__4 = cdabs_(&x[l + l * x_dim1]);
            z__3.r = x[i__4].r / d__4, z__3.i = x[i__4].i / d__4;
            z__2.r = d__3 * z__3.r, z__2.i = d__3 * z__3.i;
            nrmxl.r = z__2.r, nrmxl.i = z__2.i;
        }
        i__2 = *n - l + 1;
        z_div(&z__1, &c_b28, &nrmxl);
        zscal_(&i__2, &z__1, &x[l + l * x_dim1], &c__1);
        i__2 = l + l * x_dim1;
        i__3 = l + l * x_dim1;
        z__1.r = x[i__3].r + 1., z__1.i = x[i__3].i + 0.;
        x[i__2].r = z__1.r, x[i__2].i = z__1.i;

/*              apply the transformation to the remaining columns, */
/*              updating the norms. */

        lp1 = l + 1;
        if (*p < lp1) {
            goto L170;
        }
        i__2 = *p;
        for (j = lp1; j <= i__2; ++j) {
            i__3 = *n - l + 1;
            zdotc_(&z__3, &i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1]
                    , &c__1);
            z__2.r = -z__3.r, z__2.i = -z__3.i;
            z_div(&z__1, &z__2, &x[l + l * x_dim1]);
            t.r = z__1.r, t.i = z__1.i;
            i__3 = *n - l + 1;
            zaxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
                    c__1);
            if (j < pl || j > pu) {
                goto L150;
            }
            i__3 = j;
            i__4 = j;
            z__1.r = qraux[i__4].r * 0. - qraux[i__4].i * -1., z__1.i = qraux[
                    i__4].r * -1. + qraux[i__4].i * 0.;
            if ((d__1 = qraux[i__3].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))
                     == 0.) {
                goto L150;
            }
            i__3 = j;
/* Computing 2nd power */
            d__1 = cdabs_(&x[l + j * x_dim1]) / qraux[i__3].r;
            tt = 1. - d__1 * d__1;
            tt = max(tt,0.);
            z__1.r = tt, z__1.i = 0.;
            t.r = z__1.r, t.i = z__1.i;
            i__3 = j;
            i__4 = j;
/* Computing 2nd power */
            d__1 = qraux[i__3].r / work[i__4].r;
            tt = tt * .05 * (d__1 * d__1) + 1.;
            if (tt == 1.) {
                goto L130;
            }
            i__3 = j;
            i__4 = j;
            d__1 = cdsqrt_(&t);
            z__1.r = d__1 * qraux[i__4].r, z__1.i = d__1 * qraux[i__4].i;
            qraux[i__3].r = z__1.r, qraux[i__3].i = z__1.i;
            goto L140;
L130:
            i__3 = j;
            i__4 = *n - l;
            d__1 = dznrm2_(&i__4, &x[l + 1 + j * x_dim1], &c__1);
            z__1.r = d__1, z__1.i = 0.;
            qraux[i__3].r = z__1.r, qraux[i__3].i = z__1.i;
            i__3 = j;
            i__4 = j;
            work[i__3].r = qraux[i__4].r, work[i__3].i = qraux[i__4].i;
L140:
L150:
/* L160: */
            ;
        }
L170:

/*              save the transformation. */

        i__2 = l;
        i__3 = l + l * x_dim1;
        qraux[i__2].r = x[i__3].r, qraux[i__2].i = x[i__3].i;
        i__2 = l + l * x_dim1;
        z__1.r = -nrmxl.r, z__1.i = -nrmxl.i;
        x[i__2].r = z__1.r, x[i__2].i = z__1.i;
L180:
L190:
/* L200: */
        ;
    }
    return 0;
} /* zqrdc_ */

