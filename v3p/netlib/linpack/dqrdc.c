/* linpack/dqrdc.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static integer c__1 = 1;

/*<       subroutine dqrdc(x,ldx,n,p,qraux,jpvt,work,job) >*/
/* Subroutine */ int dqrdc_(doublereal *x, integer *ldx, integer *n, integer *
        p, doublereal *qraux, integer *jpvt, doublereal *work, integer *job)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    integer j, l;
    doublereal t;
    integer jj, jp, pl, pu;
    doublereal tt;
    integer lp1, lup;
    logical negj;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    integer maxj;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dswap_(integer *, doublereal *, integer *, doublereal
            *, integer *);
    logical swapj;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    doublereal nrmxl, maxnrm;

/*<       integer ldx,n,p,job >*/
/*<       integer jpvt(1) >*/
/*<       double precision x(ldx,1),qraux(1),work(1) >*/

/*     dqrdc uses householder transformations to compute the qr */
/*     factorization of an n by p matrix x.  column pivoting */
/*     based on the 2-norms of the reduced columns may be */
/*     performed at the users option. */

/*     on entry */

/*        x       double precision(ldx,p), where ldx .ge. n. */
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

/*        work    double precision(p). */
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
/*                which the orthogonal part of the decomposition */
/*                can be recovered.  note that if pivoting has */
/*                been requested, the decomposition is not that */
/*                of the original matrix x but that of x */
/*                with its columns permuted as described by jpvt. */

/*        qraux   double precision(p). */
/*                qraux contains further information required to recover */
/*                the orthogonal part of the decomposition. */

/*        jpvt    jpvt(k) contains the index of the column of the */
/*                original matrix that has been interchanged into */
/*                the k-th column, if pivoting was requested. */

/*     linpack. this version dated 08/14/78 . */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     dqrdc uses the following functions and subprograms. */

/*     blas daxpy,ddot,dscal,dswap,dnrm2 */
/*     fortran dabs,dmax1,min0,dsqrt */

/*     internal variables */

/*<       integer j,jp,l,lp1,lup,maxj,pl,pu >*/
/*<       double precision maxnrm,dnrm2,tt >*/
/*<       double precision ddot,nrmxl,t >*/
/*<       logical negj,swapj >*/


/*<       pl = 1 >*/
    /* Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --qraux;
    --jpvt;
    --work;

    /* Function Body */
    pl = 1;
/*<       pu = 0 >*/
    pu = 0;
/*<       if (job .eq. 0) go to 60 >*/
    if (*job == 0) {
        goto L60;
    }

/*        pivoting has been requested.  rearrange the columns */
/*        according to jpvt. */

/*<          do 20 j = 1, p >*/
    i__1 = *p;
    for (j = 1; j <= i__1; ++j) {
/*<             swapj = jpvt(j) .gt. 0 >*/
        swapj = jpvt[j] > 0;
/*<             negj = jpvt(j) .lt. 0 >*/
        negj = jpvt[j] < 0;
/*<             jpvt(j) = j >*/
        jpvt[j] = j;
/*<             if (negj) jpvt(j) = -j >*/
        if (negj) {
            jpvt[j] = -j;
        }
/*<             if (.not.swapj) go to 10 >*/
        if (! swapj) {
            goto L10;
        }
/*<                if (j .ne. pl) call dswap(n,x(1,pl),1,x(1,j),1) >*/
        if (j != pl) {
            dswap_(n, &x[pl * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
        }
/*<                jpvt(j) = jpvt(pl) >*/
        jpvt[j] = jpvt[pl];
/*<                jpvt(pl) = j >*/
        jpvt[pl] = j;
/*<                pl = pl + 1 >*/
        ++pl;
/*<    10       continue >*/
L10:
/*<    20    continue >*/
/* L20: */
        ;
    }
/*<          pu = p >*/
    pu = *p;
/*<          do 50 jj = 1, p >*/
    i__1 = *p;
    for (jj = 1; jj <= i__1; ++jj) {
/*<             j = p - jj + 1 >*/
        j = *p - jj + 1;
/*<             if (jpvt(j) .ge. 0) go to 40 >*/
        if (jpvt[j] >= 0) {
            goto L40;
        }
/*<                jpvt(j) = -jpvt(j) >*/
        jpvt[j] = -jpvt[j];
/*<                if (j .eq. pu) go to 30 >*/
        if (j == pu) {
            goto L30;
        }
/*<                   call dswap(n,x(1,pu),1,x(1,j),1) >*/
        dswap_(n, &x[pu * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
/*<                   jp = jpvt(pu) >*/
        jp = jpvt[pu];
/*<                   jpvt(pu) = jpvt(j) >*/
        jpvt[pu] = jpvt[j];
/*<                   jpvt(j) = jp >*/
        jpvt[j] = jp;
/*<    30          continue >*/
L30:
/*<                pu = pu - 1 >*/
        --pu;
/*<    40       continue >*/
L40:
/*<    50    continue >*/
/* L50: */
        ;
    }
/*<    60 continue >*/
L60:

/*     compute the norms of the free columns. */

/*<       if (pu .lt. pl) go to 80 >*/
    if (pu < pl) {
        goto L80;
    }
/*<       do 70 j = pl, pu >*/
    i__1 = pu;
    for (j = pl; j <= i__1; ++j) {
/*<          qraux(j) = dnrm2(n,x(1,j),1) >*/
        qraux[j] = dnrm2_(n, &x[j * x_dim1 + 1], &c__1);
/*<          work(j) = qraux(j) >*/
        work[j] = qraux[j];
/*<    70 continue >*/
/* L70: */
    }
/*<    80 continue >*/
L80:

/*     perform the householder reduction of x. */

/*<       lup = min0(n,p) >*/
    lup = min(*n,*p);
/*<       do 200 l = 1, lup >*/
    i__1 = lup;
    for (l = 1; l <= i__1; ++l) {
/*<          if (l .lt. pl .or. l .ge. pu) go to 120 >*/
        if (l < pl || l >= pu) {
            goto L120;
        }

/*           locate the column of largest norm and bring it */
/*           into the pivot position. */

/*<             maxnrm = 0.0d0 >*/
        maxnrm = 0.;
/*<             maxj = l >*/
        maxj = l;
/*<             do 100 j = l, pu >*/
        i__2 = pu;
        for (j = l; j <= i__2; ++j) {
/*<                if (qraux(j) .le. maxnrm) go to 90 >*/
            if (qraux[j] <= maxnrm) {
                goto L90;
            }
/*<                   maxnrm = qraux(j) >*/
            maxnrm = qraux[j];
/*<                   maxj = j >*/
            maxj = j;
/*<    90          continue >*/
L90:
/*<   100       continue >*/
/* L100: */
            ;
        }
/*<             if (maxj .eq. l) go to 110 >*/
        if (maxj == l) {
            goto L110;
        }
/*<                call dswap(n,x(1,l),1,x(1,maxj),1) >*/
        dswap_(n, &x[l * x_dim1 + 1], &c__1, &x[maxj * x_dim1 + 1], &c__1);
/*<                qraux(maxj) = qraux(l) >*/
        qraux[maxj] = qraux[l];
/*<                work(maxj) = work(l) >*/
        work[maxj] = work[l];
/*<                jp = jpvt(maxj) >*/
        jp = jpvt[maxj];
/*<                jpvt(maxj) = jpvt(l) >*/
        jpvt[maxj] = jpvt[l];
/*<                jpvt(l) = jp >*/
        jpvt[l] = jp;
/*<   110       continue >*/
L110:
/*<   120    continue >*/
L120:
/*<          qraux(l) = 0.0d0 >*/
        qraux[l] = 0.;
/*<          if (l .eq. n) go to 190 >*/
        if (l == *n) {
            goto L190;
        }

/*           compute the householder transformation for column l. */

/*<             nrmxl = dnrm2(n-l+1,x(l,l),1) >*/
        i__2 = *n - l + 1;
        nrmxl = dnrm2_(&i__2, &x[l + l * x_dim1], &c__1);
/*<             if (nrmxl .eq. 0.0d0) go to 180 >*/
        if (nrmxl == 0.) {
            goto L180;
        }
/*<                if (x(l,l) .ne. 0.0d0) nrmxl = dsign(nrmxl,x(l,l)) >*/
        if (x[l + l * x_dim1] != 0.) {
            nrmxl = d_sign(&nrmxl, &x[l + l * x_dim1]);
        }
/*<                call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1) >*/
        i__2 = *n - l + 1;
        d__1 = 1. / nrmxl;
        dscal_(&i__2, &d__1, &x[l + l * x_dim1], &c__1);
/*<                x(l,l) = 1.0d0 + x(l,l) >*/
        x[l + l * x_dim1] += 1.;

/*              apply the transformation to the remaining columns, */
/*              updating the norms. */

/*<                lp1 = l + 1 >*/
        lp1 = l + 1;
/*<                if (p .lt. lp1) go to 170 >*/
        if (*p < lp1) {
            goto L170;
        }
/*<                do 160 j = lp1, p >*/
        i__2 = *p;
        for (j = lp1; j <= i__2; ++j) {
/*<                   t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l) >*/
            i__3 = *n - l + 1;
            t = -ddot_(&i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
                    c__1) / x[l + l * x_dim1];
/*<                   call daxpy(n-l+1,t,x(l,l),1,x(l,j),1) >*/
            i__3 = *n - l + 1;
            daxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
                    c__1);
/*<                   if (j .lt. pl .or. j .gt. pu) go to 150 >*/
            if (j < pl || j > pu) {
                goto L150;
            }
/*<                   if (qraux(j) .eq. 0.0d0) go to 150 >*/
            if (qraux[j] == 0.) {
                goto L150;
            }
/*<                      tt = 1.0d0 - (dabs(x(l,j))/qraux(j))**2 >*/
/* Computing 2nd power */
            d__2 = (d__1 = x[l + j * x_dim1], abs(d__1)) / qraux[j];
            tt = 1. - d__2 * d__2;
/*<                      tt = dmax1(tt,0.0d0) >*/
            tt = max(tt,0.);
/*<                      t = tt >*/
            t = tt;
/*<                      tt = 1.0d0 + 0.05d0*tt*(qraux(j)/work(j))**2 >*/
/* Computing 2nd power */
            d__1 = qraux[j] / work[j];
            tt = tt * .05 * (d__1 * d__1) + 1.;
/*<                      if (tt .eq. 1.0d0) go to 130 >*/
            if (tt == 1.) {
                goto L130;
            }
/*<                         qraux(j) = qraux(j)*dsqrt(t) >*/
            qraux[j] *= sqrt(t);
/*<                      go to 140 >*/
            goto L140;
/*<   130                continue >*/
L130:
/*<                         qraux(j) = dnrm2(n-l,x(l+1,j),1) >*/
            i__3 = *n - l;
            qraux[j] = dnrm2_(&i__3, &x[l + 1 + j * x_dim1], &c__1);
/*<                         work(j) = qraux(j) >*/
            work[j] = qraux[j];
/*<   140                continue >*/
L140:
/*<   150             continue >*/
L150:
/*<   160          continue >*/
/* L160: */
            ;
        }
/*<   170          continue >*/
L170:

/*              save the transformation. */

/*<                qraux(l) = x(l,l) >*/
        qraux[l] = x[l + l * x_dim1];
/*<                x(l,l) = -nrmxl >*/
        x[l + l * x_dim1] = -nrmxl;
/*<   180       continue >*/
L180:
/*<   190    continue >*/
L190:
/*<   200 continue >*/
/* L200: */
        ;
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* dqrdc_ */

#ifdef __cplusplus
        }
#endif
