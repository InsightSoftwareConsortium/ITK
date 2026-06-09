/* linpack/zqrdc.f -- translated by f2c (version 20050501).
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
static doublecomplex c_b28 = {1.,0.};

/*<       subroutine zqrdc(x,ldx,n,p,qraux,jpvt,work,job) >*/
/* Subroutine */ int zqrdc_(doublecomplex *x, integer *ldx, integer *n,
        integer *p, doublecomplex *qraux, integer *jpvt, doublecomplex *work,
        integer *job)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double z_abs(doublecomplex *);
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), z_sqrt(
            doublecomplex *, doublecomplex *);

    /* Local variables */
    integer j, l;
    doublecomplex t;
    integer jj, jp, pl, pu;
    doublereal tt;
    integer lp1, lup;
    logical negj;
    integer maxj;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *);
    extern /* Double Complex */ VOID zdotc_(doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *);
    logical swapj;
    doublecomplex nrmxl;
    extern /* Subroutine */ int zswap_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), zaxpy_(integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *);
    extern doublereal dznrm2_(integer *, doublecomplex *, integer *);
    doublereal maxnrm;

/*<       integer ldx,n,p,job >*/
/*<       integer jpvt(1) >*/
/*<       complex*16 x(ldx,1),qraux(1),work(1) >*/

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

/*<       integer j,jp,l,lp1,lup,maxj,pl,pu >*/
/*<       double precision maxnrm,dznrm2,tt >*/
/*<       complex*16 zdotc,nrmxl,t >*/
/*<       logical negj,swapj >*/

/*<       complex*16 csign,zdum,zdum1,zdum2 >*/
/*<       double precision cabs1 >*/
/*<       double precision dreal,dimag >*/
/*<       complex*16 zdumr,zdumi >*/
/*<       dreal(zdumr) = zdumr >*/
/*<       dimag(zdumi) = (0.0d0,-1.0d0)*zdumi >*/
/*<       csign(zdum1,zdum2) = cdabs(zdum1)*(zdum2/cdabs(zdum2)) >*/
/*<       cabs1(zdum) = dabs(dreal(zdum)) + dabs(dimag(zdum)) >*/

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
/*<                if (j .ne. pl) call zswap(n,x(1,pl),1,x(1,j),1) >*/
        if (j != pl) {
            zswap_(n, &x[pl * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
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
/*<                   call zswap(n,x(1,pu),1,x(1,j),1) >*/
        zswap_(n, &x[pu * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
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
/*<          qraux(j) = dcmplx(dznrm2(n,x(1,j),1),0.0d0) >*/
        i__2 = j;
        d__1 = dznrm2_(n, &x[j * x_dim1 + 1], &c__1);
        z__1.r = d__1, z__1.i = 0.;
        qraux[i__2].r = z__1.r, qraux[i__2].i = z__1.i;
/*<          work(j) = qraux(j) >*/
        i__2 = j;
        i__3 = j;
        work[i__2].r = qraux[i__3].r, work[i__2].i = qraux[i__3].i;
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
/*<                if (dreal(qraux(j)) .le. maxnrm) go to 90 >*/
            i__3 = j;
            if (qraux[i__3].r <= maxnrm) {
                goto L90;
            }
/*<                   maxnrm = dreal(qraux(j)) >*/
            i__3 = j;
            maxnrm = qraux[i__3].r;
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
/*<                call zswap(n,x(1,l),1,x(1,maxj),1) >*/
        zswap_(n, &x[l * x_dim1 + 1], &c__1, &x[maxj * x_dim1 + 1], &c__1);
/*<                qraux(maxj) = qraux(l) >*/
        i__2 = maxj;
        i__3 = l;
        qraux[i__2].r = qraux[i__3].r, qraux[i__2].i = qraux[i__3].i;
/*<                work(maxj) = work(l) >*/
        i__2 = maxj;
        i__3 = l;
        work[i__2].r = work[i__3].r, work[i__2].i = work[i__3].i;
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
/*<          qraux(l) = (0.0d0,0.0d0) >*/
        i__2 = l;
        qraux[i__2].r = 0., qraux[i__2].i = 0.;
/*<          if (l .eq. n) go to 190 >*/
        if (l == *n) {
            goto L190;
        }

/*           compute the householder transformation for column l. */

/*<             nrmxl = dcmplx(dznrm2(n-l+1,x(l,l),1),0.0d0) >*/
        i__2 = *n - l + 1;
        d__1 = dznrm2_(&i__2, &x[l + l * x_dim1], &c__1);
        z__1.r = d__1, z__1.i = 0.;
        nrmxl.r = z__1.r, nrmxl.i = z__1.i;
/*<             if (cabs1(nrmxl) .eq. 0.0d0) go to 180 >*/
        z__1.r = nrmxl.r * 0. - nrmxl.i * -1., z__1.i = nrmxl.i * 0. +
                nrmxl.r * -1.;
        if ((d__1 = nrmxl.r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
            goto L180;
        }
/*<    >*/
        i__2 = l + l * x_dim1;
        i__3 = l + l * x_dim1;
        z__1.r = x[i__3].r * 0. - x[i__3].i * -1., z__1.i = x[i__3].i * 0. +
                x[i__3].r * -1.;
        if ((d__1 = x[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.)
                {
            d__3 = z_abs(&nrmxl);
            i__4 = l + l * x_dim1;
            d__4 = z_abs(&x[l + l * x_dim1]);
            z__3.r = x[i__4].r / d__4, z__3.i = x[i__4].i / d__4;
            z__2.r = d__3 * z__3.r, z__2.i = d__3 * z__3.i;
            nrmxl.r = z__2.r, nrmxl.i = z__2.i;
        }
/*<                call zscal(n-l+1,(1.0d0,0.0d0)/nrmxl,x(l,l),1) >*/
        i__2 = *n - l + 1;
        z_div(&z__1, &c_b28, &nrmxl);
        zscal_(&i__2, &z__1, &x[l + l * x_dim1], &c__1);
/*<                x(l,l) = (1.0d0,0.0d0) + x(l,l) >*/
        i__2 = l + l * x_dim1;
        i__3 = l + l * x_dim1;
        z__1.r = x[i__3].r + 1., z__1.i = x[i__3].i + 0.;
        x[i__2].r = z__1.r, x[i__2].i = z__1.i;

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
/*<                   t = -zdotc(n-l+1,x(l,l),1,x(l,j),1)/x(l,l) >*/
            i__3 = *n - l + 1;
            zdotc_(&z__3, &i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1]
                    , &c__1);
            z__2.r = -z__3.r, z__2.i = -z__3.i;
            z_div(&z__1, &z__2, &x[l + l * x_dim1]);
            t.r = z__1.r, t.i = z__1.i;
/*<                   call zaxpy(n-l+1,t,x(l,l),1,x(l,j),1) >*/
            i__3 = *n - l + 1;
            zaxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
                    c__1);
/*<                   if (j .lt. pl .or. j .gt. pu) go to 150 >*/
            if (j < pl || j > pu) {
                goto L150;
            }
/*<                   if (cabs1(qraux(j)) .eq. 0.0d0) go to 150 >*/
            i__3 = j;
            i__4 = j;
            z__1.r = qraux[i__4].r * 0. - qraux[i__4].i * -1., z__1.i = qraux[
                    i__4].i * 0. + qraux[i__4].r * -1.;
            if ((d__1 = qraux[i__3].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))
                     == 0.) {
                goto L150;
            }
/*<                      tt = 1.0d0 - (cdabs(x(l,j))/dreal(qraux(j)))**2 >*/
            i__3 = j;
/* Computing 2nd power */
            d__1 = z_abs(&x[l + j * x_dim1]) / qraux[i__3].r;
            tt = 1. - d__1 * d__1;
/*<                      tt = dmax1(tt,0.0d0) >*/
            tt = max(tt,0.);
/*<                      t = dcmplx(tt,0.0d0) >*/
            z__1.r = tt, z__1.i = 0.;
            t.r = z__1.r, t.i = z__1.i;
/*<    >*/
            i__3 = j;
            i__4 = j;
/* Computing 2nd power */
            d__1 = qraux[i__3].r / work[i__4].r;
            tt = tt * .05 * (d__1 * d__1) + 1.;
/*<                      if (tt .eq. 1.0d0) go to 130 >*/
            if (tt == 1.) {
                goto L130;
            }
/*<                         qraux(j) = qraux(j)*cdsqrt(t) >*/
            i__3 = j;
            i__4 = j;
            z_sqrt(&z__2, &t);
            z__1.r = qraux[i__4].r * z__2.r - qraux[i__4].i * z__2.i, z__1.i =
                     qraux[i__4].r * z__2.i + qraux[i__4].i * z__2.r;
            qraux[i__3].r = z__1.r, qraux[i__3].i = z__1.i;
/*<                      go to 140 >*/
            goto L140;
/*<   130                continue >*/
L130:
/*<                         qraux(j) = dcmplx(dznrm2(n-l,x(l+1,j),1),0.0d0) >*/
            i__3 = j;
            i__4 = *n - l;
            d__1 = dznrm2_(&i__4, &x[l + 1 + j * x_dim1], &c__1);
            z__1.r = d__1, z__1.i = 0.;
            qraux[i__3].r = z__1.r, qraux[i__3].i = z__1.i;
/*<                         work(j) = qraux(j) >*/
            i__3 = j;
            i__4 = j;
            work[i__3].r = qraux[i__4].r, work[i__3].i = qraux[i__4].i;
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
        i__2 = l;
        i__3 = l + l * x_dim1;
        qraux[i__2].r = x[i__3].r, qraux[i__2].i = x[i__3].i;
/*<                x(l,l) = -nrmxl >*/
        i__2 = l + l * x_dim1;
        z__1.r = -nrmxl.r, z__1.i = -nrmxl.i;
        x[i__2].r = z__1.r, x[i__2].i = z__1.i;
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
} /* zqrdc_ */

#ifdef __cplusplus
        }
#endif
