/* linpack/cqrdc.f -- translated by f2c (version 20050501).
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
static complex c_b26 = {(float)1.,(float)0.};

/*<       subroutine cqrdc(x,ldx,n,p,qraux,jpvt,work,job) >*/
/* Subroutine */ int cqrdc_(complex *x, integer *ldx, integer *n, integer *p,
        complex *qraux, integer *jpvt, complex *work, integer *job)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3, r__4;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    double r_imag(complex *), c_abs(complex *);
    void c_div(complex *, complex *, complex *), c_sqrt(complex *, complex *);

    /* Local variables */
    integer j, l;
    complex t;
    integer jj, jp, pl, pu;
    real tt;
    integer lp1, lup;
    logical negj;
    integer maxj;
    extern /* Subroutine */ int cscal_(integer *, complex *, complex *,
            integer *);
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer
            *, complex *, integer *);
    extern /* Subroutine */ int cswap_(integer *, complex *, integer *,
            complex *, integer *);
    logical swapj;
    extern /* Subroutine */ int caxpy_(integer *, complex *, complex *,
            integer *, complex *, integer *);
    complex nrmxl;
    extern doublereal scnrm2_(integer *, complex *, integer *);
    real maxnrm;

/*<       integer ldx,n,p,job >*/
/*<       integer jpvt(1) >*/
/*<       complex x(ldx,1),qraux(1),work(1) >*/

/*     cqrdc uses householder transformations to compute the qr */
/*     factorization of an n by p matrix x.  column pivoting */
/*     based on the 2-norms of the reduced columns may be */
/*     performed at the users option. */

/*     on entry */

/*        x       complex(ldx,p), where ldx .ge. n. */
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

/*        work    complex(p). */
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

/*        qraux   complex(p). */
/*                qraux contains further information required to recover */
/*                the unitary part of the decomposition. */

/*        jpvt    jpvt(k) contains the index of the column of the */
/*                original matrix that has been interchanged into */
/*                the k-th column, if pivoting was requested. */

/*     linpack. this version dated 08/14/78 . */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     cqrdc uses the following functions and subprograms. */

/*     blas caxpy,cdotc,cscal,cswap,scnrm2 */
/*     fortran abs,aimag,amax1,cabs,cmplx,csqrt,min0,real */

/*     internal variables */

/*<       integer j,jp,l,lp1,lup,maxj,pl,pu >*/
/*<       real maxnrm,scnrm2,tt >*/
/*<       complex cdotc,nrmxl,t >*/
/*<       logical negj,swapj >*/

/*<       complex csign,zdum,zdum1,zdum2 >*/
/*<       real cabs1 >*/
/*<       csign(zdum1,zdum2) = cabs(zdum1)*(zdum2/cabs(zdum2)) >*/
/*<       cabs1(zdum) = abs(real(zdum)) + abs(aimag(zdum)) >*/

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
/*<                if (j .ne. pl) call cswap(n,x(1,pl),1,x(1,j),1) >*/
        if (j != pl) {
            cswap_(n, &x[pl * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
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
/*<                   call cswap(n,x(1,pu),1,x(1,j),1) >*/
        cswap_(n, &x[pu * x_dim1 + 1], &c__1, &x[j * x_dim1 + 1], &c__1);
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
/*<          qraux(j) = cmplx(scnrm2(n,x(1,j),1),0.0e0) >*/
        i__2 = j;
        r__1 = scnrm2_(n, &x[j * x_dim1 + 1], &c__1);
        q__1.r = r__1, q__1.i = (float)0.;
        qraux[i__2].r = q__1.r, qraux[i__2].i = q__1.i;
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

/*<             maxnrm = 0.0e0 >*/
        maxnrm = (float)0.;
/*<             maxj = l >*/
        maxj = l;
/*<             do 100 j = l, pu >*/
        i__2 = pu;
        for (j = l; j <= i__2; ++j) {
/*<                if (real(qraux(j)) .le. maxnrm) go to 90 >*/
            i__3 = j;
            if (qraux[i__3].r <= maxnrm) {
                goto L90;
            }
/*<                   maxnrm = real(qraux(j)) >*/
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
/*<                call cswap(n,x(1,l),1,x(1,maxj),1) >*/
        cswap_(n, &x[l * x_dim1 + 1], &c__1, &x[maxj * x_dim1 + 1], &c__1);
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
/*<          qraux(l) = (0.0e0,0.0e0) >*/
        i__2 = l;
        qraux[i__2].r = (float)0., qraux[i__2].i = (float)0.;
/*<          if (l .eq. n) go to 190 >*/
        if (l == *n) {
            goto L190;
        }

/*           compute the householder transformation for column l. */

/*<             nrmxl = cmplx(scnrm2(n-l+1,x(l,l),1),0.0e0) >*/
        i__2 = *n - l + 1;
        r__1 = scnrm2_(&i__2, &x[l + l * x_dim1], &c__1);
        q__1.r = r__1, q__1.i = (float)0.;
        nrmxl.r = q__1.r, nrmxl.i = q__1.i;
/*<             if (cabs1(nrmxl) .eq. 0.0e0) go to 180 >*/
        if ((r__1 = nrmxl.r, dabs(r__1)) + (r__2 = r_imag(&nrmxl), dabs(r__2))
                 == (float)0.) {
            goto L180;
        }
/*<    >*/
        i__2 = l + l * x_dim1;
        if ((r__1 = x[i__2].r, dabs(r__1)) + (r__2 = r_imag(&x[l + l * x_dim1]
                ), dabs(r__2)) != (float)0.) {
            r__3 = c_abs(&nrmxl);
            i__3 = l + l * x_dim1;
            r__4 = c_abs(&x[l + l * x_dim1]);
            q__2.r = x[i__3].r / r__4, q__2.i = x[i__3].i / r__4;
            q__1.r = r__3 * q__2.r, q__1.i = r__3 * q__2.i;
            nrmxl.r = q__1.r, nrmxl.i = q__1.i;
        }
/*<                call cscal(n-l+1,(1.0e0,0.0e0)/nrmxl,x(l,l),1) >*/
        i__2 = *n - l + 1;
        c_div(&q__1, &c_b26, &nrmxl);
        cscal_(&i__2, &q__1, &x[l + l * x_dim1], &c__1);
/*<                x(l,l) = (1.0e0,0.0e0) + x(l,l) >*/
        i__2 = l + l * x_dim1;
        i__3 = l + l * x_dim1;
        q__1.r = x[i__3].r + (float)1., q__1.i = x[i__3].i + (float)0.;
        x[i__2].r = q__1.r, x[i__2].i = q__1.i;

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
/*<                   t = -cdotc(n-l+1,x(l,l),1,x(l,j),1)/x(l,l) >*/
            i__3 = *n - l + 1;
            cdotc_(&q__3, &i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1]
                    , &c__1);
            q__2.r = -q__3.r, q__2.i = -q__3.i;
            c_div(&q__1, &q__2, &x[l + l * x_dim1]);
            t.r = q__1.r, t.i = q__1.i;
/*<                   call caxpy(n-l+1,t,x(l,l),1,x(l,j),1) >*/
            i__3 = *n - l + 1;
            caxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
                    c__1);
/*<                   if (j .lt. pl .or. j .gt. pu) go to 150 >*/
            if (j < pl || j > pu) {
                goto L150;
            }
/*<                   if (cabs1(qraux(j)) .eq. 0.0e0) go to 150 >*/
            i__3 = j;
            if ((r__1 = qraux[i__3].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j])
                    , dabs(r__2)) == (float)0.) {
                goto L150;
            }
/*<                      tt = 1.0e0 - (cabs(x(l,j))/real(qraux(j)))**2 >*/
            i__3 = j;
/* Computing 2nd power */
            r__1 = c_abs(&x[l + j * x_dim1]) / qraux[i__3].r;
            tt = (float)1. - r__1 * r__1;
/*<                      tt = amax1(tt,0.0e0) >*/
            tt = dmax(tt,(float)0.);
/*<                      t = cmplx(tt,0.0e0) >*/
            q__1.r = tt, q__1.i = (float)0.;
            t.r = q__1.r, t.i = q__1.i;
/*<    >*/
            i__3 = j;
            i__4 = j;
/* Computing 2nd power */
            r__1 = qraux[i__3].r / work[i__4].r;
            tt = tt * (float).05 * (r__1 * r__1) + (float)1.;
/*<                      if (tt .eq. 1.0e0) go to 130 >*/
            if (tt == (float)1.) {
                goto L130;
            }
/*<                         qraux(j) = qraux(j)*csqrt(t) >*/
            i__3 = j;
            i__4 = j;
            c_sqrt(&q__2, &t);
            q__1.r = qraux[i__4].r * q__2.r - qraux[i__4].i * q__2.i, q__1.i =
                     qraux[i__4].r * q__2.i + qraux[i__4].i * q__2.r;
            qraux[i__3].r = q__1.r, qraux[i__3].i = q__1.i;
/*<                      go to 140 >*/
            goto L140;
/*<   130                continue >*/
L130:
/*<                         qraux(j) = cmplx(scnrm2(n-l,x(l+1,j),1),0.0e0) >*/
            i__3 = j;
            i__4 = *n - l;
            r__1 = scnrm2_(&i__4, &x[l + 1 + j * x_dim1], &c__1);
            q__1.r = r__1, q__1.i = (float)0.;
            qraux[i__3].r = q__1.r, qraux[i__3].i = q__1.i;
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
        q__1.r = -nrmxl.r, q__1.i = -nrmxl.i;
        x[i__2].r = q__1.r, x[i__2].i = q__1.i;
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
} /* cqrdc_ */

#ifdef __cplusplus
        }
#endif
