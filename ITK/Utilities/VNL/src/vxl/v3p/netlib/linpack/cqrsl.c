/* linpack/cqrsl.f -- translated by f2c (version 20050501).
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

/*<       subroutine cqrsl(x,ldx,n,k,qraux,y,qy,qty,b,rsd,xb,job,info) >*/
/* Subroutine */ int cqrsl_(complex *x, integer *ldx, integer *n, integer *k,
        complex *qraux, complex *y, complex *qy, complex *qty, complex *b,
        complex *rsd, complex *xb, integer *job, integer *info)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    real r__1, r__2;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    double r_imag(complex *);
    void c_div(complex *, complex *, complex *);

    /* Local variables */
    integer i__, j;
    complex t;
    logical cb;
    integer jj;
    logical cr;
    integer ju, kp1;
    logical cxb, cqy;
    complex temp;
    logical cqty;
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer
            *, complex *, integer *);
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *,
            complex *, integer *), caxpy_(integer *, complex *, complex *,
            integer *, complex *, integer *);

/*<       integer ldx,n,k,job,info >*/
/*<       complex x(ldx,1),qraux(1),y(1),qy(1),qty(1),b(1),rsd(1),xb(1) >*/

/*     cqrsl applies the output of cqrdc to compute coordinate */
/*     transformations, projections, and least squares solutions. */
/*     for k .le. min(n,p), let xk be the matrix */

/*            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k))) */

/*     formed from columns jpvt(1), ... ,jpvt(k) of the original */
/*     n x p matrix x that was input to cqrdc (if no pivoting was */
/*     done, xk consists of the first k columns of x in their */
/*     original order).  cqrdc produces a factored unitary matrix q */
/*     and an upper triangular matrix r such that */

/*              xk = q * (r) */
/*                       (0) */

/*     this information is contained in coded form in the arrays */
/*     x and qraux. */

/*     on entry */

/*        x      complex(ldx,p). */
/*               x contains the output of cqrdc. */

/*        ldx    integer. */
/*               ldx is the leading dimension of the array x. */

/*        n      integer. */
/*               n is the number of rows of the matrix xk.  it must */
/*               have the same value as n in cqrdc. */

/*        k      integer. */
/*               k is the number of columns of the matrix xk.  k */
/*               must nnot be greater than min(n,p), where p is the */
/*               same as in the calling sequence to cqrdc. */

/*        qraux  complex(p). */
/*               qraux contains the auxiliary output from cqrdc. */

/*        y      complex(n) */
/*               y contains an n-vector that is to be manipulated */
/*               by cqrsl. */

/*        job    integer. */
/*               job specifies what is to be computed.  job has */
/*               the decimal expansion abcde, with the following */
/*               meaning. */

/*                    if a.ne.0, compute qy. */
/*                    if b,c,d, or e .ne. 0, compute qty. */
/*                    if c.ne.0, compute b. */
/*                    if d.ne.0, compute rsd. */
/*                    if e.ne.0, compute xb. */

/*               note that a request to compute b, rsd, or xb */
/*               automatically triggers the computation of qty, for */
/*               which an array must be provided in the calling */
/*               sequence. */

/*     on return */

/*        qy     complex(n). */
/*               qy conntains q*y, if its computation has been */
/*               requested. */

/*        qty    complex(n). */
/*               qty contains ctrans(q)*y, if its computation has */
/*               been requested.  here ctrans(q) is the conjugate */
/*               transpose of the matrix q. */

/*        b      complex(k) */
/*               b contains the solution of the least squares problem */

/*                    minimize norm2(y - xk*b), */

/*               if its computation has been requested.  (note that */
/*               if pivoting was requested in cqrdc, the j-th */
/*               component of b will be associated with column jpvt(j) */
/*               of the original matrix x that was input into cqrdc.) */

/*        rsd    complex(n). */
/*               rsd contains the least squares residual y - xk*b, */
/*               if its computation has been requested.  rsd is */
/*               also the orthogonal projection of y onto the */
/*               orthogonal complement of the column space of xk. */

/*        xb     complex(n). */
/*               xb contains the least squares approximation xk*b, */
/*               if its computation has been requested.  xb is also */
/*               the orthogonal projection of y onto the column space */
/*               of x. */

/*        info   integer. */
/*               info is zero unless the computation of b has */
/*               been requested and r is exactly singular.  in */
/*               this case, info is the index of the first zero */
/*               diagonal element of r and b is left unaltered. */

/*     the parameters qy, qty, b, rsd, and xb are not referenced */
/*     if their computation is not requested and in this case */
/*     can be replaced by dummy variables in the calling program. */
/*     to save storage, the user may in some cases use the same */
/*     array for different parameters in the calling sequence.  a */
/*     frequently occurring example is when one wishes to compute */
/*     any of b, rsd, or xb and does not need y or qty.  in this */
/*     case one may identify y, qty, and one of b, rsd, or xb, while */
/*     providing separate arrays for anything else that is to be */
/*     computed.  thus the calling sequence */

/*          call cqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info) */

/*     will result in the computation of b and rsd, with rsd */
/*     overwriting y.  more generally, each item in the following */
/*     list contains groups of permissible identifications for */
/*     a single callinng sequence. */

/*          1. (y,qty,b) (rsd) (xb) (qy) */

/*          2. (y,qty,rsd) (b) (xb) (qy) */

/*          3. (y,qty,xb) (b) (rsd) (qy) */

/*          4. (y,qy) (qty,b) (rsd) (xb) */

/*          5. (y,qy) (qty,rsd) (b) (xb) */

/*          6. (y,qy) (qty,xb) (b) (rsd) */

/*     in any group the value returned in the array allocated to */
/*     the group corresponds to the last member of the group. */

/*     linpack. this version dated 08/14/78 . */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     cqrsl uses the following functions and subprograms. */

/*     blas caxpy,ccopy,cdotc */
/*     fortran abs,aimag,min0,mod,real */

/*     internal variables */

/*<       integer i,j,jj,ju,kp1 >*/
/*<       complex cdotc,t,temp >*/
/*<       logical cb,cqy,cqty,cr,cxb >*/

/*<       complex zdum >*/
/*<       real cabs1 >*/
/*<       cabs1(zdum) = abs(real(zdum)) + abs(aimag(zdum)) >*/

/*     set info flag. */

/*<       info = 0 >*/
    /* Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --qraux;
    --y;
    --qy;
    --qty;
    --b;
    --rsd;
    --xb;

    /* Function Body */
    *info = 0;

/*     determine what is to be computed. */

/*<       cqy = job/10000 .ne. 0 >*/
    cqy = *job / 10000 != 0;
/*<       cqty = mod(job,10000) .ne. 0 >*/
    cqty = *job % 10000 != 0;
/*<       cb = mod(job,1000)/100 .ne. 0 >*/
    cb = *job % 1000 / 100 != 0;
/*<       cr = mod(job,100)/10 .ne. 0 >*/
    cr = *job % 100 / 10 != 0;
/*<       cxb = mod(job,10) .ne. 0 >*/
    cxb = *job % 10 != 0;
/*<       ju = min0(k,n-1) >*/
/* Computing MIN */
    i__1 = *k, i__2 = *n - 1;
    ju = min(i__1,i__2);

/*     special action when n=1. */

/*<       if (ju .ne. 0) go to 40 >*/
    if (ju != 0) {
        goto L40;
    }
/*<          if (cqy) qy(1) = y(1) >*/
    if (cqy) {
        qy[1].r = y[1].r, qy[1].i = y[1].i;
    }
/*<          if (cqty) qty(1) = y(1) >*/
    if (cqty) {
        qty[1].r = y[1].r, qty[1].i = y[1].i;
    }
/*<          if (cxb) xb(1) = y(1) >*/
    if (cxb) {
        xb[1].r = y[1].r, xb[1].i = y[1].i;
    }
/*<          if (.not.cb) go to 30 >*/
    if (! cb) {
        goto L30;
    }
/*<             if (cabs1(x(1,1)) .ne. 0.0e0) go to 10 >*/
    i__1 = x_dim1 + 1;
    if ((r__1 = x[i__1].r, dabs(r__1)) + (r__2 = r_imag(&x[x_dim1 + 1]), dabs(
            r__2)) != (float)0.) {
        goto L10;
    }
/*<                info = 1 >*/
    *info = 1;
/*<             go to 20 >*/
    goto L20;
/*<    10       continue >*/
L10:
/*<                b(1) = y(1)/x(1,1) >*/
    c_div(&q__1, &y[1], &x[x_dim1 + 1]);
    b[1].r = q__1.r, b[1].i = q__1.i;
/*<    20       continue >*/
L20:
/*<    30    continue >*/
L30:
/*<          if (cr) rsd(1) = (0.0e0,0.0e0) >*/
    if (cr) {
        rsd[1].r = (float)0., rsd[1].i = (float)0.;
    }
/*<       go to 250 >*/
    goto L250;
/*<    40 continue >*/
L40:

/*        set up to compute qy or qty. */

/*<          if (cqy) call ccopy(n,y,1,qy,1) >*/
    if (cqy) {
        ccopy_(n, &y[1], &c__1, &qy[1], &c__1);
    }
/*<          if (cqty) call ccopy(n,y,1,qty,1) >*/
    if (cqty) {
        ccopy_(n, &y[1], &c__1, &qty[1], &c__1);
    }
/*<          if (.not.cqy) go to 70 >*/
    if (! cqy) {
        goto L70;
    }

/*           compute qy. */

/*<             do 60 jj = 1, ju >*/
    i__1 = ju;
    for (jj = 1; jj <= i__1; ++jj) {
/*<                j = ju - jj + 1 >*/
        j = ju - jj + 1;
/*<                if (cabs1(qraux(j)) .eq. 0.0e0) go to 50 >*/
        i__2 = j;
        if ((r__1 = qraux[i__2].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j]),
                dabs(r__2)) == (float)0.) {
            goto L50;
        }
/*<                   temp = x(j,j) >*/
        i__2 = j + j * x_dim1;
        temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                   x(j,j) = qraux(j) >*/
        i__2 = j + j * x_dim1;
        i__3 = j;
        x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
/*<                   t = -cdotc(n-j+1,x(j,j),1,qy(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &qy[j], &c__1);
        q__2.r = -q__3.r, q__2.i = -q__3.i;
        c_div(&q__1, &q__2, &x[j + j * x_dim1]);
        t.r = q__1.r, t.i = q__1.i;
/*<                   call caxpy(n-j+1,t,x(j,j),1,qy(j),1) >*/
        i__2 = *n - j + 1;
        caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &qy[j], &c__1);
/*<                   x(j,j) = temp >*/
        i__2 = j + j * x_dim1;
        x[i__2].r = temp.r, x[i__2].i = temp.i;
/*<    50          continue >*/
L50:
/*<    60       continue >*/
/* L60: */
        ;
    }
/*<    70    continue >*/
L70:
/*<          if (.not.cqty) go to 100 >*/
    if (! cqty) {
        goto L100;
    }

/*           compute ctrans(q)*y. */

/*<             do 90 j = 1, ju >*/
    i__1 = ju;
    for (j = 1; j <= i__1; ++j) {
/*<                if (cabs1(qraux(j)) .eq. 0.0e0) go to 80 >*/
        i__2 = j;
        if ((r__1 = qraux[i__2].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j]),
                dabs(r__2)) == (float)0.) {
            goto L80;
        }
/*<                   temp = x(j,j) >*/
        i__2 = j + j * x_dim1;
        temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                   x(j,j) = qraux(j) >*/
        i__2 = j + j * x_dim1;
        i__3 = j;
        x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
/*<                   t = -cdotc(n-j+1,x(j,j),1,qty(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &qty[j], &c__1);
        q__2.r = -q__3.r, q__2.i = -q__3.i;
        c_div(&q__1, &q__2, &x[j + j * x_dim1]);
        t.r = q__1.r, t.i = q__1.i;
/*<                   call caxpy(n-j+1,t,x(j,j),1,qty(j),1) >*/
        i__2 = *n - j + 1;
        caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &qty[j], &c__1);
/*<                   x(j,j) = temp >*/
        i__2 = j + j * x_dim1;
        x[i__2].r = temp.r, x[i__2].i = temp.i;
/*<    80          continue >*/
L80:
/*<    90       continue >*/
/* L90: */
        ;
    }
/*<   100    continue >*/
L100:

/*        set up to compute b, rsd, or xb. */

/*<          if (cb) call ccopy(k,qty,1,b,1) >*/
    if (cb) {
        ccopy_(k, &qty[1], &c__1, &b[1], &c__1);
    }
/*<          kp1 = k + 1 >*/
    kp1 = *k + 1;
/*<          if (cxb) call ccopy(k,qty,1,xb,1) >*/
    if (cxb) {
        ccopy_(k, &qty[1], &c__1, &xb[1], &c__1);
    }
/*<          if (cr .and. k .lt. n) call ccopy(n-k,qty(kp1),1,rsd(kp1),1) >*/
    if (cr && *k < *n) {
        i__1 = *n - *k;
        ccopy_(&i__1, &qty[kp1], &c__1, &rsd[kp1], &c__1);
    }
/*<          if (.not.cxb .or. kp1 .gt. n) go to 120 >*/
    if (! cxb || kp1 > *n) {
        goto L120;
    }
/*<             do 110 i = kp1, n >*/
    i__1 = *n;
    for (i__ = kp1; i__ <= i__1; ++i__) {
/*<                xb(i) = (0.0e0,0.0e0) >*/
        i__2 = i__;
        xb[i__2].r = (float)0., xb[i__2].i = (float)0.;
/*<   110       continue >*/
/* L110: */
    }
/*<   120    continue >*/
L120:
/*<          if (.not.cr) go to 140 >*/
    if (! cr) {
        goto L140;
    }
/*<             do 130 i = 1, k >*/
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                rsd(i) = (0.0e0,0.0e0) >*/
        i__2 = i__;
        rsd[i__2].r = (float)0., rsd[i__2].i = (float)0.;
/*<   130       continue >*/
/* L130: */
    }
/*<   140    continue >*/
L140:
/*<          if (.not.cb) go to 190 >*/
    if (! cb) {
        goto L190;
    }

/*           compute b. */

/*<             do 170 jj = 1, k >*/
    i__1 = *k;
    for (jj = 1; jj <= i__1; ++jj) {
/*<                j = k - jj + 1 >*/
        j = *k - jj + 1;
/*<                if (cabs1(x(j,j)) .ne. 0.0e0) go to 150 >*/
        i__2 = j + j * x_dim1;
        if ((r__1 = x[i__2].r, dabs(r__1)) + (r__2 = r_imag(&x[j + j * x_dim1]
                ), dabs(r__2)) != (float)0.) {
            goto L150;
        }
/*<                   info = j >*/
        *info = j;
/*           ......exit */
/*<                   go to 180 >*/
        goto L180;
/*<   150          continue >*/
L150:
/*<                b(j) = b(j)/x(j,j) >*/
        i__2 = j;
        c_div(&q__1, &b[j], &x[j + j * x_dim1]);
        b[i__2].r = q__1.r, b[i__2].i = q__1.i;
/*<                if (j .eq. 1) go to 160 >*/
        if (j == 1) {
            goto L160;
        }
/*<                   t = -b(j) >*/
        i__2 = j;
        q__1.r = -b[i__2].r, q__1.i = -b[i__2].i;
        t.r = q__1.r, t.i = q__1.i;
/*<                   call caxpy(j-1,t,x(1,j),1,b,1) >*/
        i__2 = j - 1;
        caxpy_(&i__2, &t, &x[j * x_dim1 + 1], &c__1, &b[1], &c__1);
/*<   160          continue >*/
L160:
/*<   170       continue >*/
/* L170: */
        ;
    }
/*<   180       continue >*/
L180:
/*<   190    continue >*/
L190:
/*<          if (.not.cr .and. .not.cxb) go to 240 >*/
    if (! cr && ! cxb) {
        goto L240;
    }

/*           compute rsd or xb as required. */

/*<             do 230 jj = 1, ju >*/
    i__1 = ju;
    for (jj = 1; jj <= i__1; ++jj) {
/*<                j = ju - jj + 1 >*/
        j = ju - jj + 1;
/*<                if (cabs1(qraux(j)) .eq. 0.0e0) go to 220 >*/
        i__2 = j;
        if ((r__1 = qraux[i__2].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j]),
                dabs(r__2)) == (float)0.) {
            goto L220;
        }
/*<                   temp = x(j,j) >*/
        i__2 = j + j * x_dim1;
        temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                   x(j,j) = qraux(j) >*/
        i__2 = j + j * x_dim1;
        i__3 = j;
        x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
/*<                   if (.not.cr) go to 200 >*/
        if (! cr) {
            goto L200;
        }
/*<                      t = -cdotc(n-j+1,x(j,j),1,rsd(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1);
        q__2.r = -q__3.r, q__2.i = -q__3.i;
        c_div(&q__1, &q__2, &x[j + j * x_dim1]);
        t.r = q__1.r, t.i = q__1.i;
/*<                      call caxpy(n-j+1,t,x(j,j),1,rsd(j),1) >*/
        i__2 = *n - j + 1;
        caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1);
/*<   200             continue >*/
L200:
/*<                   if (.not.cxb) go to 210 >*/
        if (! cxb) {
            goto L210;
        }
/*<                      t = -cdotc(n-j+1,x(j,j),1,xb(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &xb[j], &c__1);
        q__2.r = -q__3.r, q__2.i = -q__3.i;
        c_div(&q__1, &q__2, &x[j + j * x_dim1]);
        t.r = q__1.r, t.i = q__1.i;
/*<                      call caxpy(n-j+1,t,x(j,j),1,xb(j),1) >*/
        i__2 = *n - j + 1;
        caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &xb[j], &c__1);
/*<   210             continue >*/
L210:
/*<                   x(j,j) = temp >*/
        i__2 = j + j * x_dim1;
        x[i__2].r = temp.r, x[i__2].i = temp.i;
/*<   220          continue >*/
L220:
/*<   230       continue >*/
/* L230: */
        ;
    }
/*<   240    continue >*/
L240:
/*<   250 continue >*/
L250:
/*<       return >*/
    return 0;
/*<       end >*/
} /* cqrsl_ */

#ifdef __cplusplus
        }
#endif
