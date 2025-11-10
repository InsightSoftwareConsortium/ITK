/* linpack/sqrsl.f -- translated by f2c (version 20050501).
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

/*<       subroutine sqrsl(x,ldx,n,k,qraux,y,qy,qty,b,rsd,xb,job,info) >*/
/* Subroutine */ int sqrsl_(real *x, integer *ldx, integer *n, integer *k,
        real *qraux, real *y, real *qy, real *qty, real *b, real *rsd, real *
        xb, integer *job, integer *info)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;
    real t;
    logical cb;
    integer jj;
    logical cr;
    integer ju, kp1;
    logical cxb, cqy;
    real temp;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    logical cqty;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *,
            integer *), saxpy_(integer *, real *, real *, integer *, real *,
            integer *);

/*<       integer ldx,n,k,job,info >*/
/*<       real x(ldx,1),qraux(1),y(1),qy(1),qty(1),b(1),rsd(1),xb(1) >*/

/*     sqrsl applies the output of sqrdc to compute coordinate */
/*     transformations, projections, and least squares solutions. */
/*     for k .le. min(n,p), let xk be the matrix */

/*            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k))) */

/*     formed from columns jpvt(1), ... ,jpvt(k) of the original */
/*     n x p matrix x that was input to sqrdc (if no pivoting was */
/*     done, xk consists of the first k columns of x in their */
/*     original order).  sqrdc produces a factored orthogonal matrix q */
/*     and an upper triangular matrix r such that */

/*              xk = q * (r) */
/*                       (0) */

/*     this information is contained in coded form in the arrays */
/*     x and qraux. */

/*     on entry */

/*        x      real(ldx,p). */
/*               x contains the output of sqrdc. */

/*        ldx    integer. */
/*               ldx is the leading dimension of the array x. */

/*        n      integer. */
/*               n is the number of rows of the matrix xk.  it must */
/*               have the same value as n in sqrdc. */

/*        k      integer. */
/*               k is the number of columns of the matrix xk.  k */
/*               must nnot be greater than min(n,p), where p is the */
/*               same as in the calling sequence to sqrdc. */

/*        qraux  real(p). */
/*               qraux contains the auxiliary output from sqrdc. */

/*        y      real(n) */
/*               y contains an n-vector that is to be manipulated */
/*               by sqrsl. */

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

/*        qy     real(n). */
/*               qy conntains q*y, if its computation has been */
/*               requested. */

/*        qty    real(n). */
/*               qty contains trans(q)*y, if its computation has */
/*               been requested.  here trans(q) is the */
/*               transpose of the matrix q. */

/*        b      real(k) */
/*               b contains the solution of the least squares problem */

/*                    minimize norm2(y - xk*b), */

/*               if its computation has been requested.  (note that */
/*               if pivoting was requested in sqrdc, the j-th */
/*               component of b will be associated with column jpvt(j) */
/*               of the original matrix x that was input into sqrdc.) */

/*        rsd    real(n). */
/*               rsd contains the least squares residual y - xk*b, */
/*               if its computation has been requested.  rsd is */
/*               also the orthogonal projection of y onto the */
/*               orthogonal complement of the column space of xk. */

/*        xb     real(n). */
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

/*          call sqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info) */

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

/*     sqrsl uses the following functions and subprograms. */

/*     blas saxpy,scopy,sdot */
/*     fortran abs,min0,mod */

/*     internal variables */

/*<       integer i,j,jj,ju,kp1 >*/
/*<       real sdot,t,temp >*/
/*<       logical cb,cqy,cqty,cr,cxb >*/


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
        qy[1] = y[1];
    }
/*<          if (cqty) qty(1) = y(1) >*/
    if (cqty) {
        qty[1] = y[1];
    }
/*<          if (cxb) xb(1) = y(1) >*/
    if (cxb) {
        xb[1] = y[1];
    }
/*<          if (.not.cb) go to 30 >*/
    if (! cb) {
        goto L30;
    }
/*<             if (x(1,1) .ne. 0.0e0) go to 10 >*/
    if (x[x_dim1 + 1] != (float)0.) {
        goto L10;
    }
/*<                info = 1 >*/
    *info = 1;
/*<             go to 20 >*/
    goto L20;
/*<    10       continue >*/
L10:
/*<                b(1) = y(1)/x(1,1) >*/
    b[1] = y[1] / x[x_dim1 + 1];
/*<    20       continue >*/
L20:
/*<    30    continue >*/
L30:
/*<          if (cr) rsd(1) = 0.0e0 >*/
    if (cr) {
        rsd[1] = (float)0.;
    }
/*<       go to 250 >*/
    goto L250;
/*<    40 continue >*/
L40:

/*        set up to compute qy or qty. */

/*<          if (cqy) call scopy(n,y,1,qy,1) >*/
    if (cqy) {
        scopy_(n, &y[1], &c__1, &qy[1], &c__1);
    }
/*<          if (cqty) call scopy(n,y,1,qty,1) >*/
    if (cqty) {
        scopy_(n, &y[1], &c__1, &qty[1], &c__1);
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
/*<                if (qraux(j) .eq. 0.0e0) go to 50 >*/
        if (qraux[j] == (float)0.) {
            goto L50;
        }
/*<                   temp = x(j,j) >*/
        temp = x[j + j * x_dim1];
/*<                   x(j,j) = qraux(j) >*/
        x[j + j * x_dim1] = qraux[j];
/*<                   t = -sdot(n-j+1,x(j,j),1,qy(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        t = -sdot_(&i__2, &x[j + j * x_dim1], &c__1, &qy[j], &c__1) / x[j + j
                * x_dim1];
/*<                   call saxpy(n-j+1,t,x(j,j),1,qy(j),1) >*/
        i__2 = *n - j + 1;
        saxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &qy[j], &c__1);
/*<                   x(j,j) = temp >*/
        x[j + j * x_dim1] = temp;
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

/*           compute trans(q)*y. */

/*<             do 90 j = 1, ju >*/
    i__1 = ju;
    for (j = 1; j <= i__1; ++j) {
/*<                if (qraux(j) .eq. 0.0e0) go to 80 >*/
        if (qraux[j] == (float)0.) {
            goto L80;
        }
/*<                   temp = x(j,j) >*/
        temp = x[j + j * x_dim1];
/*<                   x(j,j) = qraux(j) >*/
        x[j + j * x_dim1] = qraux[j];
/*<                   t = -sdot(n-j+1,x(j,j),1,qty(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        t = -sdot_(&i__2, &x[j + j * x_dim1], &c__1, &qty[j], &c__1) / x[j +
                j * x_dim1];
/*<                   call saxpy(n-j+1,t,x(j,j),1,qty(j),1) >*/
        i__2 = *n - j + 1;
        saxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &qty[j], &c__1);
/*<                   x(j,j) = temp >*/
        x[j + j * x_dim1] = temp;
/*<    80          continue >*/
L80:
/*<    90       continue >*/
/* L90: */
        ;
    }
/*<   100    continue >*/
L100:

/*        set up to compute b, rsd, or xb. */

/*<          if (cb) call scopy(k,qty,1,b,1) >*/
    if (cb) {
        scopy_(k, &qty[1], &c__1, &b[1], &c__1);
    }
/*<          kp1 = k + 1 >*/
    kp1 = *k + 1;
/*<          if (cxb) call scopy(k,qty,1,xb,1) >*/
    if (cxb) {
        scopy_(k, &qty[1], &c__1, &xb[1], &c__1);
    }
/*<          if (cr .and. k .lt. n) call scopy(n-k,qty(kp1),1,rsd(kp1),1) >*/
    if (cr && *k < *n) {
        i__1 = *n - *k;
        scopy_(&i__1, &qty[kp1], &c__1, &rsd[kp1], &c__1);
    }
/*<          if (.not.cxb .or. kp1 .gt. n) go to 120 >*/
    if (! cxb || kp1 > *n) {
        goto L120;
    }
/*<             do 110 i = kp1, n >*/
    i__1 = *n;
    for (i__ = kp1; i__ <= i__1; ++i__) {
/*<                xb(i) = 0.0e0 >*/
        xb[i__] = (float)0.;
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
/*<                rsd(i) = 0.0e0 >*/
        rsd[i__] = (float)0.;
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
/*<                if (x(j,j) .ne. 0.0e0) go to 150 >*/
        if (x[j + j * x_dim1] != (float)0.) {
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
        b[j] /= x[j + j * x_dim1];
/*<                if (j .eq. 1) go to 160 >*/
        if (j == 1) {
            goto L160;
        }
/*<                   t = -b(j) >*/
        t = -b[j];
/*<                   call saxpy(j-1,t,x(1,j),1,b,1) >*/
        i__2 = j - 1;
        saxpy_(&i__2, &t, &x[j * x_dim1 + 1], &c__1, &b[1], &c__1);
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
/*<                if (qraux(j) .eq. 0.0e0) go to 220 >*/
        if (qraux[j] == (float)0.) {
            goto L220;
        }
/*<                   temp = x(j,j) >*/
        temp = x[j + j * x_dim1];
/*<                   x(j,j) = qraux(j) >*/
        x[j + j * x_dim1] = qraux[j];
/*<                   if (.not.cr) go to 200 >*/
        if (! cr) {
            goto L200;
        }
/*<                      t = -sdot(n-j+1,x(j,j),1,rsd(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        t = -sdot_(&i__2, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1) / x[j +
                j * x_dim1];
/*<                      call saxpy(n-j+1,t,x(j,j),1,rsd(j),1) >*/
        i__2 = *n - j + 1;
        saxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1);
/*<   200             continue >*/
L200:
/*<                   if (.not.cxb) go to 210 >*/
        if (! cxb) {
            goto L210;
        }
/*<                      t = -sdot(n-j+1,x(j,j),1,xb(j),1)/x(j,j) >*/
        i__2 = *n - j + 1;
        t = -sdot_(&i__2, &x[j + j * x_dim1], &c__1, &xb[j], &c__1) / x[j + j
                * x_dim1];
/*<                      call saxpy(n-j+1,t,x(j,j),1,xb(j),1) >*/
        i__2 = *n - j + 1;
        saxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &xb[j], &c__1);
/*<   210             continue >*/
L210:
/*<                   x(j,j) = temp >*/
        x[j + j * x_dim1] = temp;
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
} /* sqrsl_ */

#ifdef __cplusplus
        }
#endif
