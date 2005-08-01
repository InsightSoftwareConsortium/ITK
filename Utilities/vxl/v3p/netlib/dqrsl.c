#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void dqrsl_(x, ldx, n, k, qraux, y, qy, qty, b, rsd, xb, job, info)
const doublereal *x;
const integer *ldx, *n, *k;
const doublereal *qraux, *y;
doublereal *qy, *qty, *b, *rsd, *xb;
const integer *job;
integer *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal temp;
    static logical cqty;
    static integer i, j;
    static doublereal t;
    static logical cb;
    static logical cr;
    static integer ju;
    static logical cxb, cqy;

/*     dqrsl applies the output of dqrdc to compute coordinate           */
/*     transformations, projections, and least squares solutions.        */
/*     for k .le. min(n,p), let xk be the matrix                         */

/*            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k)))              */

/*     formed from columns jpvt(1), ... ,jpvt(k) of the original         */
/*     n x p matrix x that was input to dqrdc (if no pivoting was        */
/*     done, xk consists of the first k columns of x in their            */
/*     original order).  dqrdc produces a factored orthogonal matrix q   */
/*     and an upper triangular matrix r such that                        */
/*                                                                       */
/*              xk = q * (r)                                             */
/*                       (0)                                             */
/*                                                                       */
/*     this information is contained in coded form in the arrays         */
/*     x and qraux.                                                      */
/*                                                                       */
/*     on entry                                                          */
/*                                                                       */
/*        x      double precision(ldx,p).                                */
/*               x contains the output of dqrdc.                         */
/*                                                                       */
/*        ldx    integer.                                                */
/*               ldx is the leading dimension of the array x.            */
/*                                                                       */
/*        n      integer.                                                */
/*               n is the number of rows of the matrix xk.  it must      */
/*               have the same value as n in dqrdc.                      */
/*                                                                       */
/*        k      integer.                                                */
/*               k is the number of columns of the matrix xk.  k         */
/*               must nnot be greater than min(n,p), where p is the      */
/*               same as in the calling sequence to dqrdc.               */
/*                                                                       */
/*        qraux  double precision(p).                                    */
/*               qraux contains the auxiliary output from dqrdc.         */
/*                                                                       */
/*        y      double precision(n)                                     */
/*               y contains an n-vector that is to be manipulated        */
/*               by dqrsl.                                               */
/*                                                                       */
/*        job    integer.                                                */
/*               job specifies what is to be computed.  job has          */
/*               the decimal expansion abcde, with the following         */
/*               meaning.                                                */
/*                                                                       */
/*                    if a.ne.0, compute qy.                             */
/*                    if b,c,d, or e .ne. 0, compute qty.                */
/*                    if c.ne.0, compute b.                              */
/*                    if d.ne.0, compute rsd.                            */
/*                    if e.ne.0, compute xb.                             */
/*                                                                       */
/*               note that a request to compute b, rsd, or xb            */
/*               automatically triggers the computation of qty, for      */
/*               which an array must be provided in the calling          */
/*               sequence.                                               */
/*                                                                       */
/*     on return                                                         */
/*                                                                       */
/*        qy     double precision(n).                                    */
/*               qy contains q*y, if its computation has been            */
/*               requested.                                              */
/*                                                                       */
/*        qty    double precision(n).                                    */
/*               qty contains trans(q)*y, if its computation has         */
/*               been requested.  here trans(q) is the                   */
/*               transpose of the matrix q.                              */
/*                                                                       */
/*        b      double precision(k)                                     */
/*               b contains the solution of the least squares problem    */
/*                                                                       */
/*                    minimize norm2(y - xk*b),                          */
/*                                                                       */
/*               if its computation has been requested.  (note that      */
/*               if pivoting was requested in dqrdc, the j-th            */
/*               component of b will be associated with column jpvt(j)   */
/*               of the original matrix x that was input into dqrdc.)    */
/*                                                                       */
/*        rsd    double precision(n).                                    */
/*               rsd contains the least squares residual y - xk*b,       */
/*               if its computation has been requested.  rsd is          */
/*               also the orthogonal projection of y onto the            */
/*               orthogonal complement of the column space of xk.        */
/*                                                                       */
/*        xb     double precision(n).                                    */
/*               xb contains the least squares approximation xk*b,       */
/*               if its computation has been requested.  xb is also      */
/*               the orthogonal projection of y onto the column space    */
/*               of x.                                                   */
/*                                                                       */
/*        info   integer.                                                */
/*               info is zero unless the computation of b has            */
/*               been requested and r is exactly singular.  in           */
/*               this case, info is the index of the first zero          */
/*               diagonal element of r and b is left unaltered.          */
/*                                                                       */
/*     the parameters qy, qty, b, rsd, and xb are not referenced         */
/*     if their computation is not requested and in this case            */
/*     can be replaced by dummy variables in the calling program.        */
/*     to save storage, the user may in some cases use the same          */
/*     array for different parameters in the calling sequence.  a        */
/*     frequently occurring example is when one wishes to compute        */
/*     any of b, rsd, or xb and does not need y or qty.  in this         */
/*     case one may identify y, qty, and one of b, rsd, or xb, while     */
/*     providing separate arrays for anything else that is to be         */
/*     computed.  thus the calling sequence                              */
/*                                                                       */
/*          call dqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info)         */
/*                                                                       */
/*     will result in the computation of b and rsd, with rsd             */
/*     overwriting y.  more generally, each item in the following        */
/*     list contains groups of permissible identifications for           */
/*     a single callinng sequence.                                       */
/*                                                                       */
/*          1. (y,qty,b) (rsd) (xb) (qy)                                 */
/*                                                                       */
/*          2. (y,qty,rsd) (b) (xb) (qy)                                 */
/*                                                                       */
/*          3. (y,qty,xb) (b) (rsd) (qy)                                 */
/*                                                                       */
/*          4. (y,qy) (qty,b) (rsd) (xb)                                 */
/*                                                                       */
/*          5. (y,qy) (qty,rsd) (b) (xb)                                 */
/*                                                                       */
/*          6. (y,qy) (qty,xb) (b) (rsd)                                 */
/*                                                                       */
/*     in any group the value returned in the array allocated to         */
/*     the group corresponds to the last member of the group.            */

/*     linpack. this version dated 08/14/78 .                      */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     dqrsl uses the following functions and subprograms.         */

/*     blas daxpy,dcopy,ddot                                       */
/*     fortran dabs,min0,mod                                       */

/*     set info flag. */
    *info = 0;

/*     determine what is to be computed. */

    cqy = *job / 10000 != 0;
    cqty = *job % 10000 != 0;
    cb = *job % 1000 / 100 != 0;
    cr = *job % 100 / 10 != 0;
    cxb = *job % 10 != 0;
    ju = min(*k,*n - 1);

/*     special action when n=1. */

    if (ju == 0) {
        if (cqy)  qy[0] = y[0];
        if (cqty) qty[0] = y[0];
        if (cxb)  xb[0] = y[0];
        if (cb) {
            if (x[0] == 0.) *info = 1;
            else            b[0] = y[0] / x[0];
        }
        if (cr) rsd[0] = 0.;
        return;
    }

/*        set up to compute qy or qty. */

    if (cqy) {
        dcopy_(n, y, &c__1, qy, &c__1);
    }
    if (cqty) {
        dcopy_(n, y, &c__1, qty, &c__1);
    }

/*           compute qy. */

    if (cqy)
    for (j = ju-1; j >= 0; --j) {
        if (qraux[j] == 0.)
            continue;
        temp = x[j + j * *ldx];
        ((doublereal*)x)[j + j * *ldx] = qraux[j]; /* breaks const-ness, but will be restored */
        i__1 = *n - j;
        t = -ddot_(&i__1, &x[j + j * *ldx], &c__1, &qy[j], &c__1) / x[j + j * *ldx];
        daxpy_(&i__1, &t, &x[j + j * *ldx], &c__1, &qy[j], &c__1);
        ((doublereal*)x)[j + j * *ldx] = temp; /* restore original */
    }

/*           compute trans(q)*y. */

    if (cqty)
    for (j = 0; j < ju; ++j) {
        if (qraux[j] == 0.)
            continue;
        temp = x[j + j * *ldx];
        ((doublereal*)x)[j + j * *ldx] = qraux[j]; /* breaks const-ness, but will be restored */
        i__1 = *n - j;
        t = -ddot_(&i__1, &x[j + j * *ldx], &c__1, &qty[j], &c__1) / x[j + j * *ldx];
        daxpy_(&i__1, &t, &x[j + j * *ldx], &c__1, &qty[j], &c__1);
        ((doublereal*)x)[j + j * *ldx] = temp; /* restore original */
    }

/*        set up to compute b, rsd, or xb. */

    if (cb) {
        dcopy_(k, qty, &c__1, b, &c__1);
    }
    if (cxb) {
        dcopy_(k, qty, &c__1, xb, &c__1);
    }
    if (cr && *k < *n) {
        i__1 = *n - *k;
        dcopy_(&i__1, &qty[*k], &c__1, &rsd[*k], &c__1);
    }
    if (cxb)
    for (i = *k; i < *n; ++i) {
        xb[i] = 0.;
    }
    if (cr)
    for (i = 0; i < *k; ++i) {
        rsd[i] = 0.;
    }

/*           compute b. */

    if (cb)
    for (j = *k-1; j >= 0; --j) {
        if (x[j + j * *ldx] == 0.) {
            *info = j+1;
            break;
        }
        b[j] /= x[j + j * *ldx];
        if (j != 0) {
            t = -b[j];
            daxpy_(&j, &t, &x[j * *ldx], &c__1, b, &c__1);
        }
    }
    if (! cr && ! cxb)
        return;

/*           compute rsd or xb as required. */

    for (j = ju-1; j >= 0; --j) {
        if (qraux[j] == 0.) {
            continue;
        }
        temp = x[j + j * *ldx];
        ((doublereal*)x)[j + j * *ldx] = qraux[j]; /* breaks const-ness, but will be restored */
        i__1 = *n - j;
        if (cr) {
            t = -ddot_(&i__1, &x[j + j * *ldx], &c__1, &rsd[j], &c__1) / x[j + j * *ldx];
            daxpy_(&i__1, &t, &x[j + j * *ldx], &c__1, &rsd[j], &c__1);
        }
        if (cxb) {
            t = -ddot_(&i__1, &x[j + j * *ldx], &c__1, &xb[j], &c__1) / x[j + j * *ldx];
            daxpy_(&i__1, &t, &x[j + j * *ldx], &c__1, &xb[j], &c__1);
        }
        ((doublereal*)x)[j + j * *ldx] = temp; /* restore original */
    }
} /* dqrsl_ */
