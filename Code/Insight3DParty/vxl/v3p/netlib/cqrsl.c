/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int cqrsl_(x, ldx, n, k, qraux, y, qy, qty, b, rsd, xb, job, 
	info)
complex *x;
integer *ldx, *n, *k;
complex *qraux, *y, *qy, *qty, *b, *rsd, *xb;
integer *job, *info;
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    real r__1, r__2;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    double r_imag();
    void c_div();

    /* Local variables */
    static complex temp;
    static logical cqty;
    static integer i, j;
    static complex t;
    extern /* Complex */ int cdotc_();
    extern /* Subroutine */ int ccopy_(), caxpy_();
    static logical cb;
    static integer jj;
    static logical cr;
    static integer ju, kp1;
    static logical cxb, cqy;


/*     cqrsl applies the output of cqrdc to compute coordinate */
/*     transformations, projections, and least squares solutions. */
/*     for k .le. min(n,p), let xk be the matrix */

/*            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k))) */

/*     formed from columnns jpvt(1), ... ,jpvt(k) of the original */
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
/*     frequently occuring example is when one wishes to compute */
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



/*     set info flag. */

    /* Parameter adjustments */
    --xb;
    --rsd;
    --b;
    --qty;
    --qy;
    --y;
    --qraux;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    *info = 0;

/*     determine what is to be computed. */

    cqy = *job / 10000 != 0;
    cqty = *job % 10000 != 0;
    cb = *job % 1000 / 100 != 0;
    cr = *job % 100 / 10 != 0;
    cxb = *job % 10 != 0;
/* Computing MIN */
    i__1 = *k, i__2 = *n - 1;
    ju = min(i__1,i__2);

/*     special action when n=1. */

    if (ju != 0) {
	goto L40;
    }
    if (cqy) {
	qy[1].r = y[1].r, qy[1].i = y[1].i;
    }
    if (cqty) {
	qty[1].r = y[1].r, qty[1].i = y[1].i;
    }
    if (cxb) {
	xb[1].r = y[1].r, xb[1].i = y[1].i;
    }
    if (! cb) {
	goto L30;
    }
    i__1 = x_dim1 + 1;
    if ((r__1 = x[i__1].r, dabs(r__1)) + (r__2 = r_imag(&x[x_dim1 + 1]), dabs(
	    r__2)) != (float)0.) {
	goto L10;
    }
    *info = 1;
    goto L20;
L10:
    c_div(&q__1, &y[1], &x[x_dim1 + 1]);
    b[1].r = q__1.r, b[1].i = q__1.i;
L20:
L30:
    if (cr) {
	rsd[1].r = (float)0., rsd[1].i = (float)0.;
    }
    goto L250;
L40:

/*        set up to compute qy or qty. */

    if (cqy) {
	ccopy_(n, &y[1], &c__1, &qy[1], &c__1);
    }
    if (cqty) {
	ccopy_(n, &y[1], &c__1, &qty[1], &c__1);
    }
    if (! cqy) {
	goto L70;
    }

/*           compute qy. */

    i__1 = ju;
    for (jj = 1; jj <= i__1; ++jj) {
	j = ju - jj + 1;
	i__2 = j;
	if ((r__1 = qraux[i__2].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j]), 
		dabs(r__2)) == (float)0.) {
	    goto L50;
	}
	i__2 = j + j * x_dim1;
	temp.r = x[i__2].r, temp.i = x[i__2].i;
	i__2 = j + j * x_dim1;
	i__3 = j;
	x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
	i__2 = *n - j + 1;
	cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &qy[j], &c__1);
	q__2.r = -(doublereal)q__3.r, q__2.i = -(doublereal)q__3.i;
	c_div(&q__1, &q__2, &x[j + j * x_dim1]);
	t.r = q__1.r, t.i = q__1.i;
	i__2 = *n - j + 1;
	caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &qy[j], &c__1);
	i__2 = j + j * x_dim1;
	x[i__2].r = temp.r, x[i__2].i = temp.i;
L50:
/* L60: */
	;
    }
L70:
    if (! cqty) {
	goto L100;
    }

/*           compute ctrans(q)*y. */

    i__1 = ju;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	if ((r__1 = qraux[i__2].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j]), 
		dabs(r__2)) == (float)0.) {
	    goto L80;
	}
	i__2 = j + j * x_dim1;
	temp.r = x[i__2].r, temp.i = x[i__2].i;
	i__2 = j + j * x_dim1;
	i__3 = j;
	x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
	i__2 = *n - j + 1;
	cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &qty[j], &c__1);
	q__2.r = -(doublereal)q__3.r, q__2.i = -(doublereal)q__3.i;
	c_div(&q__1, &q__2, &x[j + j * x_dim1]);
	t.r = q__1.r, t.i = q__1.i;
	i__2 = *n - j + 1;
	caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &qty[j], &c__1);
	i__2 = j + j * x_dim1;
	x[i__2].r = temp.r, x[i__2].i = temp.i;
L80:
/* L90: */
	;
    }
L100:

/*        set up to compute b, rsd, or xb. */

    if (cb) {
	ccopy_(k, &qty[1], &c__1, &b[1], &c__1);
    }
    kp1 = *k + 1;
    if (cxb) {
	ccopy_(k, &qty[1], &c__1, &xb[1], &c__1);
    }
    if (cr && *k < *n) {
	i__1 = *n - *k;
	ccopy_(&i__1, &qty[kp1], &c__1, &rsd[kp1], &c__1);
    }
    if (! cxb || kp1 > *n) {
	goto L120;
    }
    i__1 = *n;
    for (i = kp1; i <= i__1; ++i) {
	i__2 = i;
	xb[i__2].r = (float)0., xb[i__2].i = (float)0.;
/* L110: */
    }
L120:
    if (! cr) {
	goto L140;
    }
    i__1 = *k;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	rsd[i__2].r = (float)0., rsd[i__2].i = (float)0.;
/* L130: */
    }
L140:
    if (! cb) {
	goto L190;
    }

/*           compute b. */

    i__1 = *k;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *k - jj + 1;
	i__2 = j + j * x_dim1;
	if ((r__1 = x[i__2].r, dabs(r__1)) + (r__2 = r_imag(&x[j + j * x_dim1]
		), dabs(r__2)) != (float)0.) {
	    goto L150;
	}
	*info = j;
/*           ......exit */
	goto L180;
L150:
	i__2 = j;
	c_div(&q__1, &b[j], &x[j + j * x_dim1]);
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
	if (j == 1) {
	    goto L160;
	}
	i__2 = j;
	q__1.r = -(doublereal)b[i__2].r, q__1.i = -(doublereal)b[i__2].i;
	t.r = q__1.r, t.i = q__1.i;
	i__2 = j - 1;
	caxpy_(&i__2, &t, &x[j * x_dim1 + 1], &c__1, &b[1], &c__1);
L160:
/* L170: */
	;
    }
L180:
L190:
    if (! cr && ! cxb) {
	goto L240;
    }

/*           compute rsd or xb as required. */

    i__1 = ju;
    for (jj = 1; jj <= i__1; ++jj) {
	j = ju - jj + 1;
	i__2 = j;
	if ((r__1 = qraux[i__2].r, dabs(r__1)) + (r__2 = r_imag(&qraux[j]), 
		dabs(r__2)) == (float)0.) {
	    goto L220;
	}
	i__2 = j + j * x_dim1;
	temp.r = x[i__2].r, temp.i = x[i__2].i;
	i__2 = j + j * x_dim1;
	i__3 = j;
	x[i__2].r = qraux[i__3].r, x[i__2].i = qraux[i__3].i;
	if (! cr) {
	    goto L200;
	}
	i__2 = *n - j + 1;
	cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1);
	q__2.r = -(doublereal)q__3.r, q__2.i = -(doublereal)q__3.i;
	c_div(&q__1, &q__2, &x[j + j * x_dim1]);
	t.r = q__1.r, t.i = q__1.i;
	i__2 = *n - j + 1;
	caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1);
L200:
	if (! cxb) {
	    goto L210;
	}
	i__2 = *n - j + 1;
	cdotc_(&q__3, &i__2, &x[j + j * x_dim1], &c__1, &xb[j], &c__1);
	q__2.r = -(doublereal)q__3.r, q__2.i = -(doublereal)q__3.i;
	c_div(&q__1, &q__2, &x[j + j * x_dim1]);
	t.r = q__1.r, t.i = q__1.i;
	i__2 = *n - j + 1;
	caxpy_(&i__2, &t, &x[j + j * x_dim1], &c__1, &xb[j], &c__1);
L210:
	i__2 = j + j * x_dim1;
	x[i__2].r = temp.r, x[i__2].i = temp.i;
L220:
/* L230: */
	;
    }
L240:
L250:
    return 0;
} /* cqrsl_ */

