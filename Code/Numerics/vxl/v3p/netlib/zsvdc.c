/* zsvdc.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublecomplex c_b10 = {1.,0.};
static doublecomplex c_b60 = {-1.,0.};

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module ZSVDC from package LINPACK. */
/* Retrieved from NETLIB on Fri May  9 10:03:02 1997. */
/* ====================================================================== */
/* Subroutine */ int zsvdc_(x, ldx, n, p, s, e, u, ldu, v, ldv, work, job, 
	info)
doublecomplex *x;
integer *ldx, *n, *p;
doublecomplex *s, *e, *u;
integer *ldu;
doublecomplex *v;
integer *ldv;
doublecomplex *work;
integer *job, *info;
{
    /* System generated locals */
    integer x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3, i__4, i__5;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double z_abs();
    void z_div(), d_cnjg();
    double sqrt();

    /* Local variables */
    static integer kase, jobu, iter;
    static doublereal test;
    static integer nctp1;
    static doublereal b, c;
    static integer nrtp1;
    static doublereal f, g;
    static integer i, j, k, l, m;
    static doublecomplex r, t;
    static doublereal scale;
    extern /* Subroutine */ int zscal_();
    static doublereal shift;
    extern /* Subroutine */ int drotg_();
    static integer maxit;
    extern /* Double Complex */ void zdotc_();
    static logical wantu, wantv;
    extern /* Subroutine */ int zdrot_(), zswap_();
    static doublereal t1, ztest;
    extern /* Subroutine */ int zaxpy_();
    extern doublereal dznrm2_();
    static doublereal el;
    static integer kk;
    static doublereal cs;
    static integer ll, mm, ls;
    static doublereal sl;
    static integer lu;
    static doublereal sm, sn;
    static integer lm1, mm1, lp1, mp1, nct, ncu, lls, nrt;
    static doublereal emm1, smm1;



/*     zsvdc is a subroutine to reduce a complex*16 nxp matrix x by */
/*     unitary transformations u and v to diagonal form.  the */
/*     diagonal elements s(i) are the singular values of x.  the */
/*     columns of u are the corresponding left singular vectors, */
/*     and the columns of v the right singular vectors. */

/*     on entry */

/*         x         complex*16(ldx,p), where ldx.ge.n. */
/*                   x contains the matrix whose singular value */
/*                   decomposition is to be computed.  x is */
/*                   destroyed by zsvdc. */

/*         ldx       integer. */
/*                   ldx is the leading dimension of the array x. */

/*         n         integer. */
/*                   n is the number of rows of the matrix x. */

/*         p         integer. */
/*                   p is the number of columns of the matrix x. */

/*         ldu       integer. */
/*                   ldu is the leading dimension of the array u */
/*                   (see below). */

/*         ldv       integer. */
/*                   ldv is the leading dimension of the array v */
/*                   (see below). */

/*         work      complex*16(n). */
/*                   work is a scratch array. */

/*         job       integer. */
/*                   job controls the computation of the singular */
/*                   vectors.  it has the decimal expansion ab */
/*                   with the following meaning */

/*                        a.eq.0    do not compute the left singular */
/*                                  vectors. */
/*                        a.eq.1    return the n left singular vectors */
/*                                  in u. */
/*                        a.ge.2    returns the first min(n,p) */
/*                                  left singular vectors in u. */
/*                        b.eq.0    do not compute the right singular */
/*                                  vectors. */
/*                        b.eq.1    return the right singular vectors */
/*                                  in v. */

/*     on return */

/*         s         complex*16(mm), where mm=min(n+1,p). */
/*                   the first min(n,p) entries of s contain the */
/*                   singular values of x arranged in descending */
/*                   order of magnitude. */

/*         e         complex*16(p). */
/*                   e ordinarily contains zeros.  however see the */
/*                   discussion of info for exceptions. */

/*         u         complex*16(ldu,k), where ldu.ge.n.  if joba.eq.1 */
/*                                   then k.eq.n, if joba.ge.2 then */

/*                                   k.eq.min(n,p). */
/*                   u contains the matrix of left singular vectors. */
/*                   u is not referenced if joba.eq.0.  if n.le.p */
/*                   or if joba.gt.2, then u may be identified with x */
/*                   in the subroutine call. */

/*         v         complex*16(ldv,p), where ldv.ge.p. */
/*                   v contains the matrix of right singular vectors. */
/*                   v is not referenced if jobb.eq.0.  if p.le.n, */
/*                   then v may be identified whth x in the */
/*                   subroutine call. */

/*         info      integer. */
/*                   the singular values (and their corresponding */
/*                   singular vectors) s(info+1),s(info+2),...,s(m) */
/*                   are correct (here m=min(n,p)).  thus if */
/*                   info.eq.0, all the singular values and their */
/*                   vectors are correct.  in any event, the matrix */
/*                   b = ctrans(u)*x*v is the bidiagonal matrix */
/*                   with the elements of s on its diagonal and the */
/*                   elements of e on its super-diagonal (ctrans(u) */
/*                   is the conjugate-transpose of u).  thus the */
/*                   singular values of x and b are the same. */

/*     linpack. this version dated 03/19/79 . */
/*              correction to shift calculation made 2/85. */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     zsvdc uses the following functions and subprograms. */

/*     external zdrot */
/*     blas zaxpy,zdotc,zscal,zswap,dznrm2,drotg */
/*     fortran dabs,dmax1,zabs,dcmplx */
/*     fortran dconjg,max0,min0,mod,dsqrt */

/*     internal variables */



/*     set the maximum number of iterations. */

    /* Parameter adjustments */
    --work;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --e;
    --s;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    maxit = 30;

/*     determine what is to be computed. */

    wantu = FALSE_;
    wantv = FALSE_;
    jobu = *job % 100 / 10;
    ncu = *n;
    if (jobu > 1) {
	ncu = min(*n,*p);
    }
    if (jobu != 0) {
	wantu = TRUE_;
    }
    if (*job % 10 != 0) {
	wantv = TRUE_;
    }

/*     reduce x to bidiagonal form, storing the diagonal elements */
/*     in s and the super-diagonal elements in e. */

    *info = 0;
/* Computing MIN */
    i__1 = *n - 1;
    nct = min(i__1,*p);
/* Computing MAX */
/* Computing MIN */
    i__3 = *p - 2;
    i__1 = 0, i__2 = min(i__3,*n);
    nrt = max(i__1,i__2);
    lu = max(nct,nrt);
    if (lu < 1) {
	goto L170;
    }
    i__1 = lu;
    for (l = 1; l <= i__1; ++l) {
	lp1 = l + 1;
	if (l > nct) {
	    goto L20;
	}

/*           compute the transformation for the l-th column and */
/*           place the l-th diagonal in s(l). */

	i__2 = l;
	i__3 = *n - l + 1;
	d__1 = dznrm2_(&i__3, &x[l + l * x_dim1], &c__1);
	z__1.r = d__1, z__1.i = 0.;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	i__2 = l;
	i__3 = l;
	z__1.r = s[i__3].r * 0. - s[i__3].i * -1., z__1.i = s[i__3].r * -1. + 
		s[i__3].i * 0.;
	if ((d__1 = s[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L10;
	}
	i__2 = l + l * x_dim1;
	i__3 = l + l * x_dim1;
	z__1.r = x[i__3].r * 0. - x[i__3].i * -1., z__1.i = x[i__3].r * -1. + 
		x[i__3].i * 0.;
	if ((d__1 = x[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) 
		{
	    i__4 = l;
	    d__3 = z_abs(&s[l]);
	    i__5 = l + l * x_dim1;
	    d__4 = z_abs(&x[l + l * x_dim1]);
	    z__3.r = x[i__5].r / d__4, z__3.i = x[i__5].i / d__4;
	    z__2.r = d__3 * z__3.r, z__2.i = d__3 * z__3.i;
	    s[i__4].r = z__2.r, s[i__4].i = z__2.i;
	}
	i__2 = *n - l + 1;
	z_div(&z__1, &c_b10, &s[l]);
	zscal_(&i__2, &z__1, &x[l + l * x_dim1], &c__1);
	i__2 = l + l * x_dim1;
	i__3 = l + l * x_dim1;
	z__1.r = x[i__3].r + 1., z__1.i = x[i__3].i + 0.;
	x[i__2].r = z__1.r, x[i__2].i = z__1.i;
L10:
	i__2 = l;
	i__3 = l;
	z__1.r = -s[i__3].r, z__1.i = -s[i__3].i;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
L20:
	if (*p < lp1) {
	    goto L50;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    if (l > nct) {
		goto L30;
	    }
	    i__3 = l;
	    i__4 = l;
	    z__1.r = s[i__4].r * 0. - s[i__4].i * -1., z__1.i = s[i__4].r * 
		    -1. + s[i__4].i * 0.;
	    if ((d__1 = s[i__3].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 
		    0.) {
		goto L30;
	    }

/*              apply the transformation. */

	    i__3 = *n - l + 1;
	    zdotc_(&z__3, &i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1]
		    , &c__1);
	    z__2.r = -z__3.r, z__2.i = -z__3.i;
	    z_div(&z__1, &z__2, &x[l + l * x_dim1]);
	    t.r = z__1.r, t.i = z__1.i;
	    i__3 = *n - l + 1;
	    zaxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1);
L30:

/*           place the l-th row of x into  e for the */
/*           subsequent calculation of the row transformation. */

	    i__3 = j;
	    d_cnjg(&z__1, &x[l + j * x_dim1]);
	    e[i__3].r = z__1.r, e[i__3].i = z__1.i;
/* L40: */
	}
L50:
	if (! wantu || l > nct) {
	    goto L70;
	}

/*           place the transformation in u for subsequent back */
/*           multiplication. */

	i__2 = *n;
	for (i = l; i <= i__2; ++i) {
	    i__3 = i + l * u_dim1;
	    i__4 = i + l * x_dim1;
	    u[i__3].r = x[i__4].r, u[i__3].i = x[i__4].i;
/* L60: */
	}
L70:
	if (l > nrt) {
	    goto L150;
	}

/*           compute the l-th row transformation and place the */
/*           l-th super-diagonal in e(l). */

	i__2 = l;
	i__3 = *p - l;
	d__1 = dznrm2_(&i__3, &e[lp1], &c__1);
	z__1.r = d__1, z__1.i = 0.;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	i__2 = l;
	i__3 = l;
	z__1.r = e[i__3].r * 0. - e[i__3].i * -1., z__1.i = e[i__3].r * -1. + 
		e[i__3].i * 0.;
	if ((d__1 = e[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L80;
	}
	i__2 = lp1;
	i__3 = lp1;
	z__1.r = e[i__3].r * 0. - e[i__3].i * -1., z__1.i = e[i__3].r * -1. + 
		e[i__3].i * 0.;
	if ((d__1 = e[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) 
		{
	    i__4 = l;
	    d__3 = z_abs(&e[l]);
	    i__5 = lp1;
	    d__4 = z_abs(&e[lp1]);
	    z__3.r = e[i__5].r / d__4, z__3.i = e[i__5].i / d__4;
	    z__2.r = d__3 * z__3.r, z__2.i = d__3 * z__3.i;
	    e[i__4].r = z__2.r, e[i__4].i = z__2.i;
	}
	i__2 = *p - l;
	z_div(&z__1, &c_b10, &e[l]);
	zscal_(&i__2, &z__1, &e[lp1], &c__1);
	i__2 = lp1;
	i__3 = lp1;
	z__1.r = e[i__3].r + 1., z__1.i = e[i__3].i + 0.;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
L80:
	i__2 = l;
	d_cnjg(&z__2, &e[l]);
	z__1.r = -z__2.r, z__1.i = -z__2.i;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	i__2 = l;
	i__3 = l;
	z__1.r = e[i__3].r * 0. - e[i__3].i * -1., z__1.i = e[i__3].r * -1. + 
		e[i__3].i * 0.;
	if (lp1 > *n || (d__1 = e[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(
		d__2)) == 0.) {
	    goto L120;
	}

/*              apply the transformation. */

	i__2 = *n;
	for (i = lp1; i <= i__2; ++i) {
	    i__3 = i;
	    work[i__3].r = 0., work[i__3].i = 0.;
/* L90: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    zaxpy_(&i__3, &e[j], &x[lp1 + j * x_dim1], &c__1, &work[lp1], &
		    c__1);
/* L100: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    i__4 = j;
	    z__3.r = -e[i__4].r, z__3.i = -e[i__4].i;
	    z_div(&z__2, &z__3, &e[lp1]);
	    d_cnjg(&z__1, &z__2);
	    zaxpy_(&i__3, &z__1, &work[lp1], &c__1, &x[lp1 + j * x_dim1], &
		    c__1);
/* L110: */
	}
L120:
	if (! wantv) {
	    goto L140;
	}

/*              place the transformation in v for subsequent */
/*              back multiplication. */

	i__2 = *p;
	for (i = lp1; i <= i__2; ++i) {
	    i__3 = i + l * v_dim1;
	    i__4 = i;
	    v[i__3].r = e[i__4].r, v[i__3].i = e[i__4].i;
/* L130: */
	}
L140:
L150:
/* L160: */
	;
    }
L170:

/*     set up the final bidiagonal matrix or order m. */

/* Computing MIN */
    i__1 = *p, i__2 = *n + 1;
    m = min(i__1,i__2);
    nctp1 = nct + 1;
    nrtp1 = nrt + 1;
    if (nct < *p) {
	i__1 = nctp1;
	i__2 = nctp1 + nctp1 * x_dim1;
	s[i__1].r = x[i__2].r, s[i__1].i = x[i__2].i;
    }
    if (*n < m) {
	i__1 = m;
	s[i__1].r = 0., s[i__1].i = 0.;
    }
    if (nrtp1 < m) {
	i__1 = nrtp1;
	i__2 = nrtp1 + m * x_dim1;
	e[i__1].r = x[i__2].r, e[i__1].i = x[i__2].i;
    }
    i__1 = m;
    e[i__1].r = 0., e[i__1].i = 0.;

/*     if required, generate u. */

    if (! wantu) {
	goto L300;
    }
    if (ncu < nctp1) {
	goto L200;
    }
    i__1 = ncu;
    for (j = nctp1; j <= i__1; ++j) {
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    i__3 = i + j * u_dim1;
	    u[i__3].r = 0., u[i__3].i = 0.;
/* L180: */
	}
	i__2 = j + j * u_dim1;
	u[i__2].r = 1., u[i__2].i = 0.;
/* L190: */
    }
L200:
    if (nct < 1) {
	goto L290;
    }
    i__1 = nct;
    for (ll = 1; ll <= i__1; ++ll) {
	l = nct - ll + 1;
	i__2 = l;
	i__3 = l;
	z__1.r = s[i__3].r * 0. - s[i__3].i * -1., z__1.i = s[i__3].r * -1. + 
		s[i__3].i * 0.;
	if ((d__1 = s[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L250;
	}
	lp1 = l + 1;
	if (ncu < lp1) {
	    goto L220;
	}
	i__2 = ncu;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    zdotc_(&z__3, &i__3, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1]
		    , &c__1);
	    z__2.r = -z__3.r, z__2.i = -z__3.i;
	    z_div(&z__1, &z__2, &u[l + l * u_dim1]);
	    t.r = z__1.r, t.i = z__1.i;
	    i__3 = *n - l + 1;
	    zaxpy_(&i__3, &t, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1);
/* L210: */
	}
L220:
	i__2 = *n - l + 1;
	zscal_(&i__2, &c_b60, &u[l + l * u_dim1], &c__1);
	i__2 = l + l * u_dim1;
	i__3 = l + l * u_dim1;
	z__1.r = u[i__3].r + 1., z__1.i = u[i__3].i + 0.;
	u[i__2].r = z__1.r, u[i__2].i = z__1.i;
	lm1 = l - 1;
	if (lm1 < 1) {
	    goto L240;
	}
	i__2 = lm1;
	for (i = 1; i <= i__2; ++i) {
	    i__3 = i + l * u_dim1;
	    u[i__3].r = 0., u[i__3].i = 0.;
/* L230: */
	}
L240:
	goto L270;
L250:
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    i__3 = i + l * u_dim1;
	    u[i__3].r = 0., u[i__3].i = 0.;
/* L260: */
	}
	i__2 = l + l * u_dim1;
	u[i__2].r = 1., u[i__2].i = 0.;
L270:
/* L280: */
	;
    }
L290:
L300:

/*     if it is required, generate v. */

    if (! wantv) {
	goto L350;
    }
    i__1 = *p;
    for (ll = 1; ll <= i__1; ++ll) {
	l = *p - ll + 1;
	lp1 = l + 1;
	if (l > nrt) {
	    goto L320;
	}
	i__2 = l;
	i__3 = l;
	z__1.r = e[i__3].r * 0. - e[i__3].i * -1., z__1.i = e[i__3].r * -1. + 
		e[i__3].i * 0.;
	if ((d__1 = e[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L320;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *p - l;
	    zdotc_(&z__3, &i__3, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * 
		    v_dim1], &c__1);
	    z__2.r = -z__3.r, z__2.i = -z__3.i;
	    z_div(&z__1, &z__2, &v[lp1 + l * v_dim1]);
	    t.r = z__1.r, t.i = z__1.i;
	    i__3 = *p - l;
	    zaxpy_(&i__3, &t, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * 
		    v_dim1], &c__1);
/* L310: */
	}
L320:
	i__2 = *p;
	for (i = 1; i <= i__2; ++i) {
	    i__3 = i + l * v_dim1;
	    v[i__3].r = 0., v[i__3].i = 0.;
/* L330: */
	}
	i__2 = l + l * v_dim1;
	v[i__2].r = 1., v[i__2].i = 0.;
/* L340: */
    }
L350:

/*     transform s and e so that they are double precision. */

    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	z__1.r = s[i__3].r * 0. - s[i__3].i * -1., z__1.i = s[i__3].r * -1. + 
		s[i__3].i * 0.;
	if ((d__1 = s[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L360;
	}
	d__1 = z_abs(&s[i]);
	z__1.r = d__1, z__1.i = 0.;
	t.r = z__1.r, t.i = z__1.i;
	z_div(&z__1, &s[i], &t);
	r.r = z__1.r, r.i = z__1.i;
	i__2 = i;
	s[i__2].r = t.r, s[i__2].i = t.i;
	if (i < m) {
	    i__2 = i;
	    z_div(&z__1, &e[i], &r);
	    e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	}
	if (wantu) {
	    zscal_(n, &r, &u[i * u_dim1 + 1], &c__1);
	}
L360:
/*     ...exit */
	if (i == m) {
	    goto L390;
	}
	i__2 = i;
	i__3 = i;
	z__1.r = e[i__3].r * 0. - e[i__3].i * -1., z__1.i = e[i__3].r * -1. + 
		e[i__3].i * 0.;
	if ((d__1 = e[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L370;
	}
	d__1 = z_abs(&e[i]);
	z__1.r = d__1, z__1.i = 0.;
	t.r = z__1.r, t.i = z__1.i;
	z_div(&z__1, &t, &e[i]);
	r.r = z__1.r, r.i = z__1.i;
	i__2 = i;
	e[i__2].r = t.r, e[i__2].i = t.i;
	i__2 = i + 1;
	i__3 = i + 1;
	z__1.r = s[i__3].r * r.r - s[i__3].i * r.i, z__1.i = s[i__3].r * r.i 
		+ s[i__3].i * r.r;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	if (wantv) {
	    zscal_(p, &r, &v[(i + 1) * v_dim1 + 1], &c__1);
	}
L370:
/* L380: */
	;
    }
L390:

/*     main iteration loop for the singular values. */

    mm = m;
    iter = 0;
L400:

/*        quit if all the singular values have been found. */

/*     ...exit */
    if (m == 0) {
	goto L660;
    }

/*        if too many iterations have been performed, set */
/*        flag and return. */

    if (iter < maxit) {
	goto L410;
    }
    *info = m;
/*     ......exit */
    goto L660;
L410:

/*        this section of the program inspects for */
/*        negligible elements in the s and e arrays.  on */
/*        completion the variables kase and l are set as follows. */

/*           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m */
/*           kase = 2     if s(l) is negligible and l.lt.m */
/*           kase = 3     if e(l-1) is negligible, l.lt.m, and */
/*                        s(l), ..., s(m) are not negligible (qr step). */
/*           kase = 4     if e(m-1) is negligible (convergence). */

    i__1 = m;
    for (ll = 1; ll <= i__1; ++ll) {
	l = m - ll;
/*        ...exit */
	if (l == 0) {
	    goto L440;
	}
	test = z_abs(&s[l]) + z_abs(&s[l + 1]);
	ztest = test + z_abs(&e[l]);
	if (ztest != test) {
	    goto L420;
	}
	i__2 = l;
	e[i__2].r = 0., e[i__2].i = 0.;
/*        ......exit */
	goto L440;
L420:
/* L430: */
	;
    }
L440:
    if (l != m - 1) {
	goto L450;
    }
    kase = 4;
    goto L520;
L450:
    lp1 = l + 1;
    mp1 = m + 1;
    i__1 = mp1;
    for (lls = lp1; lls <= i__1; ++lls) {
	ls = m - lls + lp1;
/*           ...exit */
	if (ls == l) {
	    goto L480;
	}
	test = 0.;
	if (ls != m) {
	    test += z_abs(&e[ls]);
	}
	if (ls != l + 1) {
	    test += z_abs(&e[ls - 1]);
	}
	ztest = test + z_abs(&s[ls]);
	if (ztest != test) {
	    goto L460;
	}
	i__2 = ls;
	s[i__2].r = 0., s[i__2].i = 0.;
/*           ......exit */
	goto L480;
L460:
/* L470: */
	;
    }
L480:
    if (ls != l) {
	goto L490;
    }
    kase = 3;
    goto L510;
L490:
    if (ls != m) {
	goto L500;
    }
    kase = 1;
    goto L510;
L500:
    kase = 2;
    l = ls;
L510:
L520:
    ++l;

/*        perform the task indicated by kase. */

    switch ((int)kase) {
	case 1:  goto L530;
	case 2:  goto L560;
	case 3:  goto L580;
	case 4:  goto L610;
    }

/*        deflate negligible s(m). */

L530:
    mm1 = m - 1;
    i__1 = m - 1;
    f = e[i__1].r;
    i__1 = m - 1;
    e[i__1].r = 0., e[i__1].i = 0.;
    i__1 = mm1;
    for (kk = l; kk <= i__1; ++kk) {
	k = mm1 - kk + l;
	i__2 = k;
	t1 = s[i__2].r;
	drotg_(&t1, &f, &cs, &sn);
	i__2 = k;
	z__1.r = t1, z__1.i = 0.;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	if (k == l) {
	    goto L540;
	}
	i__2 = k - 1;
	f = -sn * e[i__2].r;
	i__2 = k - 1;
	i__3 = k - 1;
	z__1.r = cs * e[i__3].r, z__1.i = cs * e[i__3].i;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
L540:
	if (wantv) {
	    zdrot_(p, &v[k * v_dim1 + 1], &c__1, &v[m * v_dim1 + 1], &c__1, &
		    cs, &sn);
	}
/* L550: */
    }
    goto L650;

/*        split at negligible s(l). */

L560:
    i__1 = l - 1;
    f = e[i__1].r;
    i__1 = l - 1;
    e[i__1].r = 0., e[i__1].i = 0.;
    i__1 = m;
    for (k = l; k <= i__1; ++k) {
	i__2 = k;
	t1 = s[i__2].r;
	drotg_(&t1, &f, &cs, &sn);
	i__2 = k;
	z__1.r = t1, z__1.i = 0.;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	i__2 = k;
	f = -sn * e[i__2].r;
	i__2 = k;
	i__3 = k;
	z__1.r = cs * e[i__3].r, z__1.i = cs * e[i__3].i;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	if (wantu) {
	    zdrot_(n, &u[k * u_dim1 + 1], &c__1, &u[(l - 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L570: */
    }
    goto L650;

/*        perform one qr step. */

L580:

/*           calculate the shift. */

/* Computing MAX */
    d__1 = z_abs(&s[m]), d__2 = z_abs(&s[m - 1]), d__1 = max(d__1,d__2), d__2 
	    = z_abs(&e[m - 1]), d__1 = max(d__1,d__2), d__2 = z_abs(&s[l]), 
	    d__1 = max(d__1,d__2), d__2 = z_abs(&e[l]);
    scale = max(d__1,d__2);
    i__1 = m;
    sm = s[i__1].r / scale;
    i__1 = m - 1;
    smm1 = s[i__1].r / scale;
    i__1 = m - 1;
    emm1 = e[i__1].r / scale;
    i__1 = l;
    sl = s[i__1].r / scale;
    i__1 = l;
    el = e[i__1].r / scale;
/* Computing 2nd power */
    d__1 = emm1;
    b = ((smm1 + sm) * (smm1 - sm) + d__1 * d__1) / 2.;
/* Computing 2nd power */
    d__1 = sm * emm1;
    c = d__1 * d__1;
    shift = 0.;
    if (b == 0. && c == 0.) {
	goto L590;
    }
/* Computing 2nd power */
    d__1 = b;
    shift = sqrt(d__1 * d__1 + c);
    if (b < 0.) {
	shift = -shift;
    }
    shift = c / (b + shift);
L590:
    f = (sl + sm) * (sl - sm) + shift;
    g = sl * el;

/*           chase zeros. */

    mm1 = m - 1;
    i__1 = mm1;
    for (k = l; k <= i__1; ++k) {
	drotg_(&f, &g, &cs, &sn);
	if (k != l) {
	    i__2 = k - 1;
	    z__1.r = f, z__1.i = 0.;
	    e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	}
	i__2 = k;
	i__3 = k;
	f = cs * s[i__2].r + sn * e[i__3].r;
	i__2 = k;
	i__3 = k;
	z__2.r = cs * e[i__3].r, z__2.i = cs * e[i__3].i;
	i__4 = k;
	z__3.r = sn * s[i__4].r, z__3.i = sn * s[i__4].i;
	z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	i__2 = k + 1;
	g = sn * s[i__2].r;
	i__2 = k + 1;
	i__3 = k + 1;
	z__1.r = cs * s[i__3].r, z__1.i = cs * s[i__3].i;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	if (wantv) {
	    zdrot_(p, &v[k * v_dim1 + 1], &c__1, &v[(k + 1) * v_dim1 + 1], &
		    c__1, &cs, &sn);
	}
	drotg_(&f, &g, &cs, &sn);
	i__2 = k;
	z__1.r = f, z__1.i = 0.;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	i__2 = k;
	i__3 = k + 1;
	f = cs * e[i__2].r + sn * s[i__3].r;
	i__2 = k + 1;
	d__1 = -sn;
	i__3 = k;
	z__2.r = d__1 * e[i__3].r, z__2.i = d__1 * e[i__3].i;
	i__4 = k + 1;
	z__3.r = cs * s[i__4].r, z__3.i = cs * s[i__4].i;
	z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
	s[i__2].r = z__1.r, s[i__2].i = z__1.i;
	i__2 = k + 1;
	g = sn * e[i__2].r;
	i__2 = k + 1;
	i__3 = k + 1;
	z__1.r = cs * e[i__3].r, z__1.i = cs * e[i__3].i;
	e[i__2].r = z__1.r, e[i__2].i = z__1.i;
	if (wantu && k < *n) {
	    zdrot_(n, &u[k * u_dim1 + 1], &c__1, &u[(k + 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L600: */
    }
    i__1 = m - 1;
    z__1.r = f, z__1.i = 0.;
    e[i__1].r = z__1.r, e[i__1].i = z__1.i;
    ++iter;
    goto L650;

/*        convergence. */

L610:

/*           make the singular value  positive */

    i__1 = l;
    if (s[i__1].r >= 0.) {
	goto L620;
    }
    i__1 = l;
    i__2 = l;
    z__1.r = -s[i__2].r, z__1.i = -s[i__2].i;
    s[i__1].r = z__1.r, s[i__1].i = z__1.i;
    if (wantv) {
	zscal_(p, &c_b60, &v[l * v_dim1 + 1], &c__1);
    }
L620:

/*           order the singular value. */

L630:
    if (l == mm) {
	goto L640;
    }
/*           ...exit */
    i__1 = l;
    i__2 = l + 1;
    if (s[i__1].r >= s[i__2].r) {
	goto L640;
    }
    i__1 = l;
    t.r = s[i__1].r, t.i = s[i__1].i;
    i__1 = l;
    i__2 = l + 1;
    s[i__1].r = s[i__2].r, s[i__1].i = s[i__2].i;
    i__1 = l + 1;
    s[i__1].r = t.r, s[i__1].i = t.i;
    if (wantv && l < *p) {
	zswap_(p, &v[l * v_dim1 + 1], &c__1, &v[(l + 1) * v_dim1 + 1], &c__1);
    }
    if (wantu && l < *n) {
	zswap_(n, &u[l * u_dim1 + 1], &c__1, &u[(l + 1) * u_dim1 + 1], &c__1);
    }
    ++l;
    goto L630;
L640:
    iter = 0;
    --m;
L650:
    goto L400;
L660:
    return 0;
} /* zsvdc_ */

doublereal dznrm2_(n, x, incx)
integer *n;
doublecomplex *x;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_imag(), sqrt();

    /* Local variables */
    static doublereal temp, norm, scale;
    static integer ix;
    static doublereal ssq;

/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */

/*  DZNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     DZNRM2 := sqrt( conjg( x' )*x ) */



/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to ZLASSQ. */
/*     Sven Hammarling, Nag Ltd. */


/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1 || *incx < 1) {
	norm = 0.;
    } else {
	scale = 0.;
	ssq = 1.;
/*        The following loop is equivalent to this call to the LAPACK 
*/
/*        auxiliary routine: */
/*        CALL ZLASSQ( N, X, INCX, SCALE, SSQ ) */

	i__1 = (*n - 1) * *incx + 1;
	i__2 = *incx;
	for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
	    i__3 = ix;
	    if (x[i__3].r != 0.) {
		i__3 = ix;
		temp = (d__1 = x[i__3].r, abs(d__1));
		if (scale < temp) {
/* Computing 2nd power */
		    d__1 = scale / temp;
		    ssq = ssq * (d__1 * d__1) + 1.;
		    scale = temp;
		} else {
/* Computing 2nd power */
		    d__1 = temp / scale;
		    ssq += d__1 * d__1;
		}
	    }
	    if (d_imag(&x[ix]) != 0.) {
		temp = (d__1 = d_imag(&x[ix]), abs(d__1));
		if (scale < temp) {
/* Computing 2nd power */
		    d__1 = scale / temp;
		    ssq = ssq * (d__1 * d__1) + 1.;
		    scale = temp;
		} else {
/* Computing 2nd power */
		    d__1 = temp / scale;
		    ssq += d__1 * d__1;
		}
	    }
/* L10: */
	}
	norm = scale * sqrt(ssq);
    }

    ret_val = norm;
    return ret_val;

/*     End of DZNRM2. */

} /* dznrm2_ */

/* Subroutine */ int zaxpy_(n, za, zx, incx, zy, incy)
integer *n;
doublecomplex *za, *zx;
integer *incx;
doublecomplex *zy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2;

    /* Local variables */
    static integer i;
    extern doublereal dcabs1_();
    static integer ix, iy;


/*     constant times a vector plus a vector. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (dcabs1_(za) == 0.) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = iy;
	i__3 = iy;
	i__4 = ix;
	z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i, z__2.i = za->r * zx[
		i__4].i + za->i * zx[i__4].r;
	z__1.r = zy[i__3].r + z__2.r, z__1.i = zy[i__3].i + z__2.i;
	zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	i__4 = i;
	z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i, z__2.i = za->r * zx[
		i__4].i + za->i * zx[i__4].r;
	z__1.r = zy[i__3].r + z__2.r, z__1.i = zy[i__3].i + z__2.i;
	zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
/* L30: */
    }
    return 0;
} /* zaxpy_ */

/* Double Complex */ void zdotc_( ret_val, n, zx, incx, zy, incy)
doublecomplex * ret_val;
integer *n;
doublecomplex *zx;
integer *incx;
doublecomplex *zy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void d_cnjg();

    /* Local variables */
    static integer i;
    static doublecomplex ztemp;
    static integer ix, iy;


/*     forms the dot product of a vector. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    ztemp.r = 0., ztemp.i = 0.;
     ret_val->r = 0.,  ret_val->i = 0.;
    if (*n <= 0) {
	return ;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d_cnjg(&z__3, &zx[ix]);
	i__2 = iy;
	z__2.r = z__3.r * zy[i__2].r - z__3.i * zy[i__2].i, z__2.i = z__3.r * 
		zy[i__2].i + z__3.i * zy[i__2].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d_cnjg(&z__3, &zx[i]);
	i__2 = i;
	z__2.r = z__3.r * zy[i__2].r - z__3.i * zy[i__2].i, z__2.i = z__3.r * 
		zy[i__2].i + z__3.i * zy[i__2].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
/* L30: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;
} /* zdotc_ */

/* Subroutine */ int zdrot_(n, zx, incx, zy, incy, c, s)
integer *n;
doublecomplex *zx;
integer *incx;
doublecomplex *zy;
integer *incy;
doublereal *c, *s;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2, z__3;

    /* Local variables */
    static integer i;
    static doublecomplex ztemp;
    static integer ix, iy;


/*     applies a plane rotation, where the cos and sin (c and s) are */
/*     double precision and the vectors zx and zy are double complex. */
/*     jack dongarra, linpack, 3/11/78. */


    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       code for unequal increments or equal increments not equal */
/*         to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = ix;
	z__2.r = *c * zx[i__2].r, z__2.i = *c * zx[i__2].i;
	i__3 = iy;
	z__3.r = *s * zy[i__3].r, z__3.i = *s * zy[i__3].i;
	z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
	i__2 = iy;
	i__3 = iy;
	z__2.r = *c * zy[i__3].r, z__2.i = *c * zy[i__3].i;
	i__4 = ix;
	z__3.r = *s * zx[i__4].r, z__3.i = *s * zx[i__4].i;
	z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
	zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
	i__2 = ix;
	zx[i__2].r = ztemp.r, zx[i__2].i = ztemp.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	z__2.r = *c * zx[i__2].r, z__2.i = *c * zx[i__2].i;
	i__3 = i;
	z__3.r = *s * zy[i__3].r, z__3.i = *s * zy[i__3].i;
	z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
	i__2 = i;
	i__3 = i;
	z__2.r = *c * zy[i__3].r, z__2.i = *c * zy[i__3].i;
	i__4 = i;
	z__3.r = *s * zx[i__4].r, z__3.i = *s * zx[i__4].i;
	z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
	zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
	i__2 = i;
	zx[i__2].r = ztemp.r, zx[i__2].i = ztemp.i;
/* L30: */
    }
    return 0;
} /* zdrot_ */

/* Subroutine */ int zscal_(n, za, zx, incx)
integer *n;
doublecomplex *za, *zx;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublecomplex z__1;

    /* Local variables */
    static integer i, ix;


/*     scales a vector by a constant. */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */


    /* Parameter adjustments */
    --zx;

    /* Function Body */
    if (*n <= 0 || *incx <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        code for increment not equal to 1 */

    ix = 1;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = ix;
	i__3 = ix;
	z__1.r = za->r * zx[i__3].r - za->i * zx[i__3].i, z__1.i = za->r * zx[
		i__3].i + za->i * zx[i__3].r;
	zx[i__2].r = z__1.r, zx[i__2].i = z__1.i;
	ix += *incx;
/* L10: */
    }
    return 0;

/*        code for increment equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	z__1.r = za->r * zx[i__3].r - za->i * zx[i__3].i, z__1.i = za->r * zx[
		i__3].i + za->i * zx[i__3].r;
	zx[i__2].r = z__1.r, zx[i__2].i = z__1.i;
/* L30: */
    }
    return 0;
} /* zscal_ */

/* Subroutine */ int zswap_(n, zx, incx, zy, incy)
integer *n;
doublecomplex *zx;
integer *incx;
doublecomplex *zy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i;
    static doublecomplex ztemp;
    static integer ix, iy;


/*     interchanges two vectors. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */


    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       code for unequal increments or equal increments not equal */
/*         to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = ix;
	ztemp.r = zx[i__2].r, ztemp.i = zx[i__2].i;
	i__2 = ix;
	i__3 = iy;
	zx[i__2].r = zy[i__3].r, zx[i__2].i = zy[i__3].i;
	i__2 = iy;
	zy[i__2].r = ztemp.r, zy[i__2].i = ztemp.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */
L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	ztemp.r = zx[i__2].r, ztemp.i = zx[i__2].i;
	i__2 = i;
	i__3 = i;
	zx[i__2].r = zy[i__3].r, zx[i__2].i = zy[i__3].i;
	i__2 = i;
	zy[i__2].r = ztemp.r, zy[i__2].i = ztemp.i;
/* L30: */
    }
    return 0;
} /* zswap_ */

doublereal dcabs1_(z)
doublecomplex *z;
{
    /* System generated locals */
    doublereal ret_val;
    static doublecomplex equiv_0[1];

    /* Local variables */
#define t ((doublereal *)equiv_0)
#define zz (equiv_0)

    zz->r = z->r, zz->i = z->i;
    ret_val = abs(t[0]) + abs(t[1]);
    return ret_val;
} /* dcabs1_ */

#undef zz
#undef t


