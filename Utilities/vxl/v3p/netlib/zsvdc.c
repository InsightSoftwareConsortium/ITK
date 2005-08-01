#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */
/*     and moved zswap() zscal() zdrot() zdotc() zaxpy() to separate files */

/*
 * Calling this ensures that the operands are spilled to
 * memory and thus avoids excessive precision when compiling
 * for x86 with heavy optimization (gcc). It is better to do
 * this than to turn on -ffloat-store.
 */
static int fsm_ieee_doubles_equal(const doublereal *x, const doublereal *y);

/* Table of constant values */
static integer c__1 = 1;
static doublecomplex c_1 = {1.,0.};
static doublecomplex c_m1 = {-1.,0.};

/* ====================================================================== */
/* NIST Guide to Available Math Software. */
/* Fullsource for module ZSVDC from package LINPACK. */
/* Retrieved from NETLIB on Fri May  9 10:03:02 1997. */
/* ====================================================================== */
/* Subroutine */ void zsvdc_(x, ldx, n, p, s, e, u, ldu, v, ldv, work, job, info)
doublecomplex *x;
const integer *ldx, *n, *p;
doublecomplex *s, *e, *u;
const integer *ldu;
doublecomplex *v;
const integer *ldv;
doublecomplex *work;
const integer *job;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;
    doublecomplex z__1;

    /* Local variables */
    static integer jobu, iter;
    static doublereal test;
    static doublereal b, c;
    static doublereal f, g;
    static integer i, j, k, l, m;
    static doublecomplex r, t;
    static doublereal scale;
    static doublereal shift;
    static integer maxit;
    static logical wantu, wantv;
    static doublereal t1, ztest;
    static doublereal el;
    static doublereal cs;
    static integer mm, ls;
    static doublereal sl;
    static integer lu;
    static doublereal sm, sn;
    static integer nct, ncu, nrt;
    static doublereal emm1, smm1;

/************************************************************************/
/*                                                                      */
/*     zsvdc is a subroutine to reduce a complex*16 nxp matrix x by     */
/*     unitary transformations u and v to diagonal form.  the           */
/*     diagonal elements s(i) are the singular values of x.  the        */
/*     columns of u are the corresponding left singular vectors,        */
/*     and the columns of v the right singular vectors.                 */
/*                                                                      */
/*     on entry                                                         */
/*                                                                      */
/*         x         complex*16(ldx,p), where ldx.ge.n.                 */
/*                   x contains the matrix whose singular value         */
/*                   decomposition is to be computed.  x is             */
/*                   destroyed by zsvdc.                                */
/*                                                                      */
/*         ldx       integer.                                           */
/*                   ldx is the leading dimension of the array x.       */
/*                                                                      */
/*         n         integer.                                           */
/*                   n is the number of rows of the matrix x.           */
/*                                                                      */
/*         p         integer.                                           */
/*                   p is the number of columns of the matrix x.        */
/*                                                                      */
/*         ldu       integer.                                           */
/*                   ldu is the leading dimension of the array u        */
/*                   (see below).                                       */
/*                                                                      */
/*         ldv       integer.                                           */
/*                   ldv is the leading dimension of the array v        */
/*                   (see below).                                       */
/*                                                                      */
/*         work      complex*16(n).                                     */
/*                   work is a scratch array.                           */
/*                                                                      */
/*         job       integer.                                           */
/*                   job controls the computation of the singular       */
/*                   vectors.  it has the decimal expansion ab          */
/*                   with the following meaning                         */
/*                                                                      */
/*                        a.eq.0    do not compute the left singular    */
/*                                  vectors.                            */
/*                        a.eq.1    return the n left singular vectors  */
/*                                  in u.                               */
/*                        a.ge.2    returns the first min(n,p)          */
/*                                  left singular vectors in u.         */
/*                        b.eq.0    do not compute the right singular   */
/*                                  vectors.                            */
/*                        b.eq.1    return the right singular vectors   */
/*                                  in v.                               */
/*                                                                      */
/*     on return                                                        */
/*                                                                      */
/*         s         complex*16(mm), where mm=min(n+1,p).               */
/*                   the first min(n,p) entries of s contain the        */
/*                   singular values of x arranged in descending        */
/*                   order of magnitude.                                */
/*                                                                      */
/*         e         complex*16(p).                                     */
/*                   e ordinarily contains zeros.  however see the      */
/*                   discussion of info for exceptions.                 */
/*                                                                      */
/*         u         complex*16(ldu,k), where ldu.ge.n.  if joba.eq.1   */
/*                                   then k.eq.n, if joba.ge.2 then     */
/*                                   k.eq.min(n,p).                     */
/*                   u contains the matrix of left singular vectors.    */
/*                   u is not referenced if joba.eq.0.  if n.le.p       */
/*                   or if joba.gt.2, then u may be identified with x   */
/*                   in the subroutine call.                            */
/*                                                                      */
/*         v         complex*16(ldv,p), where ldv.ge.p.                 */
/*                   v contains the matrix of right singular vectors.   */
/*                   v is not referenced if jobb.eq.0.  if p.le.n,      */
/*                   then v may be identified whth x in the             */
/*                   subroutine call.                                   */
/*                                                                      */
/*         info      integer.                                           */
/*                   the singular values (and their corresponding       */
/*                   singular vectors) s(info+1),s(info+2),...,s(m)     */
/*                   are correct (here m=min(n,p)).  thus if            */
/*                   info.eq.0, all the singular values and their       */
/*                   vectors are correct.  in any event, the matrix     */
/*                   b = ctrans(u)*x*v is the bidiagonal matrix         */
/*                   with the elements of s on its diagonal and the     */
/*                   elements of e on its super-diagonal (ctrans(u)     */
/*                   is the conjugate-transpose of u).  thus the        */
/*                   singular values of x and b are the same.           */
/*                                                                      */
/************************************************************************/

/*     linpack. this version dated 03/19/79 .                           */
/*              correction to shift calculation made 2/85.              */
/*     g.w. stewart, university of maryland, argonne national lab.      */
/*                                                                      */
/*     zsvdc uses the following functions and subprograms.              */
/*                                                                      */
/*     external zdrot                                                   */
/*     blas zaxpy,zdotc,zscal,zswap,dznrm2,drotg                        */
/*     fortran dmax1,zabs,dcmplx                                        */
/*     fortran dconjg,max0,min0,mod,dsqrt                               */

/*     set the maximum number of iterations. */
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
    nct = min(*n - 1, *p);
    nrt = max(0, min(*p - 2, *n));
    lu = max(nct,nrt);

    for (l = 0; l < lu; ++l) {
        if (l > nct-1) {
            goto L20;
        }

/*           compute the transformation for the l-th column and */
/*           place the l-th diagonal in s(l). */

        i__1 = *n - l;
        s[l].r = dznrm2_(&i__1, &x[l+l* *ldx], &c__1);
        s[l].i = 0.;
        if (s[l].r == 0.) {
            goto L10;
        }
        i__2 = l + l * *ldx; /* index [l,l] */
        if (x[i__2].r != 0. || x[i__2].i != 0.) {
            d__1 = z_abs(&s[l]);
            d__2 = z_abs(&x[i__2]);
            s[l].r = d__1 * x[i__2].r / d__2,
            s[l].i = d__1 * x[i__2].i / d__2;
        }
        z_div(&z__1, &c_1, &s[l]);
        i__1 = *n - l;
        zscal_(&i__1, &z__1, &x[i__2], &c__1);
        x[i__2].r += 1.;
L10:
        s[l].r = -s[l].r, s[l].i = -s[l].i;
L20:
        for (j = l+1; j < *p; ++j) {

/*              apply the transformation. */

            if (l < nct && (s[l].r != 0. || s[l].i != 0.)) {
                i__1 = *n - l;
                i__2 = l + l * *ldx; /* index [l,l] */
                zdotc_(&t, &i__1, &x[i__2], &c__1, &x[l+j* *ldx], &c__1);
                t.r = -t.r, t.i = -t.i;
                z_div(&t, &t, &x[i__2]);
                zaxpy_(&i__1, &t, &x[i__2], &c__1, &x[l+j* *ldx], &c__1);
            }

/*           place the l-th row of x into e for the */
/*           subsequent calculation of the row transformation. */

            d_cnjg(&e[j], &x[l+j* *ldx]);
        }

/*           place the transformation in u for subsequent back */
/*           multiplication. */

        if (wantu && l < nct)
        for (i = l; i < *n; ++i) {
            i__1 = i + l * *ldu; /* index [i,l] */
            i__2 = i + l * *ldx; /* index [i,l] */
            u[i__1].r = x[i__2].r, u[i__1].i = x[i__2].i;
        }

        if (l >= nrt) {
            continue; /* next l */
        }

/*           compute the l-th row transformation and place the */
/*           l-th super-diagonal in e(l). */

        i__1 = *p - l - 1;
        e[l].r = dznrm2_(&i__1, &e[l+1], &c__1);
        e[l].i = 0.;
        if (e[l].r != 0.) {
            if (e[l+1].r != 0. || e[l+1].i != 0.) {
                d__1 = z_abs(&e[l]); d__2 = z_abs(&e[l+1]);
                e[l].r = d__1 * e[l+1].r / d__2,
                e[l].i = d__1 * e[l+1].i / d__2;
            }
            i__1 = *p - l - 1;
            z_div(&z__1, &c_1, &e[l]);
            zscal_(&i__1, &z__1, &e[l+1], &c__1);
            e[l+1].r += 1.;
        }
        e[l].r = -e[l].r; /* e[l] = - conj(e[l]) */
        if (l >= *n-1 || (e[l].r == 0. && e[l].i == 0.)) {
            goto L120;
        }

/*              apply the transformation. */

        for (i = l+1; i < *n; ++i) {
            work[i].r = 0., work[i].i = 0.;
        }
        for (j = l+1; j < *p; ++j) {
            i__1 = *n - l - 1;
            zaxpy_(&i__1, &e[j], &x[l+1 +j* *ldx], &c__1, &work[l+1], &c__1);
        }
        for (j = l+1; j < *p; ++j) {
            z__1.r = -e[j].r, z__1.i = -e[j].i;
            z_div(&z__1, &z__1, &e[l+1]);
            z__1.i = -z__1.i; /* d_cnjg(&z__1, &z__1); */
            i__1 = *n - l - 1;
            zaxpy_(&i__1, &z__1, &work[l+1], &c__1, &x[l+1 +j* *ldx], &c__1);
        }

/*              place the transformation in v for subsequent */
/*              back multiplication. */

L120:
        if (wantv)
        for (i = l+1; i < *p; ++i) {
            i__1 = i + l * *ldv; /* index [i,l] */
            v[i__1].r = e[i].r, v[i__1].i = e[i].i;
        }
    }

/*     set up the final bidiagonal matrix or order m. */

    m = min(*p-1, *n);
    if (nct < *p) {
        i__1 = nct * (*ldx+1); /* index [nct,nct] */
        s[nct].r = x[i__1].r, s[nct].i = x[i__1].i;
    }
    if (*n-1 < m) {
        s[m].r = 0., s[m].i = 0.;
    }
    if (nrt < m) {
        i__1 = nrt + m * *ldx; /* index [nrt,m] */
        e[nrt].r = x[i__1].r, e[nrt].i = x[i__1].i;
    }
    e[m].r = 0., e[m].i = 0.;

/*     if required, generate u. */

    if (wantu)
    for (j = nct; j < ncu; ++j) {
        for (i = 0; i < *n; ++i) {
            i__1 = i + j * *ldu; /* index [i,j] */
            u[i__1].r = 0., u[i__1].i = 0.;
        }
        i__1 = j + j * *ldu; /* index [j,j] */
        u[i__1].r = 1., u[i__1].i = 0.;
    }
    if (wantu)
    for (l = nct-1; l >= 0; --l) {
        if (s[l].r == 0. && s[l].i == 0.) {
            for (i = 0; i < *n; ++i) {
                i__1 = i + l * *ldu; /* index [i,l] */
                u[i__1].r = 0., u[i__1].i = 0.;
            }
            i__1 = l + l * *ldu; /* index [l,l] */
            u[i__1].r = 1., u[i__1].i = 0.;
            continue; /* next l */
        }
        i__1 = *n - l;
        i__2 = l + l * *ldu; /* index [l,l] */
        for (j = l+1; j < ncu; ++j) {
            zdotc_(&t, &i__1, &u[i__2], &c__1, &u[l+j* *ldu], &c__1);
            t.r = -t.r, t.i = -t.i;
            z_div(&t, &t, &u[i__2]);
            zaxpy_(&i__1, &t, &u[i__2], &c__1, &u[l+j* *ldu], &c__1);
        }
        zscal_(&i__1, &c_m1, &u[i__2], &c__1);
        u[i__2].r += 1.;
        for (i = 0; i < l; ++i) {
            i__1 = i + l * *ldu; /* index [i,l] */
            u[i__1].r = 0., u[i__1].i = 0.;
        }
    }

/*     if it is required, generate v. */

    if (wantv)
    for (l = *p-1; l >= 0; --l) {
        if (l < nrt && (e[l].r != 0. || e[l].i != 0.))
        for (j = l+1; j < *p; ++j) {
            i__1 = *p - l - 1;
            i__2 = l+1 + l * *ldv; /* index [l+1,l] */
            zdotc_(&t, &i__1, &v[i__2], &c__1, &v[l+1 +j* *ldv], &c__1);
            t.r = -t.r, t.i = -t.i;
            z_div(&t, &t, &v[i__2]);
            zaxpy_(&i__1, &t, &v[i__2], &c__1, &v[l+1 +j* *ldv], &c__1);
        }
        for (i = 0; i < *p; ++i) {
            i__1 = i + l * *ldv; /* index [i,l] */
            v[i__1].r = 0., v[i__1].i = 0.;
        }
        i__1 = l + l * *ldv; /* index [l,l] */
        v[i__1].r = 1., v[i__1].i = 0.;
    }

/*     transform s and e so that they are double precision. */

    for (i = 0; i <= m; ++i) {
        if (s[i].r != 0. || s[i].i != 0.) {
            t.r = z_abs(&s[i]), t.i = 0.;
            z_div(&r, &s[i], &t);
            s[i].r = t.r, s[i].i = t.i;
            if (i < m) {
                z_div(&e[i], &e[i], &r);
            }
            if (wantu) {
                zscal_(n, &r, &u[i* *ldu], &c__1);
            }
        }
        if (i == m) {
            break; /* last i */
        }
        if (e[i].r == 0. && e[i].i == 0.) {
            continue; /* next i */
        }
        t.r = z_abs(&e[i]), t.i = 0.;
        z_div(&r, &t, &e[i]);
        e[i].r = t.r, e[i].i = t.i;
        z__1.r = s[i+1].r * r.r - s[i+1].i * r.i,
        z__1.i = s[i+1].r * r.i + s[i+1].i * r.r;
        s[i+1].r = z__1.r, s[i+1].i = z__1.i;
        if (wantv) {
            zscal_(p, &r, &v[(i+1)* *ldv], &c__1);
        }
    }

/*     main iteration loop for the singular values. */

    mm = m;
    iter = 0;

/*        quit if all the singular values have been found. */

L400:
    if (m == -1) {
        return; /* exit from zsvdc */
    }

/*        if too many iterations have been performed, set */
/*        flag and return. */

    if (iter >= maxit) {
        *info = m+1;
        return; /* exit from zsvdc */
    }

/*        this section of the program inspects for */
/*        negligible elements in the s and e arrays.  on */
/*        completion the variables kase and l are set as follows. */

/*           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m */
/*           kase = 2     if s(l) is negligible and l.lt.m */
/*           kase = 3     if e(l-1) is negligible, l.lt.m, and */
/*                        s(l), ..., s(m) are not negligible (qr step). */
/*           kase = 4     if e(m-1) is negligible (convergence). */

    for (l = m; l > 0; --l) {
        test = z_abs(&s[l-1]) + z_abs(&s[l]);
        ztest = test + z_abs(&e[l-1]);
        if (fsm_ieee_doubles_equal(&ztest, &test)) {
/* WAS: if (ztest == test) { */
            e[l-1].r = 0., e[l-1].i = 0.;
            break; /* last l */
        }
    }
    if (l == m) { /* kase = 4 */ /*        convergence. */

/*           make the singular value  positive */

        if (s[l].r < 0.) {
            s[l].r = -s[l].r, s[l].i = -s[l].i;
            if (wantv) {
                zscal_(p, &c_m1, &v[l* *ldv], &c__1);
            }
        }

/*           order the singular value. */

        while (l != mm && s[l].r < s[l+1].r) {
            t.r = s[l].r, t.i = s[l].i;
            s[l].r = s[l+1].r, s[l].i = s[l+1].i;
            s[l+1].r = t.r, s[l+1].i = t.i;
            if (wantv && l < *p-1) {
                zswap_(p, &v[l* *ldv], &c__1, &v[(l+1)* *ldv], &c__1);
            }
            if (wantu && l < *n-1) {
                zswap_(n, &u[l* *ldu], &c__1, &u[(l+1)* *ldu], &c__1);
            }
            ++l;
        }
        iter = 0;
        --m;
        goto L400;
    }
    for (ls = m; ls >= l; --ls) {
        test = 0.;
        if (ls != m) {
            test += z_abs(&e[ls]);
        }
        if (ls != l) {
            test += z_abs(&e[ls-1]);
        }
        ztest = test + z_abs(&s[ls]);
        if (fsm_ieee_doubles_equal(&ztest, &test)) {
/* WAS: if (ztest == test) { */
            s[ls].r = 0., s[ls].i = 0.;
            break; /* last ls */
        }
    }
    if (ls == l-1) { /* kase = 3 */ /*        perform one qr step. */

/*           calculate the shift. */

        scale =            z_abs(&s[m]),
        scale = max(scale, z_abs(&s[m-1])),
        scale = max(scale, z_abs(&e[m-1])),
        scale = max(scale, z_abs(&s[l])),
        scale = max(scale, z_abs(&e[l]));
        sm = s[m].r / scale;
        smm1 = s[m-1].r / scale;
        emm1 = e[m-1].r / scale;
        sl = s[l].r / scale;
        el = e[l].r / scale;
        b = ((smm1+sm) * (smm1-sm) + emm1*emm1) / 2.;
        c = sm * emm1; c *= c;
        shift = 0.;
        if (b != 0. || c != 0.) {
            shift = sqrt(b*b + c);
            if (b < 0.) {
                shift = -shift;
            }
            shift = c / (b + shift);
        }
        f = (sl + sm) * (sl - sm) + shift;
        g = sl * el;

/*           chase zeros. */

        for (k = l; k < m; ++k) {
            drotg_(&f, &g, &cs, &sn);
            if (k != l) {
                e[k-1].r = f, e[k-1].i = 0.;
            }
            f      = cs * s[k].r + sn * e[k].r;
            e[k].r = cs * e[k].r - sn * s[k].r,
            e[k].i = cs * e[k].i - sn * s[k].i;
            g = sn * s[k+1].r;
            s[k+1].r *= cs, s[k+1].i *= cs;
            if (wantv) {
                zdrot_(p, &v[k* *ldv], &c__1, &v[(k+1)* *ldv], &c__1, &cs, &sn);
            }
            drotg_(&f, &g, &cs, &sn);
            s[k].r = f, s[k].i = 0.;
            f        =  cs * e[k].r + sn * s[k+1].r;
            s[k+1].r = -sn * e[k].r + cs * s[k+1].r,
            s[k+1].i = -sn * e[k].i + cs * s[k+1].i;
            g = sn * e[k+1].r;
            e[k+1].r *= cs, e[k+1].i *= cs;
            if (wantu && k < *n-1) {
                zdrot_(n, &u[k* *ldu], &c__1, &u[(k+1)* *ldu], &c__1, &cs, &sn);
            }
        }
        e[m-1].r = f, e[m-1].i = 0.;
        ++iter;
    }
    else if (ls == m) { /* kase = 1 */ /*        deflate negligible s(m). */
        f = e[m-1].r;
        e[m-1].r = 0., e[m-1].i = 0.;
        for (k = m-1; k >= l; --k) {
            t1 = s[k].r;
            drotg_(&t1, &f, &cs, &sn);
            s[k].r = t1, s[k].i = 0.;
            if (k != l) {
                f = -sn * e[k-1].r;
                e[k-1].r *= cs, e[k-1].i *= cs;
            }
            if (wantv) {
                zdrot_(p, &v[k* *ldv], &c__1, &v[m* *ldv], &c__1, &cs, &sn);
            }
        }
    }
    else { /* kase = 2 */ /*        split at negligible s(l). */
        /* l = ls + 1; */
        f = e[ls].r;
        e[ls].r = 0., e[ls].i = 0.;
        for (k = ls+1; k <= m; ++k) {
            t1 = s[k].r;
            drotg_(&t1, &f, &cs, &sn);
            s[k].r = t1, s[k].i = 0.;
            f = -sn * e[k].r;
            e[k].r *= cs, e[k].i *= cs;
            if (wantu) {
                zdrot_(n, &u[k* *ldu], &c__1, &u[ls* *ldu], &c__1, &cs, &sn);
            }
        }
    }
    goto L400;

} /* zsvdc_ */

static int fsm_ieee_doubles_equal(const double *x, const double *y)
{
  return *x == *y;
}
