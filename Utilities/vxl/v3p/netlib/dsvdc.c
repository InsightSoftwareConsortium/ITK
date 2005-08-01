#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/*
 * Calling this ensures that the operands are spilled to
 * memory and thus avoids excessive precision when compiling
 * for x86 with heavy optimization (gcc). It is better to do
 * this than to turn on -ffloat-store.
 */
static int fsm_ieee_doubles_equal(const doublereal *x, const doublereal *y);

/* Table of constant values */
static integer c__1 = 1;
static doublereal c_m1 = -1.;

/* Subroutine */ void dsvdc_(x, ldx, n, p, s, e, u, ldu, v, ldv, work, job, info)
doublereal *x;
const integer *ldx, *n, *p;
doublereal *s, *e, *u;
const integer *ldu;
doublereal *v;
const integer *ldv;
doublereal *work;
const integer *job;
integer *info;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer kase, jobu, iter;
    static doublereal test;
    static doublereal b, c;
    static doublereal f, g;
    static integer i, j, k, l, m;
    static doublereal t, scale;
    static doublereal shift;
    static integer maxit;
    static logical wantu, wantv;
    static doublereal t1, ztest, el;
    static doublereal cs;
    static integer mm, ls;
    static doublereal sl;
    static integer lu;
    static doublereal sm, sn;
    static integer lp1, nct, ncu, nrt;
    static doublereal emm1, smm1;

/*     dsvdc is a subroutine to reduce a double precision nxp matrix x  */
/*     by orthogonal transformations u and v to diagonal form.  The     */
/*     diagonal elements s(i) are the singular values of x.  The        */
/*     columns of u are the corresponding left singular vectors,        */
/*     and the columns of v the right singular vectors.                 */
/*                                                                      */
/*     on entry                                                         */
/*                                                                      */
/*         x         double precision(ldx,p), where ldx.ge.n.           */
/*                   x contains the matrix whose singular value         */
/*                   decomposition is to be computed.  x is             */
/*                   destroyed by dsvdc.                                */
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
/*                   ldu is the leading dimension of the array u.       */
/*                   (see below).                                       */
/*                                                                      */
/*         ldv       integer.                                           */
/*                   ldv is the leading dimension of the array v.       */
/*                   (see below).                                       */
/*                                                                      */
/*         work      double precision(n).                               */
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
/*                        a.ge.2    return the first min(n,p) singular  */
/*                                  vectors in u.                       */
/*                        b.eq.0    do not compute the right singular   */
/*                                  vectors.                            */
/*                        b.eq.1    return the right singular vectors   */
/*                                  in v.                               */
/*                                                                      */
/*     on return                                                        */
/*                                                                      */
/*         s         double precision(mm), where mm=min(n+1,p).         */
/*                   the first min(n,p) entries of s contain the        */
/*                   singular values of x arranged in descending        */
/*                   order of magnitude.                                */
/*                                                                      */
/*         e         double precision(p).                               */
/*                   e ordinarily contains zeros.  however see the      */
/*                   discussion of info for exceptions.                 */
/*                                                                      */
/*         u         double precision(ldu,k), where ldu.ge.n.  if       */
/*                               joba.eq.1 then k.eq.n, if joba.ge.2    */
/*                               then k.eq.min(n,p).                    */
/*                   u contains the matrix of left singular vectors.    */
/*                   u is not referenced if joba.eq.0.  if n.le.p       */
/*                   or if joba.eq.2, then u may be identified with x   */
/*                   in the subroutine call.                            */
/*                                                                      */
/*         v         double precision(ldv,p), where ldv.ge.p.           */
/*                   v contains the matrix of right singular vectors.   */
/*                   v is not referenced if job.eq.0.  if p.le.n,       */
/*                   then v may be identified with x in the             */
/*                   subroutine call.                                   */
/*                                                                      */
/*         info      integer.                                           */
/*                   the singular values (and their corresponding       */
/*                   singular vectors) s(info+1),s(info+2),...,s(m)     */
/*                   are correct (here m=min(n,p)).  thus if            */
/*                   info.eq.0, all the singular values and their       */
/*                   vectors are correct.  in any event, the matrix     */
/*                   b = trans(u)*x*v is the bidiagonal matrix          */
/*                   with the elements of s on its diagonal and the     */
/*                   elements of e on its super-diagonal (trans(u)      */
/*                   is the transpose of u).  thus the singular         */
/*                   values of x and b are the same.                    */
/*                                                                      */
/*     linpack. this version dated 08/14/78 .                           */
/*              correction made to shift 2/84.                          */
/*     g.w. stewart, university of maryland, argonne national lab.      */

/*     dsvdc uses the following functions and subprograms. */
/*                                                         */
/*     external drot                                       */
/*     blas daxpy,ddot,dscal,dswap,dnrm2,drotg             */
/*     fortran dabs,dmax1,max0,min0,mod,dsqrt              */

/*     set the maximum number of iterations. */

    maxit = 1000;

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
    nct = min(*n-1,*p);
    nrt = max(0,min(*p-2,*n));
    lu = max(nct,nrt);
    for (l = 0; l < lu; ++l) {
        lp1 = l+1;
        if (lp1 > nct) {
            goto L20;
        }

/*           compute the transformation for the l-th column and */
/*           place the l-th diagonal in s(l). */

        i__1 = *n - l;
        s[l] = dnrm2_(&i__1, &x[l + l * *ldx], &c__1);
        if (s[l] == 0.) {
            goto L10;
        }
        if (x[l + l * *ldx] != 0.) {
            s[l] = d_sign(&s[l], &x[l + l * *ldx]);
        }
        i__1 = *n - l;
        d__1 = 1. / s[l];
        dscal_(&i__1, &d__1, &x[l + l * *ldx], &c__1);
        x[l + l * *ldx] += 1.;
L10:
        s[l] = -s[l];
L20:
        for (j = lp1; j < *p; ++j) {

/*              apply the transformation. */

            if (l < nct && s[l] != 0.) {
                i__1 = *n - l;
                t = -ddot_(&i__1, &x[l + l * *ldx], &c__1, &x[l + j * *ldx], &c__1) / x[l + l * *ldx];
                daxpy_(&i__1, &t, &x[l + l * *ldx], &c__1, &x[l + j * *ldx], &c__1);
            }

/*           place the l-th row of x into  e for the */
/*           subsequent calculation of the row transformation. */

            e[j] = x[l + j * *ldx];
        }

/*           place the transformation in u for subsequent back */
/*           multiplication. */

        if (wantu && l < nct)
        for (i = l; i < *n; ++i) {
            u[i + l * *ldu] = x[i + l * *ldx];
        }
        if (lp1 > nrt) {
            continue;
        }

/*           compute the l-th row transformation and place the */
/*           l-th super-diagonal in e(l). */

        i__1 = *p - lp1;
        e[l] = dnrm2_(&i__1, &e[lp1], &c__1);
        if (e[l] == 0.) {
            goto L80;
        }
        if (e[lp1] != 0.) {
            e[l] = d_sign(&e[l], &e[lp1]);
        }
        i__1 = *p - lp1;
        d__1 = 1. / e[l];
        dscal_(&i__1, &d__1, &e[lp1], &c__1);
        e[lp1] += 1.;
L80:
        e[l] = -e[l];
        if (l+2 > *n || e[l] == 0.) {
            goto L120;
        }

/*              apply the transformation. */

        for (i = lp1; i < *n; ++i) {
            work[i] = 0.;
        }
        for (j = lp1; j < *p; ++j) {
            i__1 = *n - lp1;
            daxpy_(&i__1, &e[j], &x[lp1 + j * *ldx], &c__1, &work[lp1], &c__1);
        }
        for (j = lp1; j < *p; ++j) {
            i__1 = *n - lp1;
            d__1 = -e[j] / e[lp1];
            daxpy_(&i__1, &d__1, &work[lp1], &c__1, &x[lp1 + j * *ldx], &c__1);
        }
L120:

/*              place the transformation in v for subsequent */
/*              back multiplication. */

        if (wantv)
        for (i = lp1; i < *p; ++i) {
            v[i + l * *ldv] = e[i];
        }
    }

/*     set up the final bidiagonal matrix or order m. */

    m = min(*p-1,*n);
    if (nct < *p) {
        s[nct] = x[nct + nct * *ldx];
    }
    if (*n < m+1) {
        s[m] = 0.;
    }
    if (nrt < m) {
        e[nrt] = x[nrt + m * *ldx];
    }
    e[m] = 0.;

/*     if required, generate u. */

    if (wantu)
    for (j = nct; j < ncu; ++j) {
        for (i = 0; i < *n; ++i) {
            u[i + j * *ldu] = 0.;
        }
        u[j + j * *ldu] = 1.;
    }
    if (wantu)
    for (l = nct-1; l >= 0; --l) {
        if (s[l] == 0.) {
            for (i = 0; i < *n; ++i) {
                u[i + l * *ldu] = 0.;
            }
            u[l + l * *ldu] = 1.;
            continue;
        }
        for (j = l+1; j < ncu; ++j) {
            i__1 = *n - l;
            t = -ddot_(&i__1, &u[l + l * *ldu], &c__1, &u[l + j * *ldu], &c__1) / u[l + l * *ldu];
            daxpy_(&i__1, &t, &u[l + l * *ldu], &c__1, &u[l + j * *ldu], &c__1);
        }
        i__1 = *n - l;
        dscal_(&i__1, &c_m1, &u[l + l * *ldu], &c__1);
        u[l + l * *ldu] += 1.;
        for (i = 0; i < l; ++i) {
            u[i + l * *ldu] = 0.;
        }
    }

/*     if it is required, generate v. */

    if (wantv)
    for (l = *p-1; l >= 0; --l) {
        lp1 = l+1;
        if (l < nrt && e[l] != 0.)
        for (j = lp1; j < *p; ++j) {
            i__1 = *p - lp1;
            t = -ddot_(&i__1, &v[lp1 + l * *ldv], &c__1, &v[lp1 + j * *ldv], &c__1) / v[lp1 + l * *ldv];
            daxpy_(&i__1, &t, &v[lp1 + l * *ldv], &c__1, &v[lp1 + j * *ldv], &c__1);
        }
        for (i = 0; i < *p; ++i) {
            v[i + l * *ldv] = 0.;
        }
        v[l + l * *ldv] = 1.;
    }

/*     main iteration loop for the singular values. */

    mm = m;
    iter = 0;
L360:

/*        quit if all the singular values have been found. */

    if (m < 0) {
        return;
    }

/*        if too many iterations have been performed, set */
/*        flag and return. */

    if (iter >= maxit) {
        *info = m+1;
        return;
    }

/*        this section of the program inspects for */
/*        negligible elements in the s and e arrays.  on */
/*        completion the variables kase and l are set as follows. */

/*           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m */
/*           kase = 2     if s(l) is negligible and l.lt.m */
/*           kase = 3     if e(l-1) is negligible, l.lt.m, and */
/*                        s(l), ..., s(m) are not negligible (qr step). */
/*           kase = 4     if e(m-1) is negligible (convergence). */

    for (l = m-1; l >= 0; --l) {
        test = abs(s[l]) + abs(s[l+1]);
        ztest = test + abs(e[l]);
        if (fsm_ieee_doubles_equal(&ztest, &test)) {
/* WAS: if (ztest == test) { */
            e[l] = 0.;
            break;
        }
    }
    if (l == m-1) {
        kase = 4;
        goto L480;
    }
    for (ls = m; ls > l; --ls) {
        test = 0.;
        if (ls != m) {
            test += abs(e[ls]);
        }
        if (ls != l+1) {
            test += abs(e[ls-1]);
        }
        ztest = test + abs(s[ls]);
        if (fsm_ieee_doubles_equal(&ztest, &test)) {
/* WAS: if (ztest == test) { */
            s[ls] = 0.;
            break;
        }
    }
    if (ls == l) {
        kase = 3;
    }
    else if (ls == m) {
        kase = 1;
    }
    else {
        kase = 2;
        l = ls;
    }
L480:
    ++l;

/*        perform the task indicated by kase. */

    switch ((int)kase) {
        case 1:  goto L490;
        case 2:  goto L520;
        case 3:  goto L540;
        case 4:  goto L570;
    }

/*        deflate negligible s(m). */

L490:
    f = e[m-1];
    e[m-1] = 0.;
    for (k = m-1; k >= l; --k) {
        t1 = s[k];
        drotg_(&t1, &f, &cs, &sn);
        s[k] = t1;
        if (k != l) {
            f = -sn * e[k-1];
            e[k-1] *= cs;
        }
        if (wantv) {
            drot_(p, &v[k * *ldv], &c__1, &v[m * *ldv], &c__1, &cs, &sn);
        }
    }
    goto L360;

/*        split at negligible s(l). */

L520:
    f = e[l-1];
    e[l-1] = 0.;
    for (k = l; k <= m; ++k) {
        t1 = s[k];
        drotg_(&t1, &f, &cs, &sn);
        s[k] = t1;
        f = -sn * e[k];
        e[k] *= cs;
        if (wantu) {
            drot_(n, &u[k * *ldu], &c__1, &u[(l-1) * *ldu], &c__1, &cs, &sn);
        }
    }
    goto L360;

/*        perform one qr step. */

L540:

/*           calculate the shift. */

    scale = max(max(max(max(abs(s[m]),abs(s[m-1])),abs(e[m-1])),abs(s[l])),abs(e[l]));
    sm = s[m] / scale;
    smm1 = s[m-1] / scale;
    emm1 = e[m-1] / scale;
    sl = s[l] / scale;
    el = e[l] / scale;
    b = ((smm1 + sm) * (smm1 - sm) + emm1 * emm1) / 2.;
    c = sm * emm1; c *= c;
    if (b == 0. && c == 0.) {
        shift = 0.;
    }
    else {
        shift = sqrt(b * b + c);
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
            e[k-1] = f;
        }
        f = cs * s[k] + sn * e[k];
        e[k] = cs * e[k] - sn * s[k];
        g = sn * s[k+1];
        s[k+1] *= cs;
        if (wantv) {
            drot_(p, &v[k * *ldv], &c__1, &v[(k+1) * *ldv], &c__1, &cs, &sn);
        }
        drotg_(&f, &g, &cs, &sn);
        s[k] = f;
        f = cs * e[k] + sn * s[k+1];
        s[k+1] = -sn * e[k] + cs * s[k+1];
        g = sn * e[k+1];
        e[k+1] *= cs;
        if (wantu && k+1 < *n) {
            drot_(n, &u[k * *ldu], &c__1, &u[(k+1) * *ldu], &c__1, &cs, &sn);
        }
    }
    e[m-1] = f;
    ++iter;
    goto L360;

/*        convergence. */

L570:

/*           make the singular value  positive. */

    if (s[l] < 0.) {
        s[l] = -s[l];
        if (wantv) {
            dscal_(p, &c_m1, &v[l * *ldv], &c__1);
        }
    }

/*           order the singular value. */

L590:
    if (l == mm) {
        goto L600;
    }
    if (s[l] >= s[l+1]) {
        goto L600;
    }
    t = s[l];
    s[l] = s[l+1];
    ++l;
    s[l] = t;
    if (wantv && l < *p) {
        dswap_(p, &v[(l-1) * *ldv], &c__1, &v[l * *ldv], &c__1);
    }
    if (wantu && l < *n) {
        dswap_(n, &u[(l-1) * *ldu], &c__1, &u[l * *ldu], &c__1);
    }
    goto L590;
L600:
    iter = 0;
    --m;
    goto L360;
} /* dsvdc_ */

static int fsm_ieee_doubles_equal(const doublereal *x, const doublereal *y)
{
  return *x == *y;
}
