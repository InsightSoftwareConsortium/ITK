#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/*
 * Calling this ensures that the operands are spilled to
 * memory and thus avoids excessive precision when compiling
 * for x86 with heavy optimization (gcc). It is better to do
 * this than to turn on -ffloat-store.
 */
static int fsm_ieee_floats_equal(const real *x, const real *y);

/* Table of constant values */
static integer c__1 = 1;
static complex c_1 = {1.f,0.f};
static complex c_m1 = {-1.f,0.f};

/* Subroutine */ void csvdc_(x, ldx, n, p, s, e, u, ldu, v, ldv, work, job, info)
complex *x;
const integer *ldx, *n, *p;
complex *s, *e, *u;
const integer *ldu;
complex *v;
const integer *ldv;
complex *work;
const integer *job;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;
    complex q__1;

    /* Local variables */
    static integer jobu, iter;
    static real test;
    static real b, c;
    static real f, g;
    static integer i, j, k, l, m;
    static complex r, t;
    static real scale;
    static real shift;
    static integer maxit;
    static logical wantu, wantv;
    static real t1, ztest;
    static real el;
    static real cs;
    static integer mm, ls;
    static real sl;
    static integer lu;
    static real sm, sn;
    static integer nct, ncu, nrt;
    static real emm1, smm1;

/************************************************************************/
/*                                                                      */
/*     csvdc is a subroutine to reduce a complex nxp matrix x by        */
/*     unitary transformations u and v to diagonal form.  the           */
/*     diagonal elements s(i) are the singular values of x.  the        */
/*     columns of u are the corresponding left singular vectors,        */
/*     and the columns of v the right singular vectors.                 */
/*                                                                      */
/*     on entry                                                         */
/*                                                                      */
/*         x         complex(ldx,p), where ldx.ge.n.                    */
/*                   x contains the matrix whose singular value         */
/*                   decomposition is to be computed.  x is             */
/*                   destroyed by csvdc.                                */
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
/*         work      complex(n).                                        */
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
/*         s         complex(mm), where mm=min(n+1,p).                  */
/*                   the first min(n,p) entries of s contain the        */
/*                   singular values of x arranged in descending        */
/*                   order of magnitude.                                */
/*                                                                      */
/*         e         complex(p).                                        */
/*                   e ordinarily contains zeros.  however see the      */
/*                   discussion of info for exceptions.                 */
/*                                                                      */
/*         u         complex(ldu,k), where ldu.ge.n.  if joba.eq.1      */
/*                                   then k.eq.n, if joba.ge.2 then     */
/*                                   k.eq.min(n,p).                     */
/*                   u contains the matrix of left singular vectors.    */
/*                   u is not referenced if joba.eq.0.  if n.le.p       */
/*                   or if joba.gt.2, then u may be identified with x   */
/*                   in the subroutine call.                            */
/*                                                                      */
/*         v         complex(ldv,p), where ldv.ge.p.                    */
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

/*     linpack. this version dated 03/19/79 . */
/*              correction to shift calculation made 2/85. */
/*     g.w. stewart, university of maryland, argonne national lab. */

/*     csvdc uses the following functions and subprograms. */
/*                                                         */
/*     external csrot                                      */
/*     blas caxpy,cdotc,cscal,cswap,scnrm2,srotg           */
/*     fortran aimag,amax1,cabs,cmplx                      */
/*     fortran conjg,max0,min0,mod,real,sqrt               */

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
        s[l].r = scnrm2_(&i__1, &x[l+l* *ldx], &c__1);
        s[l].i = 0.f;
        if (s[l].r == 0.f) {
            goto L10;
        }
        i__2 = l + l * *ldx; /* index [l,l] */
        if (x[i__2].r != 0.f || x[i__2].i != 0.f) {
            r__1 = c_abs(&s[l]);
            r__2 = c_abs(&x[i__2]);
            s[l].r = r__1 * x[i__2].r / r__2,
            s[l].i = r__1 * x[i__2].i / r__2;
        }
        c_div(&q__1, &c_1, &s[l]);
        i__1 = *n - l;
        cscal_(&i__1, &q__1, &x[i__2], &c__1);
        x[i__2].r += 1.f;
L10:
        s[l].r = -s[l].r, s[l].i = -s[l].i;
L20:
        for (j = l+1; j < *p; ++j) {

/*              apply the transformation. */

            if (l < nct && (s[l].r != 0.f || s[l].i != 0.f)) {
                i__1 = *n - l;
                i__2 = l + l * *ldx; /* index [l,l] */
                cdotc_(&t, &i__1, &x[i__2], &c__1, &x[l+j* *ldx], &c__1);
                t.r = -t.r, t.i = -t.i;
                c_div(&t, &t, &x[i__2]);
                caxpy_(&i__1, &t, &x[i__2], &c__1, &x[l+j* *ldx], &c__1);
            }

/*           place the l-th row of x into  e for the */
/*           subsequent calculation of the row transformation. */

            r_cnjg(&e[j], &x[l+j* *ldx]);
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
        e[l].r = scnrm2_(&i__1, &e[l+1], &c__1);
        e[l].i = 0.f;
        if (e[l].r != 0.f) {
            if (e[l+1].r != 0.f || e[l+1].i != 0.f) {
                r__1 = c_abs(&e[l]); r__2 = c_abs(&e[l+1]);
                e[l].r = r__1 * e[l+1].r / r__2,
                e[l].i = r__1 * e[l+1].i / r__2;
            }
            i__1 = *p - l - 1;
            c_div(&q__1, &c_1, &e[l]);
            cscal_(&i__1, &q__1, &e[l+1], &c__1);
            e[l+1].r += 1.f;
        }
        e[l].r = -e[l].r; /* e[l] = - conj(e[l]) */
        if (l >= *n-1 || (e[l].r == 0.f && e[l].i == 0.f)) {
            goto L120;
        }

/*              apply the transformation. */

        for (i = l+1; i < *n; ++i) {
            work[i].r = 0.f, work[i].i = 0.f;
        }
        for (j = l+1; j < *p; ++j) {
            i__1 = *n - l - 1;
            caxpy_(&i__1, &e[j], &x[l+1 +j* *ldx], &c__1, &work[l+1], &c__1);
        }
        for (j = l+1; j < *p; ++j) {
            q__1.r = -e[j].r, q__1.i = -e[j].i;
            c_div(&q__1, &q__1, &e[l+1]);
            q__1.i = -q__1.i; /* r_cnjg(&q__1, &q__1); */
            i__1 = *n - l - 1;
            caxpy_(&i__1, &q__1, &work[l+1], &c__1, &x[l+1 +j* *ldx], &c__1);
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
        s[m].r = 0.f, s[m].i = 0.f;
    }
    if (nrt < m) {
        i__1 = nrt + m * *ldx; /* index [nrt,m] */
        e[nrt].r = x[i__1].r, e[nrt].i = x[i__1].i;
    }
    e[m].r = 0.f, e[m].i = 0.f;

/*     if required, generate u. */

    if (wantu)
    for (j = nct; j < ncu; ++j) {
        for (i = 0; i < *n; ++i) {
            i__1 = i + j * *ldu; /* index [i,j] */
            u[i__1].r = 0.f, u[i__1].i = 0.f;
        }
        i__1 = j + j * *ldu;
        u[i__1].r = 1.f, u[i__1].i = 0.f;
    }
    if (wantu)
    for (l = nct-1; l >= 0; --l) {
        if (s[l].r == 0.f && s[l].i == 0.f) {
            for (i = 0; i < *n; ++i) {
                i__1 = i + l * *ldu; /* index [i,l] */
                u[i__1].r = 0.f, u[i__1].i = 0.f;
            }
            i__1 = l + l * *ldu; /* index [l,l] */
            u[i__1].r = 1.f, u[i__1].i = 0.f;
            continue; /* next l */
        }
        i__1 = *n - l;
        i__2 = l + l * *ldu; /* index [l,l] */
        for (j = l+1; j < ncu; ++j) {
            cdotc_(&t, &i__1, &u[i__2], &c__1, &u[l+j* *ldu], &c__1);
            t.r = -t.r, t.i = -t.i;
            c_div(&t, &t, &u[i__2]);
            caxpy_(&i__1, &t, &u[i__2], &c__1, &u[l+j* *ldu], &c__1);
        }
        cscal_(&i__1, &c_m1, &u[i__2], &c__1);
        u[i__2].r += 1.f;
        for (i = 0; i < l; ++i) {
            i__1 = i + l * *ldu;
            u[i__1].r = 0.f, u[i__1].i = 0.f;
        }
    }

/*     if it is required, generate v. */

    if (wantv)
    for (l = *p-1; l >= 0; --l) {
        if (l < nrt && (e[l].r != 0.f || e[l].i != 0.f))
        for (j = l+1; j < *p; ++j) {
            i__1 = *p - l - 1;
            i__2 = l+1 + l * *ldv; /* index [l+1,l] */
            cdotc_(&t, &i__1, &v[i__2], &c__1, &v[l+1 +j* *ldv], &c__1);
            t.r = -t.r, t.i = -t.i;
            c_div(&t, &t, &v[i__2]);
            caxpy_(&i__1, &t, &v[i__2], &c__1, &v[l+1 +j* *ldv], &c__1);
        }
        for (i = 0; i < *p; ++i) {
            i__1 = i + l * *ldv; /* index [i,l] */
            v[i__1].r = 0.f, v[i__1].i = 0.f;
        }
        i__1 = l + l * *ldv; /* index [l,l] */
        v[i__1].r = 1.f, v[i__1].i = 0.f;
    }

/*     transform s and e so that they are real. */

    for (i = 0; i <= m; ++i) {
        if (s[i].r != 0.f || s[i].i != 0.f) {
            t.r = c_abs(&s[i]), t.i = 0.f;
            c_div(&r, &s[i], &t);
            s[i].r = t.r, s[i].i = t.i;
            if (i < m) {
                c_div(&e[i], &e[i], &r);
            }
            if (wantu) {
                cscal_(n, &r, &u[i* *ldu], &c__1);
            }
        }
        if (i == m) {
            break; /* last i */
        }
        if (e[i].r == 0.f && e[i].i == 0.f) {
            continue; /* next i */
        }
        t.r = c_abs(&e[i]), t.i = 0.f;
        c_div(&r, &t, &e[i]);
        e[i].r = t.r, e[i].i = t.i;
        q__1.r = s[i+1].r * r.r - s[i+1].i * r.i,
        q__1.i = s[i+1].r * r.i + s[i+1].i * r.r;
        s[i+1].r = q__1.r, s[i+1].i = q__1.i;
        if (wantv) {
            cscal_(p, &r, &v[(i+1)* *ldv], &c__1);
        }
    }

/*     main iteration loop for the singular values. */

    mm = m;
    iter = 0;

/*        quit if all the singular values have been found. */

L400:
    if (m == -1) {
        return; /* exit from csvdc */
    }

/*        if too many iterations have been performed, set */
/*        flag and return. */

    if (iter >= maxit) {
        *info = m+1;
        return; /* exit from csvdc */
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
        test = c_abs(&s[l-1]) + c_abs(&s[l]);
        ztest = test + c_abs(&e[l-1]);
        if (fsm_ieee_floats_equal(&ztest, &test)) {
/* WAS: if (ztest == test) { */
            e[l-1].r = 0.f, e[l-1].i = 0.f;
            break; /* last l */
        }
    }
    if (l == m) { /* kase = 4 */ /*        convergence. */

/*           make the singular value positive */

        if (s[l].r < 0.f) {
            s[l].r = -s[l].r, s[l].i = -s[l].i;
            if (wantv) {
                cscal_(p, &c_m1, &v[l* *ldv], &c__1);
            }
        }

/*           order the singular value. */

        while (l != mm && s[l].r < s[l+1].r) {
            t.r = s[l].r, t.i = s[l].i;
            s[l].r = s[l+1].r, s[l].i = s[l+1].i;
            s[l+1].r = t.r, s[l+1].i = t.i;
            if (wantv && l < *p-1) {
                cswap_(p, &v[l* *ldv], &c__1, &v[(l+1)* *ldv], &c__1);
            }
            if (wantu && l < *n-1) {
                cswap_(n, &u[l* *ldu], &c__1, &u[(l+1)* *ldu], &c__1);
            }
            ++l;
        }
        iter = 0;
        --m;
        goto L400;
    }
    for (ls = m; ls >= l; --ls) {
        test = 0.f;
        if (ls != m) {
            test += c_abs(&e[ls]);
        }
        if (ls != l) {
            test += c_abs(&e[ls-1]);
        }
        ztest = test + c_abs(&s[ls]);
        if (fsm_ieee_floats_equal(&ztest, &test)) {
/* WAS: if (ztest == test) { */
            s[ls].r = 0.f, s[ls].i = 0.f;
            break; /* last ls */
        }
    }
    if (ls == l-1) { /* kase = 3 */ /*        perform one qr step. */

/*           calculate the shift. */

        scale =            c_abs(&s[m]),
        scale = max(scale, c_abs(&s[m-1])),
        scale = max(scale, c_abs(&e[m-1])),
        scale = max(scale, c_abs(&s[l])),
        scale = max(scale, c_abs(&e[l]));
        sm = s[m].r / scale;
        smm1 = s[m-1].r / scale;
        emm1 = e[m-1].r / scale;
        sl = s[l].r / scale;
        el = e[l].r / scale;
        b = ((smm1+sm) * (smm1-sm) + emm1*emm1) / 2.f;
        c = sm * emm1; c *= c;
        shift = 0.f;
        if (b != 0.f || c != 0.f) {
            shift = sqrtf(b*b + c);
            if (b < 0.f) {
                shift = -shift;
            }
            shift = c / (b + shift);
        }
        f = (sl + sm) * (sl - sm) + shift;
        g = sl * el;

/*           chase zeros. */

        for (k = l; k < m; ++k) {
            srotg_(&f, &g, &cs, &sn);
            if (k != l) {
                e[k-1].r = f, e[k-1].i = 0.f;
            }
            f      = cs * s[k].r + sn * e[k].r;
            e[k].r = cs * e[k].r - sn * s[k].r,
            e[k].i = cs * e[k].i - sn * s[k].i;
            g = sn * s[k+1].r;
            s[k+1].r *= cs, s[k+1].i *= cs;
            if (wantv) {
                csrot_(p, &v[k* *ldv], &c__1, &v[(k+1)* *ldv], &c__1, &cs, &sn);
            }
            srotg_(&f, &g, &cs, &sn);
            s[k].r = f, s[k].i = 0.f;
            f        =  cs * e[k].r + sn * s[k+1].r;
            s[k+1].r = -sn * e[k].r + cs * s[k+1].r,
            s[k+1].i = -sn * e[k].i + cs * s[k+1].i;
            g = sn * e[k+1].r;
            e[k+1].r *= cs, e[k+1].i *= cs;
            if (wantu && k < *n-1) {
                csrot_(n, &u[k* *ldu], &c__1, &u[(k+1)* *ldu], &c__1, &cs, &sn);
            }
        }
        e[m-1].r = f, e[m-1].i = 0.f;
        ++iter;
    }
    else if (ls == m) { /* kase = 1 */ /*        deflate negligible s(m). */
        f = e[m-1].r;
        e[m-1].r = 0.f, e[m-1].i = 0.f;
        for (k = m-1; k >= l; --k) {
            t1 = s[k].r;
            srotg_(&t1, &f, &cs, &sn);
            s[k].r = t1, s[k].i = 0.f;
            if (k != l) {
                f = -sn * e[k-1].r;
                e[k-1].r *= cs, e[k-1].i *= cs;
            }
            if (wantv) {
                csrot_(p, &v[k* *ldv], &c__1, &v[m* *ldv], &c__1, &cs, &sn);
            }
        }
    }
    else { /* kase = 2 */ /*        split at negligible s(l). */
        /* l = ls + 1; */
        f = e[ls].r;
        e[ls].r = 0.f, e[ls].i = 0.f;
        for (k = ls+1; k <= m; ++k) {
            t1 = s[k].r;
            srotg_(&t1, &f, &cs, &sn);
            s[k].r = t1, s[k].i = 0.f;
            f = -sn * e[k].r;
            e[k].r *= cs, e[k].i *= cs;
            if (wantu) {
                csrot_(n, &u[k* *ldu], &c__1, &u[ls* *ldu], &c__1, &cs, &sn);
            }
        }
    }
    goto L400;

} /* csvdc_ */

static int fsm_ieee_floats_equal(const real *x, const real *y)
{
  return *x == *y;
}
