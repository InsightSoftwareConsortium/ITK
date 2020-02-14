/* eispack/otqlrat.f -- translated by f2c (version 20050501).
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

static doublereal c_b11 = 1.;

/*<       subroutine tqlrat(n,d,e2,ierr) >*/
/* Subroutine */ int tqlrat_(integer *n, doublereal *d__, doublereal *e2,
        integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal b=0, c__=0, f, g, h__;
    integer i__, j, l, m;
    doublereal p, r__, s, t;
    integer l1, ii, mml;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal
            *);


/*<       integer i,j,l,m,n,ii,l1,mml,ierr >*/
/*<       double precision d(n),e2(n) >*/
/*<       double precision b,c,f,g,h,p,r,s,t,epslon,pythag >*/

/*     this subroutine is a translation of the algol procedure tqlrat, */
/*     algorithm 464, comm. acm 16, 689(1973) by reinsch. */

/*     this subroutine finds the eigenvalues of a symmetric */
/*     tridiagonal matrix by the rational ql method. */

/*     on input */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e2 contains the squares of the subdiagonal elements of the */
/*          input matrix in its last n-1 positions.  e2(1) is arbitrary. */

/*      on output */

/*        d contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct and */
/*          ordered for indices 1,2,...ierr-1, but may not be */
/*          the smallest eigenvalues. */

/*        e2 has been destroyed. */

/*        ierr is set to */
/*          zero       for normal return, */
/*          j          if the j-th eigenvalue has not been */
/*                     determined after 30 iterations. */

/*     calls pythag for  dsqrt(a*a + b*b) . */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       ierr = 0 >*/
    /* Parameter adjustments */
    --e2;
    --d__;

    /* Function Body */
    *ierr = 0;
/*<       if (n .eq. 1) go to 1001 >*/
    if (*n == 1) {
        goto L1001;
    }

/*<       do 100 i = 2, n >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<   100 e2(i-1) = e2(i) >*/
/* L100: */
        e2[i__ - 1] = e2[i__];
    }

/*<       f = 0.0d0 >*/
    f = 0.;
/*<       t = 0.0d0 >*/
    t = 0.;
/*<       e2(n) = 0.0d0 >*/
    e2[*n] = 0.;

/*<       do 290 l = 1, n >*/
    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
/*<          j = 0 >*/
        j = 0;
/*<          h = dabs(d(l)) + dsqrt(e2(l)) >*/
        h__ = (d__1 = d__[l], abs(d__1)) + sqrt(e2[l]);
/*<          if (t .gt. h) go to 105 >*/
        if (t > h__) {
            goto L105;
        }
/*<          t = h >*/
        t = h__;
/*<          b = epslon(t) >*/
        b = epslon_(&t);
/*<          c = b * b >*/
        c__ = b * b;
/*     .......... look for small squared sub-diagonal element .......... */
/*<   105    do 110 m = l, n >*/
L105:
        i__2 = *n;
        for (m = l; m <= i__2; ++m) {
/*<             if (e2(m) .le. c) go to 120 >*/
            if (e2[m] <= c__) {
                goto L120;
            }
/*     .......... e2(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
/*<   110    continue >*/
/* L110: */
        }

/*<   120    if (m .eq. l) go to 210 >*/
L120:
        if (m == l) {
            goto L210;
        }
/*<   130    if (j .eq. 30) go to 1000 >*/
L130:
        if (j == 30) {
            goto L1000;
        }
/*<          j = j + 1 >*/
        ++j;
/*     .......... form shift .......... */
/*<          l1 = l + 1 >*/
        l1 = l + 1;
/*<          s = dsqrt(e2(l)) >*/
        s = sqrt(e2[l]);
/*<          g = d(l) >*/
        g = d__[l];
/*<          p = (d(l1) - g) / (2.0d0 * s) >*/
        p = (d__[l1] - g) / (s * 2.);
/*<          r = pythag(p,1.0d0) >*/
        r__ = pythag_(&p, &c_b11);
/*<          d(l) = s / (p + dsign(r,p)) >*/
        d__[l] = s / (p + d_sign(&r__, &p));
/*<          h = g - d(l) >*/
        h__ = g - d__[l];

/*<          do 140 i = l1, n >*/
        i__2 = *n;
        for (i__ = l1; i__ <= i__2; ++i__) {
/*<   140    d(i) = d(i) - h >*/
/* L140: */
            d__[i__] -= h__;
        }

/*<          f = f + h >*/
        f += h__;
/*     .......... rational ql transformation .......... */
/*<          g = d(m) >*/
        g = d__[m];
/*<          if (g .eq. 0.0d0) g = b >*/
        if (g == 0.) {
            g = b;
        }
/*<          h = g >*/
        h__ = g;
/*<          s = 0.0d0 >*/
        s = 0.;
/*<          mml = m - l >*/
        mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
/*<          do 200 ii = 1, mml >*/
        i__2 = mml;
        for (ii = 1; ii <= i__2; ++ii) {
/*<             i = m - ii >*/
            i__ = m - ii;
/*<             p = g * h >*/
            p = g * h__;
/*<             r = p + e2(i) >*/
            r__ = p + e2[i__];
/*<             e2(i+1) = s * r >*/
            e2[i__ + 1] = s * r__;
/*<             s = e2(i) / r >*/
            s = e2[i__] / r__;
/*<             d(i+1) = h + s * (h + d(i)) >*/
            d__[i__ + 1] = h__ + s * (h__ + d__[i__]);
/*<             g = d(i) - e2(i) / g >*/
            g = d__[i__] - e2[i__] / g;
/*<             if (g .eq. 0.0d0) g = b >*/
            if (g == 0.) {
                g = b;
            }
/*<             h = g * p / r >*/
            h__ = g * p / r__;
/*<   200    continue >*/
/* L200: */
        }

/*<          e2(l) = s * g >*/
        e2[l] = s * g;
/*<          d(l) = h >*/
        d__[l] = h__;
/*     .......... guard against underflow in convergence test .......... */
/*<          if (h .eq. 0.0d0) go to 210 >*/
        if (h__ == 0.) {
            goto L210;
        }
/*<          if (dabs(e2(l)) .le. dabs(c/h)) go to 210 >*/
        if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c__ / h__, abs(d__2))) {
            goto L210;
        }
/*<          e2(l) = h * e2(l) >*/
        e2[l] = h__ * e2[l];
/*<          if (e2(l) .ne. 0.0d0) go to 130 >*/
        if (e2[l] != 0.) {
            goto L130;
        }
/*<   210    p = d(l) + f >*/
L210:
        p = d__[l] + f;
/*     .......... order eigenvalues .......... */
/*<          if (l .eq. 1) go to 250 >*/
        if (l == 1) {
            goto L250;
        }
/*     .......... for i=l step -1 until 2 do -- .......... */
/*<          do 230 ii = 2, l >*/
        i__2 = l;
        for (ii = 2; ii <= i__2; ++ii) {
/*<             i = l + 2 - ii >*/
            i__ = l + 2 - ii;
/*<             if (p .ge. d(i-1)) go to 270 >*/
            if (p >= d__[i__ - 1]) {
                goto L270;
            }
/*<             d(i) = d(i-1) >*/
            d__[i__] = d__[i__ - 1];
/*<   230    continue >*/
/* L230: */
        }

/*<   250    i = 1 >*/
L250:
        i__ = 1;
/*<   270    d(i) = p >*/
L270:
        d__[i__] = p;
/*<   290 continue >*/
/* L290: */
    }

/*<       go to 1001 >*/
    goto L1001;
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
/*<  1000 ierr = l >*/
L1000:
    *ierr = l;
/*<  1001 return >*/
L1001:
    return 0;
/*<       end >*/
} /* tqlrat_ */

#ifdef __cplusplus
        }
#endif
