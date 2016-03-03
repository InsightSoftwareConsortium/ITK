/* eispack/tred1.f -- translated by f2c (version 20050501).
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

/*<       subroutine tred1(nm,n,a,d,e,e2) >*/
/* Subroutine */ int tred1_(integer *nm, integer *n, doublereal *a,
        doublereal *d__, doublereal *e, doublereal *e2)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal f, g, h__;
    integer i__, j, k, l, ii, jp1;
    doublereal scale;


/*<       integer i,j,k,l,n,ii,nm,jp1 >*/
/*<       double precision a(nm,n),d(n),e(n),e2(n) >*/
/*<       double precision f,g,h,scale >*/

/*     this subroutine is a translation of the algol procedure tred1, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a real symmetric matrix */
/*     to a symmetric tridiagonal matrix using */
/*     orthogonal similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the real symmetric input matrix.  only the */
/*          lower triangle of the matrix need be supplied. */

/*     on output */

/*        a contains information about the orthogonal trans- */
/*          formations used in the reduction in its strict lower */
/*          triangle.  the full upper triangle of a is unaltered. */

/*        d contains the diagonal elements of the tridiagonal matrix. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2 may coincide with e if the squares are not needed. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       do 100 i = 1, n >*/
    /* Parameter adjustments */
    --e2;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          d(i) = a(n,i) >*/
        d__[i__] = a[*n + i__ * a_dim1];
/*<          a(n,i) = a(i,i) >*/
        a[*n + i__ * a_dim1] = a[i__ + i__ * a_dim1];
/*<   100 continue >*/
/* L100: */
    }
/*     .......... for i=n step -1 until 1 do -- .......... */
/*<       do 300 ii = 1, n >*/
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
/*<          i = n + 1 - ii >*/
        i__ = *n + 1 - ii;
/*<          l = i - 1 >*/
        l = i__ - 1;
/*<          h = 0.0d0 >*/
        h__ = 0.;
/*<          scale = 0.0d0 >*/
        scale = 0.;
/*<          if (l .lt. 1) go to 130 >*/
        if (l < 1) {
            goto L130;
        }
/*     .......... scale row (algol tol then not needed) .......... */
/*<          do 120 k = 1, l >*/
        i__2 = l;
        for (k = 1; k <= i__2; ++k) {
/*<   120    scale = scale + dabs(d(k)) >*/
/* L120: */
            scale += (d__1 = d__[k], abs(d__1));
        }

/*<          if (scale .ne. 0.0d0) go to 140 >*/
        if (scale != 0.) {
            goto L140;
        }

/*<          do 125 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             d(j) = a(l,j) >*/
            d__[j] = a[l + j * a_dim1];
/*<             a(l,j) = a(i,j) >*/
            a[l + j * a_dim1] = a[i__ + j * a_dim1];
/*<             a(i,j) = 0.0d0 >*/
            a[i__ + j * a_dim1] = 0.;
/*<   125    continue >*/
/* L125: */
        }

/*<   130    e(i) = 0.0d0 >*/
L130:
        e[i__] = 0.;
/*<          e2(i) = 0.0d0 >*/
        e2[i__] = 0.;
/*<          go to 300 >*/
        goto L300;

/*<   140    do 150 k = 1, l >*/
L140:
        i__2 = l;
        for (k = 1; k <= i__2; ++k) {
/*<             d(k) = d(k) / scale >*/
            d__[k] /= scale;
/*<             h = h + d(k) * d(k) >*/
            h__ += d__[k] * d__[k];
/*<   150    continue >*/
/* L150: */
        }

/*<          e2(i) = scale * scale * h >*/
        e2[i__] = scale * scale * h__;
/*<          f = d(l) >*/
        f = d__[l];
/*<          g = -dsign(dsqrt(h),f) >*/
        d__1 = sqrt(h__);
        g = -d_sign(&d__1, &f);
/*<          e(i) = scale * g >*/
        e[i__] = scale * g;
/*<          h = h - f * g >*/
        h__ -= f * g;
/*<          d(l) = f - g >*/
        d__[l] = f - g;
/*<          if (l .eq. 1) go to 285 >*/
        if (l == 1) {
            goto L285;
        }
/*     .......... form a*u .......... */
/*<          do 170 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<   170    e(j) = 0.0d0 >*/
/* L170: */
            e[j] = 0.;
        }

/*<          do 240 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             f = d(j) >*/
            f = d__[j];
/*<             g = e(j) + a(j,j) * f >*/
            g = e[j] + a[j + j * a_dim1] * f;
/*<             jp1 = j + 1 >*/
            jp1 = j + 1;
/*<             if (l .lt. jp1) go to 220 >*/
            if (l < jp1) {
                goto L220;
            }

/*<             do 200 k = jp1, l >*/
            i__3 = l;
            for (k = jp1; k <= i__3; ++k) {
/*<                g = g + a(k,j) * d(k) >*/
                g += a[k + j * a_dim1] * d__[k];
/*<                e(k) = e(k) + a(k,j) * f >*/
                e[k] += a[k + j * a_dim1] * f;
/*<   200       continue >*/
/* L200: */
            }

/*<   220       e(j) = g >*/
L220:
            e[j] = g;
/*<   240    continue >*/
/* L240: */
        }
/*     .......... form p .......... */
/*<          f = 0.0d0 >*/
        f = 0.;

/*<          do 245 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             e(j) = e(j) / h >*/
            e[j] /= h__;
/*<             f = f + e(j) * d(j) >*/
            f += e[j] * d__[j];
/*<   245    continue >*/
/* L245: */
        }

/*<          h = f / (h + h) >*/
        h__ = f / (h__ + h__);
/*     .......... form q .......... */
/*<          do 250 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<   250    e(j) = e(j) - h * d(j) >*/
/* L250: */
            e[j] -= h__ * d__[j];
        }
/*     .......... form reduced a .......... */
/*<          do 280 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             f = d(j) >*/
            f = d__[j];
/*<             g = e(j) >*/
            g = e[j];

/*<             do 260 k = j, l >*/
            i__3 = l;
            for (k = j; k <= i__3; ++k) {
/*<   260       a(k,j) = a(k,j) - f * e(k) - g * d(k) >*/
/* L260: */
                a[k + j * a_dim1] = a[k + j * a_dim1] - f * e[k] - g * d__[k];
            }

/*<   280    continue >*/
/* L280: */
        }

/*<   285    do 290 j = 1, l >*/
L285:
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             f = d(j) >*/
            f = d__[j];
/*<             d(j) = a(l,j) >*/
            d__[j] = a[l + j * a_dim1];
/*<             a(l,j) = a(i,j) >*/
            a[l + j * a_dim1] = a[i__ + j * a_dim1];
/*<             a(i,j) = f * scale >*/
            a[i__ + j * a_dim1] = f * scale;
/*<   290    continue >*/
/* L290: */
        }

/*<   300 continue >*/
L300:
        ;
    }

/*<       return >*/
    return 0;
/*<       end >*/
} /* tred1_ */

#ifdef __cplusplus
        }
#endif
