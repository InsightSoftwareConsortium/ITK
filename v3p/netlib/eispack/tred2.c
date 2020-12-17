/* eispack/tred2.f -- translated by f2c (version 20050501).
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

/*<       subroutine tred2(nm,n,a,d,e,z) >*/
/* Subroutine */ int tred2_(integer *nm, integer *n, doublereal *a,
        doublereal *d__, doublereal *e, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal f, g, h__;
    integer i__, j, k, l;
    doublereal hh;
    integer ii, jp1;
    doublereal scale;


/*<       integer i,j,k,l,n,ii,nm,jp1 >*/
/*<       double precision a(nm,n),d(n),e(n),z(nm,n) >*/
/*<       double precision f,g,h,hh,scale >*/

/*     this subroutine is a translation of the algol procedure tred2, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a real symmetric matrix to a */
/*     symmetric tridiagonal matrix using and accumulating */
/*     orthogonal similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the real symmetric input matrix.  only the */
/*          lower triangle of the matrix need be supplied. */

/*     on output */

/*        d contains the diagonal elements of the tridiagonal matrix. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        z contains the orthogonal transformation matrix */
/*          produced in the reduction. */

/*        a and z may coincide.  if distinct, a is unaltered. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       do 100 i = 1, n >*/
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*<          do 80 j = i, n >*/
        i__2 = *n;
        for (j = i__; j <= i__2; ++j) {
/*<    80    z(j,i) = a(j,i) >*/
/* L80: */
            z__[j + i__ * z_dim1] = a[j + i__ * a_dim1];
        }

/*<          d(i) = a(n,i) >*/
        d__[i__] = a[*n + i__ * a_dim1];
/*<   100 continue >*/
/* L100: */
    }

/*<       if (n .eq. 1) go to 510 >*/
    if (*n == 1) {
        goto L510;
    }
/*     .......... for i=n step -1 until 2 do -- .......... */
/*<       do 300 ii = 2, n >*/
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
/*<          i = n + 2 - ii >*/
        i__ = *n + 2 - ii;
/*<          l = i - 1 >*/
        l = i__ - 1;
/*<          h = 0.0d0 >*/
        h__ = 0.;
/*<          scale = 0.0d0 >*/
        scale = 0.;
/*<          if (l .lt. 2) go to 130 >*/
        if (l < 2) {
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
/*<   130    e(i) = d(l) >*/
L130:
        e[i__] = d__[l];

/*<          do 135 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             d(j) = z(l,j) >*/
            d__[j] = z__[l + j * z_dim1];
/*<             z(i,j) = 0.0d0 >*/
            z__[i__ + j * z_dim1] = 0.;
/*<             z(j,i) = 0.0d0 >*/
            z__[j + i__ * z_dim1] = 0.;
/*<   135    continue >*/
/* L135: */
        }

/*<          go to 290 >*/
        goto L290;

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
/*<             z(j,i) = f >*/
            z__[j + i__ * z_dim1] = f;
/*<             g = e(j) + z(j,j) * f >*/
            g = e[j] + z__[j + j * z_dim1] * f;
/*<             jp1 = j + 1 >*/
            jp1 = j + 1;
/*<             if (l .lt. jp1) go to 220 >*/
            if (l < jp1) {
                goto L220;
            }

/*<             do 200 k = jp1, l >*/
            i__3 = l;
            for (k = jp1; k <= i__3; ++k) {
/*<                g = g + z(k,j) * d(k) >*/
                g += z__[k + j * z_dim1] * d__[k];
/*<                e(k) = e(k) + z(k,j) * f >*/
                e[k] += z__[k + j * z_dim1] * f;
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

/*<          hh = f / (h + h) >*/
        hh = f / (h__ + h__);
/*     .......... form q .......... */
/*<          do 250 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<   250    e(j) = e(j) - hh * d(j) >*/
/* L250: */
            e[j] -= hh * d__[j];
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
/*<   260       z(k,j) = z(k,j) - f * e(k) - g * d(k) >*/
/* L260: */
                z__[k + j * z_dim1] = z__[k + j * z_dim1] - f * e[k] - g *
                        d__[k];
            }

/*<             d(j) = z(l,j) >*/
            d__[j] = z__[l + j * z_dim1];
/*<             z(i,j) = 0.0d0 >*/
            z__[i__ + j * z_dim1] = 0.;
/*<   280    continue >*/
/* L280: */
        }

/*<   290    d(i) = h >*/
L290:
        d__[i__] = h__;
/*<   300 continue >*/
/* L300: */
    }
/*     .......... accumulation of transformation matrices .......... */
/*<       do 500 i = 2, n >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          l = i - 1 >*/
        l = i__ - 1;
/*<          z(n,l) = z(l,l) >*/
        z__[*n + l * z_dim1] = z__[l + l * z_dim1];
/*<          z(l,l) = 1.0d0 >*/
        z__[l + l * z_dim1] = 1.;
/*<          h = d(i) >*/
        h__ = d__[i__];
/*<          if (h .eq. 0.0d0) go to 380 >*/
        if (h__ == 0.) {
            goto L380;
        }

/*<          do 330 k = 1, l >*/
        i__2 = l;
        for (k = 1; k <= i__2; ++k) {
/*<   330    d(k) = z(k,i) / h >*/
/* L330: */
            d__[k] = z__[k + i__ * z_dim1] / h__;
        }

/*<          do 360 j = 1, l >*/
        i__2 = l;
        for (j = 1; j <= i__2; ++j) {
/*<             g = 0.0d0 >*/
            g = 0.;

/*<             do 340 k = 1, l >*/
            i__3 = l;
            for (k = 1; k <= i__3; ++k) {
/*<   340       g = g + z(k,i) * z(k,j) >*/
/* L340: */
                g += z__[k + i__ * z_dim1] * z__[k + j * z_dim1];
            }

/*<             do 360 k = 1, l >*/
            i__3 = l;
            for (k = 1; k <= i__3; ++k) {
/*<                z(k,j) = z(k,j) - g * d(k) >*/
                z__[k + j * z_dim1] -= g * d__[k];
/*<   360    continue >*/
/* L360: */
            }
        }

/*<   380    do 400 k = 1, l >*/
L380:
        i__3 = l;
        for (k = 1; k <= i__3; ++k) {
/*<   400    z(k,i) = 0.0d0 >*/
/* L400: */
            z__[k + i__ * z_dim1] = 0.;
        }

/*<   500 continue >*/
/* L500: */
    }

/*<   510 do 520 i = 1, n >*/
L510:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          d(i) = z(n,i) >*/
        d__[i__] = z__[*n + i__ * z_dim1];
/*<          z(n,i) = 0.0d0 >*/
        z__[*n + i__ * z_dim1] = 0.;
/*<   520 continue >*/
/* L520: */
    }

/*<       z(n,n) = 1.0d0 >*/
    z__[*n + *n * z_dim1] = 1.;
/*<       e(1) = 0.0d0 >*/
    e[1] = 0.;
/*<       return >*/
    return 0;
/*<       end >*/
} /* tred2_ */

#ifdef __cplusplus
        }
#endif
