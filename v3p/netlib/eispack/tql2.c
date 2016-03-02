/* eispack/tql2.f -- translated by f2c (version 20050501).
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

static doublereal c_b10 = 1.;

/*<       subroutine tql2(nm,n,d,e,z,ierr) >*/
/* Subroutine */ int tql2_(integer *nm, integer *n, doublereal *d__,
        doublereal *e, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal c__, f, g, h__;
    integer i__, j, k, l, m;
    doublereal p, r__, s, c2, c3=0;
    integer l1, l2;
    doublereal s2=0;
    integer ii;
    doublereal dl1, el1;
    integer mml;
    doublereal tst1, tst2;
    extern doublereal pythag_(doublereal *, doublereal *);


/*<       integer i,j,k,l,m,n,ii,l1,l2,nm,mml,ierr >*/
/*<       double precision d(n),e(n),z(nm,n) >*/
/*<       double precision c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2,pythag >*/

/*     this subroutine is a translation of the algol procedure tql2, */
/*     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and */
/*     wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971). */

/*     this subroutine finds the eigenvalues and eigenvectors */
/*     of a symmetric tridiagonal matrix by the ql method. */
/*     the eigenvectors of a full symmetric matrix can also */
/*     be found if  tred2  has been used to reduce this */
/*     full matrix to tridiagonal form. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        d contains the diagonal elements of the input matrix. */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary. */

/*        z contains the transformation matrix produced in the */
/*          reduction by  tred2, if performed.  if the eigenvectors */
/*          of the tridiagonal matrix are desired, z must contain */
/*          the identity matrix. */

/*      on output */

/*        d contains the eigenvalues in ascending order.  if an */
/*          error exit is made, the eigenvalues are correct but */
/*          unordered for indices 1,2,...,ierr-1. */

/*        e has been destroyed. */

/*        z contains orthonormal eigenvectors of the symmetric */
/*          tridiagonal (or full) matrix.  if an error exit is made, */
/*          z contains the eigenvectors associated with the stored */
/*          eigenvalues. */

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
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --e;
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
/*<   100 e(i-1) = e(i) >*/
/* L100: */
        e[i__ - 1] = e[i__];
    }

/*<       f = 0.0d0 >*/
    f = 0.;
/*<       tst1 = 0.0d0 >*/
    tst1 = 0.;
/*<       e(n) = 0.0d0 >*/
    e[*n] = 0.;

/*<       do 240 l = 1, n >*/
    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
/*<          j = 0 >*/
        j = 0;
/*<          h = dabs(d(l)) + dabs(e(l)) >*/
        h__ = (d__1 = d__[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
/*<          if (tst1 .lt. h) tst1 = h >*/
        if (tst1 < h__) {
            tst1 = h__;
        }
/*     .......... look for small sub-diagonal element .......... */
/*<          do 110 m = l, n >*/
        i__2 = *n;
        for (m = l; m <= i__2; ++m) {
/*<             tst2 = tst1 + dabs(e(m)) >*/
            tst2 = tst1 + (d__1 = e[m], abs(d__1));
/*<             if (tst2 .eq. tst1) go to 120 >*/
            if (tst2 == tst1) {
                goto L120;
            }
/*     .......... e(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
/*<   110    continue >*/
/* L110: */
        }

/*<   120    if (m .eq. l) go to 220 >*/
L120:
        if (m == l) {
            goto L220;
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
/*<          l2 = l1 + 1 >*/
        l2 = l1 + 1;
/*<          g = d(l) >*/
        g = d__[l];
/*<          p = (d(l1) - g) / (2.0d0 * e(l)) >*/
        p = (d__[l1] - g) / (e[l] * 2.);
/*<          r = pythag(p,1.0d0) >*/
        r__ = pythag_(&p, &c_b10);
/*<          d(l) = e(l) / (p + dsign(r,p)) >*/
        d__[l] = e[l] / (p + d_sign(&r__, &p));
/*<          d(l1) = e(l) * (p + dsign(r,p)) >*/
        d__[l1] = e[l] * (p + d_sign(&r__, &p));
/*<          dl1 = d(l1) >*/
        dl1 = d__[l1];
/*<          h = g - d(l) >*/
        h__ = g - d__[l];
/*<          if (l2 .gt. n) go to 145 >*/
        if (l2 > *n) {
            goto L145;
        }

/*<          do 140 i = l2, n >*/
        i__2 = *n;
        for (i__ = l2; i__ <= i__2; ++i__) {
/*<   140    d(i) = d(i) - h >*/
/* L140: */
            d__[i__] -= h__;
        }

/*<   145    f = f + h >*/
L145:
        f += h__;
/*     .......... ql transformation .......... */
/*<          p = d(m) >*/
        p = d__[m];
/*<          c = 1.0d0 >*/
        c__ = 1.;
/*<          c2 = c >*/
        c2 = c__;
/*<          el1 = e(l1) >*/
        el1 = e[l1];
/*<          s = 0.0d0 >*/
        s = 0.;
/*<          mml = m - l >*/
        mml = m - l;
/*     .......... for i=m-1 step -1 until l do -- .......... */
/*<          do 200 ii = 1, mml >*/
        i__2 = mml;
        for (ii = 1; ii <= i__2; ++ii) {
/*<             c3 = c2 >*/
            c3 = c2;
/*<             c2 = c >*/
            c2 = c__;
/*<             s2 = s >*/
            s2 = s;
/*<             i = m - ii >*/
            i__ = m - ii;
/*<             g = c * e(i) >*/
            g = c__ * e[i__];
/*<             h = c * p >*/
            h__ = c__ * p;
/*<             r = pythag(p,e(i)) >*/
            r__ = pythag_(&p, &e[i__]);
/*<             e(i+1) = s * r >*/
            e[i__ + 1] = s * r__;
/*<             s = e(i) / r >*/
            s = e[i__] / r__;
/*<             c = p / r >*/
            c__ = p / r__;
/*<             p = c * d(i) - s * g >*/
            p = c__ * d__[i__] - s * g;
/*<             d(i+1) = h + s * (c * g + s * d(i)) >*/
            d__[i__ + 1] = h__ + s * (c__ * g + s * d__[i__]);
/*     .......... form vector .......... */
/*<             do 180 k = 1, n >*/
            i__3 = *n;
            for (k = 1; k <= i__3; ++k) {
/*<                h = z(k,i+1) >*/
                h__ = z__[k + (i__ + 1) * z_dim1];
/*<                z(k,i+1) = s * z(k,i) + c * h >*/
                z__[k + (i__ + 1) * z_dim1] = s * z__[k + i__ * z_dim1] + c__
                        * h__;
/*<                z(k,i) = c * z(k,i) - s * h >*/
                z__[k + i__ * z_dim1] = c__ * z__[k + i__ * z_dim1] - s * h__;
/*<   180       continue >*/
/* L180: */
            }

/*<   200    continue >*/
/* L200: */
        }

/*<          p = -s * s2 * c3 * el1 * e(l) / dl1 >*/
        p = -s * s2 * c3 * el1 * e[l] / dl1;
/*<          e(l) = s * p >*/
        e[l] = s * p;
/*<          d(l) = c * p >*/
        d__[l] = c__ * p;
/*<          tst2 = tst1 + dabs(e(l)) >*/
        tst2 = tst1 + (d__1 = e[l], abs(d__1));
/*<          if (tst2 .gt. tst1) go to 130 >*/
        if (tst2 > tst1) {
            goto L130;
        }
/*<   220    d(l) = d(l) + f >*/
L220:
        d__[l] += f;
/*<   240 continue >*/
/* L240: */
    }
/*     .......... order eigenvalues and eigenvectors .......... */
/*<       do 300 ii = 2, n >*/
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
/*<          i = ii - 1 >*/
        i__ = ii - 1;
/*<          k = i >*/
        k = i__;
/*<          p = d(i) >*/
        p = d__[i__];

/*<          do 260 j = ii, n >*/
        i__2 = *n;
        for (j = ii; j <= i__2; ++j) {
/*<             if (d(j) .ge. p) go to 260 >*/
            if (d__[j] >= p) {
                goto L260;
            }
/*<             k = j >*/
            k = j;
/*<             p = d(j) >*/
            p = d__[j];
/*<   260    continue >*/
L260:
            ;
        }

/*<          if (k .eq. i) go to 300 >*/
        if (k == i__) {
            goto L300;
        }
/*<          d(k) = d(i) >*/
        d__[k] = d__[i__];
/*<          d(i) = p >*/
        d__[i__] = p;

/*<          do 280 j = 1, n >*/
        i__2 = *n;
        for (j = 1; j <= i__2; ++j) {
/*<             p = z(j,i) >*/
            p = z__[j + i__ * z_dim1];
/*<             z(j,i) = z(j,k) >*/
            z__[j + i__ * z_dim1] = z__[j + k * z_dim1];
/*<             z(j,k) = p >*/
            z__[j + k * z_dim1] = p;
/*<   280    continue >*/
/* L280: */
        }

/*<   300 continue >*/
L300:
        ;
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
} /* tql2_ */

#ifdef __cplusplus
        }
#endif
