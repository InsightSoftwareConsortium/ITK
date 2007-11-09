/* eispack/elmhes.f -- translated by f2c (version 20050501).
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

/*<       subroutine elmhes(nm,n,low,igh,a,int) >*/
/* Subroutine */ int elmhes_(integer *nm, integer *n, integer *low, integer *
        igh, doublereal *a, integer *int__)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    integer i__, j, m;
    doublereal x, y;
    integer la, mm1, kp1, mp1;


/*<       integer i,j,m,n,la,nm,igh,kp1,low,mm1,mp1 >*/
/*<       double precision a(nm,n) >*/
/*<       double precision x,y >*/
/*<       integer int(igh) >*/

/*     this subroutine is a translation of the algol procedure elmhes, */
/*     num. math. 12, 349-368(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). */

/*     given a real general matrix, this subroutine */
/*     reduces a submatrix situated in rows and columns */
/*     low through igh to upper hessenberg form by */
/*     stabilized elementary similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        a contains the input matrix. */

/*     on output */

/*        a contains the hessenberg matrix.  the multipliers */
/*          which were used in the reduction are stored in the */
/*          remaining triangle under the hessenberg matrix. */

/*        int contains information on the rows and columns */
/*          interchanged in the reduction. */
/*          only elements low through igh are used. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       la = igh - 1 >*/
    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --int__;

    /* Function Body */
    la = *igh - 1;
/*<       kp1 = low + 1 >*/
    kp1 = *low + 1;
/*<       if (la .lt. kp1) go to 200 >*/
    if (la < kp1) {
        goto L200;
    }

/*<       do 180 m = kp1, la >*/
    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
/*<          mm1 = m - 1 >*/
        mm1 = m - 1;
/*<          x = 0.0d0 >*/
        x = 0.;
/*<          i = m >*/
        i__ = m;

/*<          do 100 j = m, igh >*/
        i__2 = *igh;
        for (j = m; j <= i__2; ++j) {
/*<             if (dabs(a(j,mm1)) .le. dabs(x)) go to 100 >*/
            if ((d__1 = a[j + mm1 * a_dim1], abs(d__1)) <= abs(x)) {
                goto L100;
            }
/*<             x = a(j,mm1) >*/
            x = a[j + mm1 * a_dim1];
/*<             i = j >*/
            i__ = j;
/*<   100    continue >*/
L100:
            ;
        }

/*<          int(m) = i >*/
        int__[m] = i__;
/*<          if (i .eq. m) go to 130 >*/
        if (i__ == m) {
            goto L130;
        }
/*     .......... interchange rows and columns of a .......... */
/*<          do 110 j = mm1, n >*/
        i__2 = *n;
        for (j = mm1; j <= i__2; ++j) {
/*<             y = a(i,j) >*/
            y = a[i__ + j * a_dim1];
/*<             a(i,j) = a(m,j) >*/
            a[i__ + j * a_dim1] = a[m + j * a_dim1];
/*<             a(m,j) = y >*/
            a[m + j * a_dim1] = y;
/*<   110    continue >*/
/* L110: */
        }

/*<          do 120 j = 1, igh >*/
        i__2 = *igh;
        for (j = 1; j <= i__2; ++j) {
/*<             y = a(j,i) >*/
            y = a[j + i__ * a_dim1];
/*<             a(j,i) = a(j,m) >*/
            a[j + i__ * a_dim1] = a[j + m * a_dim1];
/*<             a(j,m) = y >*/
            a[j + m * a_dim1] = y;
/*<   120    continue >*/
/* L120: */
        }
/*     .......... end interchange .......... */
/*<   130    if (x .eq. 0.0d0) go to 180 >*/
L130:
        if (x == 0.) {
            goto L180;
        }
/*<          mp1 = m + 1 >*/
        mp1 = m + 1;

/*<          do 160 i = mp1, igh >*/
        i__2 = *igh;
        for (i__ = mp1; i__ <= i__2; ++i__) {
/*<             y = a(i,mm1) >*/
            y = a[i__ + mm1 * a_dim1];
/*<             if (y .eq. 0.0d0) go to 160 >*/
            if (y == 0.) {
                goto L160;
            }
/*<             y = y / x >*/
            y /= x;
/*<             a(i,mm1) = y >*/
            a[i__ + mm1 * a_dim1] = y;

/*<             do 140 j = m, n >*/
            i__3 = *n;
            for (j = m; j <= i__3; ++j) {
/*<   140       a(i,j) = a(i,j) - y * a(m,j) >*/
/* L140: */
                a[i__ + j * a_dim1] -= y * a[m + j * a_dim1];
            }

/*<             do 150 j = 1, igh >*/
            i__3 = *igh;
            for (j = 1; j <= i__3; ++j) {
/*<   150       a(j,m) = a(j,m) + y * a(j,i) >*/
/* L150: */
                a[j + m * a_dim1] += y * a[j + i__ * a_dim1];
            }

/*<   160    continue >*/
L160:
            ;
        }

/*<   180 continue >*/
L180:
        ;
    }

/*<   200 return >*/
L200:
    return 0;
/*<       end >*/
} /* elmhes_ */

#ifdef __cplusplus
        }
#endif
