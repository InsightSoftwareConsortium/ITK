/* eispack/rebak.f -- translated by f2c (version 20050501).
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

/*<       subroutine rebak(nm,n,b,dl,m,z) >*/
/* Subroutine */ int rebak_(integer *nm, integer *n, doublereal *b,
        doublereal *dl, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, k;
    doublereal x;
    integer i1, ii;


/*<       integer i,j,k,m,n,i1,ii,nm >*/
/*<       double precision b(nm,n),dl(n),z(nm,m) >*/
/*<       double precision x >*/

/*     this subroutine is a translation of the algol procedure rebaka, */
/*     num. math. 11, 99-110(1968) by martin and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 303-314(1971). */

/*     this subroutine forms the eigenvectors of a generalized */
/*     symmetric eigensystem by back transforming those of the */
/*     derived symmetric matrix determined by  reduc. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix system. */

/*        b contains information about the similarity transformation */
/*          (cholesky decomposition) used in the reduction by  reduc */
/*          in its strict lower triangle. */

/*        dl contains further information about the transformation. */

/*        m is the number of eigenvectors to be back transformed. */

/*        z contains the eigenvectors to be back transformed */
/*          in its first m columns. */

/*     on output */

/*        z contains the transformed eigenvectors */
/*          in its first m columns. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       if (m .eq. 0) go to 200 >*/
    /* Parameter adjustments */
    --dl;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
        goto L200;
    }

/*<       do 100 j = 1, m >*/
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
/*     .......... for i=n step -1 until 1 do -- .......... */
/*<          do 100 ii = 1, n >*/
        i__2 = *n;
        for (ii = 1; ii <= i__2; ++ii) {
/*<             i = n + 1 - ii >*/
            i__ = *n + 1 - ii;
/*<             i1 = i + 1 >*/
            i1 = i__ + 1;
/*<             x = z(i,j) >*/
            x = z__[i__ + j * z_dim1];
/*<             if (i .eq. n) go to 80 >*/
            if (i__ == *n) {
                goto L80;
            }

/*<             do 60 k = i1, n >*/
            i__3 = *n;
            for (k = i1; k <= i__3; ++k) {
/*<    60       x = x - b(k,i) * z(k,j) >*/
/* L60: */
                x -= b[k + i__ * b_dim1] * z__[k + j * z_dim1];
            }

/*<    80       z(i,j) = x / dl(i) >*/
L80:
            z__[i__ + j * z_dim1] = x / dl[i__];
/*<   100 continue >*/
/* L100: */
        }
    }

/*<   200 return >*/
L200:
    return 0;
/*<       end >*/
} /* rebak_ */

#ifdef __cplusplus
        }
#endif
