/* eispack/balbak.f -- translated by f2c (version 20050501).
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

/*<       subroutine balbak(nm,n,low,igh,scale,m,z) >*/
/* Subroutine */ int balbak_(integer *nm, integer *n, integer *low, integer *
        igh, doublereal *scale, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, k;
    doublereal s;
    integer ii;


/*<       integer i,j,k,m,n,ii,nm,igh,low >*/
/*<       double precision scale(n),z(nm,m) >*/
/*<       double precision s >*/

/*     this subroutine is a translation of the algol procedure balbak, */
/*     num. math. 13, 293-304(1969) by parlett and reinsch. */
/*     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971). */

/*     this subroutine forms the eigenvectors of a real general */
/*     matrix by back transforming those of the corresponding */
/*     balanced matrix determined by  balanc. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by  balanc. */

/*        scale contains information determining the permutations */
/*          and scaling factors used by  balanc. */

/*        m is the number of columns of z to be back transformed. */

/*        z contains the real and imaginary parts of the eigen- */
/*          vectors to be back transformed in its first m columns. */

/*     on output */

/*        z contains the real and imaginary parts of the */
/*          transformed eigenvectors in its first m columns. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       if (m .eq. 0) go to 200 >*/
    /* Parameter adjustments */
    --scale;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
        goto L200;
    }
/*<       if (igh .eq. low) go to 120 >*/
    if (*igh == *low) {
        goto L120;
    }

/*<       do 110 i = low, igh >*/
    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
/*<          s = scale(i) >*/
        s = scale[i__];
/*     .......... left hand eigenvectors are back transformed */
/*                if the foregoing statement is replaced by */
/*                s=1.0d0/scale(i). .......... */
/*<          do 100 j = 1, m >*/
        i__2 = *m;
        for (j = 1; j <= i__2; ++j) {
/*<   100    z(i,j) = z(i,j) * s >*/
/* L100: */
            z__[i__ + j * z_dim1] *= s;
        }

/*<   110 continue >*/
/* L110: */
    }
/*     ......... for i=low-1 step -1 until 1, */
/*               igh+1 step 1 until n do -- .......... */
/*<   120 do 140 ii = 1, n >*/
L120:
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
/*<          i = ii >*/
        i__ = ii;
/*<          if (i .ge. low .and. i .le. igh) go to 140 >*/
        if (i__ >= *low && i__ <= *igh) {
            goto L140;
        }
/*<          if (i .lt. low) i = low - ii >*/
        if (i__ < *low) {
            i__ = *low - ii;
        }
/*<          k = scale(i) >*/
        k = (integer) scale[i__];
/*<          if (k .eq. i) go to 140 >*/
        if (k == i__) {
            goto L140;
        }

/*<          do 130 j = 1, m >*/
        i__2 = *m;
        for (j = 1; j <= i__2; ++j) {
/*<             s = z(i,j) >*/
            s = z__[i__ + j * z_dim1];
/*<             z(i,j) = z(k,j) >*/
            z__[i__ + j * z_dim1] = z__[k + j * z_dim1];
/*<             z(k,j) = s >*/
            z__[k + j * z_dim1] = s;
/*<   130    continue >*/
/* L130: */
        }

/*<   140 continue >*/
L140:
        ;
    }

/*<   200 return >*/
L200:
    return 0;
/*<       end >*/
} /* balbak_ */

#ifdef __cplusplus
        }
#endif
