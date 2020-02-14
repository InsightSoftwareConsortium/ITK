/* eispack/eltran.f -- translated by f2c (version 20050501).
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

/*<       subroutine eltran(nm,n,low,igh,a,int,z) >*/
/* Subroutine */ int eltran_(integer *nm, integer *n, integer *low, integer *
        igh, doublereal *a, integer *int__, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, kl, mm, mp, mp1;


/*<       integer i,j,n,kl,mm,mp,nm,igh,low,mp1 >*/
/*<       double precision a(nm,igh),z(nm,n) >*/
/*<       integer int(igh) >*/

/*     this subroutine is a translation of the algol procedure elmtrans, */
/*     num. math. 16, 181-204(1970) by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). */

/*     this subroutine accumulates the stabilized elementary */
/*     similarity transformations used in the reduction of a */
/*     real general matrix to upper hessenberg form by  elmhes. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        low and igh are integers determined by the balancing */
/*          subroutine  balanc.  if  balanc  has not been used, */
/*          set low=1, igh=n. */

/*        a contains the multipliers which were used in the */
/*          reduction by  elmhes  in its lower triangle */
/*          below the subdiagonal. */

/*        int contains information on the rows and columns */
/*          interchanged in the reduction by  elmhes. */
/*          only elements low through igh are used. */

/*     on output */

/*        z contains the transformation matrix produced in the */
/*          reduction by  elmhes. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*     .......... initialize z to identity matrix .......... */
/*<       do 80 j = 1, n >*/
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --int__;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

/*<          do 60 i = 1, n >*/
        i__2 = *n;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<    60    z(i,j) = 0.0d0 >*/
/* L60: */
            z__[i__ + j * z_dim1] = 0.;
        }

/*<          z(j,j) = 1.0d0 >*/
        z__[j + j * z_dim1] = 1.;
/*<    80 continue >*/
/* L80: */
    }

/*<       kl = igh - low - 1 >*/
    kl = *igh - *low - 1;
/*<       if (kl .lt. 1) go to 200 >*/
    if (kl < 1) {
        goto L200;
    }
/*     .......... for mp=igh-1 step -1 until low+1 do -- .......... */
/*<       do 140 mm = 1, kl >*/
    i__1 = kl;
    for (mm = 1; mm <= i__1; ++mm) {
/*<          mp = igh - mm >*/
        mp = *igh - mm;
/*<          mp1 = mp + 1 >*/
        mp1 = mp + 1;

/*<          do 100 i = mp1, igh >*/
        i__2 = *igh;
        for (i__ = mp1; i__ <= i__2; ++i__) {
/*<   100    z(i,mp) = a(i,mp-1) >*/
/* L100: */
            z__[i__ + mp * z_dim1] = a[i__ + (mp - 1) * a_dim1];
        }

/*<          i = int(mp) >*/
        i__ = int__[mp];
/*<          if (i .eq. mp) go to 140 >*/
        if (i__ == mp) {
            goto L140;
        }

/*<          do 130 j = mp, igh >*/
        i__2 = *igh;
        for (j = mp; j <= i__2; ++j) {
/*<             z(mp,j) = z(i,j) >*/
            z__[mp + j * z_dim1] = z__[i__ + j * z_dim1];
/*<             z(i,j) = 0.0d0 >*/
            z__[i__ + j * z_dim1] = 0.;
/*<   130    continue >*/
/* L130: */
        }

/*<          z(i,mp) = 1.0d0 >*/
        z__[i__ + mp * z_dim1] = 1.;
/*<   140 continue >*/
L140:
        ;
    }

/*<   200 return >*/
L200:
    return 0;
/*<       end >*/
} /* eltran_ */

#ifdef __cplusplus
        }
#endif
