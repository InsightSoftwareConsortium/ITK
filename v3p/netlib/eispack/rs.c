/* eispack/rs.f -- translated by f2c (version 20050501).
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

/*<       subroutine rs(nm,n,a,w,matz,z,fv1,fv2,ierr) >*/
/* Subroutine */ int rs_(integer *nm, integer *n, doublereal *a, doublereal *
        w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2,
        integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tql1_(integer *, doublereal *, doublereal *,
            integer *), tql2_(integer *, integer *, doublereal *, doublereal *
            , doublereal *, integer *), tred1_(integer *, integer *,
            doublereal *, doublereal *, doublereal *, doublereal *), tred2_(
            integer *, integer *, doublereal *, doublereal *, doublereal *,
            doublereal *);


/*<       integer n,nm,ierr,matz >*/
/*<       double precision a(nm,n),w(n),z(nm,n),fv1(n),fv2(n) >*/

/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real symmetric matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the real symmetric matrix. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        w  contains the eigenvalues in ascending order. */

/*        z  contains the eigenvectors if matz is not zero. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for tqlrat */
/*           and tql2.  the normal completion code is zero. */

/*        fv1  and  fv2  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       if (n .le. nm) go to 10 >*/
    /* Parameter adjustments */
    --fv2;
    --fv1;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --w;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
        goto L10;
    }
/*<       ierr = 10 * n >*/
    *ierr = *n * 10;
/*<       go to 50 >*/
    goto L50;

/*<    10 if (matz .ne. 0) go to 20 >*/
L10:
    if (*matz != 0) {
        goto L20;
    }
/*     .......... find eigenvalues only .......... */
/*<       call  tred1(nm,n,a,w,fv1,fv2) >*/
    tred1_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
/*  tqlrat encounters catastrophic underflow on the Vax */
/*     call  tqlrat(n,w,fv2,ierr) */
/*<       call  tql1(n,w,fv1,ierr) >*/
    tql1_(n, &w[1], &fv1[1], ierr);
/*<       go to 50 >*/
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
/*<    20 call  tred2(nm,n,a,w,fv1,z) >*/
L20:
    tred2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset]);
/*<       call  tql2(nm,n,w,fv1,z,ierr) >*/
    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
/*<    50 return >*/
L50:
    return 0;
/*<       end >*/
} /* rs_ */

#ifdef __cplusplus
        }
#endif
