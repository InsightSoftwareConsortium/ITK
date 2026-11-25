/* eispack/rg.f -- translated by f2c (version 20050501).
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

/*<       subroutine rg(nm,n,a,wr,wi,matz,z,iv1,fv1,ierr) >*/
/* Subroutine */ int rg_(integer *nm, integer *n, doublereal *a, doublereal *
        wr, doublereal *wi, integer *matz, doublereal *z__, integer *iv1,
        doublereal *fv1, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    integer is1, is2;
    extern /* Subroutine */ int hqr_(integer *, integer *, integer *, integer
            *, doublereal *, doublereal *, doublereal *, integer *), hqr2_(
            integer *, integer *, integer *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, integer *), balbak_(
            integer *, integer *, integer *, integer *, doublereal *, integer
            *, doublereal *), balanc_(integer *, integer *, doublereal *,
            integer *, integer *, doublereal *), elmhes_(integer *, integer *,
             integer *, integer *, doublereal *, integer *), eltran_(integer *
            , integer *, integer *, integer *, doublereal *, integer *,
            doublereal *);


/*<       integer n,nm,is1,is2,ierr,matz >*/
/*<       double precision a(nm,n),wr(n),wi(n),z(nm,n),fv1(n) >*/
/*<       integer iv1(n) >*/

/*     this subroutine calls the recommended sequence of */
/*     subroutines from the eigensystem subroutine package (eispack) */
/*     to find the eigenvalues and eigenvectors (if desired) */
/*     of a real general matrix. */

/*     on input */

/*        nm  must be set to the row dimension of the two-dimensional */
/*        array parameters as declared in the calling program */
/*        dimension statement. */

/*        n  is the order of the matrix  a. */

/*        a  contains the real general matrix. */

/*        matz  is an integer variable set equal to zero if */
/*        only eigenvalues are desired.  otherwise it is set to */
/*        any non-zero integer for both eigenvalues and eigenvectors. */

/*     on output */

/*        wr  and  wi  contain the real and imaginary parts, */
/*        respectively, of the eigenvalues.  complex conjugate */
/*        pairs of eigenvalues appear consecutively with the */
/*        eigenvalue having the positive imaginary part first. */

/*        z  contains the real and imaginary parts of the eigenvectors */
/*        if matz is not zero.  if the j-th eigenvalue is real, the */
/*        j-th column of  z  contains its eigenvector.  if the j-th */
/*        eigenvalue is complex with positive imaginary part, the */
/*        j-th and (j+1)-th columns of  z  contain the real and */
/*        imaginary parts of its eigenvector.  the conjugate of this */
/*        vector is the eigenvector for the conjugate eigenvalue. */

/*        ierr  is an integer output variable set equal to an error */
/*           completion code described in the documentation for hqr */
/*           and hqr2.  the normal completion code is zero. */

/*        iv1  and  fv1  are temporary storage arrays. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

/*<       if (n .le. nm) go to 10 >*/
    /* Parameter adjustments */
    --fv1;
    --iv1;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --wi;
    --wr;
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

/*<    10 call  balanc(nm,n,a,is1,is2,fv1) >*/
L10:
    balanc_(nm, n, &a[a_offset], &is1, &is2, &fv1[1]);
/*<       call  elmhes(nm,n,is1,is2,a,iv1) >*/
    elmhes_(nm, n, &is1, &is2, &a[a_offset], &iv1[1]);
/*<       if (matz .ne. 0) go to 20 >*/
    if (*matz != 0) {
        goto L20;
    }
/*     .......... find eigenvalues only .......... */
/*<       call  hqr(nm,n,is1,is2,a,wr,wi,ierr) >*/
    hqr_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], ierr);
/*<       go to 50 >*/
    goto L50;
/*     .......... find both eigenvalues and eigenvectors .......... */
/*<    20 call  eltran(nm,n,is1,is2,a,iv1,z) >*/
L20:
    eltran_(nm, n, &is1, &is2, &a[a_offset], &iv1[1], &z__[z_offset]);
/*<       call  hqr2(nm,n,is1,is2,a,wr,wi,z,ierr) >*/
    hqr2_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], &z__[z_offset],
            ierr);
/*<       if (ierr .ne. 0) go to 50 >*/
    if (*ierr != 0) {
        goto L50;
    }
/*<       call  balbak(nm,n,is1,is2,fv1,n,z) >*/
    balbak_(nm, n, &is1, &is2, &fv1[1], n, &z__[z_offset]);
/*<    50 return >*/
L50:
    return 0;
/*<       end >*/
} /* rg_ */

#ifdef __cplusplus
        }
#endif
