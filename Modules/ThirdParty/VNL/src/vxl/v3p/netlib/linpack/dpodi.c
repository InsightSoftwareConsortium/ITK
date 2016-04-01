/* linpack/dpodi.f -- translated by f2c (version 20050501).
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

static integer c__1 = 1;

/*<       subroutine dpodi(a,lda,n,det,job) >*/
/* Subroutine */ int dpodi_(doublereal *a, integer *lda, integer *n,
        doublereal *det, integer *job)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    integer i__, j, k;
    doublereal s, t;
    integer jm1, kp1;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), daxpy_(integer *, doublereal *, doublereal *, integer
            *, doublereal *, integer *);

/*<       integer lda,n,job >*/
/*<       double precision a(lda,1) >*/
/*<       double precision det(2) >*/

/*     dpodi computes the determinant and inverse of a certain */
/*     double precision symmetric positive definite matrix (see below) */
/*     using the factors computed by dpoco, dpofa or dqrdc. */

/*     on entry */

/*        a       double precision(lda, n) */
/*                the output  a  from dpoco or dpofa */
/*                or the output  x  from dqrdc. */

/*        lda     integer */
/*                the leading dimension of the array  a . */

/*        n       integer */
/*                the order of the matrix  a . */

/*        job     integer */
/*                = 11   both determinant and inverse. */
/*                = 01   inverse only. */
/*                = 10   determinant only. */

/*     on return */

/*        a       if dpoco or dpofa was used to factor  a  then */
/*                dpodi produces the upper half of inverse(a) . */
/*                if dqrdc was used to decompose  x  then */
/*                dpodi produces the upper half of inverse(trans(x)*x) */
/*                where trans(x) is the transpose. */
/*                elements of  a  below the diagonal are unchanged. */
/*                if the units digit of job is zero,  a  is unchanged. */

/*        det     double precision(2) */
/*                determinant of  a  or of  trans(x)*x  if requested. */
/*                otherwise not referenced. */
/*                determinant = det(1) * 10.0**det(2) */
/*                with  1.0 .le. det(1) .lt. 10.0 */
/*                or  det(1) .eq. 0.0 . */

/*     error condition */

/*        a division by zero will occur if the input factor contains */
/*        a zero on the diagonal and the inverse is requested. */
/*        it will not occur if the subroutines are called correctly */
/*        and if dpoco or dpofa has set info .eq. 0 . */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas daxpy,dscal */
/*     fortran mod */

/*     internal variables */

/*<       double precision t >*/
/*<       double precision s >*/
/*<       integer i,j,jm1,k,kp1 >*/

/*     compute determinant */

/*<       if (job/10 .eq. 0) go to 70 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --det;

    /* Function Body */
    if (*job / 10 == 0) {
        goto L70;
    }
/*<          det(1) = 1.0d0 >*/
    det[1] = 1.;
/*<          det(2) = 0.0d0 >*/
    det[2] = 0.;
/*<          s = 10.0d0 >*/
    s = 10.;
/*<          do 50 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<             det(1) = a(i,i)**2*det(1) >*/
/* Computing 2nd power */
        d__1 = a[i__ + i__ * a_dim1];
        det[1] = d__1 * d__1 * det[1];
/*        ...exit */
/*<             if (det(1) .eq. 0.0d0) go to 60 >*/
        if (det[1] == 0.) {
            goto L60;
        }
/*<    10       if (det(1) .ge. 1.0d0) go to 20 >*/
L10:
        if (det[1] >= 1.) {
            goto L20;
        }
/*<                det(1) = s*det(1) >*/
        det[1] = s * det[1];
/*<                det(2) = det(2) - 1.0d0 >*/
        det[2] += -1.;
/*<             go to 10 >*/
        goto L10;
/*<    20       continue >*/
L20:
/*<    30       if (det(1) .lt. s) go to 40 >*/
L30:
        if (det[1] < s) {
            goto L40;
        }
/*<                det(1) = det(1)/s >*/
        det[1] /= s;
/*<                det(2) = det(2) + 1.0d0 >*/
        det[2] += 1.;
/*<             go to 30 >*/
        goto L30;
/*<    40       continue >*/
L40:
/*<    50    continue >*/
/* L50: */
        ;
    }
/*<    60    continue >*/
L60:
/*<    70 continue >*/
L70:

/*     compute inverse(r) */

/*<       if (mod(job,10) .eq. 0) go to 140 >*/
    if (*job % 10 == 0) {
        goto L140;
    }
/*<          do 100 k = 1, n >*/
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/*<             a(k,k) = 1.0d0/a(k,k) >*/
        a[k + k * a_dim1] = 1. / a[k + k * a_dim1];
/*<             t = -a(k,k) >*/
        t = -a[k + k * a_dim1];
/*<             call dscal(k-1,t,a(1,k),1) >*/
        i__2 = k - 1;
        dscal_(&i__2, &t, &a[k * a_dim1 + 1], &c__1);
/*<             kp1 = k + 1 >*/
        kp1 = k + 1;
/*<             if (n .lt. kp1) go to 90 >*/
        if (*n < kp1) {
            goto L90;
        }
/*<             do 80 j = kp1, n >*/
        i__2 = *n;
        for (j = kp1; j <= i__2; ++j) {
/*<                t = a(k,j) >*/
            t = a[k + j * a_dim1];
/*<                a(k,j) = 0.0d0 >*/
            a[k + j * a_dim1] = 0.;
/*<                call daxpy(k,t,a(1,k),1,a(1,j),1) >*/
            daxpy_(&k, &t, &a[k * a_dim1 + 1], &c__1, &a[j * a_dim1 + 1], &
                    c__1);
/*<    80       continue >*/
/* L80: */
        }
/*<    90       continue >*/
L90:
/*<   100    continue >*/
/* L100: */
        ;
    }

/*        form  inverse(r) * trans(inverse(r)) */

/*<          do 130 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             jm1 = j - 1 >*/
        jm1 = j - 1;
/*<             if (jm1 .lt. 1) go to 120 >*/
        if (jm1 < 1) {
            goto L120;
        }
/*<             do 110 k = 1, jm1 >*/
        i__2 = jm1;
        for (k = 1; k <= i__2; ++k) {
/*<                t = a(k,j) >*/
            t = a[k + j * a_dim1];
/*<                call daxpy(k,t,a(1,j),1,a(1,k),1) >*/
            daxpy_(&k, &t, &a[j * a_dim1 + 1], &c__1, &a[k * a_dim1 + 1], &
                    c__1);
/*<   110       continue >*/
/* L110: */
        }
/*<   120       continue >*/
L120:
/*<             t = a(j,j) >*/
        t = a[j + j * a_dim1];
/*<             call dscal(j,t,a(1,j),1) >*/
        dscal_(&j, &t, &a[j * a_dim1 + 1], &c__1);
/*<   130    continue >*/
/* L130: */
    }
/*<   140 continue >*/
L140:
/*<       return >*/
    return 0;
/*<       end >*/
} /* dpodi_ */

#ifdef __cplusplus
        }
#endif
