/* minpack/qrfac.f -- translated by f2c (version 20050501).
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

/*<       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa) >*/
/* Subroutine */ int qrfac_(integer *m, integer *n, doublereal *a, integer *
        lda, logical *pivot, integer *ipvt, integer *lipvt, doublereal *rdiag,
         doublereal *acnorm, doublereal *wa)
{
    /* Initialized data */

    static doublereal one = 1.; /* constant */
    static doublereal p05 = .05; /* constant */
    static doublereal zero = 0.; /* constant */

    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, k, jp1;
    doublereal sum;
    integer kmax;
    doublereal temp;
    integer minmn;
    extern doublereal enorm_(integer *, doublereal *);
    doublereal epsmch;
    extern doublereal dpmpar_(integer *);
    doublereal ajnorm;

    (void)lipvt;

/*<       integer m,n,lda,lipvt >*/
/*<       integer ipvt(lipvt) >*/
/*<       logical pivot >*/
/*<       double precision a(lda,n),rdiag(n),acnorm(n),wa(n) >*/
/*     ********** */

/*     subroutine qrfac */

/*     this subroutine uses householder transformations with column */
/*     pivoting (optional) to compute a qr factorization of the */
/*     m by n matrix a. that is, qrfac determines an orthogonal */
/*     matrix q, a permutation matrix p, and an upper trapezoidal */
/*     matrix r with diagonal elements of nonincreasing magnitude, */
/*     such that a*p = q*r. the householder transformation for */
/*     column k, k = 1,2,...,min(m,n), is of the form */

/*                           t */
/*           i - (1/u(k))*u*u */

/*     where u has zeros in the first k-1 positions. the form of */
/*     this transformation and the method of pivoting first */
/*     appeared in the corresponding linpack subroutine. */

/*     the subroutine statement is */

/*       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa) */

/*     where */

/*       m is a positive integer input variable set to the number */
/*         of rows of a. */

/*       n is a positive integer input variable set to the number */
/*         of columns of a. */

/*       a is an m by n array. on input a contains the matrix for */
/*         which the qr factorization is to be computed. on output */
/*         the strict upper trapezoidal part of a contains the strict */
/*         upper trapezoidal part of r, and the lower trapezoidal */
/*         part of a contains a factored form of q (the non-trivial */
/*         elements of the u vectors described above). */

/*       lda is a positive integer input variable not less than m */
/*         which specifies the leading dimension of the array a. */

/*       pivot is a logical input variable. if pivot is set true, */
/*         then column pivoting is enforced. if pivot is set false, */
/*         then no column pivoting is done. */

/*       ipvt is an integer output array of length lipvt. ipvt */
/*         defines the permutation matrix p such that a*p = q*r. */
/*         column j of p is column ipvt(j) of the identity matrix. */
/*         if pivot is false, ipvt is not referenced. */

/*       lipvt is a positive integer input variable. if pivot is false, */
/*         then lipvt may be as small as 1. if pivot is true, then */
/*         lipvt must be at least n. */

/*       rdiag is an output array of length n which contains the */
/*         diagonal elements of r. */

/*       acnorm is an output array of length n which contains the */
/*         norms of the corresponding columns of the input matrix a. */
/*         if this information is not needed, then acnorm can coincide */
/*         with rdiag. */

/*       wa is a work array of length n. if pivot is false, then wa */
/*         can coincide with rdiag. */

/*     subprograms called */

/*       minpack-supplied ... dpmpar,enorm */

/*       fortran-supplied ... dmax1,dsqrt,min0 */

/*     argonne national laboratory. minpack project. march 1980. */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more */

/*     ********** */
/*<       integer i,j,jp1,k,kmax,minmn >*/
/*<       double precision ajnorm,epsmch,one,p05,sum,temp,zero >*/
/*<       double precision dpmpar,enorm >*/
/*<       data one,p05,zero /1.0d0,5.0d-2,0.0d0/ >*/
    /* Parameter adjustments */
    --wa;
    --acnorm;
    --rdiag;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;

    /* Function Body */

/*     epsmch is the machine precision. */

/*<       epsmch = dpmpar(1) >*/
    epsmch = dpmpar_(&c__1);

/*     compute the initial column norms and initialize several arrays. */

/*<       do 10 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          acnorm(j) = enorm(m,a(1,j)) >*/
        acnorm[j] = enorm_(m, &a[j * a_dim1 + 1]);
/*<          rdiag(j) = acnorm(j) >*/
        rdiag[j] = acnorm[j];
/*<          wa(j) = rdiag(j) >*/
        wa[j] = rdiag[j];
/*<          if (pivot) ipvt(j) = j >*/
        if (*pivot) {
            ipvt[j] = j;
        }
/*<    10    continue >*/
/* L10: */
    }

/*     reduce a to r with householder transformations. */

/*<       minmn = min0(m,n) >*/
    minmn = min(*m,*n);
/*<       do 110 j = 1, minmn >*/
    i__1 = minmn;
    for (j = 1; j <= i__1; ++j) {
/*<          if (.not.pivot) go to 40 >*/
        if (! (*pivot)) {
            goto L40;
        }

/*        bring the column of largest norm into the pivot position. */

/*<          kmax = j >*/
        kmax = j;
/*<          do 20 k = j, n >*/
        i__2 = *n;
        for (k = j; k <= i__2; ++k) {
/*<             if (rdiag(k) .gt. rdiag(kmax)) kmax = k >*/
            if (rdiag[k] > rdiag[kmax]) {
                kmax = k;
            }
/*<    20       continue >*/
/* L20: */
        }
/*<          if (kmax .eq. j) go to 40 >*/
        if (kmax == j) {
            goto L40;
        }
/*<          do 30 i = 1, m >*/
        i__2 = *m;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             temp = a(i,j) >*/
            temp = a[i__ + j * a_dim1];
/*<             a(i,j) = a(i,kmax) >*/
            a[i__ + j * a_dim1] = a[i__ + kmax * a_dim1];
/*<             a(i,kmax) = temp >*/
            a[i__ + kmax * a_dim1] = temp;
/*<    30       continue >*/
/* L30: */
        }
/*<          rdiag(kmax) = rdiag(j) >*/
        rdiag[kmax] = rdiag[j];
/*<          wa(kmax) = wa(j) >*/
        wa[kmax] = wa[j];
/*<          k = ipvt(j) >*/
        k = ipvt[j];
/*<          ipvt(j) = ipvt(kmax) >*/
        ipvt[j] = ipvt[kmax];
/*<          ipvt(kmax) = k >*/
        ipvt[kmax] = k;
/*<    40    continue >*/
L40:

/*        compute the householder transformation to reduce the */
/*        j-th column of a to a multiple of the j-th unit vector. */

/*<          ajnorm = enorm(m-j+1,a(j,j)) >*/
        i__2 = *m - j + 1;
        ajnorm = enorm_(&i__2, &a[j + j * a_dim1]);
/*<          if (ajnorm .eq. zero) go to 100 >*/
        if (ajnorm == zero) {
            goto L100;
        }
/*<          if (a(j,j) .lt. zero) ajnorm = -ajnorm >*/
        if (a[j + j * a_dim1] < zero) {
            ajnorm = -ajnorm;
        }
/*<          do 50 i = j, m >*/
        i__2 = *m;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<             a(i,j) = a(i,j)/ajnorm >*/
            a[i__ + j * a_dim1] /= ajnorm;
/*<    50       continue >*/
/* L50: */
        }
/*<          a(j,j) = a(j,j) + one >*/
        a[j + j * a_dim1] += one;

/*        apply the transformation to the remaining columns */
/*        and update the norms. */

/*<          jp1 = j + 1 >*/
        jp1 = j + 1;
/*<          if (n .lt. jp1) go to 100 >*/
        if (*n < jp1) {
            goto L100;
        }
/*<          do 90 k = jp1, n >*/
        i__2 = *n;
        for (k = jp1; k <= i__2; ++k) {
/*<             sum = zero >*/
            sum = zero;
/*<             do 60 i = j, m >*/
            i__3 = *m;
            for (i__ = j; i__ <= i__3; ++i__) {
/*<                sum = sum + a(i,j)*a(i,k) >*/
                sum += a[i__ + j * a_dim1] * a[i__ + k * a_dim1];
/*<    60          continue >*/
/* L60: */
            }
/*<             temp = sum/a(j,j) >*/
            temp = sum / a[j + j * a_dim1];
/*<             do 70 i = j, m >*/
            i__3 = *m;
            for (i__ = j; i__ <= i__3; ++i__) {
/*<                a(i,k) = a(i,k) - temp*a(i,j) >*/
                a[i__ + k * a_dim1] -= temp * a[i__ + j * a_dim1];
/*<    70          continue >*/
/* L70: */
            }
/*<             if (.not.pivot .or. rdiag(k) .eq. zero) go to 80 >*/
            if (! (*pivot) || rdiag[k] == zero) {
                goto L80;
            }
/*<             temp = a(j,k)/rdiag(k) >*/
            temp = a[j + k * a_dim1] / rdiag[k];
/*<             rdiag(k) = rdiag(k)*dsqrt(dmax1(zero,one-temp**2)) >*/
/* Computing MAX */
/* Computing 2nd power */
            d__3 = temp;
            d__1 = zero, d__2 = one - d__3 * d__3;
            rdiag[k] *= sqrt((max(d__1,d__2)));
/*<             if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80 >*/
/* Computing 2nd power */
            d__1 = rdiag[k] / wa[k];
            if (p05 * (d__1 * d__1) > epsmch) {
                goto L80;
            }
/*<             rdiag(k) = enorm(m-j,a(jp1,k)) >*/
            i__3 = *m - j;
            rdiag[k] = enorm_(&i__3, &a[jp1 + k * a_dim1]);
/*<             wa(k) = rdiag(k) >*/
            wa[k] = rdiag[k];
/*<    80       continue >*/
L80:
/*<    90       continue >*/
/* L90: */
            ;
        }
/*<   100    continue >*/
L100:
/*<          rdiag(j) = -ajnorm >*/
        rdiag[j] = -ajnorm;
/*<   110    continue >*/
/* L110: */
    }
/*<       return >*/
    return 0;

/*     last card of subroutine qrfac. */

/*<       end >*/
} /* qrfac_ */

#ifdef __cplusplus
        }
#endif
