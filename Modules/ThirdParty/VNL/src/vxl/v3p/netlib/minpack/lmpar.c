/* minpack/lmpar.f -- translated by f2c (version 20050501).
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

static integer c__2 = 2;

/*<    >*/
/* Subroutine */ int lmpar_(integer *n, doublereal *r__, integer *ldr,
        integer *ipvt, doublereal *diag, doublereal *qtb, doublereal *delta,
        doublereal *par, doublereal *x, doublereal *sdiag, doublereal *wa1,
        doublereal *wa2)
{
    /* Initialized data */

    static doublereal p1 = .1; /* constant */
    static doublereal p001 = .001; /* constant */
    static doublereal zero = 0.; /* constant */

    /* System generated locals */
    integer r_dim1, r_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, k, l;
    doublereal fp;
    integer jm1, jp1;
    doublereal sum, parc, parl;
    integer iter;
    doublereal temp, paru, dwarf;
    integer nsing;
    extern doublereal enorm_(integer *, doublereal *);
    doublereal gnorm;
    extern doublereal dpmpar_(integer *);
    doublereal dxnorm;
    extern /* Subroutine */ int qrsolv_(integer *, doublereal *, integer *,
            integer *, doublereal *, doublereal *, doublereal *, doublereal *,
             doublereal *);

/*<       integer n,ldr >*/
/*<       integer ipvt(n) >*/
/*<       double precision delta,par >*/
/*<    >*/
/*     ********** */

/*     subroutine lmpar */

/*     given an m by n matrix a, an n by n nonsingular diagonal */
/*     matrix d, an m-vector b, and a positive number delta, */
/*     the problem is to determine a value for the parameter */
/*     par such that if x solves the system */

/*           a*x = b ,     sqrt(par)*d*x = 0 , */

/*     in the least squares sense, and dxnorm is the euclidean */
/*     norm of d*x, then either par is zero and */

/*           (dxnorm-delta) .le. 0.1*delta , */

/*     or par is positive and */

/*           abs(dxnorm-delta) .le. 0.1*delta . */

/*     this subroutine completes the solution of the problem */
/*     if it is provided with the necessary information from the */
/*     qr factorization, with column pivoting, of a. that is, if */
/*     a*p = q*r, where p is a permutation matrix, q has orthogonal */
/*     columns, and r is an upper triangular matrix with diagonal */
/*     elements of nonincreasing magnitude, then lmpar expects */
/*     the full upper triangle of r, the permutation matrix p, */
/*     and the first n components of (q transpose)*b. on output */
/*     lmpar also provides an upper triangular matrix s such that */

/*            t   t                   t */
/*           p *(a *a + par*d*d)*p = s *s . */

/*     s is employed within lmpar and may be of separate interest. */

/*     only a few iterations are generally needed for convergence */
/*     of the algorithm. if, however, the limit of 10 iterations */
/*     is reached, then the output par will contain the best */
/*     value obtained so far. */

/*     the subroutine statement is */

/*       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag, */
/*                        wa1,wa2) */

/*     where */

/*       n is a positive integer input variable set to the order of r. */

/*       r is an n by n array. on input the full upper triangle */
/*         must contain the full upper triangle of the matrix r. */
/*         on output the full upper triangle is unaltered, and the */
/*         strict lower triangle contains the strict upper triangle */
/*         (transposed) of the upper triangular matrix s. */

/*       ldr is a positive integer input variable not less than n */
/*         which specifies the leading dimension of the array r. */

/*       ipvt is an integer input array of length n which defines the */
/*         permutation matrix p such that a*p = q*r. column j of p */
/*         is column ipvt(j) of the identity matrix. */

/*       diag is an input array of length n which must contain the */
/*         diagonal elements of the matrix d. */

/*       qtb is an input array of length n which must contain the first */
/*         n elements of the vector (q transpose)*b. */

/*       delta is a positive input variable which specifies an upper */
/*         bound on the euclidean norm of d*x. */

/*       par is a nonnegative variable. on input par contains an */
/*         initial estimate of the levenberg-marquardt parameter. */
/*         on output par contains the final estimate. */

/*       x is an output array of length n which contains the least */
/*         squares solution of the system a*x = b, sqrt(par)*d*x = 0, */
/*         for the output par. */

/*       sdiag is an output array of length n which contains the */
/*         diagonal elements of the upper triangular matrix s. */

/*       wa1 and wa2 are work arrays of length n. */

/*     subprograms called */

/*       minpack-supplied ... dpmpar,enorm,qrsolv */

/*       fortran-supplied ... dabs,dmax1,dmin1,dsqrt */

/*     argonne national laboratory. minpack project. march 1980. */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more */

/*     ********** */
/*<       integer i,iter,j,jm1,jp1,k,l,nsing >*/
/*<    >*/
/*<       double precision dpmpar,enorm >*/
/*<       data p1,p001,zero /1.0d-1,1.0d-3,0.0d0/ >*/
    /* Parameter adjustments */
    --wa2;
    --wa1;
    --sdiag;
    --x;
    --qtb;
    --diag;
    --ipvt;
    r_dim1 = *ldr;
    r_offset = 1 + r_dim1;
    r__ -= r_offset;

    /* Function Body */

/*     dwarf is the smallest positive magnitude. */

/*<       dwarf = dpmpar(2) >*/
    dwarf = dpmpar_(&c__2);

/*     compute and store in x the gauss-newton direction. if the */
/*     jacobian is rank-deficient, obtain a least squares solution. */

/*<       nsing = n >*/
    nsing = *n;
/*<       do 10 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          wa1(j) = qtb(j) >*/
        wa1[j] = qtb[j];
/*<          if (r(j,j) .eq. zero .and. nsing .eq. n) nsing = j - 1 >*/
        if (r__[j + j * r_dim1] == zero && nsing == *n) {
            nsing = j - 1;
        }
/*<          if (nsing .lt. n) wa1(j) = zero >*/
        if (nsing < *n) {
            wa1[j] = zero;
        }
/*<    10    continue >*/
/* L10: */
    }
/*<       if (nsing .lt. 1) go to 50 >*/
    if (nsing < 1) {
        goto L50;
    }
/*<       do 40 k = 1, nsing >*/
    i__1 = nsing;
    for (k = 1; k <= i__1; ++k) {
/*<          j = nsing - k + 1 >*/
        j = nsing - k + 1;
/*<          wa1(j) = wa1(j)/r(j,j) >*/
        wa1[j] /= r__[j + j * r_dim1];
/*<          temp = wa1(j) >*/
        temp = wa1[j];
/*<          jm1 = j - 1 >*/
        jm1 = j - 1;
/*<          if (jm1 .lt. 1) go to 30 >*/
        if (jm1 < 1) {
            goto L30;
        }
/*<          do 20 i = 1, jm1 >*/
        i__2 = jm1;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             wa1(i) = wa1(i) - r(i,j)*temp >*/
            wa1[i__] -= r__[i__ + j * r_dim1] * temp;
/*<    20       continue >*/
/* L20: */
        }
/*<    30    continue >*/
L30:
/*<    40    continue >*/
/* L40: */
        ;
    }
/*<    50 continue >*/
L50:
/*<       do 60 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          l = ipvt(j) >*/
        l = ipvt[j];
/*<          x(l) = wa1(j) >*/
        x[l] = wa1[j];
/*<    60    continue >*/
/* L60: */
    }

/*     initialize the iteration counter. */
/*     evaluate the function at the origin, and test */
/*     for acceptance of the gauss-newton direction. */

/*<       iter = 0 >*/
    iter = 0;
/*<       do 70 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          wa2(j) = diag(j)*x(j) >*/
        wa2[j] = diag[j] * x[j];
/*<    70    continue >*/
/* L70: */
    }
/*<       dxnorm = enorm(n,wa2) >*/
    dxnorm = enorm_(n, &wa2[1]);
/*<       fp = dxnorm - delta >*/
    fp = dxnorm - *delta;
/*<       if (fp .le. p1*delta) go to 220 >*/
    if (fp <= p1 * *delta) {
        goto L220;
    }

/*     if the jacobian is not rank deficient, the newton */
/*     step provides a lower bound, parl, for the zero of */
/*     the function. otherwise set this bound to zero. */

/*<       parl = zero >*/
    parl = zero;
/*<       if (nsing .lt. n) go to 120 >*/
    if (nsing < *n) {
        goto L120;
    }
/*<       do 80 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          l = ipvt(j) >*/
        l = ipvt[j];
/*<          wa1(j) = diag(l)*(wa2(l)/dxnorm) >*/
        wa1[j] = diag[l] * (wa2[l] / dxnorm);
/*<    80    continue >*/
/* L80: */
    }
/*<       do 110 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          sum = zero >*/
        sum = zero;
/*<          jm1 = j - 1 >*/
        jm1 = j - 1;
/*<          if (jm1 .lt. 1) go to 100 >*/
        if (jm1 < 1) {
            goto L100;
        }
/*<          do 90 i = 1, jm1 >*/
        i__2 = jm1;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             sum = sum + r(i,j)*wa1(i) >*/
            sum += r__[i__ + j * r_dim1] * wa1[i__];
/*<    90       continue >*/
/* L90: */
        }
/*<   100    continue >*/
L100:
/*<          wa1(j) = (wa1(j) - sum)/r(j,j) >*/
        wa1[j] = (wa1[j] - sum) / r__[j + j * r_dim1];
/*<   110    continue >*/
/* L110: */
    }
/*<       temp = enorm(n,wa1) >*/
    temp = enorm_(n, &wa1[1]);
/*<       parl = ((fp/delta)/temp)/temp >*/
    parl = fp / *delta / temp / temp;
/*<   120 continue >*/
L120:

/*     calculate an upper bound, paru, for the zero of the function. */

/*<       do 140 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          sum = zero >*/
        sum = zero;
/*<          do 130 i = 1, j >*/
        i__2 = j;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             sum = sum + r(i,j)*qtb(i) >*/
            sum += r__[i__ + j * r_dim1] * qtb[i__];
/*<   130       continue >*/
/* L130: */
        }
/*<          l = ipvt(j) >*/
        l = ipvt[j];
/*<          wa1(j) = sum/diag(l) >*/
        wa1[j] = sum / diag[l];
/*<   140    continue >*/
/* L140: */
    }
/*<       gnorm = enorm(n,wa1) >*/
    gnorm = enorm_(n, &wa1[1]);
/*<       paru = gnorm/delta >*/
    paru = gnorm / *delta;
/*<       if (paru .eq. zero) paru = dwarf/dmin1(delta,p1) >*/
    if (paru == zero) {
        paru = dwarf / min(*delta,p1);
    }

/*     if the input par lies outside of the interval (parl,paru), */
/*     set par to the closer endpoint. */

/*<       par = dmax1(par,parl) >*/
    *par = max(*par,parl);
/*<       par = dmin1(par,paru) >*/
    *par = min(*par,paru);
/*<       if (par .eq. zero) par = gnorm/dxnorm >*/
    if (*par == zero) {
        *par = gnorm / dxnorm;
    }

/*     beginning of an iteration. */

/*<   150 continue >*/
L150:
/*<          iter = iter + 1 >*/
    ++iter;

/*        evaluate the function at the current value of par. */

/*<          if (par .eq. zero) par = dmax1(dwarf,p001*paru) >*/
    if (*par == zero) {
/* Computing MAX */
        d__1 = dwarf, d__2 = p001 * paru;
        *par = max(d__1,d__2);
    }
/*<          temp = dsqrt(par) >*/
    temp = sqrt(*par);
/*<          do 160 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             wa1(j) = temp*diag(j) >*/
        wa1[j] = temp * diag[j];
/*<   160       continue >*/
/* L160: */
    }
/*<          call qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2) >*/
    qrsolv_(n, &r__[r_offset], ldr, &ipvt[1], &wa1[1], &qtb[1], &x[1], &sdiag[
            1], &wa2[1]);
/*<          do 170 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             wa2(j) = diag(j)*x(j) >*/
        wa2[j] = diag[j] * x[j];
/*<   170       continue >*/
/* L170: */
    }
/*<          dxnorm = enorm(n,wa2) >*/
    dxnorm = enorm_(n, &wa2[1]);
/*<          temp = fp >*/
    temp = fp;
/*<          fp = dxnorm - delta >*/
    fp = dxnorm - *delta;

/*        if the function is small enough, accept the current value */
/*        of par. also test for the exceptional cases where parl */
/*        is zero or the number of iterations has reached 10. */

/*<    >*/
    if (abs(fp) <= p1 * *delta || (parl == zero && fp <= temp && temp < zero) ||
             iter == 10) {
        goto L220;
    }

/*        compute the newton correction. */

/*<          do 180 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             l = ipvt(j) >*/
        l = ipvt[j];
/*<             wa1(j) = diag(l)*(wa2(l)/dxnorm) >*/
        wa1[j] = diag[l] * (wa2[l] / dxnorm);
/*<   180       continue >*/
/* L180: */
    }
/*<          do 210 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             wa1(j) = wa1(j)/sdiag(j) >*/
        wa1[j] /= sdiag[j];
/*<             temp = wa1(j) >*/
        temp = wa1[j];
/*<             jp1 = j + 1 >*/
        jp1 = j + 1;
/*<             if (n .lt. jp1) go to 200 >*/
        if (*n < jp1) {
            goto L200;
        }
/*<             do 190 i = jp1, n >*/
        i__2 = *n;
        for (i__ = jp1; i__ <= i__2; ++i__) {
/*<                wa1(i) = wa1(i) - r(i,j)*temp >*/
            wa1[i__] -= r__[i__ + j * r_dim1] * temp;
/*<   190          continue >*/
/* L190: */
        }
/*<   200       continue >*/
L200:
/*<   210       continue >*/
/* L210: */
        ;
    }
/*<          temp = enorm(n,wa1) >*/
    temp = enorm_(n, &wa1[1]);
/*<          parc = ((fp/delta)/temp)/temp >*/
    parc = fp / *delta / temp / temp;

/*        depending on the sign of the function, update parl or paru. */

/*<          if (fp .gt. zero) parl = dmax1(parl,par) >*/
    if (fp > zero) {
        parl = max(parl,*par);
    }
/*<          if (fp .lt. zero) paru = dmin1(paru,par) >*/
    if (fp < zero) {
        paru = min(paru,*par);
    }

/*        compute an improved estimate for par. */

/*<          par = dmax1(parl,par+parc) >*/
/* Computing MAX */
    d__1 = parl, d__2 = *par + parc;
    *par = max(d__1,d__2);

/*        end of an iteration. */

/*<          go to 150 >*/
    goto L150;
/*<   220 continue >*/
L220:

/*     termination. */

/*<       if (iter .eq. 0) par = zero >*/
    if (iter == 0) {
        *par = zero;
    }
/*<       return >*/
    return 0;

/*     last card of subroutine lmpar. */

/*<       end >*/
} /* lmpar_ */

#ifdef __cplusplus
        }
#endif
