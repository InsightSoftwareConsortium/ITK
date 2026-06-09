/* minpack/qrsolv.f -- translated by f2c (version 20050501).
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

/*<       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa) >*/
/* Subroutine */ int qrsolv_(integer *n, doublereal *r__, integer *ldr,
        integer *ipvt, doublereal *diag, doublereal *qtb, doublereal *x,
        doublereal *sdiag, doublereal *wa)
{
    /* Initialized data */

    static doublereal p5 = .5; /* constant */
    static doublereal p25 = .25; /* constant */
    static doublereal zero = 0.; /* constant */

    /* System generated locals */
    integer r_dim1, r_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, k, l, jp1, kp1;
    doublereal tan__, cos__, sin__, sum, temp, cotan;
    integer nsing;
    doublereal qtbpj;

/*<       integer n,ldr >*/
/*<       integer ipvt(n) >*/
/*<       double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa(n) >*/
/*     ********** */

/*     subroutine qrsolv */

/*     given an m by n matrix a, an n by n diagonal matrix d, */
/*     and an m-vector b, the problem is to determine an x which */
/*     solves the system */

/*           a*x = b ,     d*x = 0 , */

/*     in the least squares sense. */

/*     this subroutine completes the solution of the problem */
/*     if it is provided with the necessary information from the */
/*     qr factorization, with column pivoting, of a. that is, if */
/*     a*p = q*r, where p is a permutation matrix, q has orthogonal */
/*     columns, and r is an upper triangular matrix with diagonal */
/*     elements of nonincreasing magnitude, then qrsolv expects */
/*     the full upper triangle of r, the permutation matrix p, */
/*     and the first n components of (q transpose)*b. the system */
/*     a*x = b, d*x = 0, is then equivalent to */

/*                  t       t */
/*           r*z = q *b ,  p *d*p*z = 0 , */

/*     where x = p*z. if this system does not have full rank, */
/*     then a least squares solution is obtained. on output qrsolv */
/*     also provides an upper triangular matrix s such that */

/*            t   t               t */
/*           p *(a *a + d*d)*p = s *s . */

/*     s is computed within qrsolv and may be of separate interest. */

/*     the subroutine statement is */

/*       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa) */

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

/*       x is an output array of length n which contains the least */
/*         squares solution of the system a*x = b, d*x = 0. */

/*       sdiag is an output array of length n which contains the */
/*         diagonal elements of the upper triangular matrix s. */

/*       wa is a work array of length n. */

/*     subprograms called */

/*       fortran-supplied ... dabs,dsqrt */

/*     argonne national laboratory. minpack project. march 1980. */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more */

/*     ********** */
/*<       integer i,j,jp1,k,kp1,l,nsing >*/
/*<       double precision cos,cotan,p5,p25,qtbpj,sin,sum,tan,temp,zero >*/
/*<       data p5,p25,zero /5.0d-1,2.5d-1,0.0d0/ >*/
    /* Parameter adjustments */
    --wa;
    --sdiag;
    --x;
    --qtb;
    --diag;
    --ipvt;
    r_dim1 = *ldr;
    r_offset = 1 + r_dim1;
    r__ -= r_offset;

    /* Function Body */

/*     copy r and (q transpose)*b to preserve input and initialize s. */
/*     in particular, save the diagonal elements of r in x. */

/*<       do 20 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          do 10 i = j, n >*/
        i__2 = *n;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<             r(i,j) = r(j,i) >*/
            r__[i__ + j * r_dim1] = r__[j + i__ * r_dim1];
/*<    10       continue >*/
/* L10: */
        }
/*<          x(j) = r(j,j) >*/
        x[j] = r__[j + j * r_dim1];
/*<          wa(j) = qtb(j) >*/
        wa[j] = qtb[j];
/*<    20    continue >*/
/* L20: */
    }

/*     eliminate the diagonal matrix d using a givens rotation. */

/*<       do 100 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

/*        prepare the row of d to be eliminated, locating the */
/*        diagonal element using p from the qr factorization. */

/*<          l = ipvt(j) >*/
        l = ipvt[j];
/*<          if (diag(l) .eq. zero) go to 90 >*/
        if (diag[l] == zero) {
            goto L90;
        }
/*<          do 30 k = j, n >*/
        i__2 = *n;
        for (k = j; k <= i__2; ++k) {
/*<             sdiag(k) = zero >*/
            sdiag[k] = zero;
/*<    30       continue >*/
/* L30: */
        }
/*<          sdiag(j) = diag(l) >*/
        sdiag[j] = diag[l];

/*        the transformations to eliminate the row of d */
/*        modify only a single element of (q transpose)*b */
/*        beyond the first n, which is initially zero. */

/*<          qtbpj = zero >*/
        qtbpj = zero;
/*<          do 80 k = j, n >*/
        i__2 = *n;
        for (k = j; k <= i__2; ++k) {

/*           determine a givens rotation which eliminates the */
/*           appropriate element in the current row of d. */

/*<             if (sdiag(k) .eq. zero) go to 70 >*/
            if (sdiag[k] == zero) {
                goto L70;
            }
/*<             if (dabs(r(k,k)) .ge. dabs(sdiag(k))) go to 40 >*/
            if ((d__1 = r__[k + k * r_dim1], abs(d__1)) >= (d__2 = sdiag[k],
                    abs(d__2))) {
                goto L40;
            }
/*<                cotan = r(k,k)/sdiag(k) >*/
            cotan = r__[k + k * r_dim1] / sdiag[k];
/*<                sin = p5/dsqrt(p25+p25*cotan**2) >*/
/* Computing 2nd power */
            d__1 = cotan;
            sin__ = p5 / sqrt(p25 + p25 * (d__1 * d__1));
/*<                cos = sin*cotan >*/
            cos__ = sin__ * cotan;
/*<                go to 50 >*/
            goto L50;
/*<    40       continue >*/
L40:
/*<                tan = sdiag(k)/r(k,k) >*/
            tan__ = sdiag[k] / r__[k + k * r_dim1];
/*<                cos = p5/dsqrt(p25+p25*tan**2) >*/
/* Computing 2nd power */
            d__1 = tan__;
            cos__ = p5 / sqrt(p25 + p25 * (d__1 * d__1));
/*<                sin = cos*tan >*/
            sin__ = cos__ * tan__;
/*<    50       continue >*/
L50:

/*           compute the modified diagonal element of r and */
/*           the modified element of ((q transpose)*b,0). */

/*<             r(k,k) = cos*r(k,k) + sin*sdiag(k) >*/
            r__[k + k * r_dim1] = cos__ * r__[k + k * r_dim1] + sin__ * sdiag[
                    k];
/*<             temp = cos*wa(k) + sin*qtbpj >*/
            temp = cos__ * wa[k] + sin__ * qtbpj;
/*<             qtbpj = -sin*wa(k) + cos*qtbpj >*/
            qtbpj = -sin__ * wa[k] + cos__ * qtbpj;
/*<             wa(k) = temp >*/
            wa[k] = temp;

/*           accumulate the transformation in the row of s. */

/*<             kp1 = k + 1 >*/
            kp1 = k + 1;
/*<             if (n .lt. kp1) go to 70 >*/
            if (*n < kp1) {
                goto L70;
            }
/*<             do 60 i = kp1, n >*/
            i__3 = *n;
            for (i__ = kp1; i__ <= i__3; ++i__) {
/*<                temp = cos*r(i,k) + sin*sdiag(i) >*/
                temp = cos__ * r__[i__ + k * r_dim1] + sin__ * sdiag[i__];
/*<                sdiag(i) = -sin*r(i,k) + cos*sdiag(i) >*/
                sdiag[i__] = -sin__ * r__[i__ + k * r_dim1] + cos__ * sdiag[
                        i__];
/*<                r(i,k) = temp >*/
                r__[i__ + k * r_dim1] = temp;
/*<    60          continue >*/
/* L60: */
            }
/*<    70       continue >*/
L70:
/*<    80       continue >*/
/* L80: */
            ;
        }
/*<    90    continue >*/
L90:

/*        store the diagonal element of s and restore */
/*        the corresponding diagonal element of r. */

/*<          sdiag(j) = r(j,j) >*/
        sdiag[j] = r__[j + j * r_dim1];
/*<          r(j,j) = x(j) >*/
        r__[j + j * r_dim1] = x[j];
/*<   100    continue >*/
/* L100: */
    }

/*     solve the triangular system for z. if the system is */
/*     singular, then obtain a least squares solution. */

/*<       nsing = n >*/
    nsing = *n;
/*<       do 110 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1 >*/
        if (sdiag[j] == zero && nsing == *n) {
            nsing = j - 1;
        }
/*<          if (nsing .lt. n) wa(j) = zero >*/
        if (nsing < *n) {
            wa[j] = zero;
        }
/*<   110    continue >*/
/* L110: */
    }
/*<       if (nsing .lt. 1) go to 150 >*/
    if (nsing < 1) {
        goto L150;
    }
/*<       do 140 k = 1, nsing >*/
    i__1 = nsing;
    for (k = 1; k <= i__1; ++k) {
/*<          j = nsing - k + 1 >*/
        j = nsing - k + 1;
/*<          sum = zero >*/
        sum = zero;
/*<          jp1 = j + 1 >*/
        jp1 = j + 1;
/*<          if (nsing .lt. jp1) go to 130 >*/
        if (nsing < jp1) {
            goto L130;
        }
/*<          do 120 i = jp1, nsing >*/
        i__2 = nsing;
        for (i__ = jp1; i__ <= i__2; ++i__) {
/*<             sum = sum + r(i,j)*wa(i) >*/
            sum += r__[i__ + j * r_dim1] * wa[i__];
/*<   120       continue >*/
/* L120: */
        }
/*<   130    continue >*/
L130:
/*<          wa(j) = (wa(j) - sum)/sdiag(j) >*/
        wa[j] = (wa[j] - sum) / sdiag[j];
/*<   140    continue >*/
/* L140: */
    }
/*<   150 continue >*/
L150:

/*     permute the components of z back to components of x. */

/*<       do 160 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          l = ipvt(j) >*/
        l = ipvt[j];
/*<          x(l) = wa(j) >*/
        x[l] = wa[j];
/*<   160    continue >*/
/* L160: */
    }
/*<       return >*/
    return 0;

/*     last card of subroutine qrsolv. */

/*<       end >*/
} /* qrsolv_ */

#ifdef __cplusplus
        }
#endif
