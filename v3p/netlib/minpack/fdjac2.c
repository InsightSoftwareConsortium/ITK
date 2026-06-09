/* minpack/fdjac2.f -- translated by f2c (version 20050501).
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

/*<       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa) >*/
/* Subroutine */ int fdjac2_(
        void (*fcn)(integer*,integer*,doublereal*,doublereal*,integer*,void*),
        integer *m, integer *n, doublereal *x,
        doublereal *fvec, doublereal *fjac, integer *ldfjac, integer *iflag,
        doublereal *epsfcn, doublereal *wa, void* userdata)
{
    /* Initialized data */

    static doublereal zero = 0.; /* constant */

    /* System generated locals */
    integer fjac_dim1, fjac_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal h__;
    integer i__, j;
    doublereal eps, temp, epsmch;
    extern doublereal dpmpar_(integer *);

/*<       integer m,n,ldfjac,iflag >*/
/*<       double precision epsfcn >*/
/*<       double precision x(n),fvec(m),fjac(ldfjac,n),wa(m) >*/
/*     ********** */

/*     subroutine fdjac2 */

/*     this subroutine computes a forward-difference approximation */
/*     to the m by n jacobian matrix associated with a specified */
/*     problem of m functions in n variables. */

/*     the subroutine statement is */

/*       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa) */

/*     where */

/*       fcn is the name of the user-supplied subroutine which */
/*         calculates the functions. fcn must be declared */
/*         in an external statement in the user calling */
/*         program, and should be written as follows. */

/*         subroutine fcn(m,n,x,fvec,iflag) */
/*         integer m,n,iflag */
/*         double precision x(n),fvec(m) */
/*         ---------- */
/*         calculate the functions at x and */
/*         return this vector in fvec. */
/*         ---------- */
/*         return */
/*         end */

/*         the value of iflag should not be changed by fcn unless */
/*         the user wants to terminate execution of fdjac2. */
/*         in this case set iflag to a negative integer. */

/*       m is a positive integer input variable set to the number */
/*         of functions. */

/*       n is a positive integer input variable set to the number */
/*         of variables. n must not exceed m. */

/*       x is an input array of length n. */

/*       fvec is an input array of length m which must contain the */
/*         functions evaluated at x. */

/*       fjac is an output m by n array which contains the */
/*         approximation to the jacobian matrix evaluated at x. */

/*       ldfjac is a positive integer input variable not less than m */
/*         which specifies the leading dimension of the array fjac. */

/*       iflag is an integer variable which can be used to terminate */
/*         the execution of fdjac2. see description of fcn. */

/*       epsfcn is an input variable used in determining a suitable */
/*         step length for the forward-difference approximation. this */
/*         approximation assumes that the relative errors in the */
/*         functions are of the order of epsfcn. if epsfcn is less */
/*         than the machine precision, it is assumed that the relative */
/*         errors in the functions are of the order of the machine */
/*         precision. */

/*       wa is a work array of length m. */

/*     subprograms called */

/*       user-supplied ...... fcn */

/*       minpack-supplied ... dpmpar */

/*       fortran-supplied ... dabs,dmax1,dsqrt */

/*     argonne national laboratory. minpack project. march 1980. */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more */

/*     ********** */
/*<       integer i,j >*/
/*<       double precision eps,epsmch,h,temp,zero >*/
/*<       double precision dpmpar >*/
/*<       data zero /0.0d0/ >*/
    /* Parameter adjustments */
    --wa;
    --fvec;
    --x;
    fjac_dim1 = *ldfjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;

    /* Function Body */

/*     epsmch is the machine precision. */

/*<       epsmch = dpmpar(1) >*/
    epsmch = dpmpar_(&c__1);

/*<       eps = dsqrt(dmax1(epsfcn,epsmch)) >*/
    eps = sqrt((max(*epsfcn,epsmch)));
/*<       do 20 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          temp = x(j) >*/
        temp = x[j];
/*<          h = eps*dabs(temp) >*/
        h__ = eps * abs(temp);
/*<          if (h .eq. zero) h = eps >*/
        if (h__ == zero) {
            h__ = eps;
        }
/*<          x(j) = temp + h >*/
        x[j] = temp + h__;
/*<          call fcn(m,n,x,wa,iflag) >*/
        (*fcn)(m, n, &x[1], &wa[1], iflag, userdata);
/*<          if (iflag .lt. 0) go to 30 >*/
        if (*iflag < 0) {
            goto L30;
        }
/*<          x(j) = temp >*/
        x[j] = temp;
/*<          do 10 i = 1, m >*/
        i__2 = *m;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             fjac(i,j) = (wa(i) - fvec(i))/h >*/
            fjac[i__ + j * fjac_dim1] = (wa[i__] - fvec[i__]) / h__;
/*<    10       continue >*/
/* L10: */
        }
/*<    20    continue >*/
/* L20: */
    }
/*<    30 continue >*/
L30:
/*<       return >*/
    return 0;

/*     last card of subroutine fdjac2. */

/*<       end >*/
} /* fdjac2_ */

#ifdef __cplusplus
        }
#endif
