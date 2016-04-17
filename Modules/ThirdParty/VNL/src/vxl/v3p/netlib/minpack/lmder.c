/* minpack/lmder.f -- translated by f2c (version 20050501).
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
static logical c_true = TRUE_;

/*<    >*/
/* Subroutine */ int lmder_(
  void (*fcn)(v3p_netlib_integer*,
              v3p_netlib_integer*,
              v3p_netlib_doublereal*,
              v3p_netlib_doublereal*,
              v3p_netlib_doublereal*,
              v3p_netlib_integer*,
              v3p_netlib_integer*,
              void*),
        integer *m, integer *n, doublereal *x,
        doublereal *fvec, doublereal *fjac, integer *ldfjac, doublereal *ftol,
         doublereal *xtol, doublereal *gtol, integer *maxfev, doublereal *
        diag, integer *mode, doublereal *factor, integer *nprint, integer *
        info, integer *nfev, integer *njev, integer *ipvt, doublereal *qtf,
        doublereal *wa1, doublereal *wa2, doublereal *wa3, doublereal *wa4,
        void* userdata)
{
    /* Initialized data */

    static doublereal one = 1.; /* constant */
    static doublereal p1 = .1; /* constant */
    static doublereal p5 = .5; /* constant */
    static doublereal p25 = .25; /* constant */
    static doublereal p75 = .75; /* constant */
    static doublereal p0001 = 1e-4; /* constant */
    static doublereal zero = 0.; /* constant */

    /* System generated locals */
    integer fjac_dim1, fjac_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, l;
    doublereal par, sum;
    integer iter;
    doublereal temp=0, temp1, temp2;
    integer iflag;
    doublereal delta;
    extern /* Subroutine */ int qrfac_(integer *, integer *, doublereal *,
            integer *, logical *, integer *, integer *, doublereal *,
            doublereal *, doublereal *), lmpar_(integer *, doublereal *,
            integer *, integer *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    doublereal ratio;
    extern doublereal enorm_(integer *, doublereal *);
    doublereal fnorm, gnorm, pnorm, xnorm=0, fnorm1, actred, dirder, epsmch,
            prered;
    extern doublereal dpmpar_(integer *);

/*<       integer m,n,ldfjac,maxfev,mode,nprint,info,nfev,njev >*/
/*<       integer ipvt(n) >*/
/*<       double precision ftol,xtol,gtol,factor >*/
/*<    >*/
/*     ********** */

/*     subroutine lmder */

/*     the purpose of lmder is to minimize the sum of the squares of */
/*     m nonlinear functions in n variables by a modification of */
/*     the levenberg-marquardt algorithm. the user must provide a */
/*     subroutine which calculates the functions and the jacobian. */

/*     the subroutine statement is */

/*       subroutine lmder(fcn,m,n,x,fvec,fjac,ldfjac,ftol,xtol,gtol, */
/*                        maxfev,diag,mode,factor,nprint,info,nfev, */
/*                        njev,ipvt,qtf,wa1,wa2,wa3,wa4) */

/*     where */

/*       fcn is the name of the user-supplied subroutine which */
/*         calculates the functions and the jacobian. fcn must */
/*         be declared in an external statement in the user */
/*         calling program, and should be written as follows. */

/*         subroutine fcn(m,n,x,fvec,fjac,ldfjac,iflag) */
/*         integer m,n,ldfjac,iflag */
/*         double precision x(n),fvec(m),fjac(ldfjac,n) */
/*         ---------- */
/*         if iflag = 1 calculate the functions at x and */
/*         return this vector in fvec. do not alter fjac. */
/*         if iflag = 2 calculate the jacobian at x and */
/*         return this matrix in fjac. do not alter fvec. */
/*         ---------- */
/*         return */
/*         end */

/*         the value of iflag should not be changed by fcn unless */
/*         the user wants to terminate execution of lmder. */
/*         in this case set iflag to a negative integer. */

/*       m is a positive integer input variable set to the number */
/*         of functions. */

/*       n is a positive integer input variable set to the number */
/*         of variables. n must not exceed m. */

/*       x is an array of length n. on input x must contain */
/*         an initial estimate of the solution vector. on output x */
/*         contains the final estimate of the solution vector. */

/*       fvec is an output array of length m which contains */
/*         the functions evaluated at the output x. */

/*       fjac is an output m by n array. the upper n by n submatrix */
/*         of fjac contains an upper triangular matrix r with */
/*         diagonal elements of nonincreasing magnitude such that */

/*                t     t           t */
/*               p *(jac *jac)*p = r *r, */

/*         where p is a permutation matrix and jac is the final */
/*         calculated jacobian. column j of p is column ipvt(j) */
/*         (see below) of the identity matrix. the lower trapezoidal */
/*         part of fjac contains information generated during */
/*         the computation of r. */

/*       ldfjac is a positive integer input variable not less than m */
/*         which specifies the leading dimension of the array fjac. */

/*       ftol is a nonnegative input variable. termination */
/*         occurs when both the actual and predicted relative */
/*         reductions in the sum of squares are at most ftol. */
/*         therefore, ftol measures the relative error desired */
/*         in the sum of squares. */

/*       xtol is a nonnegative input variable. termination */
/*         occurs when the relative error between two consecutive */
/*         iterates is at most xtol. therefore, xtol measures the */
/*         relative error desired in the approximate solution. */

/*       gtol is a nonnegative input variable. termination */
/*         occurs when the cosine of the angle between fvec and */
/*         any column of the jacobian is at most gtol in absolute */
/*         value. therefore, gtol measures the orthogonality */
/*         desired between the function vector and the columns */
/*         of the jacobian. */

/*       maxfev is a positive integer input variable. termination */
/*         occurs when the number of calls to fcn with iflag = 1 */
/*         has reached maxfev. */

/*       diag is an array of length n. if mode = 1 (see */
/*         below), diag is internally set. if mode = 2, diag */
/*         must contain positive entries that serve as */
/*         multiplicative scale factors for the variables. */

/*       mode is an integer input variable. if mode = 1, the */
/*         variables will be scaled internally. if mode = 2, */
/*         the scaling is specified by the input diag. other */
/*         values of mode are equivalent to mode = 1. */

/*       factor is a positive input variable used in determining the */
/*         initial step bound. this bound is set to the product of */
/*         factor and the euclidean norm of diag*x if nonzero, or else */
/*         to factor itself. in most cases factor should lie in the */
/*         interval (.1,100.).100. is a generally recommended value. */

/*       nprint is an integer input variable that enables controlled */
/*         printing of iterates if it is positive. in this case, */
/*         fcn is called with iflag = 0 at the beginning of the first */
/*         iteration and every nprint iterations thereafter and */
/*         immediately prior to return, with x, fvec, and fjac */
/*         available for printing. fvec and fjac should not be */
/*         altered. if nprint is not positive, no special calls */
/*         of fcn with iflag = 0 are made. */

/*       info is an integer output variable. if the user has */
/*         terminated execution, info is set to the (negative) */
/*         value of iflag. see description of fcn. otherwise, */
/*         info is set as follows. */

/*         info = 0  improper input parameters. */

/*         info = 1  both actual and predicted relative reductions */
/*                   in the sum of squares are at most ftol. */

/*         info = 2  relative error between two consecutive iterates */
/*                   is at most xtol. */

/*         info = 3  conditions for info = 1 and info = 2 both hold. */

/*         info = 4  the cosine of the angle between fvec and any */
/*                   column of the jacobian is at most gtol in */
/*                   absolute value. */

/*         info = 5  number of calls to fcn with iflag = 1 has */
/*                   reached maxfev. */

/*         info = 6  ftol is too small. no further reduction in */
/*                   the sum of squares is possible. */

/*         info = 7  xtol is too small. no further improvement in */
/*                   the approximate solution x is possible. */

/*         info = 8  gtol is too small. fvec is orthogonal to the */
/*                   columns of the jacobian to machine precision. */

/*       nfev is an integer output variable set to the number of */
/*         calls to fcn with iflag = 1. */

/*       njev is an integer output variable set to the number of */
/*         calls to fcn with iflag = 2. */

/*       ipvt is an integer output array of length n. ipvt */
/*         defines a permutation matrix p such that jac*p = q*r, */
/*         where jac is the final calculated jacobian, q is */
/*         orthogonal (not stored), and r is upper triangular */
/*         with diagonal elements of nonincreasing magnitude. */
/*         column j of p is column ipvt(j) of the identity matrix. */

/*       qtf is an output array of length n which contains */
/*         the first n elements of the vector (q transpose)*fvec. */

/*       wa1, wa2, and wa3 are work arrays of length n. */

/*       wa4 is a work array of length m. */

/*     subprograms called */

/*       user-supplied ...... fcn */

/*       minpack-supplied ... dpmpar,enorm,lmpar,qrfac */

/*       fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod */

/*     argonne national laboratory. minpack project. march 1980. */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more */

/*     ********** */
/*<       integer i,iflag,iter,j,l >*/
/*<    >*/
/*<       double precision dpmpar,enorm >*/
/*<    >*/
    /* Parameter adjustments */
    --wa4;
    --fvec;
    --wa3;
    --wa2;
    --wa1;
    --qtf;
    --ipvt;
    --diag;
    --x;
    fjac_dim1 = *ldfjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;

    /* Function Body */

/*     epsmch is the machine precision. */

/*<       epsmch = dpmpar(1) >*/
    epsmch = dpmpar_(&c__1);

/*<       info = 0 >*/
    *info = 0;
/*<       iflag = 0 >*/
    iflag = 0;
/*<       nfev = 0 >*/
    *nfev = 0;
/*<       njev = 0 >*/
    *njev = 0;

/*     check the input parameters for errors. */

/*<    >*/
    if (*n <= 0 || *m < *n || *ldfjac < *m || *ftol < zero || *xtol < zero ||
            *gtol < zero || *maxfev <= 0 || *factor <= zero) {
        goto L300;
    }
/*<       if (mode .ne. 2) go to 20 >*/
    if (*mode != 2) {
        goto L20;
    }
/*<       do 10 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          if (diag(j) .le. zero) go to 300 >*/
        if (diag[j] <= zero) {
            goto L300;
        }
/*<    10    continue >*/
/* L10: */
    }
/*<    20 continue >*/
L20:

/*     evaluate the function at the starting point */
/*     and calculate its norm. */

/*<       iflag = 1 >*/
    iflag = 1;
/*<       call fcn(m,n,x,fvec,fjac,ldfjac,iflag) >*/
    (*fcn)(m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,
           userdata);
/*<       nfev = 1 >*/
    *nfev = 1;
/*<       if (iflag .lt. 0) go to 300 >*/
    if (iflag < 0) {
        goto L300;
    }
/*<       fnorm = enorm(m,fvec) >*/
    fnorm = enorm_(m, &fvec[1]);

/*     initialize levenberg-marquardt parameter and iteration counter. */

/*<       par = zero >*/
    par = zero;
/*<       iter = 1 >*/
    iter = 1;

/*     beginning of the outer loop. */

/*<    30 continue >*/
L30:

/*        calculate the jacobian matrix. */

/*<          iflag = 2 >*/
    iflag = 2;
/*<          call fcn(m,n,x,fvec,fjac,ldfjac,iflag) >*/
    (*fcn)(m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,
           userdata);
/*<          njev = njev + 1 >*/
    ++(*njev);
/*<          if (iflag .lt. 0) go to 300 >*/
    if (iflag < 0) {
        goto L300;
    }

/*        if requested, call fcn to enable printing of iterates. */

/*<          if (nprint .le. 0) go to 40 >*/
    if (*nprint <= 0) {
        goto L40;
    }
/*<          iflag = 0 >*/
    iflag = 0;
/*<    >*/
    if ((iter - 1) % *nprint == 0) {
        (*fcn)(m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,
               userdata);
    }
/*<          if (iflag .lt. 0) go to 300 >*/
    if (iflag < 0) {
        goto L300;
    }
/*<    40    continue >*/
L40:

/*        compute the qr factorization of the jacobian. */

/*<          call qrfac(m,n,fjac,ldfjac,.true.,ipvt,n,wa1,wa2,wa3) >*/
    qrfac_(m, n, &fjac[fjac_offset], ldfjac, &c_true, &ipvt[1], n, &wa1[1], &
            wa2[1], &wa3[1]);

/*        on the first iteration and if mode is 1, scale according */
/*        to the norms of the columns of the initial jacobian. */

/*<          if (iter .ne. 1) go to 80 >*/
    if (iter != 1) {
        goto L80;
    }
/*<          if (mode .eq. 2) go to 60 >*/
    if (*mode == 2) {
        goto L60;
    }
/*<          do 50 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             diag(j) = wa2(j) >*/
        diag[j] = wa2[j];
/*<             if (wa2(j) .eq. zero) diag(j) = one >*/
        if (wa2[j] == zero) {
            diag[j] = one;
        }
/*<    50       continue >*/
/* L50: */
    }
/*<    60    continue >*/
L60:

/*        on the first iteration, calculate the norm of the scaled x */
/*        and initialize the step bound delta. */

/*<          do 70 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             wa3(j) = diag(j)*x(j) >*/
        wa3[j] = diag[j] * x[j];
/*<    70       continue >*/
/* L70: */
    }
/*<          xnorm = enorm(n,wa3) >*/
    xnorm = enorm_(n, &wa3[1]);
/*<          delta = factor*xnorm >*/
    delta = *factor * xnorm;
/*<          if (delta .eq. zero) delta = factor >*/
    if (delta == zero) {
        delta = *factor;
    }
/*<    80    continue >*/
L80:

/*        form (q transpose)*fvec and store the first n components in */
/*        qtf. */

/*<          do 90 i = 1, m >*/
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<             wa4(i) = fvec(i) >*/
        wa4[i__] = fvec[i__];
/*<    90       continue >*/
/* L90: */
    }
/*<          do 130 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             if (fjac(j,j) .eq. zero) go to 120 >*/
        if (fjac[j + j * fjac_dim1] == zero) {
            goto L120;
        }
/*<             sum = zero >*/
        sum = zero;
/*<             do 100 i = j, m >*/
        i__2 = *m;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<                sum = sum + fjac(i,j)*wa4(i) >*/
            sum += fjac[i__ + j * fjac_dim1] * wa4[i__];
/*<   100          continue >*/
/* L100: */
        }
/*<             temp = -sum/fjac(j,j) >*/
        temp = -sum / fjac[j + j * fjac_dim1];
/*<             do 110 i = j, m >*/
        i__2 = *m;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<                wa4(i) = wa4(i) + fjac(i,j)*temp >*/
            wa4[i__] += fjac[i__ + j * fjac_dim1] * temp;
/*<   110          continue >*/
/* L110: */
        }
/*<   120       continue >*/
L120:
/*<             fjac(j,j) = wa1(j) >*/
        fjac[j + j * fjac_dim1] = wa1[j];
/*<             qtf(j) = wa4(j) >*/
        qtf[j] = wa4[j];
/*<   130       continue >*/
/* L130: */
    }

/*        compute the norm of the scaled gradient. */

/*<          gnorm = zero >*/
    gnorm = zero;
/*<          if (fnorm .eq. zero) go to 170 >*/
    if (fnorm == zero) {
        goto L170;
    }
/*<          do 160 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             l = ipvt(j) >*/
        l = ipvt[j];
/*<             if (wa2(l) .eq. zero) go to 150 >*/
        if (wa2[l] == zero) {
            goto L150;
        }
/*<             sum = zero >*/
        sum = zero;
/*<             do 140 i = 1, j >*/
        i__2 = j;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                sum = sum + fjac(i,j)*(qtf(i)/fnorm) >*/
            sum += fjac[i__ + j * fjac_dim1] * (qtf[i__] / fnorm);
/*<   140          continue >*/
/* L140: */
        }
/*<             gnorm = dmax1(gnorm,dabs(sum/wa2(l))) >*/
/* Computing MAX */
        d__2 = gnorm, d__3 = (d__1 = sum / wa2[l], abs(d__1));
        gnorm = max(d__2,d__3);
/*<   150       continue >*/
L150:
/*<   160       continue >*/
/* L160: */
        ;
    }
/*<   170    continue >*/
L170:

/*        test for convergence of the gradient norm. */

/*<          if (gnorm .le. gtol) info = 4 >*/
    if (gnorm <= *gtol) {
        *info = 4;
    }
/*<          if (info .ne. 0) go to 300 >*/
    if (*info != 0) {
        goto L300;
    }

/*        rescale if necessary. */

/*<          if (mode .eq. 2) go to 190 >*/
    if (*mode == 2) {
        goto L190;
    }
/*<          do 180 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<             diag(j) = dmax1(diag(j),wa2(j)) >*/
/* Computing MAX */
        d__1 = diag[j], d__2 = wa2[j];
        diag[j] = max(d__1,d__2);
/*<   180       continue >*/
/* L180: */
    }
/*<   190    continue >*/
L190:

/*        beginning of the inner loop. */

/*<   200    continue >*/
L200:

/*           determine the levenberg-marquardt parameter. */

/*<    >*/
    lmpar_(n, &fjac[fjac_offset], ldfjac, &ipvt[1], &diag[1], &qtf[1], &delta,
             &par, &wa1[1], &wa2[1], &wa3[1], &wa4[1]);

/*           store the direction p and x + p. calculate the norm of p. */

/*<             do 210 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<                wa1(j) = -wa1(j) >*/
        wa1[j] = -wa1[j];
/*<                wa2(j) = x(j) + wa1(j) >*/
        wa2[j] = x[j] + wa1[j];
/*<                wa3(j) = diag(j)*wa1(j) >*/
        wa3[j] = diag[j] * wa1[j];
/*<   210          continue >*/
/* L210: */
    }
/*<             pnorm = enorm(n,wa3) >*/
    pnorm = enorm_(n, &wa3[1]);

/*           on the first iteration, adjust the initial step bound. */

/*<             if (iter .eq. 1) delta = dmin1(delta,pnorm) >*/
    if (iter == 1) {
        delta = min(delta,pnorm);
    }

/*           evaluate the function at x + p and calculate its norm. */

/*<             iflag = 1 >*/
    iflag = 1;
/*<             call fcn(m,n,wa2,wa4,fjac,ldfjac,iflag) >*/
    (*fcn)(m, n, &wa2[1], &wa4[1], &fjac[fjac_offset], ldfjac, &iflag,
           userdata);
/*<             nfev = nfev + 1 >*/
    ++(*nfev);
/*<             if (iflag .lt. 0) go to 300 >*/
    if (iflag < 0) {
        goto L300;
    }
/*<             fnorm1 = enorm(m,wa4) >*/
    fnorm1 = enorm_(m, &wa4[1]);

/*           compute the scaled actual reduction. */

/*<             actred = -one >*/
    actred = -one;
/*<             if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2 >*/
    if (p1 * fnorm1 < fnorm) {
/* Computing 2nd power */
        d__1 = fnorm1 / fnorm;
        actred = one - d__1 * d__1;
    }

/*           compute the scaled predicted reduction and */
/*           the scaled directional derivative. */

/*<             do 230 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<                wa3(j) = zero >*/
        wa3[j] = zero;
/*<                l = ipvt(j) >*/
        l = ipvt[j];
/*<                temp = wa1(l) >*/
        temp = wa1[l];
/*<                do 220 i = 1, j >*/
        i__2 = j;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   wa3(i) = wa3(i) + fjac(i,j)*temp >*/
            wa3[i__] += fjac[i__ + j * fjac_dim1] * temp;
/*<   220             continue >*/
/* L220: */
        }
/*<   230          continue >*/
/* L230: */
    }
/*<             temp1 = enorm(n,wa3)/fnorm >*/
    temp1 = enorm_(n, &wa3[1]) / fnorm;
/*<             temp2 = (dsqrt(par)*pnorm)/fnorm >*/
    temp2 = sqrt(par) * pnorm / fnorm;
/*<             prered = temp1**2 + temp2**2/p5 >*/
/* Computing 2nd power */
    d__1 = temp1;
/* Computing 2nd power */
    d__2 = temp2;
    prered = d__1 * d__1 + d__2 * d__2 / p5;
/*<             dirder = -(temp1**2 + temp2**2) >*/
/* Computing 2nd power */
    d__1 = temp1;
/* Computing 2nd power */
    d__2 = temp2;
    dirder = -(d__1 * d__1 + d__2 * d__2);

/*           compute the ratio of the actual to the predicted */
/*           reduction. */

/*<             ratio = zero >*/
    ratio = zero;
/*<             if (prered .ne. zero) ratio = actred/prered >*/
    if (prered != zero) {
        ratio = actred / prered;
    }

/*           update the step bound. */

/*<             if (ratio .gt. p25) go to 240 >*/
    if (ratio > p25) {
        goto L240;
    }
/*<                if (actred .ge. zero) temp = p5 >*/
    if (actred >= zero) {
        temp = p5;
    }
/*<    >*/
    if (actred < zero) {
        temp = p5 * dirder / (dirder + p5 * actred);
    }
/*<                if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1 >*/
    if (p1 * fnorm1 >= fnorm || temp < p1) {
        temp = p1;
    }
/*<                delta = temp*dmin1(delta,pnorm/p1) >*/
/* Computing MIN */
    d__1 = delta, d__2 = pnorm / p1;
    delta = temp * min(d__1,d__2);
/*<                par = par/temp >*/
    par /= temp;
/*<                go to 260 >*/
    goto L260;
/*<   240       continue >*/
L240:
/*<                if (par .ne. zero .and. ratio .lt. p75) go to 250 >*/
    if (par != zero && ratio < p75) {
        goto L250;
    }
/*<                delta = pnorm/p5 >*/
    delta = pnorm / p5;
/*<                par = p5*par >*/
    par = p5 * par;
/*<   250          continue >*/
L250:
/*<   260       continue >*/
L260:

/*           test for successful iteration. */

/*<             if (ratio .lt. p0001) go to 290 >*/
    if (ratio < p0001) {
        goto L290;
    }

/*           successful iteration. update x, fvec, and their norms. */

/*<             do 270 j = 1, n >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<                x(j) = wa2(j) >*/
        x[j] = wa2[j];
/*<                wa2(j) = diag(j)*x(j) >*/
        wa2[j] = diag[j] * x[j];
/*<   270          continue >*/
/* L270: */
    }
/*<             do 280 i = 1, m >*/
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                fvec(i) = wa4(i) >*/
        fvec[i__] = wa4[i__];
/*<   280          continue >*/
/* L280: */
    }
/*<             xnorm = enorm(n,wa2) >*/
    xnorm = enorm_(n, &wa2[1]);
/*<             fnorm = fnorm1 >*/
    fnorm = fnorm1;
/*<             iter = iter + 1 >*/
    ++iter;
/*<   290       continue >*/
L290:

/*           tests for convergence. */

/*<    >*/
    if (abs(actred) <= *ftol && prered <= *ftol && p5 * ratio <= one) {
        *info = 1;
    }
/*<             if (delta .le. xtol*xnorm) info = 2 >*/
    if (delta <= *xtol * xnorm) {
        *info = 2;
    }
/*<    >*/
    if (abs(actred) <= *ftol && prered <= *ftol && p5 * ratio <= one && *info
            == 2) {
        *info = 3;
    }
/*<             if (info .ne. 0) go to 300 >*/
    if (*info != 0) {
        goto L300;
    }

/*           tests for termination and stringent tolerances. */

/*<             if (nfev .ge. maxfev) info = 5 >*/
    if (*nfev >= *maxfev) {
        *info = 5;
    }
/*<    >*/
    if (abs(actred) <= epsmch && prered <= epsmch && p5 * ratio <= one) {
        *info = 6;
    }
/*<             if (delta .le. epsmch*xnorm) info = 7 >*/
    if (delta <= epsmch * xnorm) {
        *info = 7;
    }
/*<             if (gnorm .le. epsmch) info = 8 >*/
    if (gnorm <= epsmch) {
        *info = 8;
    }
/*<             if (info .ne. 0) go to 300 >*/
    if (*info != 0) {
        goto L300;
    }

/*           end of the inner loop. repeat if iteration unsuccessful. */

/*<             if (ratio .lt. p0001) go to 200 >*/
    if (ratio < p0001) {
        goto L200;
    }

/*        end of the outer loop. */

/*<          go to 30 >*/
    goto L30;
/*<   300 continue >*/
L300:

/*     termination, either normal or user imposed. */

/*<       if (iflag .lt. 0) info = iflag >*/
    if (iflag < 0) {
        *info = iflag;
    }
/*<       iflag = 0 >*/
    iflag = 0;
/*<       if (nprint .gt. 0) call fcn(m,n,x,fvec,fjac,ldfjac,iflag) >*/
    if (*nprint > 0) {
        (*fcn)(m, n, &x[1], &fvec[1], &fjac[fjac_offset], ldfjac, &iflag,
               userdata);
    }
/*<       return >*/
    return 0;

/*     last card of subroutine lmder. */

/*<       end >*/
} /* lmder_ */

#ifdef __cplusplus
        }
#endif
