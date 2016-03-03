/* mathews/adaquad.f -- translated by f2c (version 20050501).
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

/*     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994 */
/*     To accompany the text: */
/*     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992 */
/*     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A. */
/*     This free software is complements of the author. */

/*     Algorithm 7.5 (Adaptive Quadrature Using Simpson's Rule). */
/*     Section 7.4, Adaptive Quadrature, Page 389 */

/*     add missing variable F in Refine subrutine. */
/*<       SUBROUTINE AdaptQuad(F,A,B,Tol,SRmat,Integral,ErrBdd,M,State) >*/
/* Subroutine */ int adaptquad_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  doublereal *a, doublereal *b,
        doublereal *tol, doublereal *srmat, doublereal *integral, doublereal *
        errbdd, integer *m, integer *state)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    integer j, k, n, iterating;
    doublereal sum1, sum2;
    integer done;
    doublereal srvec[11];
    extern /* Subroutine */ int srule_(
      v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
            doublereal *, doublereal *,
            doublereal *, doublereal *),
      refine_(v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
              integer *, doublereal * , integer *, integer *);

/*<       INTEGER M,State >*/
/*<       DOUBLE PRECISION A,B,Tol,SRmat,Integral,ErrBdd >*/
/*<       INTEGER J,K,N,Iterating,Done >*/
/*<       DOUBLE PRECISION Sum1,Sum2,SRvec >*/
/*<       DIMENSION SRmat(1:101,1:11),SRvec(1:11) >*/
/*<       EXTERNAL F >*/
/*<       Iterating = 0 >*/
    /* Parameter adjustments */
    srmat -= 102;

    /* Function Body */
    iterating = 0;
/*<       Done = 1 >*/
//    done = 1;
/*<       CALL Srule(F,A,B,Tol,SRvec) >*/
    srule_(f, a, b, tol, srvec);
/*<       DO K=1,11 >*/
    for (k = 1; k <= 11; ++k) {
/*<         SRmat(1, K) = SRvec(K) >*/
        srmat[k * 101 + 1] = srvec[k - 1];
/*<       ENDDO >*/
    }
/*<       M = 1 >*/
    *m = 1;
/*<       State = Iterating >*/
    *state = iterating;
/*<       DO WHILE (State .EQ. Iterating) >*/
    while(*state == iterating) {
/*<         N = M >*/
        n = *m;
/*<         DO J=N,1,-1 >*/
        for (j = n; j >= 1; --j) {
/*<           CALL Refine(F,J,SRmat,M,State) >*/
            refine_(f, &j, &srmat[102], m, state);
/*<         ENDDO >*/
        }
/*<       ENDDO >*/
    }
/*<       Sum1 = 0 >*/
    sum1 = 0.;
/*<       Sum2 = 0 >*/
    sum2 = 0.;
/*<       DO J=1,M >*/
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
/*<         Sum1 = Sum1 + SRmat(J, 8) >*/
        sum1 += srmat[j + 808];
/*<         Sum2 = Sum2 + Abs(SRmat(J, 9)) >*/
        sum2 += (d__1 = srmat[j + 909], abs(d__1));
/*<       ENDDO >*/
    }
/*<       Integral = Sum1 >*/
    *integral = sum1;
/*<       ErrBdd = Sum2 >*/
    *errbdd = sum2;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* adaptquad_ */

/*<       SUBROUTINE Refine(F, P,SRmat,M,State) >*/
/* Subroutine */ int refine_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  integer *p, doublereal *srmat, integer * m, integer *state
  )
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    doublereal a, b, c__;
    integer j, k;
    doublereal s;
    integer iterating;
    doublereal s2, fa, fb, fc, err, tol, tol2;
    integer done;
    doublereal check;
    extern /* Subroutine */ int srule_(
      v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
      doublereal *, doublereal *,
            doublereal *, doublereal *);
    doublereal sr0vec[11], sr1vec[11], sr2vec[11];

/*<       INTEGER P,M,State >*/
/*<       DOUBLE PRECISION SRmat >*/
/*<       INTEGER J,K,Iterating,Done >*/
/*<       DOUBLE PRECISION A,B,C,Err,Fa,Fb,Fc,S,S2,Tol,Tol2,Err,Check >*/
/*<       DOUBLE PRECISION SR0vec,SR1vec,SR2vec >*/
/*<       DIMENSION SRmat(1:101,1:11) >*/
/*<       DIMENSION SR0vec(1:11),SR1vec(1:11),SR2vec(1:11) >*/
/*<       EXTERNAL F >*/
/*<       Iterating = 0 >*/
    /* Parameter adjustments */
    srmat -= 102;

    /* Function Body */
    iterating = 0;
/*<       Done = 1 >*/
    done = 1;
/*<       State = Done >*/
    *state = done;
/*<       DO K=1,11 >*/
    for (k = 1; k <= 11; ++k) {
/*<         SR0vec(K) = SRmat(P, K) >*/
        sr0vec[k - 1] = srmat[*p + k * 101];
/*<       ENDDO >*/
    }
/*<       A = SR0vec(1) >*/
    a = sr0vec[0];
/*<       C = SR0vec(2) >*/
    c__ = sr0vec[1];
/*<       B = SR0vec(3) >*/
    b = sr0vec[2];
/*<       Fa = SR0vec(4) >*/
//    fa = sr0vec[3];
/*<       Fc = SR0vec(5) >*/
//    fc = sr0vec[4];
/*<       Fb = SR0vec(6) >*/
//    fb = sr0vec[5];
/*<       S = SR0vec(7) >*/
//    s = sr0vec[6];
/*<       S2 = SR0vec(8) >*/
//    s2 = sr0vec[7];
/*<       Err = SR0vec(9) >*/
//    err = sr0vec[8];
/*<       Tol = SR0vec(10) >*/
    tol = sr0vec[9];
/*<       Check = SR0vec(11) >*/
    check = sr0vec[10];
/*<       IF (Check .EQ. 1) RETURN >*/
    if (check == 1.) {
        return 0;
    }
/*<       Tol2 = Tol / 2 >*/
    tol2 = tol / 2;
/*<       CALL Srule(F, A, C, Tol2, SR1vec) >*/
    srule_(f, &a, &c__, &tol2, sr1vec);
/*<       CALL Srule(F, C, B, Tol2, SR2vec) >*/
    srule_(f, &c__, &b, &tol2, sr2vec);
/*<       Err = ABS(SR0vec(7) - SR1vec(7) - SR2vec(7)) / 10 >*/
    err = (d__1 = sr0vec[6] - sr1vec[6] - sr2vec[6], abs(d__1)) / 10;
/*<       IF (Err .LT. Tol) THEN >*/
    if (err < tol) {
/*<         SR0vec(11) = 1 >*/
        sr0vec[10] = 1.;
/*<       ENDIF >*/
    }
/*<       IF (Err .LT. Tol) THEN >*/
    if (err < tol) {
/*<         DO K=1,11 >*/
        for (k = 1; k <= 11; ++k) {
/*<           SRmat(P, K) = SR0vec(K) >*/
            srmat[*p + k * 101] = sr0vec[k - 1];
/*<         ENDDO >*/
        }
/*<         SRmat(P, 8) = SR1vec(7) + SR2vec(7) >*/
        srmat[*p + 808] = sr1vec[6] + sr2vec[6];
/*<         SRmat(P, 9) = Err >*/
        srmat[*p + 909] = err;
/*<       ELSE >*/
    } else {
/*<         DO J=(M + 1),P,-1 >*/
        i__1 = *p;
        for (j = *m + 1; j >= i__1; --j) {
/*<           DO K=1,11 >*/
            for (k = 1; k <= 11; ++k) {
/*<             SRmat(J, K) = SRmat(J - 1, K) >*/
                srmat[j + k * 101] = srmat[j - 1 + k * 101];
/*<           ENDDO >*/
            }
/*<         ENDDO >*/
        }
/*<         M = M + 1 >*/
        ++(*m);
/*<         DO K=1,11 >*/
        for (k = 1; k <= 11; ++k) {
/*<           SRmat(P, K) = SR1vec(K) >*/
            srmat[*p + k * 101] = sr1vec[k - 1];
/*<         ENDDO >*/
        }
/*<         DO K=1,11 >*/
        for (k = 1; k <= 11; ++k) {
/*<           SRmat(P + 1, K) = SR2vec(K) >*/
            srmat[*p + 1 + k * 101] = sr2vec[k - 1];
/*<         ENDDO >*/
        }
/*<         State = Iterating >*/
        *state = iterating;
/*<       ENDIF >*/
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* refine_ */

/*<       SUBROUTINE Srule(F,A,B,Tol0,SRvec) >*/
/* Subroutine */ int srule_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  doublereal *a, doublereal *b, doublereal * tol0, doublereal *srvec)
{
    doublereal c__, h__, s, s2, fa, fb, fc, err, tol1, check;

/*<       DOUBLE PRECISION A,B,Tol0,SRvec >*/
/*<       DOUBLE PRECISION C,H,Fa,Fb,Fc,S,S2,Tol1,Err,Check >*/
/*<       DIMENSION SRvec(1:11) >*/
/*<       EXTERNAL F >*/
/*<         H = (B - A) / 2 >*/
    /* Parameter adjustments */
    --srvec;

    /* Function Body */
    h__ = (*b - *a) / 2;
/*<         C = (A + B) / 2 >*/
    c__ = (*a + *b) / 2;
/*<         Fa = F(A) >*/
    fa = (*f)(a);
/*<         Fc = F(C) >*/
    fc = (*f)(&c__);
/*<         Fb = F(B) >*/
    fb = (*f)(b);
/*<         S = H * (F(A) + 4 * F(C) + F(B)) / 3 >*/
    s = h__ * ((*f)(a) + (*f)(&c__) * 4 + (*f)(b)) / 3;
/*<         S2 = S >*/
    s2 = s;
/*<         Tol1 = Tol0 >*/
    tol1 = *tol0;
/*<         Err = Tol0 >*/
    err = *tol0;
/*<         Check = 0 >*/
    check = 0.;
/*<         SRvec(1) = A >*/
    srvec[1] = *a;
/*<         SRvec(2) = C >*/
    srvec[2] = c__;
/*<         SRvec(3) = B >*/
    srvec[3] = *b;
/*<         SRvec(4) = Fa >*/
    srvec[4] = fa;
/*<         SRvec(5) = Fc >*/
    srvec[5] = fc;
/*<         SRvec(6) = Fb >*/
    srvec[6] = fb;
/*<         SRvec(7) = S >*/
    srvec[7] = s;
/*<         SRvec(8) = S2 >*/
    srvec[8] = s2;
/*<         SRvec(9) = Err >*/
    srvec[9] = err;
/*<         SRvec(10) = Tol1 >*/
    srvec[10] = tol1;
/*<         SRvec(11) = Check >*/
    srvec[11] = check;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* srule_ */

#ifdef __cplusplus
        }
#endif
