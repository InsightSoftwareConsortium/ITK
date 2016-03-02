/* laso/dlabfc.f -- translated by f2c (version 20050501).
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

static integer c__0 = 0;
static integer c__1 = 1;


/* *********************************************************************** */

/*<    >*/
/* Subroutine */ int dlabfc_(integer *n, integer *nband, doublereal *a,
        doublereal *sigma, integer *number, integer *lde, doublereal *eigvec,
        integer *numl, integer *ldad, doublereal *atemp, doublereal *d__,
        doublereal *atol)
{
    /* System generated locals */
    integer a_dim1, a_offset, eigvec_dim1, eigvec_offset, atemp_dim1,
            atemp_offset, d_dim1, d_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k, l, m, la, ld, kk, nb1, lpm;
    doublereal zero[1];
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *), dswap_(integer *, doublereal *, integer
            *, doublereal *, integer *), daxpy_(integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *);


/*  THIS SUBROUTINE FACTORS (A-SIGMA*I) WHERE A IS A GIVEN BAND */
/*  MATRIX AND SIGMA IS AN INPUT PARAMETER.  IT ALSO SOLVES ZERO */
/*  OR MORE SYSTEMS OF LINEAR EQUATIONS.  IT RETURNS THE NUMBER */
/*  OF EIGENVALUES OF A LESS THAN SIGMA BY COUNTING THE STURM */
/*  SEQUENCE DURING THE FACTORIZATION.  TO OBTAIN THE STURM */
/*  SEQUENCE COUNT WHILE ALLOWING NON-SYMMETRIC PIVOTING FOR */
/*  STABILITY, THE CODE USES A GUPTA'S MULTIPLE PIVOTING */
/*  ALGORITHM. */

/*  FORMAL PARAMETERS */

/*<       INTEGER N, NBAND, NUMBER, LDE, NUML, LDAD >*/
/*<    >*/

/*  LOCAL VARIABLES */

/*<       INTEGER I, J, K, KK, L, LA, LD, LPM, M, NB1 >*/
/*<       DOUBLE PRECISION ZERO(1) >*/

/*  FUNCTIONS CALLED */

/*<       INTEGER MIN0 >*/
/*<       DOUBLE PRECISION DABS >*/

/*  SUBROUTINES CALLED */

/*     DAXPY, DCOPY, DSWAP */


/*  INITIALIZE */

/*<       ZERO(1) = 0.0D0 >*/
    /* Parameter adjustments */
    a_dim1 = *nband;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    eigvec_dim1 = *lde;
    eigvec_offset = 1 + eigvec_dim1;
    eigvec -= eigvec_offset;
    d_dim1 = *ldad;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    atemp_dim1 = *ldad;
    atemp_offset = 1 + atemp_dim1;
    atemp -= atemp_offset;

    /* Function Body */
    zero[0] = 0.;
/*<       NB1 = NBAND - 1 >*/
    nb1 = *nband - 1;
/*<       NUML = 0 >*/
    *numl = 0;
/*<       CALL DCOPY(LDAD*NBAND, ZERO, 0, D, 1) >*/
    i__1 = *ldad * *nband;
    dcopy_(&i__1, zero, &c__0, &d__[d_offset], &c__1);

/*   LOOP OVER COLUMNS OF A */

/*<       DO 100 K = 1, N >*/
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

/*   ADD A COLUMN OF A TO D */

/*<          D(NBAND, NBAND) = A(1,K) - SIGMA >*/
        d__[*nband + *nband * d_dim1] = a[k * a_dim1 + 1] - *sigma;
/*<          M = MIN0(K, NBAND) - 1 >*/
        m = min(k,*nband) - 1;
/*<          IF(M .EQ. 0) GO TO 20 >*/
        if (m == 0) {
            goto L20;
        }
/*<          DO 10 I = 1, M >*/
        i__2 = m;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             LA = K - I >*/
            la = k - i__;
/*<             LD = NBAND - I >*/
            ld = *nband - i__;
/*<             D(LD,NBAND) = A(I+1, LA) >*/
            d__[ld + *nband * d_dim1] = a[i__ + 1 + la * a_dim1];
/*<    10    CONTINUE >*/
/* L10: */
        }

/*<    20    M = MIN0(N-K, NB1) >*/
L20:
/* Computing MIN */
        i__2 = *n - k;
        m = min(i__2,nb1);
/*<          IF(M .EQ. 0) GO TO 40 >*/
        if (m == 0) {
            goto L40;
        }
/*<          DO 30 I = 1, M >*/
        i__2 = m;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             LD = NBAND + I >*/
            ld = *nband + i__;
/*<             D(LD, NBAND) = A(I+1, K) >*/
            d__[ld + *nband * d_dim1] = a[i__ + 1 + k * a_dim1];
/*<    30    CONTINUE >*/
/* L30: */
        }

/*   TERMINATE */

/*<    40    LPM = 1 >*/
L40:
        lpm = 1;
/*<          IF(NB1 .EQ. 0) GO TO 70 >*/
        if (nb1 == 0) {
            goto L70;
        }
/*<          DO 60 I = 1, NB1 >*/
        i__2 = nb1;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             L = K - NBAND + I >*/
            l = k - *nband + i__;
/*<             IF(D(I,NBAND) .EQ. 0.0D0) GO TO 60 >*/
            if (d__[i__ + *nband * d_dim1] == 0.) {
                goto L60;
            }
/*<             IF(DABS(D(I,I)) .GE. DABS(D(I,NBAND))) GO TO 50 >*/
            if ((d__1 = d__[i__ + i__ * d_dim1], abs(d__1)) >= (d__2 = d__[
                    i__ + *nband * d_dim1], abs(d__2))) {
                goto L50;
            }
/*<    >*/
            if ((d__[i__ + *nband * d_dim1] < 0. && d__[i__ + i__ * d_dim1] <
                    0.) || (d__[i__ + *nband * d_dim1] > 0. && d__[i__ + i__ *
                    d_dim1] >= 0.)) {
                lpm = -lpm;
            }
/*<             CALL DSWAP(LDAD-I+1, D(I,I), 1, D(I,NBAND), 1) >*/
            i__3 = *ldad - i__ + 1;
            dswap_(&i__3, &d__[i__ + i__ * d_dim1], &c__1, &d__[i__ + *nband *
                     d_dim1], &c__1);
/*<             CALL DSWAP(NUMBER, EIGVEC(L,1), LDE, EIGVEC(K,1), LDE) >*/
            dswap_(number, &eigvec[l + eigvec_dim1], lde, &eigvec[k +
                    eigvec_dim1], lde);
/*<    >*/
L50:
            i__3 = *ldad - i__;
            d__1 = -d__[i__ + *nband * d_dim1] / d__[i__ + i__ * d_dim1];
            daxpy_(&i__3, &d__1, &d__[i__ + 1 + i__ * d_dim1], &c__1, &d__[
                    i__ + 1 + *nband * d_dim1], &c__1);
/*<    >*/
            d__1 = -d__[i__ + *nband * d_dim1] / d__[i__ + i__ * d_dim1];
            daxpy_(number, &d__1, &eigvec[l + eigvec_dim1], lde, &eigvec[k +
                    eigvec_dim1], lde);
/*<    60    CONTINUE >*/
L60:
            ;
        }

/*  UPDATE STURM SEQUENCE COUNT */

/*<    70    IF(D(NBAND,NBAND) .LT. 0.0D0) LPM = -LPM >*/
L70:
        if (d__[*nband + *nband * d_dim1] < 0.) {
            lpm = -lpm;
        }
/*<          IF(LPM .LT. 0) NUML = NUML + 1 >*/
        if (lpm < 0) {
            ++(*numl);
        }
/*<          IF(K .EQ. N) GO TO 110 >*/
        if (k == *n) {
            goto L110;
        }

/*   COPY FIRST COLUMN OF D INTO ATEMP */
/*<          IF(K .LT. NBAND) GO TO 80 >*/
        if (k < *nband) {
            goto L80;
        }
/*<          L = K - NB1 >*/
        l = k - nb1;
/*<          CALL DCOPY(LDAD, D, 1, ATEMP(1,L), 1) >*/
        dcopy_(ldad, &d__[d_offset], &c__1, &atemp[l * atemp_dim1 + 1], &c__1)
                ;

/*   SHIFT THE COLUMNS OF D OVER AND UP */

/*<          IF(NB1 .EQ. 0) GO TO 100 >*/
        if (nb1 == 0) {
            goto L100;
        }
/*<    80    DO 90 I = 1, NB1 >*/
L80:
        i__2 = nb1;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             CALL DCOPY(LDAD-I, D(I+1,I+1), 1, D(I,I), 1) >*/
            i__3 = *ldad - i__;
            dcopy_(&i__3, &d__[i__ + 1 + (i__ + 1) * d_dim1], &c__1, &d__[i__
                    + i__ * d_dim1], &c__1);
/*<             D(LDAD,I) = 0.0D0 >*/
            d__[*ldad + i__ * d_dim1] = 0.;
/*<    90    CONTINUE >*/
/* L90: */
        }
/*<   100 CONTINUE >*/
L100:
        ;
    }

/*  TRANSFER D TO ATEMP */

/*<   110 DO 120 I = 1, NBAND >*/
L110:
    i__1 = *nband;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          L = N - NBAND + I >*/
        l = *n - *nband + i__;
/*<          CALL DCOPY(NBAND-I+1, D(I,I), 1, ATEMP(1,L), 1) >*/
        i__2 = *nband - i__ + 1;
        dcopy_(&i__2, &d__[i__ + i__ * d_dim1], &c__1, &atemp[l * atemp_dim1
                + 1], &c__1);
/*<   120 CONTINUE >*/
/* L120: */
    }

/*   BACK SUBSTITUTION */

/*<       IF(NUMBER .EQ. 0) RETURN >*/
    if (*number == 0) {
        return 0;
    }
/*<       DO 160 KK = 1, N >*/
    i__1 = *n;
    for (kk = 1; kk <= i__1; ++kk) {
/*<          K = N - KK + 1 >*/
        k = *n - kk + 1;
/*<    >*/
        if ((d__1 = atemp[k * atemp_dim1 + 1], abs(d__1)) <= *atol) {
            atemp[k * atemp_dim1 + 1] = d_sign(atol, &atemp[k * atemp_dim1 +
                    1]);
        }

/*<   130    DO 150 I = 1, NUMBER >*/
/* L130: */
        i__2 = *number;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             EIGVEC(K,I) = EIGVEC(K,I)/ATEMP(1,K) >*/
            eigvec[k + i__ * eigvec_dim1] /= atemp[k * atemp_dim1 + 1];
/*<             M = MIN0(LDAD, K) - 1 >*/
            m = min(*ldad,k) - 1;
/*<             IF(M .EQ. 0) GO TO 150 >*/
            if (m == 0) {
                goto L150;
            }
/*<             DO 140 J = 1, M >*/
            i__3 = m;
            for (j = 1; j <= i__3; ++j) {
/*<                 L = K - J >*/
                l = k - j;
/*<                 EIGVEC(L,I) = EIGVEC(L,I) - ATEMP(J+1,L)*EIGVEC(K,I) >*/
                eigvec[l + i__ * eigvec_dim1] -= atemp[j + 1 + l * atemp_dim1]
                         * eigvec[k + i__ * eigvec_dim1];
/*<   140       CONTINUE >*/
/* L140: */
            }
/*<   150    CONTINUE >*/
L150:
            ;
        }
/*<   160 CONTINUE >*/
/* L160: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dlabfc_ */

#ifdef __cplusplus
        }
#endif
