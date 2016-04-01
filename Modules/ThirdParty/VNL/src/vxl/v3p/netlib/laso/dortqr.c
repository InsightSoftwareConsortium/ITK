/* laso/dortqr.f -- translated by f2c (version 20050501).
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


/* ------------------------------------------------------------------ */

/*<       SUBROUTINE DORTQR(NZ, N, NBLOCK, Z, B) >*/
/* Subroutine */ int dortqr_(integer *nz, integer *n, integer *nblock,
        doublereal *z__, doublereal *b)
{
    /* System generated locals */
    integer z_dim1, z_offset, b_dim1, b_offset, i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k, m;
    doublereal tau;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    doublereal temp;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    doublereal sigma;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    integer length;


/*<       INTEGER NZ, N, NBLOCK >*/
/*<       DOUBLE PRECISION Z(NZ,1), B(NBLOCK,1) >*/

/* THIS SUBROUTINE COMPUTES THE QR FACTORIZATION OF THE N X NBLOCK */
/* MATRIX Z.  Q IS FORMED IN PLACE AND RETURNED IN Z.  R IS */
/* RETURNED IN B. */

/*<       INTEGER I, J, K, LENGTH, M >*/
/*<       DOUBLE PRECISION SIGMA, TAU, TEMP, DDOT, DNRM2, DSIGN >*/
/*<       EXTERNAL DAXPY, DDOT, DNRM2, DSCAL >*/

/* THIS SECTION REDUCES Z TO TRIANGULAR FORM. */

/*<       DO 30 I=1,NBLOCK >*/
    /* Parameter adjustments */
    z_dim1 = *nz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    b_dim1 = *nblock;
    b_offset = 1 + b_dim1;
    b -= b_offset;

    /* Function Body */
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {

/* THIS FORMS THE ITH REFLECTION. */

/*<          LENGTH = N - I + 1 >*/
        length = *n - i__ + 1;
/*<          SIGMA = DSIGN(DNRM2(LENGTH,Z(I,I),1),Z(I,I)) >*/
        d__1 = dnrm2_(&length, &z__[i__ + i__ * z_dim1], &c__1);
        sigma = d_sign(&d__1, &z__[i__ + i__ * z_dim1]);
/*<          B(I,I) = -SIGMA >*/
        b[i__ + i__ * b_dim1] = -sigma;
/*<          Z(I,I) = Z(I,I) + SIGMA >*/
        z__[i__ + i__ * z_dim1] += sigma;
/*<          TAU = SIGMA*Z(I,I) >*/
        tau = sigma * z__[i__ + i__ * z_dim1];
/*<          IF (I.EQ.NBLOCK) GO TO 30 >*/
        if (i__ == *nblock) {
            goto L30;
        }
/*<          J = I + 1 >*/
        j = i__ + 1;

/* THIS APPLIES THE ROTATION TO THE REST OF THE COLUMNS. */

/*<          DO 20 K=J,NBLOCK >*/
        i__2 = *nblock;
        for (k = j; k <= i__2; ++k) {
/*<             IF (TAU.EQ.0.0D0) GO TO 10 >*/
            if (tau == 0.) {
                goto L10;
            }
/*<             TEMP = -DDOT(LENGTH,Z(I,I),1,Z(I,K),1)/TAU >*/
            temp = -ddot_(&length, &z__[i__ + i__ * z_dim1], &c__1, &z__[i__
                    + k * z_dim1], &c__1) / tau;
/*<             CALL DAXPY(LENGTH, TEMP, Z(I,I), 1, Z(I,K), 1) >*/
            daxpy_(&length, &temp, &z__[i__ + i__ * z_dim1], &c__1, &z__[i__
                    + k * z_dim1], &c__1);
/*<    10       B(I,K) = Z(I,K) >*/
L10:
            b[i__ + k * b_dim1] = z__[i__ + k * z_dim1];
/*<             Z(I,K) = 0.0D0 >*/
            z__[i__ + k * z_dim1] = 0.;
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<    30 CONTINUE >*/
L30:
        ;
    }

/* THIS ACCUMULATES THE REFLECTIONS IN REVERSE ORDER. */

/*<       DO 70 M=1,NBLOCK >*/
    i__1 = *nblock;
    for (m = 1; m <= i__1; ++m) {

/* THIS RECREATES THE ITH = NBLOCK-M+1)TH REFLECTION. */

/*<          I = NBLOCK + 1 - M >*/
        i__ = *nblock + 1 - m;
/*<          SIGMA = -B(I,I) >*/
        sigma = -b[i__ + i__ * b_dim1];
/*<          TAU = Z(I,I)*SIGMA >*/
        tau = z__[i__ + i__ * z_dim1] * sigma;
/*<          IF (TAU.EQ.0.0D0) GO TO 60 >*/
        if (tau == 0.) {
            goto L60;
        }
/*<          LENGTH = N - NBLOCK + M >*/
        length = *n - *nblock + m;
/*<          IF (I.EQ.NBLOCK) GO TO 50 >*/
        if (i__ == *nblock) {
            goto L50;
        }
/*<          J = I + 1 >*/
        j = i__ + 1;

/* THIS APPLIES IT TO THE LATER COLUMNS. */

/*<          DO 40 K=J,NBLOCK >*/
        i__2 = *nblock;
        for (k = j; k <= i__2; ++k) {
/*<             TEMP = -DDOT(LENGTH,Z(I,I),1,Z(I,K),1)/TAU >*/
            temp = -ddot_(&length, &z__[i__ + i__ * z_dim1], &c__1, &z__[i__
                    + k * z_dim1], &c__1) / tau;
/*<             CALL DAXPY(LENGTH, TEMP, Z(I,I), 1, Z(I,K), 1) >*/
            daxpy_(&length, &temp, &z__[i__ + i__ * z_dim1], &c__1, &z__[i__
                    + k * z_dim1], &c__1);
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<    50    CALL DSCAL(LENGTH, -1.0D0/SIGMA, Z(I,I), 1) >*/
L50:
        d__1 = -1. / sigma;
        dscal_(&length, &d__1, &z__[i__ + i__ * z_dim1], &c__1);
/*<    60    Z(I,I) = 1.0D0 + Z(I,I) >*/
L60:
        z__[i__ + i__ * z_dim1] += 1.;
/*<    70 CONTINUE >*/
/* L70: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dortqr_ */

#ifdef __cplusplus
        }
#endif
