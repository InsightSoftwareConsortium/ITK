/* lapack/double/drscl.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DRSCL( N, SA, SX, INCX ) >*/
/* Subroutine */ int drscl_(integer *n, doublereal *sa, doublereal *sx,
        integer *incx)
{
    doublereal mul, cden;
    logical done;
    doublereal cnum, cden1, cnum1;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    doublereal bignum, smlnum;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*<       DOUBLE PRECISION   SA >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   SX( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DRSCL multiplies an n-element real vector x by the real scalar 1/a. */
/*  This is done without overflow or underflow as long as */
/*  the final result x/a does not overflow or underflow. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of components of the vector x. */

/*  SA      (input) DOUBLE PRECISION */
/*          The scalar a which is used to divide each component of x. */
/*          SA must be >= 0, or the subroutine will divide by zero. */

/*  SX      (input/output) DOUBLE PRECISION array, dimension */
/*                         (1+(N-1)*abs(INCX)) */
/*          The n-element vector x. */

/*  INCX    (input) INTEGER */
/*          The increment between successive values of the vector SX. */
/*          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n */

/* ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            DONE >*/
/*<       DOUBLE PRECISION   BIGNUM, CDEN, CDEN1, CNUM, CNUM1, MUL, SMLNUM >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           DLAMCH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DSCAL >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Quick return if possible */

/*<    >*/
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    if (*n <= 0) {
        return 0;
    }

/*     Get machine parameters */

/*<       SMLNUM = DLAMCH( 'S' ) >*/
    smlnum = dlamch_("S", (ftnlen)1);
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       CALL DLABAD( SMLNUM, BIGNUM ) >*/
    dlabad_(&smlnum, &bignum);

/*     Initialize the denominator to SA and the numerator to 1. */

/*<       CDEN = SA >*/
    cden = *sa;
/*<       CNUM = ONE >*/
    cnum = 1.;

/*<    10 CONTINUE >*/
L10:
/*<       CDEN1 = CDEN*SMLNUM >*/
    cden1 = cden * smlnum;
/*<       CNUM1 = CNUM / BIGNUM >*/
    cnum1 = cnum / bignum;
/*<       IF( ABS( CDEN1 ).GT.ABS( CNUM ) .AND. CNUM.NE.ZERO ) THEN >*/
    if (abs(cden1) > abs(cnum) && cnum != 0.) {

/*        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM. */

/*<          MUL = SMLNUM >*/
        mul = smlnum;
/*<          DONE = .FALSE. >*/
        done = FALSE_;
/*<          CDEN = CDEN1 >*/
        cden = cden1;
/*<       ELSE IF( ABS( CNUM1 ).GT.ABS( CDEN ) ) THEN >*/
    } else if (abs(cnum1) > abs(cden)) {

/*        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM. */

/*<          MUL = BIGNUM >*/
        mul = bignum;
/*<          DONE = .FALSE. >*/
        done = FALSE_;
/*<          CNUM = CNUM1 >*/
        cnum = cnum1;
/*<       ELSE >*/
    } else {

/*        Multiply X by CNUM / CDEN and return. */

/*<          MUL = CNUM / CDEN >*/
        mul = cnum / cden;
/*<          DONE = .TRUE. >*/
        done = TRUE_;
/*<       END IF >*/
    }

/*     Scale the vector X by MUL */

/*<       CALL DSCAL( N, MUL, SX, INCX ) >*/
    dscal_(n, &mul, &sx[1], incx);

/*<    >*/
    if (! done) {
        goto L10;
    }

/*<       RETURN >*/
    return 0;

/*     End of DRSCL */

/*<       END >*/
} /* drscl_ */

#ifdef __cplusplus
        }
#endif
