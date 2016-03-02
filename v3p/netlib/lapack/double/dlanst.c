/* lapack/double/dlanst.f -- translated by f2c (version 20090411).
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

/*<       DOUBLE PRECISION FUNCTION DLANST( NORM, N, D, E ) >*/
doublereal dlanst_(char *norm, integer *n, doublereal *d__, doublereal *e,
        ftnlen norm_len)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1, d__2, d__3, d__4, d__5;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;
    doublereal sum, scale;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal anorm;
    extern /* Subroutine */ int dlassq_(integer *, doublereal *, integer *,
            doublereal *, doublereal *);


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          NORM >*/
/*<       INTEGER            N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   D( * ), E( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLANST  returns the value of the one norm,  or the Frobenius norm, or */
/*  the  infinity norm,  or the  element of  largest absolute value  of a */
/*  real symmetric tridiagonal matrix A. */

/*  Description */
/*  =========== */

/*  DLANST returns the value */

/*     DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm' */
/*              ( */
/*              ( norm1(A),         NORM = '1', 'O' or 'o' */
/*              ( */
/*              ( normI(A),         NORM = 'I' or 'i' */
/*              ( */
/*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e' */

/*  where  norm1  denotes the  one norm of a matrix (maximum column sum), */
/*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and */
/*  normF  denotes the  Frobenius norm of a matrix (square root of sum of */
/*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm. */

/*  Arguments */
/*  ========= */

/*  NORM    (input) CHARACTER*1 */
/*          Specifies the value to be returned in DLANST as described */
/*          above. */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0.  When N = 0, DLANST is */
/*          set to zero. */

/*  D       (input) DOUBLE PRECISION array, dimension (N) */
/*          The diagonal elements of A. */

/*  E       (input) DOUBLE PRECISION array, dimension (N-1) */
/*          The (n-1) sub-diagonal or super-diagonal elements of A. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I >*/
/*<       DOUBLE PRECISION   ANORM, SCALE, SUM >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLASSQ >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( N.LE.0 ) THEN >*/
    /* Parameter adjustments */
    --e;
    --d__;

    /* Function Body */
    if (*n <= 0) {
/*<          ANORM = ZERO >*/
        anorm = 0.;
/*<       ELSE IF( LSAME( NORM, 'M' ) ) THEN >*/
    } else if (lsame_(norm, "M", (ftnlen)1, (ftnlen)1)) {

/*        Find max(abs(A(i,j))). */

/*<          ANORM = ABS( D( N ) ) >*/
        anorm = (d__1 = d__[*n], abs(d__1));
/*<          DO 10 I = 1, N - 1 >*/
        i__1 = *n - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             ANORM = MAX( ANORM, ABS( D( I ) ) ) >*/
/* Computing MAX */
            d__2 = anorm, d__3 = (d__1 = d__[i__], abs(d__1));
            anorm = max(d__2,d__3);
/*<             ANORM = MAX( ANORM, ABS( E( I ) ) ) >*/
/* Computing MAX */
            d__2 = anorm, d__3 = (d__1 = e[i__], abs(d__1));
            anorm = max(d__2,d__3);
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<        >*/
    } else if (lsame_(norm, "O", (ftnlen)1, (ftnlen)1) || *(unsigned char *)
            norm == '1' || lsame_(norm, "I", (ftnlen)1, (ftnlen)1)) {

/*        Find norm1(A). */

/*<          IF( N.EQ.1 ) THEN >*/
        if (*n == 1) {
/*<             ANORM = ABS( D( 1 ) ) >*/
            anorm = abs(d__[1]);
/*<          ELSE >*/
        } else {
/*<        >*/
/* Computing MAX */
            d__3 = abs(d__[1]) + abs(e[1]), d__4 = (d__1 = e[*n - 1], abs(
                    d__1)) + (d__2 = d__[*n], abs(d__2));
            anorm = max(d__3,d__4);
/*<             DO 20 I = 2, N - 1 >*/
            i__1 = *n - 1;
            for (i__ = 2; i__ <= i__1; ++i__) {
/*<        >*/
/* Computing MAX */
                d__4 = anorm, d__5 = (d__1 = d__[i__], abs(d__1)) + (d__2 = e[
                        i__], abs(d__2)) + (d__3 = e[i__ - 1], abs(d__3));
                anorm = max(d__4,d__5);
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          END IF >*/
        }
/*<       ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN >*/
    } else if (lsame_(norm, "F", (ftnlen)1, (ftnlen)1) || lsame_(norm, "E", (
            ftnlen)1, (ftnlen)1)) {

/*        Find normF(A). */

/*<          SCALE = ZERO >*/
        scale = 0.;
/*<          SUM = ONE >*/
        sum = 1.;
/*<          IF( N.GT.1 ) THEN >*/
        if (*n > 1) {
/*<             CALL DLASSQ( N-1, E, 1, SCALE, SUM ) >*/
            i__1 = *n - 1;
            dlassq_(&i__1, &e[1], &c__1, &scale, &sum);
/*<             SUM = 2*SUM >*/
            sum *= 2;
/*<          END IF >*/
        }
/*<          CALL DLASSQ( N, D, 1, SCALE, SUM ) >*/
        dlassq_(n, &d__[1], &c__1, &scale, &sum);
/*<          ANORM = SCALE*SQRT( SUM ) >*/
        anorm = scale * sqrt(sum);
/*<       END IF >*/
    }

/*<       DLANST = ANORM >*/
    ret_val = anorm;
/*<       RETURN >*/
    return ret_val;

/*     End of DLANST */

/*<       END >*/
} /* dlanst_ */

#ifdef __cplusplus
        }
#endif
