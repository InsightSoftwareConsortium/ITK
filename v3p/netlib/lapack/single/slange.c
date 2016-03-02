/* lapack/single/slange.f -- translated by f2c (version 20050501).
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

/*<       REAL             FUNCTION SLANGE( NORM, M, N, A, LDA, WORK ) >*/
doublereal slange_(char *norm, integer *m, integer *n, real *a, integer *lda,
        real *work, ftnlen norm_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    real ret_val, r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j;
    real sum, scale;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    real value=0;
    extern /* Subroutine */ int slassq_(integer *, real *, integer *, real *,
            real *);
    (void)norm_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          NORM >*/
/*<       INTEGER            LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               A( LDA, * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLANGE  returns the value of the one norm,  or the Frobenius norm, or */
/*  the  infinity norm,  or the  element of  largest absolute value  of a */
/*  real matrix A. */

/*  Description */
/*  =========== */

/*  SLANGE returns the value */

/*     SLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm' */
/*              ( */
/*              ( norm1(A),         NORM = '1', 'O' or 'o' */
/*              ( */
/*              ( normI(A),         NORM = 'I' or 'i' */
/*              ( */
/*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e' */

/*  where  norm1  denotes the  one norm of a matrix (maximum column sum), */
/*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and */
/*  normF  denotes the  Frobenius norm of a matrix (square root of sum of */
/*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm. */

/*  Arguments */
/*  ========= */

/*  NORM    (input) CHARACTER*1 */
/*          Specifies the value to be returned in SLANGE as described */
/*          above. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0.  When M = 0, */
/*          SLANGE is set to zero. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0.  When N = 0, */
/*          SLANGE is set to zero. */

/*  A       (input) REAL array, dimension (LDA,N) */
/*          The m by n matrix A. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(M,1). */

/*  WORK    (workspace) REAL array, dimension (LWORK), */
/*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not */
/*          referenced. */

/* ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, J >*/
/*<       REAL               SCALE, SUM, VALUE >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SLASSQ >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( MIN( M, N ).EQ.0 ) THEN >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --work;

    /* Function Body */
    if (min(*m,*n) == 0) {
/*<          VALUE = ZERO >*/
        value = (float)0.;
/*<       ELSE IF( LSAME( NORM, 'M' ) ) THEN >*/
    } else if (lsame_(norm, "M", (ftnlen)1, (ftnlen)1)) {

/*        Find max(abs(A(i,j))). */

/*<          VALUE = ZERO >*/
        value = (float)0.;
/*<          DO 20 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 10 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                VALUE = MAX( VALUE, ABS( A( I, J ) ) ) >*/
/* Computing MAX */
                r__2 = value, r__3 = (r__1 = a[i__ + j * a_dim1], dabs(r__1));
                value = dmax(r__2,r__3);
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN >*/
    } else if (lsame_(norm, "O", (ftnlen)1, (ftnlen)1) || *(unsigned char *)
            norm == '1') {

/*        Find norm1(A). */

/*<          VALUE = ZERO >*/
        value = (float)0.;
/*<          DO 40 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             SUM = ZERO >*/
            sum = (float)0.;
/*<             DO 30 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                SUM = SUM + ABS( A( I, J ) ) >*/
                sum += (r__1 = a[i__ + j * a_dim1], dabs(r__1));
/*<    30       CONTINUE >*/
/* L30: */
            }
/*<             VALUE = MAX( VALUE, SUM ) >*/
            value = dmax(value,sum);
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<       ELSE IF( LSAME( NORM, 'I' ) ) THEN >*/
    } else if (lsame_(norm, "I", (ftnlen)1, (ftnlen)1)) {

/*        Find normI(A). */

/*<          DO 50 I = 1, M >*/
        i__1 = *m;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             WORK( I ) = ZERO >*/
            work[i__] = (float)0.;
/*<    50    CONTINUE >*/
/* L50: */
        }
/*<          DO 70 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 60 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                WORK( I ) = WORK( I ) + ABS( A( I, J ) ) >*/
                work[i__] += (r__1 = a[i__ + j * a_dim1], dabs(r__1));
/*<    60       CONTINUE >*/
/* L60: */
            }
/*<    70    CONTINUE >*/
/* L70: */
        }
/*<          VALUE = ZERO >*/
        value = (float)0.;
/*<          DO 80 I = 1, M >*/
        i__1 = *m;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             VALUE = MAX( VALUE, WORK( I ) ) >*/
/* Computing MAX */
            r__1 = value, r__2 = work[i__];
            value = dmax(r__1,r__2);
/*<    80    CONTINUE >*/
/* L80: */
        }
/*<       ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN >*/
    } else if (lsame_(norm, "F", (ftnlen)1, (ftnlen)1) || lsame_(norm, "E", (
            ftnlen)1, (ftnlen)1)) {

/*        Find normF(A). */

/*<          SCALE = ZERO >*/
        scale = (float)0.;
/*<          SUM = ONE >*/
        sum = (float)1.;
/*<          DO 90 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             CALL SLASSQ( M, A( 1, J ), 1, SCALE, SUM ) >*/
            slassq_(m, &a[j * a_dim1 + 1], &c__1, &scale, &sum);
/*<    90    CONTINUE >*/
/* L90: */
        }
/*<          VALUE = SCALE*SQRT( SUM ) >*/
        value = scale * sqrt(sum);
/*<       END IF >*/
    }

/*<       SLANGE = VALUE >*/
    ret_val = value;
/*<       RETURN >*/
    return ret_val;

/*     End of SLANGE */

/*<       END >*/
} /* slange_ */

#ifdef __cplusplus
        }
#endif
