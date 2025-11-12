/* lapack/double/dlasr.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE DLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA ) >*/
/* Subroutine */ int dlasr_(char *side, char *pivot, char *direct, integer *m,
         integer *n, doublereal *c__, doublereal *s, doublereal *a, integer *
        lda, ftnlen side_len, ftnlen pivot_len, ftnlen direct_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, info;
    doublereal temp;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal ctemp, stemp;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          DIRECT, PIVOT, SIDE >*/
/*<       INTEGER            LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), C( * ), S( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLASR applies a sequence of plane rotations to a real matrix A, */
/*  from either the left or the right. */

/*  When SIDE = 'L', the transformation takes the form */

/*     A := P*A */

/*  and when SIDE = 'R', the transformation takes the form */

/*     A := A*P**T */

/*  where P is an orthogonal matrix consisting of a sequence of z plane */
/*  rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R', */
/*  and P**T is the transpose of P. */

/*  When DIRECT = 'F' (Forward sequence), then */

/*     P = P(z-1) * ... * P(2) * P(1) */

/*  and when DIRECT = 'B' (Backward sequence), then */

/*     P = P(1) * P(2) * ... * P(z-1) */

/*  where P(k) is a plane rotation matrix defined by the 2-by-2 rotation */

/*     R(k) = (  c(k)  s(k) ) */
/*          = ( -s(k)  c(k) ). */

/*  When PIVOT = 'V' (Variable pivot), the rotation is performed */
/*  for the plane (k,k+1), i.e., P(k) has the form */

/*     P(k) = (  1                                            ) */
/*            (       ...                                     ) */
/*            (              1                                ) */
/*            (                   c(k)  s(k)                  ) */
/*            (                  -s(k)  c(k)                  ) */
/*            (                                1              ) */
/*            (                                     ...       ) */
/*            (                                            1  ) */

/*  where R(k) appears as a rank-2 modification to the identity matrix in */
/*  rows and columns k and k+1. */

/*  When PIVOT = 'T' (Top pivot), the rotation is performed for the */
/*  plane (1,k+1), so P(k) has the form */

/*     P(k) = (  c(k)                    s(k)                 ) */
/*            (         1                                     ) */
/*            (              ...                              ) */
/*            (                     1                         ) */
/*            ( -s(k)                    c(k)                 ) */
/*            (                                 1             ) */
/*            (                                      ...      ) */
/*            (                                             1 ) */

/*  where R(k) appears in rows and columns 1 and k+1. */

/*  Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is */
/*  performed for the plane (k,z), giving P(k) the form */

/*     P(k) = ( 1                                             ) */
/*            (      ...                                      ) */
/*            (             1                                 ) */
/*            (                  c(k)                    s(k) ) */
/*            (                         1                     ) */
/*            (                              ...              ) */
/*            (                                     1         ) */
/*            (                 -s(k)                    c(k) ) */

/*  where R(k) appears in rows and columns k and z.  The rotations are */
/*  performed without ever forming P(k) explicitly. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          Specifies whether the plane rotation matrix P is applied to */
/*          A on the left or the right. */
/*          = 'L':  Left, compute A := P*A */
/*          = 'R':  Right, compute A:= A*P**T */

/*  PIVOT   (input) CHARACTER*1 */
/*          Specifies the plane for which P(k) is a plane rotation */
/*          matrix. */
/*          = 'V':  Variable pivot, the plane (k,k+1) */
/*          = 'T':  Top pivot, the plane (1,k+1) */
/*          = 'B':  Bottom pivot, the plane (k,z) */

/*  DIRECT  (input) CHARACTER*1 */
/*          Specifies whether P is a forward or backward sequence of */
/*          plane rotations. */
/*          = 'F':  Forward, P = P(z-1)*...*P(2)*P(1) */
/*          = 'B':  Backward, P = P(1)*P(2)*...*P(z-1) */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  If m <= 1, an immediate */
/*          return is effected. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  If n <= 1, an */
/*          immediate return is effected. */

/*  C       (input) DOUBLE PRECISION array, dimension */
/*                  (M-1) if SIDE = 'L' */
/*                  (N-1) if SIDE = 'R' */
/*          The cosines c(k) of the plane rotations. */

/*  S       (input) DOUBLE PRECISION array, dimension */
/*                  (M-1) if SIDE = 'L' */
/*                  (N-1) if SIDE = 'R' */
/*          The sines s(k) of the plane rotations.  The 2-by-2 plane */
/*          rotation part of the matrix P(k), R(k), has the form */
/*          R(k) = (  c(k)  s(k) ) */
/*                 ( -s(k)  c(k) ). */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          The M-by-N matrix A.  On exit, A is overwritten by P*A if */
/*          SIDE = 'R' or by A*P**T if SIDE = 'L'. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, INFO, J >*/
/*<       DOUBLE PRECISION   CTEMP, STEMP, TEMP >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --c__;
    --s;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    info = 0;
/*<       IF( .NOT.( LSAME( SIDE, 'L' ) .OR. LSAME( SIDE, 'R' ) ) ) THEN >*/
    if (! (lsame_(side, "L", (ftnlen)1, (ftnlen)1) || lsame_(side, "R", (
            ftnlen)1, (ftnlen)1))) {
/*<          INFO = 1 >*/
        info = 1;
/*<        >*/
    } else if (! (lsame_(pivot, "V", (ftnlen)1, (ftnlen)1) || lsame_(pivot,
            "T", (ftnlen)1, (ftnlen)1) || lsame_(pivot, "B", (ftnlen)1, (
            ftnlen)1))) {
/*<          INFO = 2 >*/
        info = 2;
/*<        >*/
    } else if (! (lsame_(direct, "F", (ftnlen)1, (ftnlen)1) || lsame_(direct,
            "B", (ftnlen)1, (ftnlen)1))) {
/*<          INFO = 3 >*/
        info = 3;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = 4 >*/
        info = 4;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = 5 >*/
        info = 5;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = 9 >*/
        info = 9;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (info != 0) {
/*<          CALL XERBLA( 'DLASR ', INFO ) >*/
        xerbla_("DLASR ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<        >*/
    if (*m == 0 || *n == 0) {
        return 0;
    }
/*<       IF( LSAME( SIDE, 'L' ) ) THEN >*/
    if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*        Form  P * A */

/*<          IF( LSAME( PIVOT, 'V' ) ) THEN >*/
        if (lsame_(pivot, "V", (ftnlen)1, (ftnlen)1)) {
/*<             IF( LSAME( DIRECT, 'F' ) ) THEN >*/
            if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<                DO 20 J = 1, M - 1 >*/
                i__1 = *m - 1;
                for (j = 1; j <= i__1; ++j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 10 I = 1, N >*/
                        i__2 = *n;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = A( J+1, I ) >*/
                            temp = a[j + 1 + i__ * a_dim1];
/*<                         A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I ) >*/
                            a[j + 1 + i__ * a_dim1] = ctemp * temp - stemp *
                                    a[j + i__ * a_dim1];
/*<                         A( J, I ) = STEMP*TEMP + CTEMP*A( J, I ) >*/
                            a[j + i__ * a_dim1] = stemp * temp + ctemp * a[j
                                    + i__ * a_dim1];
/*<    10                CONTINUE >*/
/* L10: */
                        }
/*<                   END IF >*/
                    }
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<             ELSE IF( LSAME( DIRECT, 'B' ) ) THEN >*/
            } else if (lsame_(direct, "B", (ftnlen)1, (ftnlen)1)) {
/*<                DO 40 J = M - 1, 1, -1 >*/
                for (j = *m - 1; j >= 1; --j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 30 I = 1, N >*/
                        i__1 = *n;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         TEMP = A( J+1, I ) >*/
                            temp = a[j + 1 + i__ * a_dim1];
/*<                         A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I ) >*/
                            a[j + 1 + i__ * a_dim1] = ctemp * temp - stemp *
                                    a[j + i__ * a_dim1];
/*<                         A( J, I ) = STEMP*TEMP + CTEMP*A( J, I ) >*/
                            a[j + i__ * a_dim1] = stemp * temp + ctemp * a[j
                                    + i__ * a_dim1];
/*<    30                CONTINUE >*/
/* L30: */
                        }
/*<                   END IF >*/
                    }
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<             END IF >*/
            }
/*<          ELSE IF( LSAME( PIVOT, 'T' ) ) THEN >*/
        } else if (lsame_(pivot, "T", (ftnlen)1, (ftnlen)1)) {
/*<             IF( LSAME( DIRECT, 'F' ) ) THEN >*/
            if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<                DO 60 J = 2, M >*/
                i__1 = *m;
                for (j = 2; j <= i__1; ++j) {
/*<                   CTEMP = C( J-1 ) >*/
                    ctemp = c__[j - 1];
/*<                   STEMP = S( J-1 ) >*/
                    stemp = s[j - 1];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 50 I = 1, N >*/
                        i__2 = *n;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = A( J, I ) >*/
                            temp = a[j + i__ * a_dim1];
/*<                         A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I ) >*/
                            a[j + i__ * a_dim1] = ctemp * temp - stemp * a[
                                    i__ * a_dim1 + 1];
/*<                         A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I ) >*/
                            a[i__ * a_dim1 + 1] = stemp * temp + ctemp * a[
                                    i__ * a_dim1 + 1];
/*<    50                CONTINUE >*/
/* L50: */
                        }
/*<                   END IF >*/
                    }
/*<    60          CONTINUE >*/
/* L60: */
                }
/*<             ELSE IF( LSAME( DIRECT, 'B' ) ) THEN >*/
            } else if (lsame_(direct, "B", (ftnlen)1, (ftnlen)1)) {
/*<                DO 80 J = M, 2, -1 >*/
                for (j = *m; j >= 2; --j) {
/*<                   CTEMP = C( J-1 ) >*/
                    ctemp = c__[j - 1];
/*<                   STEMP = S( J-1 ) >*/
                    stemp = s[j - 1];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 70 I = 1, N >*/
                        i__1 = *n;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         TEMP = A( J, I ) >*/
                            temp = a[j + i__ * a_dim1];
/*<                         A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I ) >*/
                            a[j + i__ * a_dim1] = ctemp * temp - stemp * a[
                                    i__ * a_dim1 + 1];
/*<                         A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I ) >*/
                            a[i__ * a_dim1 + 1] = stemp * temp + ctemp * a[
                                    i__ * a_dim1 + 1];
/*<    70                CONTINUE >*/
/* L70: */
                        }
/*<                   END IF >*/
                    }
/*<    80          CONTINUE >*/
/* L80: */
                }
/*<             END IF >*/
            }
/*<          ELSE IF( LSAME( PIVOT, 'B' ) ) THEN >*/
        } else if (lsame_(pivot, "B", (ftnlen)1, (ftnlen)1)) {
/*<             IF( LSAME( DIRECT, 'F' ) ) THEN >*/
            if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<                DO 100 J = 1, M - 1 >*/
                i__1 = *m - 1;
                for (j = 1; j <= i__1; ++j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 90 I = 1, N >*/
                        i__2 = *n;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = A( J, I ) >*/
                            temp = a[j + i__ * a_dim1];
/*<                         A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP >*/
                            a[j + i__ * a_dim1] = stemp * a[*m + i__ * a_dim1]
                                     + ctemp * temp;
/*<                         A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP >*/
                            a[*m + i__ * a_dim1] = ctemp * a[*m + i__ *
                                    a_dim1] - stemp * temp;
/*<    90                CONTINUE >*/
/* L90: */
                        }
/*<                   END IF >*/
                    }
/*<   100          CONTINUE >*/
/* L100: */
                }
/*<             ELSE IF( LSAME( DIRECT, 'B' ) ) THEN >*/
            } else if (lsame_(direct, "B", (ftnlen)1, (ftnlen)1)) {
/*<                DO 120 J = M - 1, 1, -1 >*/
                for (j = *m - 1; j >= 1; --j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 110 I = 1, N >*/
                        i__1 = *n;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         TEMP = A( J, I ) >*/
                            temp = a[j + i__ * a_dim1];
/*<                         A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP >*/
                            a[j + i__ * a_dim1] = stemp * a[*m + i__ * a_dim1]
                                     + ctemp * temp;
/*<                         A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP >*/
                            a[*m + i__ * a_dim1] = ctemp * a[*m + i__ *
                                    a_dim1] - stemp * temp;
/*<   110                CONTINUE >*/
/* L110: */
                        }
/*<                   END IF >*/
                    }
/*<   120          CONTINUE >*/
/* L120: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE IF( LSAME( SIDE, 'R' ) ) THEN >*/
    } else if (lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {

/*        Form A * P' */

/*<          IF( LSAME( PIVOT, 'V' ) ) THEN >*/
        if (lsame_(pivot, "V", (ftnlen)1, (ftnlen)1)) {
/*<             IF( LSAME( DIRECT, 'F' ) ) THEN >*/
            if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<                DO 140 J = 1, N - 1 >*/
                i__1 = *n - 1;
                for (j = 1; j <= i__1; ++j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 130 I = 1, M >*/
                        i__2 = *m;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = A( I, J+1 ) >*/
                            temp = a[i__ + (j + 1) * a_dim1];
/*<                         A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J ) >*/
                            a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp *
                                     a[i__ + j * a_dim1];
/*<                         A( I, J ) = STEMP*TEMP + CTEMP*A( I, J ) >*/
                            a[i__ + j * a_dim1] = stemp * temp + ctemp * a[
                                    i__ + j * a_dim1];
/*<   130                CONTINUE >*/
/* L130: */
                        }
/*<                   END IF >*/
                    }
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<             ELSE IF( LSAME( DIRECT, 'B' ) ) THEN >*/
            } else if (lsame_(direct, "B", (ftnlen)1, (ftnlen)1)) {
/*<                DO 160 J = N - 1, 1, -1 >*/
                for (j = *n - 1; j >= 1; --j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 150 I = 1, M >*/
                        i__1 = *m;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         TEMP = A( I, J+1 ) >*/
                            temp = a[i__ + (j + 1) * a_dim1];
/*<                         A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J ) >*/
                            a[i__ + (j + 1) * a_dim1] = ctemp * temp - stemp *
                                     a[i__ + j * a_dim1];
/*<                         A( I, J ) = STEMP*TEMP + CTEMP*A( I, J ) >*/
                            a[i__ + j * a_dim1] = stemp * temp + ctemp * a[
                                    i__ + j * a_dim1];
/*<   150                CONTINUE >*/
/* L150: */
                        }
/*<                   END IF >*/
                    }
/*<   160          CONTINUE >*/
/* L160: */
                }
/*<             END IF >*/
            }
/*<          ELSE IF( LSAME( PIVOT, 'T' ) ) THEN >*/
        } else if (lsame_(pivot, "T", (ftnlen)1, (ftnlen)1)) {
/*<             IF( LSAME( DIRECT, 'F' ) ) THEN >*/
            if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<                DO 180 J = 2, N >*/
                i__1 = *n;
                for (j = 2; j <= i__1; ++j) {
/*<                   CTEMP = C( J-1 ) >*/
                    ctemp = c__[j - 1];
/*<                   STEMP = S( J-1 ) >*/
                    stemp = s[j - 1];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 170 I = 1, M >*/
                        i__2 = *m;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = A( I, J ) >*/
                            temp = a[i__ + j * a_dim1];
/*<                         A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 ) >*/
                            a[i__ + j * a_dim1] = ctemp * temp - stemp * a[
                                    i__ + a_dim1];
/*<                         A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 ) >*/
                            a[i__ + a_dim1] = stemp * temp + ctemp * a[i__ +
                                    a_dim1];
/*<   170                CONTINUE >*/
/* L170: */
                        }
/*<                   END IF >*/
                    }
/*<   180          CONTINUE >*/
/* L180: */
                }
/*<             ELSE IF( LSAME( DIRECT, 'B' ) ) THEN >*/
            } else if (lsame_(direct, "B", (ftnlen)1, (ftnlen)1)) {
/*<                DO 200 J = N, 2, -1 >*/
                for (j = *n; j >= 2; --j) {
/*<                   CTEMP = C( J-1 ) >*/
                    ctemp = c__[j - 1];
/*<                   STEMP = S( J-1 ) >*/
                    stemp = s[j - 1];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 190 I = 1, M >*/
                        i__1 = *m;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         TEMP = A( I, J ) >*/
                            temp = a[i__ + j * a_dim1];
/*<                         A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 ) >*/
                            a[i__ + j * a_dim1] = ctemp * temp - stemp * a[
                                    i__ + a_dim1];
/*<                         A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 ) >*/
                            a[i__ + a_dim1] = stemp * temp + ctemp * a[i__ +
                                    a_dim1];
/*<   190                CONTINUE >*/
/* L190: */
                        }
/*<                   END IF >*/
                    }
/*<   200          CONTINUE >*/
/* L200: */
                }
/*<             END IF >*/
            }
/*<          ELSE IF( LSAME( PIVOT, 'B' ) ) THEN >*/
        } else if (lsame_(pivot, "B", (ftnlen)1, (ftnlen)1)) {
/*<             IF( LSAME( DIRECT, 'F' ) ) THEN >*/
            if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<                DO 220 J = 1, N - 1 >*/
                i__1 = *n - 1;
                for (j = 1; j <= i__1; ++j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 210 I = 1, M >*/
                        i__2 = *m;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = A( I, J ) >*/
                            temp = a[i__ + j * a_dim1];
/*<                         A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP >*/
                            a[i__ + j * a_dim1] = stemp * a[i__ + *n * a_dim1]
                                     + ctemp * temp;
/*<                         A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP >*/
                            a[i__ + *n * a_dim1] = ctemp * a[i__ + *n *
                                    a_dim1] - stemp * temp;
/*<   210                CONTINUE >*/
/* L210: */
                        }
/*<                   END IF >*/
                    }
/*<   220          CONTINUE >*/
/* L220: */
                }
/*<             ELSE IF( LSAME( DIRECT, 'B' ) ) THEN >*/
            } else if (lsame_(direct, "B", (ftnlen)1, (ftnlen)1)) {
/*<                DO 240 J = N - 1, 1, -1 >*/
                for (j = *n - 1; j >= 1; --j) {
/*<                   CTEMP = C( J ) >*/
                    ctemp = c__[j];
/*<                   STEMP = S( J ) >*/
                    stemp = s[j];
/*<                   IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN >*/
                    if (ctemp != 1. || stemp != 0.) {
/*<                      DO 230 I = 1, M >*/
                        i__1 = *m;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         TEMP = A( I, J ) >*/
                            temp = a[i__ + j * a_dim1];
/*<                         A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP >*/
                            a[i__ + j * a_dim1] = stemp * a[i__ + *n * a_dim1]
                                     + ctemp * temp;
/*<                         A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP >*/
                            a[i__ + *n * a_dim1] = ctemp * a[i__ + *n *
                                    a_dim1] - stemp * temp;
/*<   230                CONTINUE >*/
/* L230: */
                        }
/*<                   END IF >*/
                    }
/*<   240          CONTINUE >*/
/* L240: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DLASR */

/*<       END >*/
} /* dlasr_ */

#ifdef __cplusplus
        }
#endif
