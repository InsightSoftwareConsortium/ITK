/* dgeqpf.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/*<       SUBROUTINE DGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO ) >*/
/* Subroutine */ int dgeqpf_(integer *m, integer *n, doublereal *a, integer *
        lda, integer *jpvt, doublereal *tau, doublereal *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal temp;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static doublereal temp2;
    static integer i, j;
    extern /* Subroutine */ int dlarf_(char *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *,
            doublereal *, ftnlen);
    static integer itemp;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *), dgeqr2_(integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *),
            dorm2r_(char *, char *, integer *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *,
            doublereal *, integer *, ftnlen, ftnlen);
    static integer ma, mn;
    extern /* Subroutine */ int dlarfg_(integer *, doublereal *, doublereal *,
             integer *, doublereal *);
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    static doublereal aii;
    static integer pvt;


/*  -- LAPACK test routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            JPVT( * ) >*/
/*<       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGEQPF computes a QR factorization with column pivoting of a */
/*  real M-by-N matrix A: A*P = Q*R. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A. N >= 0 */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, the upper triangle of the array contains the */
/*          min(M,N)-by-N upper triangular matrix R; the elements */
/*          below the diagonal, together with the array TAU, */
/*          represent the orthogonal matrix Q as a product of */
/*          min(m,n) elementary reflectors. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  JPVT    (input/output) INTEGER array, dimension (N) */
/*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*/
/*          to the front of A*P (a leading column); if JPVT(i) = 0, */
/*          the i-th column of A is a free column. */
/*          On exit, if JPVT(i) = k, then the i-th column of A*P */
/*          was the k-th column of A. */

/*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors. */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N) */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of elementary reflectors */

/*     Q = H(1) H(2) . . . H(n) */

/*  Each H(i) has the form */

/*     H = I - tau * v * v' */

/*  where tau is a real scalar, and v is a real vector with */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).
*/

/*  The matrix P is represented in jpvt as follows: If */
/*     jpvt(j) = i */
/*  then the jth column of P is the ith canonical unit vector. */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, ITEMP, J, MA, MN, PVT >*/
/*<       DOUBLE PRECISION   AII, TEMP, TEMP2 >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DGEQR2, DLARF, DLARFG, DORM2R, DSWAP, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            IDAMAX >*/
/*<       DOUBLE PRECISION   DNRM2 >*/
/*<       EXTERNAL           IDAMAX, DNRM2 >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --work;
    --tau;
    --jpvt;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *info = 0;
/*<       IF( M.LT.0 ) THEN >*/
    if (*m < 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DGEQPF', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DGEQPF", &i__1, 6L);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       MN = MIN( M, N ) >*/
    mn = min(*m,*n);

/*     Move initial columns up front */

/*<       ITEMP = 1 >*/
    itemp = 1;
/*<       DO 10 I = 1, N >*/
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/*<          IF( JPVT( I ).NE.0 ) THEN >*/
        if (jpvt[i] != 0) {
/*<             IF( I.NE.ITEMP ) THEN >*/
            if (i != itemp) {
/*<                CALL DSWAP( M, A( 1, I ), 1, A( 1, ITEMP ), 1 ) >*/
                dswap_(m, &a[i * a_dim1 + 1], &c__1, &a[itemp * a_dim1 + 1], &
                        c__1);
/*<                JPVT( I ) = JPVT( ITEMP ) >*/
                jpvt[i] = jpvt[itemp];
/*<                JPVT( ITEMP ) = I >*/
                jpvt[itemp] = i;
/*<             ELSE >*/
            } else {
/*<                JPVT( I ) = I >*/
                jpvt[i] = i;
/*<             END IF >*/
            }
/*<             ITEMP = ITEMP + 1 >*/
            ++itemp;
/*<          ELSE >*/
        } else {
/*<             JPVT( I ) = I >*/
            jpvt[i] = i;
/*<          END IF >*/
        }
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       ITEMP = ITEMP - 1 >*/
    --itemp;

/*     Compute the QR factorization and update remaining columns */

/*<       IF( ITEMP.GT.0 ) THEN >*/
    if (itemp > 0) {
/*<          MA = MIN( ITEMP, M ) >*/
        ma = min(itemp,*m);
/*<          CALL DGEQR2( M, MA, A, LDA, TAU, WORK, INFO ) >*/
        dgeqr2_(m, &ma, &a[a_offset], lda, &tau[1], &work[1], info);
/*<          IF( MA.LT.N ) THEN >*/
        if (ma < *n) {
/*<    >*/
            i__1 = *n - ma;
            dorm2r_("Left", "Transpose", m, &i__1, &ma, &a[a_offset], lda, &
                    tau[1], &a[(ma + 1) * a_dim1 + 1], lda, &work[1], info,
                    4L, 9L);
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       IF( ITEMP.LT.MN ) THEN >*/
    if (itemp < mn) {

/*        Initialize partial column norms. The first n elements of */
/*        work store the exact column norms. */

/*<          DO 20 I = ITEMP + 1, N >*/
        i__1 = *n;
        for (i = itemp + 1; i <= i__1; ++i) {
/*<             WORK( I ) = DNRM2( M-ITEMP, A( ITEMP+1, I ), 1 ) >*/
            i__2 = *m - itemp;
            work[i] = dnrm2_(&i__2, &a[itemp + 1 + i * a_dim1], &c__1);
/*<             WORK( N+I ) = WORK( I ) >*/
            work[*n + i] = work[i];
/*<    20    CONTINUE >*/
/* L20: */
        }

/*        Compute factorization */

/*<          DO 40 I = ITEMP + 1, MN >*/
        i__1 = mn;
        for (i = itemp + 1; i <= i__1; ++i) {

/*           Determine ith pivot column and swap if necessary */

/*<             PVT = ( I-1 ) + IDAMAX( N-I+1, WORK( I ), 1 ) >*/
            i__2 = *n - i + 1;
            pvt = i - 1 + idamax_(&i__2, &work[i], &c__1);

/*<             IF( PVT.NE.I ) THEN >*/
            if (pvt != i) {
/*<                CALL DSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 ) >*/
                dswap_(m, &a[pvt * a_dim1 + 1], &c__1, &a[i * a_dim1 + 1], &
                        c__1);
/*<                ITEMP = JPVT( PVT ) >*/
                itemp = jpvt[pvt];
/*<                JPVT( PVT ) = JPVT( I ) >*/
                jpvt[pvt] = jpvt[i];
/*<                JPVT( I ) = ITEMP >*/
                jpvt[i] = itemp;
/*<                WORK( PVT ) = WORK( I ) >*/
                work[pvt] = work[i];
/*<                WORK( N+PVT ) = WORK( N+I ) >*/
                work[*n + pvt] = work[*n + i];
/*<             END IF >*/
            }

/*           Generate elementary reflector H(i) */

/*<             IF( I.LT.M ) THEN >*/
            if (i < *m) {
/*<                CALL DLARFG( M-I+1, A( I, I ), A( I+1, I ), 1, TAU( I ) ) >*/
                i__2 = *m - i + 1;
                dlarfg_(&i__2, &a[i + i * a_dim1], &a[i + 1 + i * a_dim1], &
                        c__1, &tau[i]);
/*<             ELSE >*/
            } else {
/*<                CALL DLARFG( 1, A( M, M ), A( M, M ), 1, TAU( M ) ) >*/
                dlarfg_(&c__1, &a[*m + *m * a_dim1], &a[*m + *m * a_dim1], &
                        c__1, &tau[*m]);
/*<             END IF >*/
            }

/*<             IF( I.LT.N ) THEN >*/
            if (i < *n) {

/*              Apply H(i) to A(i:m,i+1:n) from the left */

/*<                AII = A( I, I ) >*/
                aii = a[i + i * a_dim1];
/*<                A( I, I ) = ONE >*/
                a[i + i * a_dim1] = 1.;
/*<    >*/
                i__2 = *m - i + 1;
                i__3 = *n - i;
                dlarf_("LEFT", &i__2, &i__3, &a[i + i * a_dim1], &c__1, &tau[
                        i], &a[i + (i + 1) * a_dim1], lda, &work[(*n << 1) +
                        1], 4L);
/*<                A( I, I ) = AII >*/
                a[i + i * a_dim1] = aii;
/*<             END IF >*/
            }

/*           Update partial column norms */

/*<             DO 30 J = I + 1, N >*/
            i__2 = *n;
            for (j = i + 1; j <= i__2; ++j) {
/*<                IF( WORK( J ).NE.ZERO ) THEN >*/
                if (work[j] != 0.) {
/*<                   TEMP = ONE - ( ABS( A( I, J ) ) / WORK( J ) )**2 >*/
/* Computing 2nd power */
                    d__2 = (d__1 = a[i + j * a_dim1], abs(d__1)) / work[j];
                    temp = 1. - d__2 * d__2;
/*<                   TEMP = MAX( TEMP, ZERO ) >*/
                    temp = max(temp,0.);
/*<    >*/
/* Computing 2nd power */
                    d__1 = work[j] / work[*n + j];
                    temp2 = temp * .05 * (d__1 * d__1) + 1.;
/*<                   IF( TEMP2.EQ.ONE ) THEN >*/
                    if (temp2 == 1.) {
/*<                      IF( M-I.GT.0 ) THEN >*/
                        if (*m - i > 0) {
/*<                         WORK( J ) = DNRM2( M-I, A( I+1, J ), 1 ) >*/
                            i__3 = *m - i;
                            work[j] = dnrm2_(&i__3, &a[i + 1 + j * a_dim1], &
                                    c__1);
/*<                         WORK( N+J ) = WORK( J ) >*/
                            work[*n + j] = work[j];
/*<                      ELSE >*/
                        } else {
/*<                         WORK( J ) = ZERO >*/
                            work[j] = 0.;
/*<                         WORK( N+J ) = ZERO >*/
                            work[*n + j] = 0.;
/*<                      END IF >*/
                        }
/*<                   ELSE >*/
                    } else {
/*<                      WORK( J ) = WORK( J )*SQRT( TEMP ) >*/
                        work[j] *= sqrt(temp);
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }
/*<    30       CONTINUE >*/
/* L30: */
            }

/*<    40    CONTINUE >*/
/* L40: */
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of DGEQPF */

/*<       END >*/
} /* dgeqpf_ */

