/* lapack/single/sgeqpf.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO ) >*/
/* Subroutine */ int sgeqpf_(integer *m, integer *n, real *a, integer *lda,
        integer *jpvt, real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, ma, mn;
    real aii;
    integer pvt;
    real temp, temp2;
    extern doublereal snrm2_(integer *, real *, integer *);
    extern /* Subroutine */ int slarf_(char *, integer *, integer *, real *,
            integer *, real *, real *, integer *, real *, ftnlen);
    integer itemp;
    extern /* Subroutine */ int sswap_(integer *, real *, integer *, real *,
            integer *), sgeqr2_(integer *, integer *, real *, integer *, real
            *, real *, integer *), sorm2r_(char *, char *, integer *, integer
            *, integer *, real *, integer *, real *, real *, integer *, real *
            , integer *, ftnlen, ftnlen), xerbla_(char *, integer *, ftnlen),
            slarfg_(integer *, real *, real *, integer *, real *);
    extern integer isamax_(integer *, real *, integer *);


/*  -- LAPACK test routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            JPVT( * ) >*/
/*<       REAL               A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  This routine is deprecated and has been replaced by routine SGEQP3. */

/*  SGEQPF computes a QR factorization with column pivoting of a */
/*  real M-by-N matrix A: A*P = Q*R. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A. N >= 0 */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, the upper triangle of the array contains the */
/*          min(M,N)-by-N upper triangular matrix R; the elements */
/*          below the diagonal, together with the array TAU, */
/*          represent the orthogonal matrix Q as a product of */
/*          min(m,n) elementary reflectors. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  JPVT    (input/output) INTEGER array, dimension (N) */
/*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted */
/*          to the front of A*P (a leading column); if JPVT(i) = 0, */
/*          the i-th column of A is a free column. */
/*          On exit, if JPVT(i) = k, then the i-th column of A*P */
/*          was the k-th column of A. */

/*  TAU     (output) REAL array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors. */

/*  WORK    (workspace) REAL array, dimension (3*N) */

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
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i). */

/*  The matrix P is represented in jpvt as follows: If */
/*     jpvt(j) = i */
/*  then the jth column of P is the ith canonical unit vector. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, ITEMP, J, MA, MN, PVT >*/
/*<       REAL               AII, TEMP, TEMP2 >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SGEQR2, SLARF, SLARFG, SORM2R, SSWAP, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            ISAMAX >*/
/*<       REAL               SNRM2 >*/
/*<       EXTERNAL           ISAMAX, SNRM2 >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --jpvt;
    --tau;
    --work;

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
/*<          CALL XERBLA( 'SGEQPF', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("SGEQPF", &i__1, (ftnlen)6);
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
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          IF( JPVT( I ).NE.0 ) THEN >*/
        if (jpvt[i__] != 0) {
/*<             IF( I.NE.ITEMP ) THEN >*/
            if (i__ != itemp) {
/*<                CALL SSWAP( M, A( 1, I ), 1, A( 1, ITEMP ), 1 ) >*/
                sswap_(m, &a[i__ * a_dim1 + 1], &c__1, &a[itemp * a_dim1 + 1],
                         &c__1);
/*<                JPVT( I ) = JPVT( ITEMP ) >*/
                jpvt[i__] = jpvt[itemp];
/*<                JPVT( ITEMP ) = I >*/
                jpvt[itemp] = i__;
/*<             ELSE >*/
            } else {
/*<                JPVT( I ) = I >*/
                jpvt[i__] = i__;
/*<             END IF >*/
            }
/*<             ITEMP = ITEMP + 1 >*/
            ++itemp;
/*<          ELSE >*/
        } else {
/*<             JPVT( I ) = I >*/
            jpvt[i__] = i__;
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
/*<          CALL SGEQR2( M, MA, A, LDA, TAU, WORK, INFO ) >*/
        sgeqr2_(m, &ma, &a[a_offset], lda, &tau[1], &work[1], info);
/*<          IF( MA.LT.N ) THEN >*/
        if (ma < *n) {
/*<    >*/
            i__1 = *n - ma;
            sorm2r_("Left", "Transpose", m, &i__1, &ma, &a[a_offset], lda, &
                    tau[1], &a[(ma + 1) * a_dim1 + 1], lda, &work[1], info, (
                    ftnlen)4, (ftnlen)9);
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
        for (i__ = itemp + 1; i__ <= i__1; ++i__) {
/*<             WORK( I ) = SNRM2( M-ITEMP, A( ITEMP+1, I ), 1 ) >*/
            i__2 = *m - itemp;
            work[i__] = snrm2_(&i__2, &a[itemp + 1 + i__ * a_dim1], &c__1);
/*<             WORK( N+I ) = WORK( I ) >*/
            work[*n + i__] = work[i__];
/*<    20    CONTINUE >*/
/* L20: */
        }

/*        Compute factorization */

/*<          DO 40 I = ITEMP + 1, MN >*/
        i__1 = mn;
        for (i__ = itemp + 1; i__ <= i__1; ++i__) {

/*           Determine ith pivot column and swap if necessary */

/*<             PVT = ( I-1 ) + ISAMAX( N-I+1, WORK( I ), 1 ) >*/
            i__2 = *n - i__ + 1;
            pvt = i__ - 1 + isamax_(&i__2, &work[i__], &c__1);

/*<             IF( PVT.NE.I ) THEN >*/
            if (pvt != i__) {
/*<                CALL SSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 ) >*/
                sswap_(m, &a[pvt * a_dim1 + 1], &c__1, &a[i__ * a_dim1 + 1], &
                        c__1);
/*<                ITEMP = JPVT( PVT ) >*/
                itemp = jpvt[pvt];
/*<                JPVT( PVT ) = JPVT( I ) >*/
                jpvt[pvt] = jpvt[i__];
/*<                JPVT( I ) = ITEMP >*/
                jpvt[i__] = itemp;
/*<                WORK( PVT ) = WORK( I ) >*/
                work[pvt] = work[i__];
/*<                WORK( N+PVT ) = WORK( N+I ) >*/
                work[*n + pvt] = work[*n + i__];
/*<             END IF >*/
            }

/*           Generate elementary reflector H(i) */

/*<             IF( I.LT.M ) THEN >*/
            if (i__ < *m) {
/*<                CALL SLARFG( M-I+1, A( I, I ), A( I+1, I ), 1, TAU( I ) ) >*/
                i__2 = *m - i__ + 1;
                slarfg_(&i__2, &a[i__ + i__ * a_dim1], &a[i__ + 1 + i__ *
                        a_dim1], &c__1, &tau[i__]);
/*<             ELSE >*/
            } else {
/*<                CALL SLARFG( 1, A( M, M ), A( M, M ), 1, TAU( M ) ) >*/
                slarfg_(&c__1, &a[*m + *m * a_dim1], &a[*m + *m * a_dim1], &
                        c__1, &tau[*m]);
/*<             END IF >*/
            }

/*<             IF( I.LT.N ) THEN >*/
            if (i__ < *n) {

/*              Apply H(i) to A(i:m,i+1:n) from the left */

/*<                AII = A( I, I ) >*/
                aii = a[i__ + i__ * a_dim1];
/*<                A( I, I ) = ONE >*/
                a[i__ + i__ * a_dim1] = (float)1.;
/*<    >*/
                i__2 = *m - i__ + 1;
                i__3 = *n - i__;
                slarf_("LEFT", &i__2, &i__3, &a[i__ + i__ * a_dim1], &c__1, &
                        tau[i__], &a[i__ + (i__ + 1) * a_dim1], lda, &work[(*
                        n << 1) + 1], (ftnlen)4);
/*<                A( I, I ) = AII >*/
                a[i__ + i__ * a_dim1] = aii;
/*<             END IF >*/
            }

/*           Update partial column norms */

/*<             DO 30 J = I + 1, N >*/
            i__2 = *n;
            for (j = i__ + 1; j <= i__2; ++j) {
/*<                IF( WORK( J ).NE.ZERO ) THEN >*/
                if (work[j] != (float)0.) {
/*<                   TEMP = ONE - ( ABS( A( I, J ) ) / WORK( J ) )**2 >*/
/* Computing 2nd power */
                    r__2 = (r__1 = a[i__ + j * a_dim1], dabs(r__1)) / work[j];
                    temp = (float)1. - r__2 * r__2;
/*<                   TEMP = MAX( TEMP, ZERO ) >*/
                    temp = dmax(temp,(float)0.);
/*<                   TEMP2 = ONE + 0.05*TEMP*( WORK( J ) / WORK( N+J ) )**2 >*/
/* Computing 2nd power */
                    r__1 = work[j] / work[*n + j];
                    temp2 = temp * (float).05 * (r__1 * r__1) + (float)1.;
/*<                   IF( TEMP2.EQ.ONE ) THEN >*/
                    if (temp2 == (float)1.) {
/*<                      IF( M-I.GT.0 ) THEN >*/
                        if (*m - i__ > 0) {
/*<                         WORK( J ) = SNRM2( M-I, A( I+1, J ), 1 ) >*/
                            i__3 = *m - i__;
                            work[j] = snrm2_(&i__3, &a[i__ + 1 + j * a_dim1],
                                    &c__1);
/*<                         WORK( N+J ) = WORK( J ) >*/
                            work[*n + j] = work[j];
/*<                      ELSE >*/
                        } else {
/*<                         WORK( J ) = ZERO >*/
                            work[j] = (float)0.;
/*<                         WORK( N+J ) = ZERO >*/
                            work[*n + j] = (float)0.;
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

/*     End of SGEQPF */

/*<       END >*/
} /* sgeqpf_ */

#ifdef __cplusplus
        }
#endif
