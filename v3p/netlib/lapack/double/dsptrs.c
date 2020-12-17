/* dsptrs.f -- translated by f2c (version 20060506).
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

static doublereal c_b7 = -1.;
static integer c__1 = 1;
static doublereal c_b19 = 1.;

/*<       SUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO ) >*/
/* Subroutine */ int dsptrs_(char *uplo, integer *n, integer *nrhs,
        doublereal *ap, integer *ipiv, doublereal *b, integer *ldb, integer *
        info, ftnlen uplo_len)
{
    /* System generated locals */
    integer b_dim1, b_offset, i__1;
    doublereal d__1;

    /* Local variables */
    integer j, k;
    doublereal ak, bk;
    integer kc, kp;
    doublereal akm1, bkm1;
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            integer *);
    doublereal akm1k;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal denom;
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, ftnlen), dswap_(integer *,
            doublereal *, integer *, doublereal *, integer *);
    logical upper;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    (void)uplo_len;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          UPLO >*/
/*<       INTEGER            INFO, LDB, N, NRHS >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IPIV( * ) >*/
/*<       DOUBLE PRECISION   AP( * ), B( LDB, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DSPTRS solves a system of linear equations A*X = B with a real */
/*  symmetric matrix A stored in packed format using the factorization */
/*  A = U*D*U**T or A = L*D*L**T computed by DSPTRF. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          Specifies whether the details of the factorization are stored */
/*          as an upper or lower triangular matrix. */
/*          = 'U':  Upper triangular, form is A = U*D*U**T; */
/*          = 'L':  Lower triangular, form is A = L*D*L**T. */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0. */

/*  NRHS    (input) INTEGER */
/*          The number of right hand sides, i.e., the number of columns */
/*          of the matrix B.  NRHS >= 0. */

/*  AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2) */
/*          The block diagonal matrix D and the multipliers used to */
/*          obtain the factor U or L as computed by DSPTRF, stored as a */
/*          packed triangular matrix. */

/*  IPIV    (input) INTEGER array, dimension (N) */
/*          Details of the interchanges and the block structure of D */
/*          as determined by DSPTRF. */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS) */
/*          On entry, the right hand side matrix B. */
/*          On exit, the solution matrix X. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B.  LDB >= max(1,N). */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            UPPER >*/
/*<       INTEGER            J, K, KC, KP >*/
/*<       DOUBLE PRECISION   AK, AKM1, AKM1K, BK, BKM1, DENOM >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DGEMV, DGER, DSCAL, DSWAP, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --ap;
    --ipiv;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;

    /* Function Body */
    *info = 0;
/*<       UPPER = LSAME( UPLO, 'U' ) >*/
    upper = lsame_(uplo, "U", (ftnlen)1, (ftnlen)1);
/*<       IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN >*/
    if (! upper && ! lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( NRHS.LT.0 ) THEN >*/
    } else if (*nrhs < 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldb < max(1,*n)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DSPTRS', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DSPTRS", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n == 0 || *nrhs == 0) {
        return 0;
    }

/*<       IF( UPPER ) THEN >*/
    if (upper) {

/*        Solve A*X = B, where A = U*D*U'. */

/*        First solve U*D*X = B, overwriting B with X. */

/*        K is the main loop index, decreasing from N to 1 in steps of */
/*        1 or 2, depending on the size of the diagonal blocks. */

/*<          K = N >*/
        k = *n;
/*<          KC = N*( N+1 ) / 2 + 1 >*/
        kc = *n * (*n + 1) / 2 + 1;
/*<    10    CONTINUE >*/
L10:

/*        If K < 1, exit from loop. */

/*<    >*/
        if (k < 1) {
            goto L30;
        }

/*<          KC = KC - K >*/
        kc -= k;
/*<          IF( IPIV( K ).GT.0 ) THEN >*/
        if (ipiv[k] > 0) {

/*           1 x 1 diagonal block */

/*           Interchange rows K and IPIV(K). */

/*<             KP = IPIV( K ) >*/
            kp = ipiv[k];
/*<    >*/
            if (kp != k) {
                dswap_(nrhs, &b[k + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }

/*           Multiply by inv(U(K)), where U(K) is the transformation */
/*           stored in column K of A. */

/*<    >*/
            i__1 = k - 1;
            dger_(&i__1, nrhs, &c_b7, &ap[kc], &c__1, &b[k + b_dim1], ldb, &b[
                    b_dim1 + 1], ldb);

/*           Multiply by the inverse of the diagonal block. */

/*<             CALL DSCAL( NRHS, ONE / AP( KC+K-1 ), B( K, 1 ), LDB ) >*/
            d__1 = 1. / ap[kc + k - 1];
            dscal_(nrhs, &d__1, &b[k + b_dim1], ldb);
/*<             K = K - 1 >*/
            --k;
/*<          ELSE >*/
        } else {

/*           2 x 2 diagonal block */

/*           Interchange rows K-1 and -IPIV(K). */

/*<             KP = -IPIV( K ) >*/
            kp = -ipiv[k];
/*<    >*/
            if (kp != k - 1) {
                dswap_(nrhs, &b[k - 1 + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }

/*           Multiply by inv(U(K)), where U(K) is the transformation */
/*           stored in columns K-1 and K of A. */

/*<    >*/
            i__1 = k - 2;
            dger_(&i__1, nrhs, &c_b7, &ap[kc], &c__1, &b[k + b_dim1], ldb, &b[
                    b_dim1 + 1], ldb);
/*<    >*/
            i__1 = k - 2;
            dger_(&i__1, nrhs, &c_b7, &ap[kc - (k - 1)], &c__1, &b[k - 1 +
                    b_dim1], ldb, &b[b_dim1 + 1], ldb);

/*           Multiply by the inverse of the diagonal block. */

/*<             AKM1K = AP( KC+K-2 ) >*/
            akm1k = ap[kc + k - 2];
/*<             AKM1 = AP( KC-1 ) / AKM1K >*/
            akm1 = ap[kc - 1] / akm1k;
/*<             AK = AP( KC+K-1 ) / AKM1K >*/
            ak = ap[kc + k - 1] / akm1k;
/*<             DENOM = AKM1*AK - ONE >*/
            denom = akm1 * ak - 1.;
/*<             DO 20 J = 1, NRHS >*/
            i__1 = *nrhs;
            for (j = 1; j <= i__1; ++j) {
/*<                BKM1 = B( K-1, J ) / AKM1K >*/
                bkm1 = b[k - 1 + j * b_dim1] / akm1k;
/*<                BK = B( K, J ) / AKM1K >*/
                bk = b[k + j * b_dim1] / akm1k;
/*<                B( K-1, J ) = ( AK*BKM1-BK ) / DENOM >*/
                b[k - 1 + j * b_dim1] = (ak * bkm1 - bk) / denom;
/*<                B( K, J ) = ( AKM1*BK-BKM1 ) / DENOM >*/
                b[k + j * b_dim1] = (akm1 * bk - bkm1) / denom;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<             KC = KC - K + 1 >*/
            kc -= k - 1;
/*<             K = K - 2 >*/
            k -= 2;
/*<          END IF >*/
        }

/*<          GO TO 10 >*/
        goto L10;
/*<    30    CONTINUE >*/
L30:

/*        Next solve U'*X = B, overwriting B with X. */

/*        K is the main loop index, increasing from 1 to N in steps of */
/*        1 or 2, depending on the size of the diagonal blocks. */

/*<          K = 1 >*/
        k = 1;
/*<          KC = 1 >*/
        kc = 1;
/*<    40    CONTINUE >*/
L40:

/*        If K > N, exit from loop. */

/*<    >*/
        if (k > *n) {
            goto L50;
        }

/*<          IF( IPIV( K ).GT.0 ) THEN >*/
        if (ipiv[k] > 0) {

/*           1 x 1 diagonal block */

/*           Multiply by inv(U'(K)), where U(K) is the transformation */
/*           stored in column K of A. */

/*<    >*/
            i__1 = k - 1;
            dgemv_("Transpose", &i__1, nrhs, &c_b7, &b[b_offset], ldb, &ap[kc]
                    , &c__1, &c_b19, &b[k + b_dim1], ldb, (ftnlen)9);

/*           Interchange rows K and IPIV(K). */

/*<             KP = IPIV( K ) >*/
            kp = ipiv[k];
/*<    >*/
            if (kp != k) {
                dswap_(nrhs, &b[k + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }
/*<             KC = KC + K >*/
            kc += k;
/*<             K = K + 1 >*/
            ++k;
/*<          ELSE >*/
        } else {

/*           2 x 2 diagonal block */

/*           Multiply by inv(U'(K+1)), where U(K+1) is the transformation */
/*           stored in columns K and K+1 of A. */

/*<    >*/
            i__1 = k - 1;
            dgemv_("Transpose", &i__1, nrhs, &c_b7, &b[b_offset], ldb, &ap[kc]
                    , &c__1, &c_b19, &b[k + b_dim1], ldb, (ftnlen)9);
/*<    >*/
            i__1 = k - 1;
            dgemv_("Transpose", &i__1, nrhs, &c_b7, &b[b_offset], ldb, &ap[kc
                    + k], &c__1, &c_b19, &b[k + 1 + b_dim1], ldb, (ftnlen)9);

/*           Interchange rows K and -IPIV(K). */

/*<             KP = -IPIV( K ) >*/
            kp = -ipiv[k];
/*<    >*/
            if (kp != k) {
                dswap_(nrhs, &b[k + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }
/*<             KC = KC + 2*K + 1 >*/
            kc += (k << 1) + 1;
/*<             K = K + 2 >*/
            k += 2;
/*<          END IF >*/
        }

/*<          GO TO 40 >*/
        goto L40;
/*<    50    CONTINUE >*/
L50:

/*<       ELSE >*/
        ;
    } else {

/*        Solve A*X = B, where A = L*D*L'. */

/*        First solve L*D*X = B, overwriting B with X. */

/*        K is the main loop index, increasing from 1 to N in steps of */
/*        1 or 2, depending on the size of the diagonal blocks. */

/*<          K = 1 >*/
        k = 1;
/*<          KC = 1 >*/
        kc = 1;
/*<    60    CONTINUE >*/
L60:

/*        If K > N, exit from loop. */

/*<    >*/
        if (k > *n) {
            goto L80;
        }

/*<          IF( IPIV( K ).GT.0 ) THEN >*/
        if (ipiv[k] > 0) {

/*           1 x 1 diagonal block */

/*           Interchange rows K and IPIV(K). */

/*<             KP = IPIV( K ) >*/
            kp = ipiv[k];
/*<    >*/
            if (kp != k) {
                dswap_(nrhs, &b[k + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }

/*           Multiply by inv(L(K)), where L(K) is the transformation */
/*           stored in column K of A. */

/*<    >*/
            if (k < *n) {
                i__1 = *n - k;
                dger_(&i__1, nrhs, &c_b7, &ap[kc + 1], &c__1, &b[k + b_dim1],
                        ldb, &b[k + 1 + b_dim1], ldb);
            }

/*           Multiply by the inverse of the diagonal block. */

/*<             CALL DSCAL( NRHS, ONE / AP( KC ), B( K, 1 ), LDB ) >*/
            d__1 = 1. / ap[kc];
            dscal_(nrhs, &d__1, &b[k + b_dim1], ldb);
/*<             KC = KC + N - K + 1 >*/
            kc += *n - k + 1;
/*<             K = K + 1 >*/
            ++k;
/*<          ELSE >*/
        } else {

/*           2 x 2 diagonal block */

/*           Interchange rows K+1 and -IPIV(K). */

/*<             KP = -IPIV( K ) >*/
            kp = -ipiv[k];
/*<    >*/
            if (kp != k + 1) {
                dswap_(nrhs, &b[k + 1 + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }

/*           Multiply by inv(L(K)), where L(K) is the transformation */
/*           stored in columns K and K+1 of A. */

/*<             IF( K.LT.N-1 ) THEN >*/
            if (k < *n - 1) {
/*<    >*/
                i__1 = *n - k - 1;
                dger_(&i__1, nrhs, &c_b7, &ap[kc + 2], &c__1, &b[k + b_dim1],
                        ldb, &b[k + 2 + b_dim1], ldb);
/*<    >*/
                i__1 = *n - k - 1;
                dger_(&i__1, nrhs, &c_b7, &ap[kc + *n - k + 2], &c__1, &b[k +
                        1 + b_dim1], ldb, &b[k + 2 + b_dim1], ldb);
/*<             END IF >*/
            }

/*           Multiply by the inverse of the diagonal block. */

/*<             AKM1K = AP( KC+1 ) >*/
            akm1k = ap[kc + 1];
/*<             AKM1 = AP( KC ) / AKM1K >*/
            akm1 = ap[kc] / akm1k;
/*<             AK = AP( KC+N-K+1 ) / AKM1K >*/
            ak = ap[kc + *n - k + 1] / akm1k;
/*<             DENOM = AKM1*AK - ONE >*/
            denom = akm1 * ak - 1.;
/*<             DO 70 J = 1, NRHS >*/
            i__1 = *nrhs;
            for (j = 1; j <= i__1; ++j) {
/*<                BKM1 = B( K, J ) / AKM1K >*/
                bkm1 = b[k + j * b_dim1] / akm1k;
/*<                BK = B( K+1, J ) / AKM1K >*/
                bk = b[k + 1 + j * b_dim1] / akm1k;
/*<                B( K, J ) = ( AK*BKM1-BK ) / DENOM >*/
                b[k + j * b_dim1] = (ak * bkm1 - bk) / denom;
/*<                B( K+1, J ) = ( AKM1*BK-BKM1 ) / DENOM >*/
                b[k + 1 + j * b_dim1] = (akm1 * bk - bkm1) / denom;
/*<    70       CONTINUE >*/
/* L70: */
            }
/*<             KC = KC + 2*( N-K ) + 1 >*/
            kc += ((*n - k) << 1) + 1;
/*<             K = K + 2 >*/
            k += 2;
/*<          END IF >*/
        }

/*<          GO TO 60 >*/
        goto L60;
/*<    80    CONTINUE >*/
L80:

/*        Next solve L'*X = B, overwriting B with X. */

/*        K is the main loop index, decreasing from N to 1 in steps of */
/*        1 or 2, depending on the size of the diagonal blocks. */

/*<          K = N >*/
        k = *n;
/*<          KC = N*( N+1 ) / 2 + 1 >*/
        kc = *n * (*n + 1) / 2 + 1;
/*<    90    CONTINUE >*/
L90:

/*        If K < 1, exit from loop. */

/*<    >*/
        if (k < 1) {
            goto L100;
        }

/*<          KC = KC - ( N-K+1 ) >*/
        kc -= *n - k + 1;
/*<          IF( IPIV( K ).GT.0 ) THEN >*/
        if (ipiv[k] > 0) {

/*           1 x 1 diagonal block */

/*           Multiply by inv(L'(K)), where L(K) is the transformation */
/*           stored in column K of A. */

/*<    >*/
            if (k < *n) {
                i__1 = *n - k;
                dgemv_("Transpose", &i__1, nrhs, &c_b7, &b[k + 1 + b_dim1],
                        ldb, &ap[kc + 1], &c__1, &c_b19, &b[k + b_dim1], ldb,
                        (ftnlen)9);
            }

/*           Interchange rows K and IPIV(K). */

/*<             KP = IPIV( K ) >*/
            kp = ipiv[k];
/*<    >*/
            if (kp != k) {
                dswap_(nrhs, &b[k + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }
/*<             K = K - 1 >*/
            --k;
/*<          ELSE >*/
        } else {

/*           2 x 2 diagonal block */

/*           Multiply by inv(L'(K-1)), where L(K-1) is the transformation */
/*           stored in columns K-1 and K of A. */

/*<             IF( K.LT.N ) THEN >*/
            if (k < *n) {
/*<    >*/
                i__1 = *n - k;
                dgemv_("Transpose", &i__1, nrhs, &c_b7, &b[k + 1 + b_dim1],
                        ldb, &ap[kc + 1], &c__1, &c_b19, &b[k + b_dim1], ldb,
                        (ftnlen)9);
/*<    >*/
                i__1 = *n - k;
                dgemv_("Transpose", &i__1, nrhs, &c_b7, &b[k + 1 + b_dim1],
                        ldb, &ap[kc - (*n - k)], &c__1, &c_b19, &b[k - 1 +
                        b_dim1], ldb, (ftnlen)9);
/*<             END IF >*/
            }

/*           Interchange rows K and -IPIV(K). */

/*<             KP = -IPIV( K ) >*/
            kp = -ipiv[k];
/*<    >*/
            if (kp != k) {
                dswap_(nrhs, &b[k + b_dim1], ldb, &b[kp + b_dim1], ldb);
            }
/*<             KC = KC - ( N-K+2 ) >*/
            kc -= *n - k + 2;
/*<             K = K - 2 >*/
            k += -2;
/*<          END IF >*/
        }

/*<          GO TO 90 >*/
        goto L90;
/*<   100    CONTINUE >*/
L100:
/*<       END IF >*/
        ;
    }

/*<       RETURN >*/
    return 0;

/*     End of DSPTRS */

/*<       END >*/
} /* dsptrs_ */

#ifdef __cplusplus
        }
#endif
