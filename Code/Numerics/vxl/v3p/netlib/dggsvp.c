/* dggsvp.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b12 = 0.;
static doublereal c_b22 = 1.;

/*<    >*/
/* Subroutine */ int dggsvp_(char *jobu, char *jobv, char *jobq, integer *m,
        integer *p, integer *n, doublereal *a, integer *lda, doublereal *b,
        integer *ldb, doublereal *tola, doublereal *tolb, integer *k, integer
        *l, doublereal *u, integer *ldu, doublereal *v, integer *ldv,
        doublereal *q, integer *ldq, integer *iwork, doublereal *tau,
        doublereal *work, integer *info, ftnlen jobu_len, ftnlen jobv_len,
        ftnlen jobq_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, u_dim1,
            u_offset, v_dim1, v_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer i, j;
    extern logical lsame_(char *, char *, ftnlen, ftnlen);
    static logical wantq, wantu, wantv;
    extern /* Subroutine */ int dgeqr2_(integer *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, integer *), dgerq2_(
            integer *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, integer *), dorg2r_(integer *, integer *, integer *,
             doublereal *, integer *, doublereal *, doublereal *, integer *),
            dorm2r_(char *, char *, integer *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *,
            doublereal *, integer *, ftnlen, ftnlen), dormr2_(char *, char *,
            integer *, integer *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, doublereal *, integer *,
            ftnlen, ftnlen), dgeqpf_(integer *, integer *, doublereal *,
            integer *, integer *, doublereal *, doublereal *, integer *),
            dlacpy_(char *, integer *, integer *, doublereal *, integer *,
            doublereal *, integer *, ftnlen), dlaset_(char *, integer *,
            integer *, doublereal *, doublereal *, doublereal *, integer *,
            ftnlen), xerbla_(char *, integer *, ftnlen), dlapmt_(logical *,
            integer *, integer *, doublereal *, integer *, integer *);
    static logical forwrd;


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBQ, JOBU, JOBV >*/
/*<       INTEGER            INFO, K, L, LDA, LDB, LDQ, LDU, LDV, M, N, P >*/
/*<       DOUBLE PRECISION   TOLA, TOLB >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGGSVP computes orthogonal matrices U, V and Q such that */

/*                   N-K-L  K    L */
/*   U'*A*Q =     K ( 0    A12  A13 )  if M-K-L >= 0; */
/*                L ( 0     0   A23 ) */
/*            M-K-L ( 0     0    0  ) */

/*                   N-K-L  K    L */
/*          =     K ( 0    A12  A13 )  if M-K-L < 0; */
/*              M-K ( 0     0   A23 ) */

/*                 N-K-L  K    L */
/*   V'*B*Q =   L ( 0     0   B13 ) */
/*            P-L ( 0     0    0  ) */

/*  where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular */
/*  upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0, */
/*  otherwise A23 is (M-K)-by-L upper trapezoidal.  K+L = the effective */
/*  numerical rank of the (M+P)-by-N matrix (A',B')'.  Z' denotes the */
/*  transpose of Z. */

/*  This decomposition is the preprocessing step for computing the */
/*  Generalized Singular Value Decomposition (GSVD), see subroutine */
/*  DGGSVD. */

/*  Arguments */
/*  ========= */

/*  JOBU    (input) CHARACTER*1 */
/*          = 'U':  Orthogonal matrix U is computed; */
/*          = 'N':  U is not computed. */

/*  JOBV    (input) CHARACTER*1 */
/*          = 'V':  Orthogonal matrix V is computed; */
/*          = 'N':  V is not computed. */

/*  JOBQ    (input) CHARACTER*1 */
/*          = 'Q':  Orthogonal matrix Q is computed; */
/*          = 'N':  Q is not computed. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  P       (input) INTEGER */
/*          The number of rows of the matrix B.  P >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrices A and B.  N >= 0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, A contains the triangular (or trapezoidal) matrix */
/*          described in the Purpose section. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) */
/*          On entry, the P-by-N matrix B. */
/*          On exit, B contains the triangular matrix described in */
/*          the Purpose section. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDB >= max(1,P). */

/*  TOLA    (input) DOUBLE PRECISION */
/*  TOLB    (input) DOUBLE PRECISION */
/*          TOLA and TOLB are the thresholds to determine the effective */
/*          numerical rank of matrix B and a subblock of A. Generally, */
/*          they are set to */
/*             TOLA = MAX(M,N)*norm(A)*MAZHEPS, */
/*             TOLB = MAX(P,N)*norm(B)*MAZHEPS. */
/*          The size of TOLA and TOLB may affect the size of backward */
/*          errors of the decomposition. */

/*  K       (output) INTEGER */
/*  L       (output) INTEGER */
/*          On exit, K and L specify the dimension of the subblocks */
/*          described in Purpose. */
/*          K + L = effective numerical rank of (A',B')'. */

/*  U       (output) DOUBLE PRECISION array, dimension (LDU,M) */
/*          If JOBU = 'U', U contains the orthogonal matrix U. */
/*          If JOBU = 'N', U is not referenced. */

/*  LDU     (input) INTEGER */
/*          The leading dimension of the array U. LDU >= max(1,M) if */
/*          JOBU = 'U'; LDU >= 1 otherwise. */

/*  V       (output) DOUBLE PRECISION array, dimension (LDV,M) */
/*          If JOBV = 'V', V contains the orthogonal matrix V. */
/*          If JOBV = 'N', V is not referenced. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. LDV >= max(1,P) if */
/*          JOBV = 'V'; LDV >= 1 otherwise. */

/*  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N) */
/*          If JOBQ = 'Q', Q contains the orthogonal matrix Q. */
/*          If JOBQ = 'N', Q is not referenced. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. LDQ >= max(1,N) if */
/*          JOBQ = 'Q'; LDQ >= 1 otherwise. */

/*  IWORK   (workspace) INTEGER array, dimension (N) */

/*  TAU     (workspace) DOUBLE PRECISION array, dimension (N) */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(3*N,M,P))
*/

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */


/*  Further Details */
/*  =============== */

/*  The subroutine uses LAPACK subroutine DGEQPF for the QR factorization
*/
/*  with column pivoting to detect the effective numerical rank of the */
/*  a matrix. It may be replaced by a better rank determination strategy.
*/

/*  =====================================================================
*/

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            FORWRD, WANTQ, WANTU, WANTV >*/
/*<       INTEGER            I, J >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       WANTU = LSAME( JOBU, 'U' ) >*/
    /* Parameter adjustments */
    --work;
    --tau;
    --iwork;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    wantu = lsame_(jobu, "U", 1L, 1L);
/*<       WANTV = LSAME( JOBV, 'V' ) >*/
    wantv = lsame_(jobv, "V", 1L, 1L);
/*<       WANTQ = LSAME( JOBQ, 'Q' ) >*/
    wantq = lsame_(jobq, "Q", 1L, 1L);
/*<       FORWRD = .TRUE. >*/
    forwrd = TRUE_;

/*<       INFO = 0 >*/
    *info = 0;
/*<       IF( .NOT.( WANTU .OR. LSAME( JOBU, 'N' ) ) ) THEN >*/
    if (! (wantu || lsame_(jobu, "N", 1L, 1L))) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.( WANTV .OR. LSAME( JOBV, 'N' ) ) ) THEN >*/
    } else if (! (wantv || lsame_(jobv, "N", 1L, 1L))) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( .NOT.( WANTQ .OR. LSAME( JOBQ, 'N' ) ) ) THEN >*/
    } else if (! (wantq || lsame_(jobq, "N", 1L, 1L))) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( P.LT.0 ) THEN >*/
    } else if (*p < 0) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       ELSE IF( LDB.LT.MAX( 1, P ) ) THEN >*/
    } else if (*ldb < max(1,*p)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LDU.LT.1 .OR. ( WANTU .AND. LDU.LT.M ) ) THEN >*/
    } else if (*ldu < 1 || wantu && *ldu < *m) {
/*<          INFO = -16 >*/
        *info = -16;
/*<       ELSE IF( LDV.LT.1 .OR. ( WANTV .AND. LDV.LT.P ) ) THEN >*/
    } else if (*ldv < 1 || wantv && *ldv < *p) {
/*<          INFO = -18 >*/
        *info = -18;
/*<       ELSE IF( LDQ.LT.1 .OR. ( WANTQ .AND. LDQ.LT.N ) ) THEN >*/
    } else if (*ldq < 1 || wantq && *ldq < *n) {
/*<          INFO = -20 >*/
        *info = -20;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DGGSVP', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DGGSVP", &i__1, 6L);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     QR with column pivoting of B: B*P = V*( S11 S12 ) */
/*                                           (  0   0  ) */

/*<       DO 10 I = 1, N >*/
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/*<          IWORK( I ) = 0 >*/
        iwork[i] = 0;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       CALL DGEQPF( P, N, B, LDB, IWORK, TAU, WORK, INFO ) >*/
    dgeqpf_(p, n, &b[b_offset], ldb, &iwork[1], &tau[1], &work[1], info);

/*     Update A := A*P */

/*<       CALL DLAPMT( FORWRD, M, N, A, LDA, IWORK ) >*/
    dlapmt_(&forwrd, m, n, &a[a_offset], lda, &iwork[1]);

/*     Determine the effective rank of matrix B. */

/*<       L = 0 >*/
    *l = 0;
/*<       DO 20 I = 1, MIN( P, N ) >*/
    i__1 = min(*p,*n);
    for (i = 1; i <= i__1; ++i) {
/*<    >*/
        if ((d__1 = b[i + i * b_dim1], abs(d__1)) > *tolb) {
            ++(*l);
        }
/*<    20 CONTINUE >*/
/* L20: */
    }

/*<       IF( WANTV ) THEN >*/
    if (wantv) {

/*        Copy the details of V, and form V. */

/*<          CALL DLASET( 'Full', P, P, ZERO, ZERO, V, LDV ) >*/
        dlaset_("Full", p, p, &c_b12, &c_b12, &v[v_offset], ldv, 4L);
/*<    >*/
        if (*p > 1) {
            i__1 = *p - 1;
            dlacpy_("Lower", &i__1, n, &b[b_dim1 + 2], ldb, &v[v_dim1 + 2],
                    ldv, 5L);
        }
/*<          CALL DORG2R( P, P, MIN( P, N ), V, LDV, TAU, WORK, INFO ) >*/
        i__1 = min(*p,*n);
        dorg2r_(p, p, &i__1, &v[v_offset], ldv, &tau[1], &work[1], info);
/*<       END IF >*/
    }

/*     Clean up B */

/*<       DO 40 J = 1, L - 1 >*/
    i__1 = *l - 1;
    for (j = 1; j <= i__1; ++j) {
/*<          DO 30 I = J + 1, L >*/
        i__2 = *l;
        for (i = j + 1; i <= i__2; ++i) {
/*<             B( I, J ) = ZERO >*/
            b[i + j * b_dim1] = 0.;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<    >*/
    if (*p > *l) {
        i__1 = *p - *l;
        dlaset_("Full", &i__1, n, &c_b12, &c_b12, &b[*l + 1 + b_dim1], ldb,
                4L);
    }

/*<       IF( WANTQ ) THEN >*/
    if (wantq) {

/*        Set Q = I and Update Q := Q*P */

/*<          CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ ) >*/
        dlaset_("Full", n, n, &c_b12, &c_b22, &q[q_offset], ldq, 4L);
/*<          CALL DLAPMT( FORWRD, N, N, Q, LDQ, IWORK ) >*/
        dlapmt_(&forwrd, n, n, &q[q_offset], ldq, &iwork[1]);
/*<       END IF >*/
    }

/*<       IF( P.GE.L .AND. N.NE.L ) THEN >*/
    if (*p >= *l && *n != *l) {

/*        RQ factorization of (S11 S12): ( S11 S12 ) = ( 0 S12 )*Z */

/*<          CALL DGERQ2( L, N, B, LDB, TAU, WORK, INFO ) >*/
        dgerq2_(l, n, &b[b_offset], ldb, &tau[1], &work[1], info);

/*        Update A := A*Z' */

/*<    >*/
        dormr2_("Right", "Transpose", m, n, l, &b[b_offset], ldb, &tau[1], &a[
                a_offset], lda, &work[1], info, 5L, 9L);

/*<          IF( WANTQ ) THEN >*/
        if (wantq) {

/*           Update Q := Q*Z' */

/*<    >*/
            dormr2_("Right", "Transpose", n, n, l, &b[b_offset], ldb, &tau[1],
                     &q[q_offset], ldq, &work[1], info, 5L, 9L);
/*<          END IF >*/
        }

/*        Clean up B */

/*<          CALL DLASET( 'Full', L, N-L, ZERO, ZERO, B, LDB ) >*/
        i__1 = *n - *l;
        dlaset_("Full", l, &i__1, &c_b12, &c_b12, &b[b_offset], ldb, 4L);
/*<          DO 60 J = N - L + 1, N >*/
        i__1 = *n;
        for (j = *n - *l + 1; j <= i__1; ++j) {
/*<             DO 50 I = J - N + L + 1, L >*/
            i__2 = *l;
            for (i = j - *n + *l + 1; i <= i__2; ++i) {
/*<                B( I, J ) = ZERO >*/
                b[i + j * b_dim1] = 0.;
/*<    50       CONTINUE >*/
/* L50: */
            }
/*<    60    CONTINUE >*/
/* L60: */
        }

/*<       END IF >*/
    }

/*     Let              N-L     L */
/*                A = ( A11    A12 ) M, */

/*     then the following does the complete QR decomposition of A11: */

/*              A11 = U*(  0  T12 )*P1' */
/*                      (  0   0  ) */

/*<       DO 70 I = 1, N - L >*/
    i__1 = *n - *l;
    for (i = 1; i <= i__1; ++i) {
/*<          IWORK( I ) = 0 >*/
        iwork[i] = 0;
/*<    70 CONTINUE >*/
/* L70: */
    }
/*<       CALL DGEQPF( M, N-L, A, LDA, IWORK, TAU, WORK, INFO ) >*/
    i__1 = *n - *l;
    dgeqpf_(m, &i__1, &a[a_offset], lda, &iwork[1], &tau[1], &work[1], info);

/*     Determine the effective rank of A11 */

/*<       K = 0 >*/
    *k = 0;
/*<       DO 80 I = 1, MIN( M, N-L ) >*/
/* Computing MIN */
    i__2 = *m, i__3 = *n - *l;
    i__1 = min(i__2,i__3);
    for (i = 1; i <= i__1; ++i) {
/*<    >*/
        if ((d__1 = a[i + i * a_dim1], abs(d__1)) > *tola) {
            ++(*k);
        }
/*<    80 CONTINUE >*/
/* L80: */
    }

/*     Update A12 := U'*A12, where A12 = A( 1:M, N-L+1:N ) */

/*<    >*/
/* Computing MIN */
    i__2 = *m, i__3 = *n - *l;
    i__1 = min(i__2,i__3);
    dorm2r_("Left", "Transpose", m, l, &i__1, &a[a_offset], lda, &tau[1], &a[(
            *n - *l + 1) * a_dim1 + 1], lda, &work[1], info, 4L, 9L);

/*<       IF( WANTU ) THEN >*/
    if (wantu) {

/*        Copy the details of U, and form U */

/*<          CALL DLASET( 'Full', M, M, ZERO, ZERO, U, LDU ) >*/
        dlaset_("Full", m, m, &c_b12, &c_b12, &u[u_offset], ldu, 4L);
/*<    >*/
        if (*m > 1) {
            i__1 = *m - 1;
            i__2 = *n - *l;
            dlacpy_("Lower", &i__1, &i__2, &a[a_dim1 + 2], lda, &u[u_dim1 + 2]
                    , ldu, 5L);
        }
/*<          CALL DORG2R( M, M, MIN( M, N-L ), U, LDU, TAU, WORK, INFO ) >*/
/* Computing MIN */
        i__2 = *m, i__3 = *n - *l;
        i__1 = min(i__2,i__3);
        dorg2r_(m, m, &i__1, &u[u_offset], ldu, &tau[1], &work[1], info);
/*<       END IF >*/
    }

/*<       IF( WANTQ ) THEN >*/
    if (wantq) {

/*        Update Q( 1:N, 1:N-L )  = Q( 1:N, 1:N-L )*P1 */

/*<          CALL DLAPMT( FORWRD, N, N-L, Q, LDQ, IWORK ) >*/
        i__1 = *n - *l;
        dlapmt_(&forwrd, n, &i__1, &q[q_offset], ldq, &iwork[1]);
/*<       END IF >*/
    }

/*     Clean up A: set the strictly lower triangular part of */
/*     A(1:K, 1:K) = 0, and A( K+1:M, 1:N-L ) = 0. */

/*<       DO 100 J = 1, K - 1 >*/
    i__1 = *k - 1;
    for (j = 1; j <= i__1; ++j) {
/*<          DO 90 I = J + 1, K >*/
        i__2 = *k;
        for (i = j + 1; i <= i__2; ++i) {
/*<             A( I, J ) = ZERO >*/
            a[i + j * a_dim1] = 0.;
/*<    90    CONTINUE >*/
/* L90: */
        }
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<    >*/
    if (*m > *k) {
        i__1 = *m - *k;
        i__2 = *n - *l;
        dlaset_("Full", &i__1, &i__2, &c_b12, &c_b12, &a[*k + 1 + a_dim1],
                lda, 4L);
    }

/*<       IF( N-L.GT.K ) THEN >*/
    if (*n - *l > *k) {

/*        RQ factorization of ( T11 T12 ) = ( 0 T12 )*Z1 */

/*<          CALL DGERQ2( K, N-L, A, LDA, TAU, WORK, INFO ) >*/
        i__1 = *n - *l;
        dgerq2_(k, &i__1, &a[a_offset], lda, &tau[1], &work[1], info);

/*<          IF( WANTQ ) THEN >*/
        if (wantq) {

/*           Update Q( 1:N,1:N-L ) = Q( 1:N,1:N-L )*Z1' */

/*<    >*/
            i__1 = *n - *l;
            dormr2_("Right", "Transpose", n, &i__1, k, &a[a_offset], lda, &
                    tau[1], &q[q_offset], ldq, &work[1], info, 5L, 9L);
/*<          END IF >*/
        }

/*        Clean up A */

/*<          CALL DLASET( 'Full', K, N-L-K, ZERO, ZERO, A, LDA ) >*/
        i__1 = *n - *l - *k;
        dlaset_("Full", k, &i__1, &c_b12, &c_b12, &a[a_offset], lda, 4L);
/*<          DO 120 J = N - L - K + 1, N - L >*/
        i__1 = *n - *l;
        for (j = *n - *l - *k + 1; j <= i__1; ++j) {
/*<             DO 110 I = J - N + L + K + 1, K >*/
            i__2 = *k;
            for (i = j - *n + *l + *k + 1; i <= i__2; ++i) {
/*<                A( I, J ) = ZERO >*/
                a[i + j * a_dim1] = 0.;
/*<   110       CONTINUE >*/
/* L110: */
            }
/*<   120    CONTINUE >*/
/* L120: */
        }

/*<       END IF >*/
    }

/*<       IF( M.GT.K ) THEN >*/
    if (*m > *k) {

/*        QR factorization of A( K+1:M,N-L+1:N ) */

/*<          CALL DGEQR2( M-K, L, A( K+1, N-L+1 ), LDA, TAU, WORK, INFO ) >*/
        i__1 = *m - *k;
        dgeqr2_(&i__1, l, &a[*k + 1 + (*n - *l + 1) * a_dim1], lda, &tau[1], &
                work[1], info);

/*<          IF( WANTU ) THEN >*/
        if (wantu) {

/*           Update U(:,K+1:M) := U(:,K+1:M)*U1 */

/*<    >*/
            i__1 = *m - *k;
/* Computing MIN */
            i__3 = *m - *k;
            i__2 = min(i__3,*l);
            dorm2r_("Right", "No transpose", m, &i__1, &i__2, &a[*k + 1 + (*n
                    - *l + 1) * a_dim1], lda, &tau[1], &u[(*k + 1) * u_dim1 +
                    1], ldu, &work[1], info, 5L, 12L);
/*<          END IF >*/
        }

/*        Clean up */

/*<          DO 140 J = N - L + 1, N >*/
        i__1 = *n;
        for (j = *n - *l + 1; j <= i__1; ++j) {
/*<             DO 130 I = J - N + K + L + 1, M >*/
            i__2 = *m;
            for (i = j - *n + *k + *l + 1; i <= i__2; ++i) {
/*<                A( I, J ) = ZERO >*/
                a[i + j * a_dim1] = 0.;
/*<   130       CONTINUE >*/
/* L130: */
            }
/*<   140    CONTINUE >*/
/* L140: */
        }

/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DGGSVP */

/*<       END >*/
} /* dggsvp_ */

