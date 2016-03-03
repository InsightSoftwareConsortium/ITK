/* lapack/single/sggsvd.f -- translated by f2c (version 20050501).
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

/*<    >*/
/* Subroutine */ int sggsvd_(const char *jobu, const char *jobv, const char *jobq, integer *m,
        integer *n, integer *p, integer *k, integer *l, real *a, integer *lda,
         real *b, integer *ldb, real *alpha, real *beta, real *u, integer *
        ldu, real *v, integer *ldv, real *q, integer *ldq, real *work,
        integer *iwork, integer *info, ftnlen jobu_len, ftnlen jobv_len,
        ftnlen jobq_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, u_dim1,
            u_offset, v_dim1, v_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;
    real ulp;
    integer ibnd;
    real tola;
    integer isub;
    real tolb, unfl, temp, smax;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    real anorm, bnorm;
    logical wantq;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *,
            integer *);
    logical wantu, wantv;
    extern doublereal slamch_(char *, ftnlen), slange_(char *, integer *,
            integer *, real *, integer *, real *, ftnlen);
    integer ncycle;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), stgsja_(
            const char *, const char *, const char *, integer *, integer *, integer *, integer *
            , integer *, real *, integer *, real *, integer *, real *, real *,
             real *, real *, real *, integer *, real *, integer *, real *,
            integer *, real *, integer *, integer *, ftnlen, ftnlen, ftnlen),
            sggsvp_(const char *, const char *, const char *, integer *, integer *, integer *,
            real *, integer *, real *, integer *, real *, real *, integer *,
            integer *, real *, integer *, real *, integer *, real *, integer *
            , integer *, real *, real *, integer *, ftnlen, ftnlen, ftnlen);
    (void)jobu_len;
    (void)jobv_len;
    (void)jobq_len;

/*  -- LAPACK driver routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBQ, JOBU, JOBV >*/
/*<       INTEGER            INFO, K, L, LDA, LDB, LDQ, LDU, LDV, M, N, P >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SGGSVD computes the generalized singular value decomposition (GSVD) */
/*  of an M-by-N real matrix A and P-by-N real matrix B: */

/*      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R ) */

/*  where U, V and Q are orthogonal matrices, and Z' is the transpose */
/*  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')', */
/*  then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and */
/*  D2 are M-by-(K+L) and P-by-(K+L) "diagonal" matrices and of the */
/*  following structures, respectively: */

/*  If M-K-L >= 0, */

/*                      K  L */
/*         D1 =     K ( I  0 ) */
/*                  L ( 0  C ) */
/*              M-K-L ( 0  0 ) */

/*                    K  L */
/*         D2 =   L ( 0  S ) */
/*              P-L ( 0  0 ) */

/*                  N-K-L  K    L */
/*    ( 0 R ) = K (  0   R11  R12 ) */
/*              L (  0    0   R22 ) */

/*  where */

/*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ), */
/*    S = diag( BETA(K+1),  ... , BETA(K+L) ), */
/*    C**2 + S**2 = I. */

/*    R is stored in A(1:K+L,N-K-L+1:N) on exit. */

/*  If M-K-L < 0, */

/*                    K M-K K+L-M */
/*         D1 =   K ( I  0    0   ) */
/*              M-K ( 0  C    0   ) */

/*                      K M-K K+L-M */
/*         D2 =   M-K ( 0  S    0  ) */
/*              K+L-M ( 0  0    I  ) */
/*                P-L ( 0  0    0  ) */

/*                     N-K-L  K   M-K  K+L-M */
/*    ( 0 R ) =     K ( 0    R11  R12  R13  ) */
/*                M-K ( 0     0   R22  R23  ) */
/*              K+L-M ( 0     0    0   R33  ) */

/*  where */

/*    C = diag( ALPHA(K+1), ... , ALPHA(M) ), */
/*    S = diag( BETA(K+1),  ... , BETA(M) ), */
/*    C**2 + S**2 = I. */

/*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored */
/*    ( 0  R22 R23 ) */
/*    in B(M-K+1:L,N+M-K-L+1:N) on exit. */

/*  The routine computes C, S, R, and optionally the orthogonal */
/*  transformation matrices U, V and Q. */

/*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of */
/*  A and B implicitly gives the SVD of A*inv(B): */
/*                       A*inv(B) = U*(D1*inv(D2))*V'. */
/*  If ( A',B')' has orthonormal columns, then the GSVD of A and B is */
/*  also equal to the CS decomposition of A and B. Furthermore, the GSVD */
/*  can be used to derive the solution of the eigenvalue problem: */
/*                       A'*A x = lambda* B'*B x. */
/*  In some literature, the GSVD of A and B is presented in the form */
/*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 ) */
/*  where U and V are orthogonal and X is nonsingular, D1 and D2 are */
/*  ``diagonal''.  The former GSVD form can be converted to the latter */
/*  form by taking the nonsingular matrix X as */

/*                       X = Q*( I   0    ) */
/*                             ( 0 inv(R) ). */

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

/*  N       (input) INTEGER */
/*          The number of columns of the matrices A and B.  N >= 0. */

/*  P       (input) INTEGER */
/*          The number of rows of the matrix B.  P >= 0. */

/*  K       (output) INTEGER */
/*  L       (output) INTEGER */
/*          On exit, K and L specify the dimension of the subblocks */
/*          described in the Purpose section. */
/*          K + L = effective numerical rank of (A',B')'. */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, A contains the triangular matrix R, or part of R. */
/*          See Purpose for details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  B       (input/output) REAL array, dimension (LDB,N) */
/*          On entry, the P-by-N matrix B. */
/*          On exit, B contains the triangular matrix R if M-K-L < 0. */
/*          See Purpose for details. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDA >= max(1,P). */

/*  ALPHA   (output) REAL array, dimension (N) */
/*  BETA    (output) REAL array, dimension (N) */
/*          On exit, ALPHA and BETA contain the generalized singular */
/*          value pairs of A and B; */
/*            ALPHA(1:K) = 1, */
/*            BETA(1:K)  = 0, */
/*          and if M-K-L >= 0, */
/*            ALPHA(K+1:K+L) = C, */
/*            BETA(K+1:K+L)  = S, */
/*          or if M-K-L < 0, */
/*            ALPHA(K+1:M)=C, ALPHA(M+1:K+L)=0 */
/*            BETA(K+1:M) =S, BETA(M+1:K+L) =1 */
/*          and */
/*            ALPHA(K+L+1:N) = 0 */
/*            BETA(K+L+1:N)  = 0 */

/*  U       (output) REAL array, dimension (LDU,M) */
/*          If JOBU = 'U', U contains the M-by-M orthogonal matrix U. */
/*          If JOBU = 'N', U is not referenced. */

/*  LDU     (input) INTEGER */
/*          The leading dimension of the array U. LDU >= max(1,M) if */
/*          JOBU = 'U'; LDU >= 1 otherwise. */

/*  V       (output) REAL array, dimension (LDV,P) */
/*          If JOBV = 'V', V contains the P-by-P orthogonal matrix V. */
/*          If JOBV = 'N', V is not referenced. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. LDV >= max(1,P) if */
/*          JOBV = 'V'; LDV >= 1 otherwise. */

/*  Q       (output) REAL array, dimension (LDQ,N) */
/*          If JOBQ = 'Q', Q contains the N-by-N orthogonal matrix Q. */
/*          If JOBQ = 'N', Q is not referenced. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. LDQ >= max(1,N) if */
/*          JOBQ = 'Q'; LDQ >= 1 otherwise. */

/*  WORK    (workspace) REAL array, */
/*                      dimension (max(3*N,M,P)+N) */

/*  IWORK   (workspace/output) INTEGER array, dimension (N) */
/*          On exit, IWORK stores the sorting information. More */
/*          precisely, the following loop will sort ALPHA */
/*             for I = K+1, min(M,K+L) */
/*                 swap ALPHA(I) and ALPHA(IWORK(I)) */
/*             endfor */
/*          such that ALPHA(1) >= ALPHA(2) >= ... >= ALPHA(N). */

/*  INFO    (output)INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          > 0:  if INFO = 1, the Jacobi-type procedure failed to */
/*                converge.  For further details, see subroutine STGSJA. */

/*  Internal Parameters */
/*  =================== */

/*  TOLA    REAL */
/*  TOLB    REAL */
/*          TOLA and TOLB are the thresholds to determine the effective */
/*          rank of (A',B')'. Generally, they are set to */
/*                   TOLA = MAX(M,N)*norm(A)*MACHEPS, */
/*                   TOLB = MAX(P,N)*norm(B)*MACHEPS. */
/*          The size of TOLA and TOLB may affect the size of backward */
/*          errors of the decomposition. */

/*  Further Details */
/*  =============== */

/*  2-96 Based on modifications by */
/*     Ming Gu and Huan Ren, Computer Science Division, University of */
/*     California at Berkeley, USA */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       LOGICAL            WANTQ, WANTU, WANTV >*/
/*<       INTEGER            I, IBND, ISUB, J, NCYCLE >*/
/*<       REAL               ANORM, BNORM, SMAX, TEMP, TOLA, TOLB, ULP, UNFL >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       REAL               SLAMCH, SLANGE >*/
/*<       EXTERNAL           LSAME, SLAMCH, SLANGE >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SCOPY, SGGSVP, STGSJA, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       WANTU = LSAME( JOBU, 'U' ) >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    --alpha;
    --beta;
    u_dim1 = *ldu;
    u_offset = 1 + u_dim1;
    u -= u_offset;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    --work;
    --iwork;

    /* Function Body */
    wantu = lsame_(jobu, "U", (ftnlen)1, (ftnlen)1);
/*<       WANTV = LSAME( JOBV, 'V' ) >*/
    wantv = lsame_(jobv, "V", (ftnlen)1, (ftnlen)1);
/*<       WANTQ = LSAME( JOBQ, 'Q' ) >*/
    wantq = lsame_(jobq, "Q", (ftnlen)1, (ftnlen)1);

/*<       INFO = 0 >*/
    *info = 0;
/*<       IF( .NOT.( WANTU .OR. LSAME( JOBU, 'N' ) ) ) THEN >*/
    if (! (wantu || lsame_(jobu, "N", (ftnlen)1, (ftnlen)1))) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.( WANTV .OR. LSAME( JOBV, 'N' ) ) ) THEN >*/
    } else if (! (wantv || lsame_(jobv, "N", (ftnlen)1, (ftnlen)1))) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( .NOT.( WANTQ .OR. LSAME( JOBQ, 'N' ) ) ) THEN >*/
    } else if (! (wantq || lsame_(jobq, "N", (ftnlen)1, (ftnlen)1))) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( P.LT.0 ) THEN >*/
    } else if (*p < 0) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LDB.LT.MAX( 1, P ) ) THEN >*/
    } else if (*ldb < max(1,*p)) {
/*<          INFO = -12 >*/
        *info = -12;
/*<       ELSE IF( LDU.LT.1 .OR. ( WANTU .AND. LDU.LT.M ) ) THEN >*/
    } else if (*ldu < 1 || (wantu && *ldu < *m)) {
/*<          INFO = -16 >*/
        *info = -16;
/*<       ELSE IF( LDV.LT.1 .OR. ( WANTV .AND. LDV.LT.P ) ) THEN >*/
    } else if (*ldv < 1 || (wantv && *ldv < *p)) {
/*<          INFO = -18 >*/
        *info = -18;
/*<       ELSE IF( LDQ.LT.1 .OR. ( WANTQ .AND. LDQ.LT.N ) ) THEN >*/
    } else if (*ldq < 1 || (wantq && *ldq < *n)) {
/*<          INFO = -20 >*/
        *info = -20;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'SGGSVD', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("SGGSVD", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Compute the Frobenius norm of matrices A and B */

/*<       ANORM = SLANGE( '1', M, N, A, LDA, WORK ) >*/
    anorm = slange_("1", m, n, &a[a_offset], lda, &work[1], (ftnlen)1);
/*<       BNORM = SLANGE( '1', P, N, B, LDB, WORK ) >*/
    bnorm = slange_("1", p, n, &b[b_offset], ldb, &work[1], (ftnlen)1);

/*     Get machine precision and set up threshold for determining */
/*     the effective numerical rank of the matrices A and B. */

/*<       ULP = SLAMCH( 'Precision' ) >*/
    ulp = slamch_("Precision", (ftnlen)9);
/*<       UNFL = SLAMCH( 'Safe Minimum' ) >*/
    unfl = slamch_("Safe Minimum", (ftnlen)12);
/*<       TOLA = MAX( M, N )*MAX( ANORM, UNFL )*ULP >*/
    tola = max(*m,*n) * dmax(anorm,unfl) * ulp;
/*<       TOLB = MAX( P, N )*MAX( BNORM, UNFL )*ULP >*/
    tolb = max(*p,*n) * dmax(bnorm,unfl) * ulp;

/*     Preprocessing */

/*<    >*/
    sggsvp_(jobu, jobv, jobq, m, p, n, &a[a_offset], lda, &b[b_offset], ldb, &
            tola, &tolb, k, l, &u[u_offset], ldu, &v[v_offset], ldv, &q[
            q_offset], ldq, &iwork[1], &work[1], &work[*n + 1], info, (ftnlen)
            1, (ftnlen)1, (ftnlen)1);

/*     Compute the GSVD of two upper "triangular" matrices */

/*<    >*/
    stgsja_(jobu, jobv, jobq, m, p, n, k, l, &a[a_offset], lda, &b[b_offset],
            ldb, &tola, &tolb, &alpha[1], &beta[1], &u[u_offset], ldu, &v[
            v_offset], ldv, &q[q_offset], ldq, &work[1], &ncycle, info, (
            ftnlen)1, (ftnlen)1, (ftnlen)1);

/*     Sort the singular values and store the pivot indices in IWORK */
/*     Copy ALPHA to WORK, then sort ALPHA in WORK */

/*<       CALL SCOPY( N, ALPHA, 1, WORK, 1 ) >*/
    scopy_(n, &alpha[1], &c__1, &work[1], &c__1);
/*<       IBND = MIN( L, M-K ) >*/
/* Computing MIN */
    i__1 = *l, i__2 = *m - *k;
    ibnd = min(i__1,i__2);
/*<       DO 20 I = 1, IBND >*/
    i__1 = ibnd;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Scan for largest ALPHA(K+I) */

/*<          ISUB = I >*/
        isub = i__;
/*<          SMAX = WORK( K+I ) >*/
        smax = work[*k + i__];
/*<          DO 10 J = I + 1, IBND >*/
        i__2 = ibnd;
        for (j = i__ + 1; j <= i__2; ++j) {
/*<             TEMP = WORK( K+J ) >*/
            temp = work[*k + j];
/*<             IF( TEMP.GT.SMAX ) THEN >*/
            if (temp > smax) {
/*<                ISUB = J >*/
                isub = j;
/*<                SMAX = TEMP >*/
                smax = temp;
/*<             END IF >*/
            }
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          IF( ISUB.NE.I ) THEN >*/
        if (isub != i__) {
/*<             WORK( K+ISUB ) = WORK( K+I ) >*/
            work[*k + isub] = work[*k + i__];
/*<             WORK( K+I ) = SMAX >*/
            work[*k + i__] = smax;
/*<             IWORK( K+I ) = K + ISUB >*/
            iwork[*k + i__] = *k + isub;
/*<          ELSE >*/
        } else {
/*<             IWORK( K+I ) = K + I >*/
            iwork[*k + i__] = *k + i__;
/*<          END IF >*/
        }
/*<    20 CONTINUE >*/
/* L20: */
    }

/*<       RETURN >*/
    return 0;

/*     End of SGGSVD */

/*<       END >*/
} /* sggsvd_ */

#ifdef __cplusplus
        }
#endif
