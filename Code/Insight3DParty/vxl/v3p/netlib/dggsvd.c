/* dggsvd.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/*<    >*/
/* Subroutine */ int dggsvd_(char *jobu, char *jobv, char *jobq, integer *m, 
	integer *n, integer *p, integer *k, integer *l, doublereal *a, 
	integer *lda, doublereal *b, integer *ldb, doublereal *alpha, 
	doublereal *beta, doublereal *u, integer *ldu, doublereal *v, integer 
	*ldv, doublereal *q, integer *ldq, doublereal *work, integer *iwork, 
	integer *info, ftnlen jobu_len, ftnlen jobv_len, ftnlen jobq_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, u_dim1, 
	    u_offset, v_dim1, v_offset, i__1;

    /* Local variables */
    static doublereal tola, tolb, unfl;
    extern logical lsame_(char *, char *, ftnlen, ftnlen);
    static doublereal anorm, bnorm;
    static logical wantq, wantu, wantv;
    extern doublereal dlamch_(char *, ftnlen), dlange_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, ftnlen);
    extern /* Subroutine */ int dtgsja_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);
    static integer ncycle;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), dggsvp_(
	    char *, char *, char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, ftnlen, ftnlen, ftnlen);
    static doublereal ulp;


/*  -- LAPACK driver routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

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

/*  DGGSVD computes the generalized singular value decomposition (GSVD) */
/*  of an M-by-N real matrix A and P-by-N real matrix B: */

/*      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R ) */

/*  where U, V and Q are orthogonal matrices, and Z' is the transpose */
/*  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')', 
*/
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

/*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of 
*/
/*  A and B implicitly gives the SVD of A*inv(B): */
/*                       A*inv(B) = U*(D1*inv(D2))*V'. */
/*  If ( A',B')' has orthonormal columns, then the GSVD of A and B is */
/*  also equal to the CS decomposition of A and B. Furthermore, the GSVD 
*/
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

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, A contains the triangular matrix R, or part of R. */
/*          See Purpose for details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) */
/*          On entry, the P-by-N matrix B. */
/*          On exit, B contains the triangular matrix R if M-K-L < 0. */
/*          See Purpose for details. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDA >= max(1,P). */

/*  ALPHA   (output) DOUBLE PRECISION array, dimension (N) */
/*  BETA    (output) DOUBLE PRECISION array, dimension (N) */
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

/*  U       (output) DOUBLE PRECISION array, dimension (LDU,M) */
/*          If JOBU = 'U', U contains the M-by-M orthogonal matrix U. */
/*          If JOBU = 'N', U is not referenced. */

/*  LDU     (input) INTEGER */
/*          The leading dimension of the array U. LDU >= max(1,M) if */
/*          JOBU = 'U'; LDU >= 1 otherwise. */

/*  V       (output) DOUBLE PRECISION array, dimension (LDV,P) */
/*          If JOBV = 'V', V contains the P-by-P orthogonal matrix V. */
/*          If JOBV = 'N', V is not referenced. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. LDV >= max(1,P) if */
/*          JOBV = 'V'; LDV >= 1 otherwise. */

/*  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N) */
/*          If JOBQ = 'Q', Q contains the N-by-N orthogonal matrix Q. */
/*          If JOBQ = 'N', Q is not referenced. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. LDQ >= max(1,N) if */
/*          JOBQ = 'Q'; LDQ >= 1 otherwise. */

/*  WORK    (workspace) DOUBLE PRECISION array, */
/*                      dimension (max(3*N,M,P)+N) */

/*  IWORK   (workspace) INTEGER array, dimension (N) */

/*  INFO    (output)INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          > 0:  if INFO = 1, the Jacobi-type procedure failed to */
/*                converge.  For further details, see subroutine DTGSJA. 
*/

/*  Internal Parameters */
/*  =================== */

/*  TOLA    DOUBLE PRECISION */
/*  TOLB    DOUBLE PRECISION */
/*          TOLA and TOLB are the thresholds to determine the effective */
/*          rank of (A',B')'. Generally, they are set to */
/*                   TOLA = MAX(M,N)*norm(A)*MAZHEPS, */
/*                   TOLB = MAX(P,N)*norm(B)*MAZHEPS. */
/*          The size of TOLA and TOLB may affect the size of backward */
/*          errors of the decomposition. */

/*  ===================================================================== 
*/

/*     .. Local Scalars .. */
/*<       LOGICAL            WANTQ, WANTU, WANTV >*/
/*<       INTEGER            NCYCLE >*/
/*<       DOUBLE PRECISION   ANORM, BNORM, TOLA, TOLB, ULP, UNFL >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       DOUBLE PRECISION   DLAMCH, DLANGE >*/
/*<       EXTERNAL           LSAME, DLAMCH, DLANGE >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DGGSVP, DTGSJA, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       WANTU = LSAME( JOBU, 'U' ) >*/
    /* Parameter adjustments */
    --iwork;
    --work;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --beta;
    --alpha;
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
/*<          CALL XERBLA( 'DGGSVD', -INFO ) >*/
	i__1 = -(*info);
	xerbla_("DGGSVD", &i__1, 6L);
/*<          RETURN >*/
	return 0;
/*<       END IF >*/
    }

/*     Compute the Frobenius norm of matrices A and B */

/*<       ANORM = DLANGE( '1', M, N, A, LDA, WORK ) >*/
    anorm = dlange_("1", m, n, &a[a_offset], lda, &work[1], 1L);
/*<       BNORM = DLANGE( '1', P, N, B, LDB, WORK ) >*/
    bnorm = dlange_("1", p, n, &b[b_offset], ldb, &work[1], 1L);

/*     Get machine precision and set up threshold for determining */
/*     the effective numerical rank of the matrices A and B. */

/*<       ULP = DLAMCH( 'Precision' ) >*/
    ulp = dlamch_("Precision", 9L);
/*<       UNFL = DLAMCH( 'Safe Minimum' ) >*/
    unfl = dlamch_("Safe Minimum", 12L);
/*<       TOLA = MAX( M, N )*MAX( ANORM, UNFL )*ULP >*/
    tola = max(*m,*n) * max(anorm,unfl) * ulp;
/*<       TOLB = MAX( P, N )*MAX( BNORM, UNFL )*ULP >*/
    tolb = max(*p,*n) * max(bnorm,unfl) * ulp;

/*     Preprocessing */

/*<    >*/
    dggsvp_(jobu, jobv, jobq, m, p, n, &a[a_offset], lda, &b[b_offset], ldb, &
	    tola, &tolb, k, l, &u[u_offset], ldu, &v[v_offset], ldv, &q[
	    q_offset], ldq, &iwork[1], &work[1], &work[*n + 1], info, 1L, 1L, 
	    1L);

/*     Compute the GSVD of two upper "triangular" matrices */

/*<    >*/
    dtgsja_(jobu, jobv, jobq, m, p, n, k, l, &a[a_offset], lda, &b[b_offset], 
	    ldb, &tola, &tolb, &alpha[1], &beta[1], &u[u_offset], ldu, &v[
	    v_offset], ldv, &q[q_offset], ldq, &work[1], &ncycle, info, 1L, 
	    1L, 1L);

/*<       RETURN >*/
    return 0;

/*     End of DGGSVD */

/*<       END >*/
} /* dggsvd_ */

