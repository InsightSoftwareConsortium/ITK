#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void sggsvd_(char *jobu, char *jobv, char *jobq, integer *m,
         integer *n, integer *p, integer *k, integer *l, real *a, integer *lda,
         real *b, integer *ldb, real *alpha, real *beta, real *u, integer *ldu,
         real *v, integer *ldv, real *q, integer *ldq,
         real *work, integer *iwork, integer *info)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static real tola, tolb, unfl;
    static real anorm, bnorm;
    static logical wantq, wantu, wantv;
    static integer ncycle;
    static real ulp;

/*  -- LAPACK driver routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  SGGSVD computes the generalized singular value decomposition (GSVD)   */
/*  of an M-by-N real matrix A and P-by-N real matrix B:                  */
/*                                                                        */
/*      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )                       */
/*                                                                        */
/*  where U, V and Q are orthogonal matrices, and Z' is the transpose     */
/*  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')', */
/*  then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and    */
/*  D2 are M-by-(K+L) and P-by-(K+L) "diagonal" matrices and of the       */
/*  following structures, respectively:                                   */
/*                                                                        */
/*  If M-K-L >= 0,                                                        */
/*                                                                        */
/*                      K  L                                              */
/*         D1 =     K ( I  0 )                                            */
/*                  L ( 0  C )                                            */
/*              M-K-L ( 0  0 )                                            */
/*                                                                        */
/*                    K  L                                                */
/*         D2 =   L ( 0  S )                                              */
/*              P-L ( 0  0 )                                              */
/*                                                                        */
/*                  N-K-L  K    L                                         */
/*    ( 0 R ) = K (  0   R11  R12 )                                       */
/*              L (  0    0   R22 )                                       */
/*                                                                        */
/*  where                                                                 */
/*                                                                        */
/*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),                           */
/*    S = diag( BETA(K+1),  ... , BETA(K+L) ),                            */
/*    C**2 + S**2 = I.                                                    */
/*                                                                        */
/*    R is stored in A(1:K+L,N-K-L+1:N) on exit.                          */
/*                                                                        */
/*  If M-K-L < 0,                                                         */
/*                                                                        */
/*                    K M-K K+L-M                                         */
/*         D1 =   K ( I  0    0   )                                       */
/*              M-K ( 0  C    0   )                                       */
/*                                                                        */
/*                      K M-K K+L-M                                       */
/*         D2 =   M-K ( 0  S    0  )                                      */
/*              K+L-M ( 0  0    I  )                                      */
/*                P-L ( 0  0    0  )                                      */
/*                                                                        */
/*                     N-K-L  K   M-K  K+L-M                              */
/*    ( 0 R ) =     K ( 0    R11  R12  R13  )                             */
/*                M-K ( 0     0   R22  R23  )                             */
/*              K+L-M ( 0     0    0   R33  )                             */
/*                                                                        */
/*  where                                                                 */
/*                                                                        */
/*    C = diag( ALPHA(K+1), ... , ALPHA(M) ),                             */
/*    S = diag( BETA(K+1),  ... , BETA(M) ),                              */
/*    C**2 + S**2 = I.                                                    */
/*                                                                        */
/*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored    */
/*    ( 0  R22 R23 )                                                      */
/*    in B(M-K+1:L,N+M-K-L+1:N) on exit.                                  */
/*                                                                        */
/*  The routine computes C, S, R, and optionally the orthogonal           */
/*  transformation matrices U, V and Q.                                   */
/*                                                                        */
/*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of */
/*  A and B implicitly gives the SVD of A*inv(B):                         */
/*                       A*inv(B) = U*(D1*inv(D2))*V'.                    */
/*  If ( A',B')' has orthonormal columns, then the GSVD of A and B is     */
/*  also equal to the CS decomposition of A and B. Furthermore, the GSVD  */
/*  can be used to derive the solution of the eigenvalue problem:         */
/*                       A'*A x = lambda* B'*B x.                         */
/*  In some literature, the GSVD of A and B is presented in the form      */
/*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )               */
/*  where U and V are orthogonal and X is nonsingular, D1 and D2 are      */
/*  ``diagonal''.  The former GSVD form can be converted to the latter    */
/*  form by taking the nonsingular matrix X as                            */
/*                                                                        */
/*                       X = Q*( I   0    )                               */
/*                             ( 0 inv(R) ).                              */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOBU    (input) CHARACTER*1                                           */
/*          = 'U':  Orthogonal matrix U is computed;                      */
/*          = 'N':  U is not computed.                                    */
/*                                                                        */
/*  JOBV    (input) CHARACTER*1                                           */
/*          = 'V':  Orthogonal matrix V is computed;                      */
/*          = 'N':  V is not computed.                                    */
/*                                                                        */
/*  JOBQ    (input) CHARACTER*1                                           */
/*          = 'Q':  Orthogonal matrix Q is computed;                      */
/*          = 'N':  Q is not computed.                                    */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix A.  M >= 0.                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrices A and B.  N >= 0.       */
/*                                                                        */
/*  P       (input) INTEGER                                               */
/*          The number of rows of the matrix B.  P >= 0.                  */
/*                                                                        */
/*  K       (output) INTEGER                                              */
/*  L       (output) INTEGER                                              */
/*          On exit, K and L specify the dimension of the subblocks       */
/*          described in the Purpose section.                             */
/*          K + L = effective numerical rank of (A',B')'.                 */
/*                                                                        */
/*  A       (input/output) REAL array, dimension (LDA,N)                  */
/*          On entry, the M-by-N matrix A.                                */
/*          On exit, A contains the triangular matrix R, or part of R.    */
/*          See Purpose for details.                                      */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A. LDA >= max(1,M).        */
/*                                                                        */
/*  B       (input/output) REAL array, dimension (LDB,N)                  */
/*          On entry, the P-by-N matrix B.                                */
/*          On exit, B contains the triangular matrix R if M-K-L < 0.     */
/*          See Purpose for details.                                      */
/*                                                                        */
/*  LDB     (input) INTEGER                                               */
/*          The leading dimension of the array B. LDA >= max(1,P).        */
/*                                                                        */
/*  ALPHA   (output) REAL array, dimension (N)                            */
/*  BETA    (output) REAL array, dimension (N)                            */
/*          On exit, ALPHA and BETA contain the generalized singular      */
/*          value pairs of A and B;                                       */
/*            ALPHA(1:K) = 1,                                             */
/*            BETA(1:K)  = 0,                                             */
/*          and if M-K-L >= 0,                                            */
/*            ALPHA(K+1:K+L) = C,                                         */
/*            BETA(K+1:K+L)  = S,                                         */
/*          or if M-K-L < 0,                                              */
/*            ALPHA(K+1:M)=C, ALPHA(M+1:K+L)=0                            */
/*            BETA(K+1:M) =S, BETA(M+1:K+L) =1                            */
/*          and                                                           */
/*            ALPHA(K+L+1:N) = 0                                          */
/*            BETA(K+L+1:N)  = 0                                          */
/*                                                                        */
/*  U       (output) REAL array, dimension (LDU,M)                        */
/*          If JOBU = 'U', U contains the M-by-M orthogonal matrix U.     */
/*          If JOBU = 'N', U is not referenced.                           */
/*                                                                        */
/*  LDU     (input) INTEGER                                               */
/*          The leading dimension of the array U. LDU >= max(1,M) if      */
/*          JOBU = 'U'; LDU >= 1 otherwise.                               */
/*                                                                        */
/*  V       (output) REAL array, dimension (LDV,P)                        */
/*          If JOBV = 'V', V contains the P-by-P orthogonal matrix V.     */
/*          If JOBV = 'N', V is not referenced.                           */
/*                                                                        */
/*  LDV     (input) INTEGER                                               */
/*          The leading dimension of the array V. LDV >= max(1,P) if      */
/*          JOBV = 'V'; LDV >= 1 otherwise.                               */
/*                                                                        */
/*  Q       (output) REAL array, dimension (LDQ,N)                        */
/*          If JOBQ = 'Q', Q contains the N-by-N orthogonal matrix Q.     */
/*          If JOBQ = 'N', Q is not referenced.                           */
/*                                                                        */
/*  LDQ     (input) INTEGER                                               */
/*          The leading dimension of the array Q. LDQ >= max(1,N) if      */
/*          JOBQ = 'Q'; LDQ >= 1 otherwise.                               */
/*                                                                        */
/*  WORK    (workspace) REAL array,                                       */
/*                      dimension (max(3*N,M,P)+N)                        */
/*                                                                        */
/*  IWORK   (workspace) INTEGER array, dimension (N)                      */
/*                                                                        */
/*  INFO    (output)INTEGER                                               */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*          > 0:  if INFO = 1, the Jacobi-type procedure failed to        */
/*                converge.  For further details, see subroutine STGSJA.  */
/*                                                                        */
/*  Internal Parameters                                                   */
/*  ===================                                                   */
/*                                                                        */
/*  TOLA    REAL                                                          */
/*  TOLB    REAL                                                          */
/*          TOLA and TOLB are the thresholds to determine the effective   */
/*          rank of (A',B')'. Generally, they are set to                  */
/*                   TOLA = MAX(M,N)*norm(A)*MACHEPS,                     */
/*                   TOLB = MAX(P,N)*norm(B)*MACHEPS.                     */
/*          The size of TOLA and TOLB may affect the size of backward     */
/*          errors of the decomposition.                                  */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input parameters */

    wantu = lsame_(jobu, "U");
    wantv = lsame_(jobv, "V");
    wantq = lsame_(jobq, "Q");

    *info = 0;
    if (! (wantu || lsame_(jobu, "N"))) {
        *info = -1;
    } else if (! (wantv || lsame_(jobv, "N"))) {
        *info = -2;
    } else if (! (wantq || lsame_(jobq, "N"))) {
        *info = -3;
    } else if (*m < 0) {
        *info = -4;
    } else if (*n < 0) {
        *info = -5;
    } else if (*p < 0) {
        *info = -6;
    } else if (*lda < max(1,*m)) {
        *info = -10;
    } else if (*ldb < max(1,*p)) {
        *info = -12;
    } else if (*ldu < 1 || (wantu && *ldu < *m)) {
        *info = -16;
    } else if (*ldv < 1 || (wantv && *ldv < *p)) {
        *info = -18;
    } else if (*ldq < 1 || (wantq && *ldq < *n)) {
        *info = -20;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("SGGSVD", &i__1);
        return;
    }

/*     Compute the Frobenius norm of matrices A and B */

    anorm = slange_("1", m, n, a, lda, work);
    bnorm = slange_("1", p, n, b, ldb, work);

/*     Get machine precision and set up threshold for determining */
/*     the effective numerical rank of the matrices A and B. */

    ulp = slamch_("Precision");
    unfl = slamch_("Safe Minimum");
    tola = max(*m,*n) * max(anorm,unfl) * ulp;
    tolb = max(*p,*n) * max(bnorm,unfl) * ulp;

/*     Preprocessing */

    sggsvp_(jobu, jobv, jobq, m, p, n, a, lda, b, ldb, &tola, &tolb, k, l,
            u, ldu, v, ldv, q, ldq, iwork, work, &work[*n], info);

/*     Compute the GSVD of two upper "triangular" matrices */

    stgsja_(jobu, jobv, jobq, m, p, n, k, l, a, lda, b, ldb, &tola, &tolb,
            alpha, beta, u, ldu, v, ldv, q, ldq, work, &ncycle, info);

} /* sggsvd_ */
