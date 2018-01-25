/* lapack/single/stgsja.f -- translated by f2c (version 20050501).
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

static real c_b13 = (float)0.;
static real c_b14 = (float)1.;
static integer c__1 = 1;
static real c_b43 = (float)-1.;

/*<    >*/
/* Subroutine */ int stgsja_(const char *jobu, const char *jobv, const char *jobq, integer *m,
        integer *p, integer *n, integer *k, integer *l, real *a, integer *lda,
         real *b, integer *ldb, real *tola, real *tolb, real *alpha, real *
        beta, real *u, integer *ldu, real *v, integer *ldv, real *q, integer *
        ldq, real *work, integer *ncycle, integer *info, ftnlen jobu_len,
        ftnlen jobv_len, ftnlen jobq_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, u_dim1,
            u_offset, v_dim1, v_offset, i__1, i__2, i__3, i__4;
    real r__1;

    /* Local variables */
    integer i__, j;
    real a1, a2, a3, b1, b2, b3, csq, csu, csv, snq, rwk, snu, snv;
    extern /* Subroutine */ int srot_(integer *, real *, integer *, real *,
            integer *, real *, real *);
    real gamma;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    logical initq, initu, initv, wantq, upper;
    real error, ssmin;
    logical wantu, wantv;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *,
            integer *), slags2_(logical *, real *, real *, real *, real *,
            real *, real *, real *, real *, real *, real *, real *, real *);
    integer kcycle;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), slapll_(
            integer *, real *, integer *, real *, integer *, real *), slartg_(
            real *, real *, real *, real *, real *), slaset_(char *, integer *
            , integer *, real *, real *, real *, integer *, ftnlen);
    (void)jobu_len;
    (void)jobv_len;
    (void)jobq_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBQ, JOBU, JOBV >*/
/*<    >*/
/*<       REAL               TOLA, TOLB >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  STGSJA computes the generalized singular value decomposition (GSVD) */
/*  of two real upper triangular (or trapezoidal) matrices A and B. */

/*  On entry, it is assumed that matrices A and B have the following */
/*  forms, which may be obtained by the preprocessing subroutine SGGSVP */
/*  from a general M-by-N matrix A and P-by-N matrix B: */

/*               N-K-L  K    L */
/*     A =    K ( 0    A12  A13 ) if M-K-L >= 0; */
/*            L ( 0     0   A23 ) */
/*        M-K-L ( 0     0    0  ) */

/*             N-K-L  K    L */
/*     A =  K ( 0    A12  A13 ) if M-K-L < 0; */
/*        M-K ( 0     0   A23 ) */

/*             N-K-L  K    L */
/*     B =  L ( 0     0   B13 ) */
/*        P-L ( 0     0    0  ) */

/*  where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular */
/*  upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0, */
/*  otherwise A23 is (M-K)-by-L upper trapezoidal. */

/*  On exit, */

/*              U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R ), */

/*  where U, V and Q are orthogonal matrices, Z' denotes the transpose */
/*  of Z, R is a nonsingular upper triangular matrix, and D1 and D2 are */
/*  ``diagonal'' matrices, which are of the following structures: */

/*  If M-K-L >= 0, */

/*                      K  L */
/*         D1 =     K ( I  0 ) */
/*                  L ( 0  C ) */
/*              M-K-L ( 0  0 ) */

/*                    K  L */
/*         D2 = L   ( 0  S ) */
/*              P-L ( 0  0 ) */

/*                 N-K-L  K    L */
/*    ( 0 R ) = K (  0   R11  R12 ) K */
/*              L (  0    0   R22 ) L */

/*  where */

/*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ), */
/*    S = diag( BETA(K+1),  ... , BETA(K+L) ), */
/*    C**2 + S**2 = I. */

/*    R is stored in A(1:K+L,N-K-L+1:N) on exit. */

/*  If M-K-L < 0, */

/*                 K M-K K+L-M */
/*      D1 =   K ( I  0    0   ) */
/*           M-K ( 0  C    0   ) */

/*                   K M-K K+L-M */
/*      D2 =   M-K ( 0  S    0   ) */
/*           K+L-M ( 0  0    I   ) */
/*             P-L ( 0  0    0   ) */

/*                 N-K-L  K   M-K  K+L-M */
/* ( 0 R ) =    K ( 0    R11  R12  R13  ) */
/*            M-K ( 0     0   R22  R23  ) */
/*          K+L-M ( 0     0    0   R33  ) */

/*  where */
/*  C = diag( ALPHA(K+1), ... , ALPHA(M) ), */
/*  S = diag( BETA(K+1),  ... , BETA(M) ), */
/*  C**2 + S**2 = I. */

/*  R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored */
/*      (  0  R22 R23 ) */
/*  in B(M-K+1:L,N+M-K-L+1:N) on exit. */

/*  The computation of the orthogonal transformation matrices U, V or Q */
/*  is optional.  These matrices may either be formed explicitly, or they */
/*  may be postmultiplied into input matrices U1, V1, or Q1. */

/*  Arguments */
/*  ========= */

/*  JOBU    (input) CHARACTER*1 */
/*          = 'U':  U must contain an orthogonal matrix U1 on entry, and */
/*                  the product U1*U is returned; */
/*          = 'I':  U is initialized to the unit matrix, and the */
/*                  orthogonal matrix U is returned; */
/*          = 'N':  U is not computed. */

/*  JOBV    (input) CHARACTER*1 */
/*          = 'V':  V must contain an orthogonal matrix V1 on entry, and */
/*                  the product V1*V is returned; */
/*          = 'I':  V is initialized to the unit matrix, and the */
/*                  orthogonal matrix V is returned; */
/*          = 'N':  V is not computed. */

/*  JOBQ    (input) CHARACTER*1 */
/*          = 'Q':  Q must contain an orthogonal matrix Q1 on entry, and */
/*                  the product Q1*Q is returned; */
/*          = 'I':  Q is initialized to the unit matrix, and the */
/*                  orthogonal matrix Q is returned; */
/*          = 'N':  Q is not computed. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  P       (input) INTEGER */
/*          The number of rows of the matrix B.  P >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrices A and B.  N >= 0. */

/*  K       (input) INTEGER */
/*  L       (input) INTEGER */
/*          K and L specify the subblocks in the input matrices A and B: */
/*          A23 = A(K+1:MIN(K+L,M),N-L+1:N) and B13 = B(1:L,N-L+1:N) */
/*          of A and B, whose GSVD is going to be computed by STGSJA. */
/*          See Further details. */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, A(N-K+1:N,1:MIN(K+L,M) ) contains the triangular */
/*          matrix R or part of R.  See Purpose for details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  B       (input/output) REAL array, dimension (LDB,N) */
/*          On entry, the P-by-N matrix B. */
/*          On exit, if necessary, B(M-K+1:L,N+M-K-L+1:N) contains */
/*          a part of R.  See Purpose for details. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDB >= max(1,P). */

/*  TOLA    (input) REAL */
/*  TOLB    (input) REAL */
/*          TOLA and TOLB are the convergence criteria for the Jacobi- */
/*          Kogbetliantz iteration procedure. Generally, they are the */
/*          same as used in the preprocessing step, say */
/*              TOLA = max(M,N)*norm(A)*MACHEPS, */
/*              TOLB = max(P,N)*norm(B)*MACHEPS. */

/*  ALPHA   (output) REAL array, dimension (N) */
/*  BETA    (output) REAL array, dimension (N) */
/*          On exit, ALPHA and BETA contain the generalized singular */
/*          value pairs of A and B; */
/*            ALPHA(1:K) = 1, */
/*            BETA(1:K)  = 0, */
/*          and if M-K-L >= 0, */
/*            ALPHA(K+1:K+L) = diag(C), */
/*            BETA(K+1:K+L)  = diag(S), */
/*          or if M-K-L < 0, */
/*            ALPHA(K+1:M)= C, ALPHA(M+1:K+L)= 0 */
/*            BETA(K+1:M) = S, BETA(M+1:K+L) = 1. */
/*          Furthermore, if K+L < N, */
/*            ALPHA(K+L+1:N) = 0 and */
/*            BETA(K+L+1:N)  = 0. */

/*  U       (input/output) REAL array, dimension (LDU,M) */
/*          On entry, if JOBU = 'U', U must contain a matrix U1 (usually */
/*          the orthogonal matrix returned by SGGSVP). */
/*          On exit, */
/*          if JOBU = 'I', U contains the orthogonal matrix U; */
/*          if JOBU = 'U', U contains the product U1*U. */
/*          If JOBU = 'N', U is not referenced. */

/*  LDU     (input) INTEGER */
/*          The leading dimension of the array U. LDU >= max(1,M) if */
/*          JOBU = 'U'; LDU >= 1 otherwise. */

/*  V       (input/output) REAL array, dimension (LDV,P) */
/*          On entry, if JOBV = 'V', V must contain a matrix V1 (usually */
/*          the orthogonal matrix returned by SGGSVP). */
/*          On exit, */
/*          if JOBV = 'I', V contains the orthogonal matrix V; */
/*          if JOBV = 'V', V contains the product V1*V. */
/*          If JOBV = 'N', V is not referenced. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. LDV >= max(1,P) if */
/*          JOBV = 'V'; LDV >= 1 otherwise. */

/*  Q       (input/output) REAL array, dimension (LDQ,N) */
/*          On entry, if JOBQ = 'Q', Q must contain a matrix Q1 (usually */
/*          the orthogonal matrix returned by SGGSVP). */
/*          On exit, */
/*          if JOBQ = 'I', Q contains the orthogonal matrix Q; */
/*          if JOBQ = 'Q', Q contains the product Q1*Q. */
/*          If JOBQ = 'N', Q is not referenced. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. LDQ >= max(1,N) if */
/*          JOBQ = 'Q'; LDQ >= 1 otherwise. */

/*  WORK    (workspace) REAL array, dimension (2*N) */

/*  NCYCLE  (output) INTEGER */
/*          The number of cycles required for convergence. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          = 1:  the procedure does not converge after MAXIT cycles. */

/*  Internal Parameters */
/*  =================== */

/*  MAXIT   INTEGER */
/*          MAXIT specifies the total loops that the iterative procedure */
/*          may take. If after MAXIT cycles, the routine fails to */
/*          converge, we return INFO = 1. */

/*  Further Details */
/*  =============== */

/*  STGSJA essentially uses a variant of Kogbetliantz algorithm to reduce */
/*  min(L,M-K)-by-L triangular (or trapezoidal) matrix A23 and L-by-L */
/*  matrix B13 to the form: */

/*           U1'*A13*Q1 = C1*R1; V1'*B13*Q1 = S1*R1, */

/*  where U1, V1 and Q1 are orthogonal matrix, and Z' is the transpose */
/*  of Z.  C1 and S1 are diagonal matrices satisfying */

/*                C1**2 + S1**2 = I, */

/*  and R1 is an L-by-L nonsingular upper triangular matrix. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER            MAXIT >*/
/*<       PARAMETER          ( MAXIT = 40 ) >*/
/*<       REAL               ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */

/*<       LOGICAL            INITQ, INITU, INITV, UPPER, WANTQ, WANTU, WANTV >*/
/*<       INTEGER            I, J, KCYCLE >*/
/*<    >*/
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

/*     Decode and test the input parameters */

/*<       INITU = LSAME( JOBU, 'I' ) >*/
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

    /* Function Body */
    initu = lsame_(jobu, "I", (ftnlen)1, (ftnlen)1);
/*<       WANTU = INITU .OR. LSAME( JOBU, 'U' ) >*/
    wantu = initu || lsame_(jobu, "U", (ftnlen)1, (ftnlen)1);

/*<       INITV = LSAME( JOBV, 'I' ) >*/
    initv = lsame_(jobv, "I", (ftnlen)1, (ftnlen)1);
/*<       WANTV = INITV .OR. LSAME( JOBV, 'V' ) >*/
    wantv = initv || lsame_(jobv, "V", (ftnlen)1, (ftnlen)1);

/*<       INITQ = LSAME( JOBQ, 'I' ) >*/
    initq = lsame_(jobq, "I", (ftnlen)1, (ftnlen)1);
/*<       WANTQ = INITQ .OR. LSAME( JOBQ, 'Q' ) >*/
    wantq = initq || lsame_(jobq, "Q", (ftnlen)1, (ftnlen)1);

/*<       INFO = 0 >*/
    *info = 0;
/*<       IF( .NOT.( INITU .OR. WANTU .OR. LSAME( JOBU, 'N' ) ) ) THEN >*/
    if (! (initu || wantu || lsame_(jobu, "N", (ftnlen)1, (ftnlen)1))) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.( INITV .OR. WANTV .OR. LSAME( JOBV, 'N' ) ) ) THEN >*/
    } else if (! (initv || wantv || lsame_(jobv, "N", (ftnlen)1, (ftnlen)1)))
            {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( .NOT.( INITQ .OR. WANTQ .OR. LSAME( JOBQ, 'N' ) ) ) THEN >*/
    } else if (! (initq || wantq || lsame_(jobq, "N", (ftnlen)1, (ftnlen)1)))
            {
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
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LDB.LT.MAX( 1, P ) ) THEN >*/
    } else if (*ldb < max(1,*p)) {
/*<          INFO = -12 >*/
        *info = -12;
/*<       ELSE IF( LDU.LT.1 .OR. ( WANTU .AND. LDU.LT.M ) ) THEN >*/
    } else if (*ldu < 1 || (wantu && *ldu < *m)) {
/*<          INFO = -18 >*/
        *info = -18;
/*<       ELSE IF( LDV.LT.1 .OR. ( WANTV .AND. LDV.LT.P ) ) THEN >*/
    } else if (*ldv < 1 || (wantv && *ldv < *p)) {
/*<          INFO = -20 >*/
        *info = -20;
/*<       ELSE IF( LDQ.LT.1 .OR. ( WANTQ .AND. LDQ.LT.N ) ) THEN >*/
    } else if (*ldq < 1 || (wantq && *ldq < *n)) {
/*<          INFO = -22 >*/
        *info = -22;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'STGSJA', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("STGSJA", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Initialize U, V and Q, if necessary */

/*<    >*/
    if (initu) {
        slaset_("Full", m, m, &c_b13, &c_b14, &u[u_offset], ldu, (ftnlen)4);
    }
/*<    >*/
    if (initv) {
        slaset_("Full", p, p, &c_b13, &c_b14, &v[v_offset], ldv, (ftnlen)4);
    }
/*<    >*/
    if (initq) {
        slaset_("Full", n, n, &c_b13, &c_b14, &q[q_offset], ldq, (ftnlen)4);
    }

/*     Loop until convergence */

/*<       UPPER = .FALSE. >*/
    upper = FALSE_;
/*<       DO 40 KCYCLE = 1, MAXIT >*/
    for (kcycle = 1; kcycle <= 40; ++kcycle) {

/*<          UPPER = .NOT.UPPER >*/
        upper = ! upper;

/*<          DO 20 I = 1, L - 1 >*/
        i__1 = *l - 1;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             DO 10 J = I + 1, L >*/
            i__2 = *l;
            for (j = i__ + 1; j <= i__2; ++j) {

/*<                A1 = ZERO >*/
                a1 = (float)0.;
/*<                A2 = ZERO >*/
                a2 = (float)0.;
/*<                A3 = ZERO >*/
                a3 = (float)0.;
/*<    >*/
                if (*k + i__ <= *m) {
                    a1 = a[*k + i__ + (*n - *l + i__) * a_dim1];
                }
/*<    >*/
                if (*k + j <= *m) {
                    a3 = a[*k + j + (*n - *l + j) * a_dim1];
                }

/*<                B1 = B( I, N-L+I ) >*/
                b1 = b[i__ + (*n - *l + i__) * b_dim1];
/*<                B3 = B( J, N-L+J ) >*/
                b3 = b[j + (*n - *l + j) * b_dim1];

/*<                IF( UPPER ) THEN >*/
                if (upper) {
/*<    >*/
                    if (*k + i__ <= *m) {
                        a2 = a[*k + i__ + (*n - *l + j) * a_dim1];
                    }
/*<                   B2 = B( I, N-L+J ) >*/
                    b2 = b[i__ + (*n - *l + j) * b_dim1];
/*<                ELSE >*/
                } else {
/*<    >*/
                    if (*k + j <= *m) {
                        a2 = a[*k + j + (*n - *l + i__) * a_dim1];
                    }
/*<                   B2 = B( J, N-L+I ) >*/
                    b2 = b[j + (*n - *l + i__) * b_dim1];
/*<                END IF >*/
                }

/*<    >*/
                slags2_(&upper, &a1, &a2, &a3, &b1, &b2, &b3, &csu, &snu, &
                        csv, &snv, &csq, &snq);

/*              Update (K+I)-th and (K+J)-th rows of matrix A: U'*A */

/*<    >*/
                if (*k + j <= *m) {
                    srot_(l, &a[*k + j + (*n - *l + 1) * a_dim1], lda, &a[*k
                            + i__ + (*n - *l + 1) * a_dim1], lda, &csu, &snu);
                }

/*              Update I-th and J-th rows of matrix B: V'*B */

/*<    >*/
                srot_(l, &b[j + (*n - *l + 1) * b_dim1], ldb, &b[i__ + (*n - *
                        l + 1) * b_dim1], ldb, &csv, &snv);

/*              Update (N-L+I)-th and (N-L+J)-th columns of matrices */
/*              A and B: A*Q and B*Q */

/*<    >*/
/* Computing MIN */
                i__4 = *k + *l;
                i__3 = min(i__4,*m);
                srot_(&i__3, &a[(*n - *l + j) * a_dim1 + 1], &c__1, &a[(*n - *
                        l + i__) * a_dim1 + 1], &c__1, &csq, &snq);

/*<    >*/
                srot_(l, &b[(*n - *l + j) * b_dim1 + 1], &c__1, &b[(*n - *l +
                        i__) * b_dim1 + 1], &c__1, &csq, &snq);

/*<                IF( UPPER ) THEN >*/
                if (upper) {
/*<    >*/
                    if (*k + i__ <= *m) {
                        a[*k + i__ + (*n - *l + j) * a_dim1] = (float)0.;
                    }
/*<                   B( I, N-L+J ) = ZERO >*/
                    b[i__ + (*n - *l + j) * b_dim1] = (float)0.;
/*<                ELSE >*/
                } else {
/*<    >*/
                    if (*k + j <= *m) {
                        a[*k + j + (*n - *l + i__) * a_dim1] = (float)0.;
                    }
/*<                   B( J, N-L+I ) = ZERO >*/
                    b[j + (*n - *l + i__) * b_dim1] = (float)0.;
/*<                END IF >*/
                }

/*              Update orthogonal matrices U, V, Q, if desired. */

/*<    >*/
                if (wantu && *k + j <= *m) {
                    srot_(m, &u[(*k + j) * u_dim1 + 1], &c__1, &u[(*k + i__) *
                             u_dim1 + 1], &c__1, &csu, &snu);
                }

/*<    >*/
                if (wantv) {
                    srot_(p, &v[j * v_dim1 + 1], &c__1, &v[i__ * v_dim1 + 1],
                            &c__1, &csv, &snv);
                }

/*<    >*/
                if (wantq) {
                    srot_(n, &q[(*n - *l + j) * q_dim1 + 1], &c__1, &q[(*n - *
                            l + i__) * q_dim1 + 1], &c__1, &csq, &snq);
                }

/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }

/*<          IF( .NOT.UPPER ) THEN >*/
        if (! upper) {

/*           The matrices A13 and B13 were lower triangular at the start */
/*           of the cycle, and are now upper triangular. */

/*           Convergence test: test the parallelism of the corresponding */
/*           rows of A and B. */

/*<             ERROR = ZERO >*/
            error = (float)0.;
/*<             DO 30 I = 1, MIN( L, M-K ) >*/
/* Computing MIN */
            i__2 = *l, i__3 = *m - *k;
            i__1 = min(i__2,i__3);
            for (i__ = 1; i__ <= i__1; ++i__) {
/*<                CALL SCOPY( L-I+1, A( K+I, N-L+I ), LDA, WORK, 1 ) >*/
                i__2 = *l - i__ + 1;
                scopy_(&i__2, &a[*k + i__ + (*n - *l + i__) * a_dim1], lda, &
                        work[1], &c__1);
/*<                CALL SCOPY( L-I+1, B( I, N-L+I ), LDB, WORK( L+1 ), 1 ) >*/
                i__2 = *l - i__ + 1;
                scopy_(&i__2, &b[i__ + (*n - *l + i__) * b_dim1], ldb, &work[*
                        l + 1], &c__1);
/*<                CALL SLAPLL( L-I+1, WORK, 1, WORK( L+1 ), 1, SSMIN ) >*/
                i__2 = *l - i__ + 1;
                slapll_(&i__2, &work[1], &c__1, &work[*l + 1], &c__1, &ssmin);
/*<                ERROR = MAX( ERROR, SSMIN ) >*/
                error = dmax(error,ssmin);
/*<    30       CONTINUE >*/
/* L30: */
            }

/*<    >*/
            if (dabs(error) <= dmin(*tola,*tolb)) {
                goto L50;
            }
/*<          END IF >*/
        }

/*        End of cycle loop */

/*<    40 CONTINUE >*/
/* L40: */
    }

/*     The algorithm has not converged after MAXIT cycles. */

/*<       INFO = 1 >*/
    *info = 1;
/*<       GO TO 100 >*/
    goto L100;

/*<    50 CONTINUE >*/
L50:

/*     If ERROR <= MIN(TOLA,TOLB), then the algorithm has converged. */
/*     Compute the generalized singular value pairs (ALPHA, BETA), and */
/*     set the triangular matrix R to array A. */

/*<       DO 60 I = 1, K >*/
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          ALPHA( I ) = ONE >*/
        alpha[i__] = (float)1.;
/*<          BETA( I ) = ZERO >*/
        beta[i__] = (float)0.;
/*<    60 CONTINUE >*/
/* L60: */
    }

/*<       DO 70 I = 1, MIN( L, M-K ) >*/
/* Computing MIN */
    i__2 = *l, i__3 = *m - *k;
    i__1 = min(i__2,i__3);
    for (i__ = 1; i__ <= i__1; ++i__) {

/*<          A1 = A( K+I, N-L+I ) >*/
        a1 = a[*k + i__ + (*n - *l + i__) * a_dim1];
/*<          B1 = B( I, N-L+I ) >*/
        b1 = b[i__ + (*n - *l + i__) * b_dim1];

/*<          IF( A1.NE.ZERO ) THEN >*/
        if (a1 != (float)0.) {
/*<             GAMMA = B1 / A1 >*/
            gamma = b1 / a1;

/*           change sign if necessary */

/*<             IF( GAMMA.LT.ZERO ) THEN >*/
            if (gamma < (float)0.) {
/*<                CALL SSCAL( L-I+1, -ONE, B( I, N-L+I ), LDB ) >*/
                i__2 = *l - i__ + 1;
                sscal_(&i__2, &c_b43, &b[i__ + (*n - *l + i__) * b_dim1], ldb)
                        ;
/*<    >*/
                if (wantv) {
                    sscal_(p, &c_b43, &v[i__ * v_dim1 + 1], &c__1);
                }
/*<             END IF >*/
            }

/*<    >*/
            r__1 = dabs(gamma);
            slartg_(&r__1, &c_b14, &beta[*k + i__], &alpha[*k + i__], &rwk);

/*<             IF( ALPHA( K+I ).GE.BETA( K+I ) ) THEN >*/
            if (alpha[*k + i__] >= beta[*k + i__]) {
/*<    >*/
                i__2 = *l - i__ + 1;
                r__1 = (float)1. / alpha[*k + i__];
                sscal_(&i__2, &r__1, &a[*k + i__ + (*n - *l + i__) * a_dim1],
                        lda);
/*<             ELSE >*/
            } else {
/*<    >*/
                i__2 = *l - i__ + 1;
                r__1 = (float)1. / beta[*k + i__];
                sscal_(&i__2, &r__1, &b[i__ + (*n - *l + i__) * b_dim1], ldb);
/*<    >*/
                i__2 = *l - i__ + 1;
                scopy_(&i__2, &b[i__ + (*n - *l + i__) * b_dim1], ldb, &a[*k
                        + i__ + (*n - *l + i__) * a_dim1], lda);
/*<             END IF >*/
            }

/*<          ELSE >*/
        } else {

/*<             ALPHA( K+I ) = ZERO >*/
            alpha[*k + i__] = (float)0.;
/*<             BETA( K+I ) = ONE >*/
            beta[*k + i__] = (float)1.;
/*<    >*/
            i__2 = *l - i__ + 1;
            scopy_(&i__2, &b[i__ + (*n - *l + i__) * b_dim1], ldb, &a[*k +
                    i__ + (*n - *l + i__) * a_dim1], lda);

/*<          END IF >*/
        }

/*<    70 CONTINUE >*/
/* L70: */
    }

/*     Post-assignment */

/*<       DO 80 I = M + 1, K + L >*/
    i__1 = *k + *l;
    for (i__ = *m + 1; i__ <= i__1; ++i__) {
/*<          ALPHA( I ) = ZERO >*/
        alpha[i__] = (float)0.;
/*<          BETA( I ) = ONE >*/
        beta[i__] = (float)1.;
/*<    80 CONTINUE >*/
/* L80: */
    }

/*<       IF( K+L.LT.N ) THEN >*/
    if (*k + *l < *n) {
/*<          DO 90 I = K + L + 1, N >*/
        i__1 = *n;
        for (i__ = *k + *l + 1; i__ <= i__1; ++i__) {
/*<             ALPHA( I ) = ZERO >*/
            alpha[i__] = (float)0.;
/*<             BETA( I ) = ZERO >*/
            beta[i__] = (float)0.;
/*<    90    CONTINUE >*/
/* L90: */
        }
/*<       END IF >*/
    }

/*<   100 CONTINUE >*/
L100:
/*<       NCYCLE = KCYCLE >*/
    *ncycle = kcycle;
/*<       RETURN >*/
    return 0;

/*     End of STGSJA */

/*<       END >*/
} /* stgsja_ */

#ifdef __cplusplus
        }
#endif
