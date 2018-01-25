/* lapack/complex16/zhgeqz.f -- translated by f2c (version 20090411).
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

static doublecomplex c_b1 = {0.,0.};
static doublecomplex c_b2 = {1.,0.};
static integer c__1 = 1;
static integer c__2 = 2;

/*<    >*/
/* Subroutine */ int zhgeqz_(char *job, char *compq, char *compz, integer *n,
        integer *ilo, integer *ihi, doublecomplex *h__, integer *ldh,
        doublecomplex *t, integer *ldt, doublecomplex *alpha, doublecomplex *
        beta, doublecomplex *q, integer *ldq, doublecomplex *z__, integer *
        ldz, doublecomplex *work, integer *lwork, doublereal *rwork, integer *
        info, ftnlen job_len, ftnlen compq_len, ftnlen compz_len)
{
    /* System generated locals */
    integer h_dim1, h_offset, q_dim1, q_offset, t_dim1, t_offset, z_dim1,
            z_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6;

    /* Builtin functions */
    double z_abs(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);
    double d_imag(doublecomplex *);
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), pow_zi(
            doublecomplex *, doublecomplex *, integer *), z_sqrt(
            doublecomplex *, doublecomplex *);

    /* Local variables */
    doublereal c__;
    integer j;
    doublecomplex s, t1;
    integer jc, in;
    doublecomplex u12;
    integer jr;
    doublecomplex ad11, ad12, ad21, ad22;
    integer jch;
    logical ilq, ilz;
    doublereal ulp;
    doublecomplex abi22;
    doublereal absb, atol, btol, temp;
    extern /* Subroutine */ int zrot_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublereal *, doublecomplex *);
    doublereal temp2;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublecomplex ctemp;
    integer iiter, ilast, jiter;
    doublereal anorm, bnorm;
    integer maxit;
    doublecomplex shift;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *);
    doublereal tempr;
    doublecomplex ctemp2, ctemp3;
    logical ilazr2;
    doublereal ascale, bscale;
    extern doublereal dlamch_(char *, ftnlen);
    doublecomplex signbc;
    doublereal safmin;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    doublecomplex eshift;
    logical ilschr;
    integer icompq, ilastm;
    doublecomplex rtdisc;
    integer ischur;
    extern doublereal zlanhs_(char *, integer *, doublecomplex *, integer *,
            doublereal *, ftnlen);
    logical ilazro;
    integer icompz, ifirst;
    extern /* Subroutine */ int zlartg_(doublecomplex *, doublecomplex *,
            doublereal *, doublecomplex *, doublecomplex *);
    integer ifrstm;
    extern /* Subroutine */ int zlaset_(char *, integer *, integer *,
            doublecomplex *, doublecomplex *, doublecomplex *, integer *,
            ftnlen);
    integer istart;
    logical lquery;
    (void)job_len;
    (void)compq_len;
    (void)compz_len;

/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          COMPQ, COMPZ, JOB >*/
/*<       INTEGER            IHI, ILO, INFO, LDH, LDQ, LDT, LDZ, LWORK, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   RWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZHGEQZ computes the eigenvalues of a complex matrix pair (H,T), */
/*  where H is an upper Hessenberg matrix and T is upper triangular, */
/*  using the single-shift QZ method. */
/*  Matrix pairs of this type are produced by the reduction to */
/*  generalized upper Hessenberg form of a complex matrix pair (A,B): */

/*     A = Q1*H*Z1**H,  B = Q1*T*Z1**H, */

/*  as computed by ZGGHRD. */

/*  If JOB='S', then the Hessenberg-triangular pair (H,T) is */
/*  also reduced to generalized Schur form, */

/*     H = Q*S*Z**H,  T = Q*P*Z**H, */

/*  where Q and Z are unitary matrices and S and P are upper triangular. */

/*  Optionally, the unitary matrix Q from the generalized Schur */
/*  factorization may be postmultiplied into an input matrix Q1, and the */
/*  unitary matrix Z may be postmultiplied into an input matrix Z1. */
/*  If Q1 and Z1 are the unitary matrices from ZGGHRD that reduced */
/*  the matrix pair (A,B) to generalized Hessenberg form, then the output */
/*  matrices Q1*Q and Z1*Z are the unitary factors from the generalized */
/*  Schur factorization of (A,B): */

/*     A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H. */

/*  To avoid overflow, eigenvalues of the matrix pair (H,T) */
/*  (equivalently, of (A,B)) are computed as a pair of complex values */
/*  (alpha,beta).  If beta is nonzero, lambda = alpha / beta is an */
/*  eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP) */
/*     A*x = lambda*B*x */
/*  and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the */
/*  alternate form of the GNEP */
/*     mu*A*y = B*y. */
/*  The values of alpha and beta for the i-th eigenvalue can be read */
/*  directly from the generalized Schur form:  alpha = S(i,i), */
/*  beta = P(i,i). */

/*  Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix */
/*       Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973), */
/*       pp. 241--256. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          = 'E': Compute eigenvalues only; */
/*          = 'S': Computer eigenvalues and the Schur form. */

/*  COMPQ   (input) CHARACTER*1 */
/*          = 'N': Left Schur vectors (Q) are not computed; */
/*          = 'I': Q is initialized to the unit matrix and the matrix Q */
/*                 of left Schur vectors of (H,T) is returned; */
/*          = 'V': Q must contain a unitary matrix Q1 on entry and */
/*                 the product Q1*Q is returned. */

/*  COMPZ   (input) CHARACTER*1 */
/*          = 'N': Right Schur vectors (Z) are not computed; */
/*          = 'I': Q is initialized to the unit matrix and the matrix Z */
/*                 of right Schur vectors of (H,T) is returned; */
/*          = 'V': Z must contain a unitary matrix Z1 on entry and */
/*                 the product Z1*Z is returned. */

/*  N       (input) INTEGER */
/*          The order of the matrices H, T, Q, and Z.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          ILO and IHI mark the rows and columns of H which are in */
/*          Hessenberg form.  It is assumed that A is already upper */
/*          triangular in rows and columns 1:ILO-1 and IHI+1:N. */
/*          If N > 0, 1 <= ILO <= IHI <= N; if N = 0, ILO=1 and IHI=0. */

/*  H       (input/output) COMPLEX*16 array, dimension (LDH, N) */
/*          On entry, the N-by-N upper Hessenberg matrix H. */
/*          On exit, if JOB = 'S', H contains the upper triangular */
/*          matrix S from the generalized Schur factorization. */
/*          If JOB = 'E', the diagonal of H matches that of S, but */
/*          the rest of H is unspecified. */

/*  LDH     (input) INTEGER */
/*          The leading dimension of the array H.  LDH >= max( 1, N ). */

/*  T       (input/output) COMPLEX*16 array, dimension (LDT, N) */
/*          On entry, the N-by-N upper triangular matrix T. */
/*          On exit, if JOB = 'S', T contains the upper triangular */
/*          matrix P from the generalized Schur factorization. */
/*          If JOB = 'E', the diagonal of T matches that of P, but */
/*          the rest of T is unspecified. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T.  LDT >= max( 1, N ). */

/*  ALPHA   (output) COMPLEX*16 array, dimension (N) */
/*          The complex scalars alpha that define the eigenvalues of */
/*          GNEP.  ALPHA(i) = S(i,i) in the generalized Schur */
/*          factorization. */

/*  BETA    (output) COMPLEX*16 array, dimension (N) */
/*          The real non-negative scalars beta that define the */
/*          eigenvalues of GNEP.  BETA(i) = P(i,i) in the generalized */
/*          Schur factorization. */

/*          Together, the quantities alpha = ALPHA(j) and beta = BETA(j) */
/*          represent the j-th eigenvalue of the matrix pair (A,B), in */
/*          one of the forms lambda = alpha/beta or mu = beta/alpha. */
/*          Since either lambda or mu may overflow, they should not, */
/*          in general, be computed. */

/*  Q       (input/output) COMPLEX*16 array, dimension (LDQ, N) */
/*          On entry, if COMPZ = 'V', the unitary matrix Q1 used in the */
/*          reduction of (A,B) to generalized Hessenberg form. */
/*          On exit, if COMPZ = 'I', the unitary matrix of left Schur */
/*          vectors of (H,T), and if COMPZ = 'V', the unitary matrix of */
/*          left Schur vectors of (A,B). */
/*          Not referenced if COMPZ = 'N'. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q.  LDQ >= 1. */
/*          If COMPQ='V' or 'I', then LDQ >= N. */

/*  Z       (input/output) COMPLEX*16 array, dimension (LDZ, N) */
/*          On entry, if COMPZ = 'V', the unitary matrix Z1 used in the */
/*          reduction of (A,B) to generalized Hessenberg form. */
/*          On exit, if COMPZ = 'I', the unitary matrix of right Schur */
/*          vectors of (H,T), and if COMPZ = 'V', the unitary matrix of */
/*          right Schur vectors of (A,B). */
/*          Not referenced if COMPZ = 'N'. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z.  LDZ >= 1. */
/*          If COMPZ='V' or 'I', then LDZ >= N. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK)) */
/*          On exit, if INFO >= 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,N). */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */
/*          = 1,...,N: the QZ iteration did not converge.  (H,T) is not */
/*                     in Schur form, but ALPHA(i) and BETA(i), */
/*                     i=INFO+1,...,N should be correct. */
/*          = N+1,...,2*N: the shift calculation failed.  (H,T) is not */
/*                     in Schur form, but ALPHA(i) and BETA(i), */
/*                     i=INFO-N+1,...,N should be correct. */

/*  Further Details */
/*  =============== */

/*  We assume that complex ABS works as long as its value is less than */
/*  overflow. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         CZERO, CONE >*/
/*<    >*/
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*<       DOUBLE PRECISION   HALF >*/
/*<       PARAMETER          ( HALF = 0.5D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            ILAZR2, ILAZRO, ILQ, ILSCHR, ILZ, LQUERY >*/
/*<    >*/
/*<    >*/
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       DOUBLE PRECISION   DLAMCH, ZLANHS >*/
/*<       EXTERNAL           LSAME, DLAMCH, ZLANHS >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZLARTG, ZLASET, ZROT, ZSCAL >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<    >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   ABS1 >*/
/*     .. */
/*     .. Statement Function definitions .. */
/*<       ABS1( X ) = ABS( DBLE( X ) ) + ABS( DIMAG( X ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode JOB, COMPQ, COMPZ */

/*<       IF( LSAME( JOB, 'E' ) ) THEN >*/
    /* Parameter adjustments */
    h_dim1 = *ldh;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    --alpha;
    --beta;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --work;
    --rwork;

    /* Function Body */
    if (lsame_(job, "E", (ftnlen)1, (ftnlen)1)) {
/*<          ILSCHR = .FALSE. >*/
        ilschr = FALSE_;
/*<          ISCHUR = 1 >*/
        ischur = 1;
/*<       ELSE IF( LSAME( JOB, 'S' ) ) THEN >*/
    } else if (lsame_(job, "S", (ftnlen)1, (ftnlen)1)) {
/*<          ILSCHR = .TRUE. >*/
        ilschr = TRUE_;
/*<          ISCHUR = 2 >*/
        ischur = 2;
/*<       ELSE >*/
    } else {
/*<          ISCHUR = 0 >*/
        ischur = 0;
/*<       END IF >*/
    }

/*<       IF( LSAME( COMPQ, 'N' ) ) THEN >*/
    if (lsame_(compq, "N", (ftnlen)1, (ftnlen)1)) {
/*<          ILQ = .FALSE. >*/
        ilq = FALSE_;
/*<          ICOMPQ = 1 >*/
        icompq = 1;
/*<       ELSE IF( LSAME( COMPQ, 'V' ) ) THEN >*/
    } else if (lsame_(compq, "V", (ftnlen)1, (ftnlen)1)) {
/*<          ILQ = .TRUE. >*/
        ilq = TRUE_;
/*<          ICOMPQ = 2 >*/
        icompq = 2;
/*<       ELSE IF( LSAME( COMPQ, 'I' ) ) THEN >*/
    } else if (lsame_(compq, "I", (ftnlen)1, (ftnlen)1)) {
/*<          ILQ = .TRUE. >*/
        ilq = TRUE_;
/*<          ICOMPQ = 3 >*/
        icompq = 3;
/*<       ELSE >*/
    } else {
/*<          ICOMPQ = 0 >*/
        icompq = 0;
/*<       END IF >*/
    }

/*<       IF( LSAME( COMPZ, 'N' ) ) THEN >*/
    if (lsame_(compz, "N", (ftnlen)1, (ftnlen)1)) {
/*<          ILZ = .FALSE. >*/
        ilz = FALSE_;
/*<          ICOMPZ = 1 >*/
        icompz = 1;
/*<       ELSE IF( LSAME( COMPZ, 'V' ) ) THEN >*/
    } else if (lsame_(compz, "V", (ftnlen)1, (ftnlen)1)) {
/*<          ILZ = .TRUE. >*/
        ilz = TRUE_;
/*<          ICOMPZ = 2 >*/
        icompz = 2;
/*<       ELSE IF( LSAME( COMPZ, 'I' ) ) THEN >*/
    } else if (lsame_(compz, "I", (ftnlen)1, (ftnlen)1)) {
/*<          ILZ = .TRUE. >*/
        ilz = TRUE_;
/*<          ICOMPZ = 3 >*/
        icompz = 3;
/*<       ELSE >*/
    } else {
/*<          ICOMPZ = 0 >*/
        icompz = 0;
/*<       END IF >*/
    }

/*     Check Argument Values */

/*<       INFO = 0 >*/
    *info = 0;
/*<       WORK( 1 ) = MAX( 1, N ) >*/
    i__1 = max(1,*n);
    work[1].r = (doublereal) i__1, work[1].i = 0.;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       IF( ISCHUR.EQ.0 ) THEN >*/
    if (ischur == 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ICOMPQ.EQ.0 ) THEN >*/
    } else if (icompq == 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( ICOMPZ.EQ.0 ) THEN >*/
    } else if (icompz == 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( ILO.LT.1 ) THEN >*/
    } else if (*ilo < 1) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN >*/
    } else if (*ihi > *n || *ihi < *ilo - 1) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDH.LT.N ) THEN >*/
    } else if (*ldh < *n) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       ELSE IF( LDT.LT.N ) THEN >*/
    } else if (*ldt < *n) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LDQ.LT.1 .OR. ( ILQ .AND. LDQ.LT.N ) ) THEN >*/
    } else if (*ldq < 1 || (ilq && *ldq < *n)) {
/*<          INFO = -14 >*/
        *info = -14;
/*<       ELSE IF( LDZ.LT.1 .OR. ( ILZ .AND. LDZ.LT.N ) ) THEN >*/
    } else if (*ldz < 1 || (ilz && *ldz < *n)) {
/*<          INFO = -16 >*/
        *info = -16;
/*<       ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,*n) && ! lquery) {
/*<          INFO = -18 >*/
        *info = -18;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZHGEQZ', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZHGEQZ", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*     WORK( 1 ) = CMPLX( 1 ) */
/*<       IF( N.LE.0 ) THEN >*/
    if (*n <= 0) {
/*<          WORK( 1 ) = DCMPLX( 1 ) >*/
        work[1].r = 1., work[1].i = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Initialize Q and Z */

/*<    >*/
    if (icompq == 3) {
        zlaset_("Full", n, n, &c_b1, &c_b2, &q[q_offset], ldq, (ftnlen)4);
    }
/*<    >*/
    if (icompz == 3) {
        zlaset_("Full", n, n, &c_b1, &c_b2, &z__[z_offset], ldz, (ftnlen)4);
    }

/*     Machine Constants */

/*<       IN = IHI + 1 - ILO >*/
    in = *ihi + 1 - *ilo;
/*<       SAFMIN = DLAMCH( 'S' ) >*/
    safmin = dlamch_("S", (ftnlen)1);
/*<       ULP = DLAMCH( 'E' )*DLAMCH( 'B' ) >*/
    ulp = dlamch_("E", (ftnlen)1) * dlamch_("B", (ftnlen)1);
/*<       ANORM = ZLANHS( 'F', IN, H( ILO, ILO ), LDH, RWORK ) >*/
    anorm = zlanhs_("F", &in, &h__[*ilo + *ilo * h_dim1], ldh, &rwork[1], (
            ftnlen)1);
/*<       BNORM = ZLANHS( 'F', IN, T( ILO, ILO ), LDT, RWORK ) >*/
    bnorm = zlanhs_("F", &in, &t[*ilo + *ilo * t_dim1], ldt, &rwork[1], (
            ftnlen)1);
/*<       ATOL = MAX( SAFMIN, ULP*ANORM ) >*/
/* Computing MAX */
    d__1 = safmin, d__2 = ulp * anorm;
    atol = max(d__1,d__2);
/*<       BTOL = MAX( SAFMIN, ULP*BNORM ) >*/
/* Computing MAX */
    d__1 = safmin, d__2 = ulp * bnorm;
    btol = max(d__1,d__2);
/*<       ASCALE = ONE / MAX( SAFMIN, ANORM ) >*/
    ascale = 1. / max(safmin,anorm);
/*<       BSCALE = ONE / MAX( SAFMIN, BNORM ) >*/
    bscale = 1. / max(safmin,bnorm);


/*     Set Eigenvalues IHI+1:N */

/*<       DO 10 J = IHI + 1, N >*/
    i__1 = *n;
    for (j = *ihi + 1; j <= i__1; ++j) {
/*<          ABSB = ABS( T( J, J ) ) >*/
        absb = z_abs(&t[j + j * t_dim1]);
/*<          IF( ABSB.GT.SAFMIN ) THEN >*/
        if (absb > safmin) {
/*<             SIGNBC = DCONJG( T( J, J ) / ABSB ) >*/
            i__2 = j + j * t_dim1;
            z__2.r = t[i__2].r / absb, z__2.i = t[i__2].i / absb;
            d_cnjg(&z__1, &z__2);
            signbc.r = z__1.r, signbc.i = z__1.i;
/*<             T( J, J ) = ABSB >*/
            i__2 = j + j * t_dim1;
            t[i__2].r = absb, t[i__2].i = 0.;
/*<             IF( ILSCHR ) THEN >*/
            if (ilschr) {
/*<                CALL ZSCAL( J-1, SIGNBC, T( 1, J ), 1 ) >*/
                i__2 = j - 1;
                zscal_(&i__2, &signbc, &t[j * t_dim1 + 1], &c__1);
/*<                CALL ZSCAL( J, SIGNBC, H( 1, J ), 1 ) >*/
                zscal_(&j, &signbc, &h__[j * h_dim1 + 1], &c__1);
/*<             ELSE >*/
            } else {
/*<                H( J, J ) = H( J, J )*SIGNBC >*/
                i__2 = j + j * h_dim1;
                i__3 = j + j * h_dim1;
                z__1.r = h__[i__3].r * signbc.r - h__[i__3].i * signbc.i,
                        z__1.i = h__[i__3].r * signbc.i + h__[i__3].i *
                        signbc.r;
                h__[i__2].r = z__1.r, h__[i__2].i = z__1.i;
/*<             END IF >*/
            }
/*<    >*/
            if (ilz) {
                zscal_(n, &signbc, &z__[j * z_dim1 + 1], &c__1);
            }
/*<          ELSE >*/
        } else {
/*<             T( J, J ) = CZERO >*/
            i__2 = j + j * t_dim1;
            t[i__2].r = 0., t[i__2].i = 0.;
/*<          END IF >*/
        }
/*<          ALPHA( J ) = H( J, J ) >*/
        i__2 = j;
        i__3 = j + j * h_dim1;
        alpha[i__2].r = h__[i__3].r, alpha[i__2].i = h__[i__3].i;
/*<          BETA( J ) = T( J, J ) >*/
        i__2 = j;
        i__3 = j + j * t_dim1;
        beta[i__2].r = t[i__3].r, beta[i__2].i = t[i__3].i;
/*<    10 CONTINUE >*/
/* L10: */
    }

/*     If IHI < ILO, skip QZ steps */

/*<    >*/
    if (*ihi < *ilo) {
        goto L190;
    }

/*     MAIN QZ ITERATION LOOP */

/*     Initialize dynamic indices */

/*     Eigenvalues ILAST+1:N have been found. */
/*        Column operations modify rows IFRSTM:whatever */
/*        Row operations modify columns whatever:ILASTM */

/*     If only eigenvalues are being computed, then */
/*        IFRSTM is the row of the last splitting row above row ILAST; */
/*        this is always at least ILO. */
/*     IITER counts iterations since the last eigenvalue was found, */
/*        to tell when to use an extraordinary shift. */
/*     MAXIT is the maximum number of QZ sweeps allowed. */

/*<       ILAST = IHI >*/
    ilast = *ihi;
/*<       IF( ILSCHR ) THEN >*/
    if (ilschr) {
/*<          IFRSTM = 1 >*/
        ifrstm = 1;
/*<          ILASTM = N >*/
        ilastm = *n;
/*<       ELSE >*/
    } else {
/*<          IFRSTM = ILO >*/
        ifrstm = *ilo;
/*<          ILASTM = IHI >*/
        ilastm = *ihi;
/*<       END IF >*/
    }
/*<       IITER = 0 >*/
    iiter = 0;
/*<       ESHIFT = CZERO >*/
    eshift.r = 0., eshift.i = 0.;
/*<       MAXIT = 30*( IHI-ILO+1 ) >*/
    maxit = (*ihi - *ilo + 1) * 30;

/*<       DO 170 JITER = 1, MAXIT >*/
    i__1 = maxit;
    for (jiter = 1; jiter <= i__1; ++jiter) {

/*        Check for too many iterations. */

/*<    >*/
        if (jiter > maxit) {
            goto L180;
        }

/*        Split the matrix if possible. */

/*        Two tests: */
/*           1: H(j,j-1)=0  or  j=ILO */
/*           2: T(j,j)=0 */

/*        Special case: j=ILAST */

/*<          IF( ILAST.EQ.ILO ) THEN >*/
        if (ilast == *ilo) {
/*<             GO TO 60 >*/
            goto L60;
/*<          ELSE >*/
        } else {
/*<             IF( ABS1( H( ILAST, ILAST-1 ) ).LE.ATOL ) THEN >*/
            i__2 = ilast + (ilast - 1) * h_dim1;
            if ((d__1 = h__[i__2].r, abs(d__1)) + (d__2 = d_imag(&h__[ilast +
                    (ilast - 1) * h_dim1]), abs(d__2)) <= atol) {
/*<                H( ILAST, ILAST-1 ) = CZERO >*/
                i__2 = ilast + (ilast - 1) * h_dim1;
                h__[i__2].r = 0., h__[i__2].i = 0.;
/*<                GO TO 60 >*/
                goto L60;
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*<          IF( ABS( T( ILAST, ILAST ) ).LE.BTOL ) THEN >*/
        if (z_abs(&t[ilast + ilast * t_dim1]) <= btol) {
/*<             T( ILAST, ILAST ) = CZERO >*/
            i__2 = ilast + ilast * t_dim1;
            t[i__2].r = 0., t[i__2].i = 0.;
/*<             GO TO 50 >*/
            goto L50;
/*<          END IF >*/
        }

/*        General case: j<ILAST */

/*<          DO 40 J = ILAST - 1, ILO, -1 >*/
        i__2 = *ilo;
        for (j = ilast - 1; j >= i__2; --j) {

/*           Test 1: for H(j,j-1)=0 or j=ILO */

/*<             IF( J.EQ.ILO ) THEN >*/
            if (j == *ilo) {
/*<                ILAZRO = .TRUE. >*/
                ilazro = TRUE_;
/*<             ELSE >*/
            } else {
/*<                IF( ABS1( H( J, J-1 ) ).LE.ATOL ) THEN >*/
                i__3 = j + (j - 1) * h_dim1;
                if ((d__1 = h__[i__3].r, abs(d__1)) + (d__2 = d_imag(&h__[j +
                        (j - 1) * h_dim1]), abs(d__2)) <= atol) {
/*<                   H( J, J-1 ) = CZERO >*/
                    i__3 = j + (j - 1) * h_dim1;
                    h__[i__3].r = 0., h__[i__3].i = 0.;
/*<                   ILAZRO = .TRUE. >*/
                    ilazro = TRUE_;
/*<                ELSE >*/
                } else {
/*<                   ILAZRO = .FALSE. >*/
                    ilazro = FALSE_;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*           Test 2: for T(j,j)=0 */

/*<             IF( ABS( T( J, J ) ).LT.BTOL ) THEN >*/
            if (z_abs(&t[j + j * t_dim1]) < btol) {
/*<                T( J, J ) = CZERO >*/
                i__3 = j + j * t_dim1;
                t[i__3].r = 0., t[i__3].i = 0.;

/*              Test 1a: Check for 2 consecutive small subdiagonals in A */

/*<                ILAZR2 = .FALSE. >*/
                ilazr2 = FALSE_;
/*<                IF( .NOT.ILAZRO ) THEN >*/
                if (! ilazro) {
/*<    >*/
                    i__3 = j + (j - 1) * h_dim1;
                    i__4 = j + 1 + j * h_dim1;
                    i__5 = j + j * h_dim1;
                    if (((d__1 = h__[i__3].r, abs(d__1)) + (d__2 = d_imag(&
                            h__[j + (j - 1) * h_dim1]), abs(d__2))) * (ascale
                            * ((d__3 = h__[i__4].r, abs(d__3)) + (d__4 =
                            d_imag(&h__[j + 1 + j * h_dim1]), abs(d__4)))) <=
                            ((d__5 = h__[i__5].r, abs(d__5)) + (d__6 = d_imag(
                            &h__[j + j * h_dim1]), abs(d__6))) * (ascale *
                            atol)) {
                        ilazr2 = TRUE_;
                    }
/*<                END IF >*/
                }

/*              If both tests pass (1 & 2), i.e., the leading diagonal */
/*              element of B in the block is zero, split a 1x1 block off */
/*              at the top. (I.e., at the J-th row/column) The leading */
/*              diagonal element of the remainder can also be zero, so */
/*              this may have to be done repeatedly. */

/*<                IF( ILAZRO .OR. ILAZR2 ) THEN >*/
                if (ilazro || ilazr2) {
/*<                   DO 20 JCH = J, ILAST - 1 >*/
                    i__3 = ilast - 1;
                    for (jch = j; jch <= i__3; ++jch) {
/*<                      CTEMP = H( JCH, JCH ) >*/
                        i__4 = jch + jch * h_dim1;
                        ctemp.r = h__[i__4].r, ctemp.i = h__[i__4].i;
/*<    >*/
                        zlartg_(&ctemp, &h__[jch + 1 + jch * h_dim1], &c__, &
                                s, &h__[jch + jch * h_dim1]);
/*<                      H( JCH+1, JCH ) = CZERO >*/
                        i__4 = jch + 1 + jch * h_dim1;
                        h__[i__4].r = 0., h__[i__4].i = 0.;
/*<    >*/
                        i__4 = ilastm - jch;
                        zrot_(&i__4, &h__[jch + (jch + 1) * h_dim1], ldh, &
                                h__[jch + 1 + (jch + 1) * h_dim1], ldh, &c__,
                                &s);
/*<    >*/
                        i__4 = ilastm - jch;
                        zrot_(&i__4, &t[jch + (jch + 1) * t_dim1], ldt, &t[
                                jch + 1 + (jch + 1) * t_dim1], ldt, &c__, &s);
/*<    >*/
                        if (ilq) {
                            d_cnjg(&z__1, &s);
                            zrot_(n, &q[jch * q_dim1 + 1], &c__1, &q[(jch + 1)
                                     * q_dim1 + 1], &c__1, &c__, &z__1);
                        }
/*<    >*/
                        if (ilazr2) {
                            i__4 = jch + (jch - 1) * h_dim1;
                            i__5 = jch + (jch - 1) * h_dim1;
                            z__1.r = c__ * h__[i__5].r, z__1.i = c__ * h__[
                                    i__5].i;
                            h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
                        }
/*<                      ILAZR2 = .FALSE. >*/
                        ilazr2 = FALSE_;
/*<                      IF( ABS1( T( JCH+1, JCH+1 ) ).GE.BTOL ) THEN >*/
                        i__4 = jch + 1 + (jch + 1) * t_dim1;
                        if ((d__1 = t[i__4].r, abs(d__1)) + (d__2 = d_imag(&t[
                                jch + 1 + (jch + 1) * t_dim1]), abs(d__2)) >=
                                btol) {
/*<                         IF( JCH+1.GE.ILAST ) THEN >*/
                            if (jch + 1 >= ilast) {
/*<                            GO TO 60 >*/
                                goto L60;
/*<                         ELSE >*/
                            } else {
/*<                            IFIRST = JCH + 1 >*/
                                ifirst = jch + 1;
/*<                            GO TO 70 >*/
                                goto L70;
/*<                         END IF >*/
                            }
/*<                      END IF >*/
                        }
/*<                      T( JCH+1, JCH+1 ) = CZERO >*/
                        i__4 = jch + 1 + (jch + 1) * t_dim1;
                        t[i__4].r = 0., t[i__4].i = 0.;
/*<    20             CONTINUE >*/
/* L20: */
                    }
/*<                   GO TO 50 >*/
                    goto L50;
/*<                ELSE >*/
                } else {

/*                 Only test 2 passed -- chase the zero to T(ILAST,ILAST) */
/*                 Then process as in the case T(ILAST,ILAST)=0 */

/*<                   DO 30 JCH = J, ILAST - 1 >*/
                    i__3 = ilast - 1;
                    for (jch = j; jch <= i__3; ++jch) {
/*<                      CTEMP = T( JCH, JCH+1 ) >*/
                        i__4 = jch + (jch + 1) * t_dim1;
                        ctemp.r = t[i__4].r, ctemp.i = t[i__4].i;
/*<    >*/
                        zlartg_(&ctemp, &t[jch + 1 + (jch + 1) * t_dim1], &
                                c__, &s, &t[jch + (jch + 1) * t_dim1]);
/*<                      T( JCH+1, JCH+1 ) = CZERO >*/
                        i__4 = jch + 1 + (jch + 1) * t_dim1;
                        t[i__4].r = 0., t[i__4].i = 0.;
/*<    >*/
                        if (jch < ilastm - 1) {
                            i__4 = ilastm - jch - 1;
                            zrot_(&i__4, &t[jch + (jch + 2) * t_dim1], ldt, &
                                    t[jch + 1 + (jch + 2) * t_dim1], ldt, &
                                    c__, &s);
                        }
/*<    >*/
                        i__4 = ilastm - jch + 2;
                        zrot_(&i__4, &h__[jch + (jch - 1) * h_dim1], ldh, &
                                h__[jch + 1 + (jch - 1) * h_dim1], ldh, &c__,
                                &s);
/*<    >*/
                        if (ilq) {
                            d_cnjg(&z__1, &s);
                            zrot_(n, &q[jch * q_dim1 + 1], &c__1, &q[(jch + 1)
                                     * q_dim1 + 1], &c__1, &c__, &z__1);
                        }
/*<                      CTEMP = H( JCH+1, JCH ) >*/
                        i__4 = jch + 1 + jch * h_dim1;
                        ctemp.r = h__[i__4].r, ctemp.i = h__[i__4].i;
/*<    >*/
                        zlartg_(&ctemp, &h__[jch + 1 + (jch - 1) * h_dim1], &
                                c__, &s, &h__[jch + 1 + jch * h_dim1]);
/*<                      H( JCH+1, JCH-1 ) = CZERO >*/
                        i__4 = jch + 1 + (jch - 1) * h_dim1;
                        h__[i__4].r = 0., h__[i__4].i = 0.;
/*<    >*/
                        i__4 = jch + 1 - ifrstm;
                        zrot_(&i__4, &h__[ifrstm + jch * h_dim1], &c__1, &h__[
                                ifrstm + (jch - 1) * h_dim1], &c__1, &c__, &s)
                                ;
/*<    >*/
                        i__4 = jch - ifrstm;
                        zrot_(&i__4, &t[ifrstm + jch * t_dim1], &c__1, &t[
                                ifrstm + (jch - 1) * t_dim1], &c__1, &c__, &s)
                                ;
/*<    >*/
                        if (ilz) {
                            zrot_(n, &z__[jch * z_dim1 + 1], &c__1, &z__[(jch
                                    - 1) * z_dim1 + 1], &c__1, &c__, &s);
                        }
/*<    30             CONTINUE >*/
/* L30: */
                    }
/*<                   GO TO 50 >*/
                    goto L50;
/*<                END IF >*/
                }
/*<             ELSE IF( ILAZRO ) THEN >*/
            } else if (ilazro) {

/*              Only test 1 passed -- work on J:ILAST */

/*<                IFIRST = J >*/
                ifirst = j;
/*<                GO TO 70 >*/
                goto L70;
/*<             END IF >*/
            }

/*           Neither test passed -- try next J */

/*<    40    CONTINUE >*/
/* L40: */
        }

/*        (Drop-through is "impossible") */

/*<          INFO = 2*N + 1 >*/
        *info = (*n << 1) + 1;
/*<          GO TO 210 >*/
        goto L210;

/*        T(ILAST,ILAST)=0 -- clear H(ILAST,ILAST-1) to split off a */
/*        1x1 block. */

/*<    50    CONTINUE >*/
L50:
/*<          CTEMP = H( ILAST, ILAST ) >*/
        i__2 = ilast + ilast * h_dim1;
        ctemp.r = h__[i__2].r, ctemp.i = h__[i__2].i;
/*<    >*/
        zlartg_(&ctemp, &h__[ilast + (ilast - 1) * h_dim1], &c__, &s, &h__[
                ilast + ilast * h_dim1]);
/*<          H( ILAST, ILAST-1 ) = CZERO >*/
        i__2 = ilast + (ilast - 1) * h_dim1;
        h__[i__2].r = 0., h__[i__2].i = 0.;
/*<    >*/
        i__2 = ilast - ifrstm;
        zrot_(&i__2, &h__[ifrstm + ilast * h_dim1], &c__1, &h__[ifrstm + (
                ilast - 1) * h_dim1], &c__1, &c__, &s);
/*<    >*/
        i__2 = ilast - ifrstm;
        zrot_(&i__2, &t[ifrstm + ilast * t_dim1], &c__1, &t[ifrstm + (ilast -
                1) * t_dim1], &c__1, &c__, &s);
/*<    >*/
        if (ilz) {
            zrot_(n, &z__[ilast * z_dim1 + 1], &c__1, &z__[(ilast - 1) *
                    z_dim1 + 1], &c__1, &c__, &s);
        }

/*        H(ILAST,ILAST-1)=0 -- Standardize B, set ALPHA and BETA */

/*<    60    CONTINUE >*/
L60:
/*<          ABSB = ABS( T( ILAST, ILAST ) ) >*/
        absb = z_abs(&t[ilast + ilast * t_dim1]);
/*<          IF( ABSB.GT.SAFMIN ) THEN >*/
        if (absb > safmin) {
/*<             SIGNBC = DCONJG( T( ILAST, ILAST ) / ABSB ) >*/
            i__2 = ilast + ilast * t_dim1;
            z__2.r = t[i__2].r / absb, z__2.i = t[i__2].i / absb;
            d_cnjg(&z__1, &z__2);
            signbc.r = z__1.r, signbc.i = z__1.i;
/*<             T( ILAST, ILAST ) = ABSB >*/
            i__2 = ilast + ilast * t_dim1;
            t[i__2].r = absb, t[i__2].i = 0.;
/*<             IF( ILSCHR ) THEN >*/
            if (ilschr) {
/*<                CALL ZSCAL( ILAST-IFRSTM, SIGNBC, T( IFRSTM, ILAST ), 1 ) >*/
                i__2 = ilast - ifrstm;
                zscal_(&i__2, &signbc, &t[ifrstm + ilast * t_dim1], &c__1);
/*<    >*/
                i__2 = ilast + 1 - ifrstm;
                zscal_(&i__2, &signbc, &h__[ifrstm + ilast * h_dim1], &c__1);
/*<             ELSE >*/
            } else {
/*<                H( ILAST, ILAST ) = H( ILAST, ILAST )*SIGNBC >*/
                i__2 = ilast + ilast * h_dim1;
                i__3 = ilast + ilast * h_dim1;
                z__1.r = h__[i__3].r * signbc.r - h__[i__3].i * signbc.i,
                        z__1.i = h__[i__3].r * signbc.i + h__[i__3].i *
                        signbc.r;
                h__[i__2].r = z__1.r, h__[i__2].i = z__1.i;
/*<             END IF >*/
            }
/*<    >*/
            if (ilz) {
                zscal_(n, &signbc, &z__[ilast * z_dim1 + 1], &c__1);
            }
/*<          ELSE >*/
        } else {
/*<             T( ILAST, ILAST ) = CZERO >*/
            i__2 = ilast + ilast * t_dim1;
            t[i__2].r = 0., t[i__2].i = 0.;
/*<          END IF >*/
        }
/*<          ALPHA( ILAST ) = H( ILAST, ILAST ) >*/
        i__2 = ilast;
        i__3 = ilast + ilast * h_dim1;
        alpha[i__2].r = h__[i__3].r, alpha[i__2].i = h__[i__3].i;
/*<          BETA( ILAST ) = T( ILAST, ILAST ) >*/
        i__2 = ilast;
        i__3 = ilast + ilast * t_dim1;
        beta[i__2].r = t[i__3].r, beta[i__2].i = t[i__3].i;

/*        Go to next block -- exit if finished. */

/*<          ILAST = ILAST - 1 >*/
        --ilast;
/*<    >*/
        if (ilast < *ilo) {
            goto L190;
        }

/*        Reset counters */

/*<          IITER = 0 >*/
        iiter = 0;
/*<          ESHIFT = CZERO >*/
        eshift.r = 0., eshift.i = 0.;
/*<          IF( .NOT.ILSCHR ) THEN >*/
        if (! ilschr) {
/*<             ILASTM = ILAST >*/
            ilastm = ilast;
/*<    >*/
            if (ifrstm > ilast) {
                ifrstm = *ilo;
            }
/*<          END IF >*/
        }
/*<          GO TO 160 >*/
        goto L160;

/*        QZ step */

/*        This iteration only involves rows/columns IFIRST:ILAST.  We */
/*        assume IFIRST < ILAST, and that the diagonal of B is non-zero. */

/*<    70    CONTINUE >*/
L70:
/*<          IITER = IITER + 1 >*/
        ++iiter;
/*<          IF( .NOT.ILSCHR ) THEN >*/
        if (! ilschr) {
/*<             IFRSTM = IFIRST >*/
            ifrstm = ifirst;
/*<          END IF >*/
        }

/*        Compute the Shift. */

/*        At this point, IFIRST < ILAST, and the diagonal elements of */
/*        T(IFIRST:ILAST,IFIRST,ILAST) are larger than BTOL (in */
/*        magnitude) */

/*<          IF( ( IITER / 10 )*10.NE.IITER ) THEN >*/
        if (iiter / 10 * 10 != iiter) {

/*           The Wilkinson shift (AEP p.512), i.e., the eigenvalue of */
/*           the bottom-right 2x2 block of A inv(B) which is nearest to */
/*           the bottom-right element. */

/*           We factor B as U*D, where U has unit diagonals, and */
/*           compute (A*inv(D))*inv(U). */

/*<    >*/
            i__2 = ilast - 1 + ilast * t_dim1;
            z__2.r = bscale * t[i__2].r, z__2.i = bscale * t[i__2].i;
            i__3 = ilast + ilast * t_dim1;
            z__3.r = bscale * t[i__3].r, z__3.i = bscale * t[i__3].i;
            z_div(&z__1, &z__2, &z__3);
            u12.r = z__1.r, u12.i = z__1.i;
/*<    >*/
            i__2 = ilast - 1 + (ilast - 1) * h_dim1;
            z__2.r = ascale * h__[i__2].r, z__2.i = ascale * h__[i__2].i;
            i__3 = ilast - 1 + (ilast - 1) * t_dim1;
            z__3.r = bscale * t[i__3].r, z__3.i = bscale * t[i__3].i;
            z_div(&z__1, &z__2, &z__3);
            ad11.r = z__1.r, ad11.i = z__1.i;
/*<    >*/
            i__2 = ilast + (ilast - 1) * h_dim1;
            z__2.r = ascale * h__[i__2].r, z__2.i = ascale * h__[i__2].i;
            i__3 = ilast - 1 + (ilast - 1) * t_dim1;
            z__3.r = bscale * t[i__3].r, z__3.i = bscale * t[i__3].i;
            z_div(&z__1, &z__2, &z__3);
            ad21.r = z__1.r, ad21.i = z__1.i;
/*<    >*/
            i__2 = ilast - 1 + ilast * h_dim1;
            z__2.r = ascale * h__[i__2].r, z__2.i = ascale * h__[i__2].i;
            i__3 = ilast + ilast * t_dim1;
            z__3.r = bscale * t[i__3].r, z__3.i = bscale * t[i__3].i;
            z_div(&z__1, &z__2, &z__3);
            ad12.r = z__1.r, ad12.i = z__1.i;
/*<    >*/
            i__2 = ilast + ilast * h_dim1;
            z__2.r = ascale * h__[i__2].r, z__2.i = ascale * h__[i__2].i;
            i__3 = ilast + ilast * t_dim1;
            z__3.r = bscale * t[i__3].r, z__3.i = bscale * t[i__3].i;
            z_div(&z__1, &z__2, &z__3);
            ad22.r = z__1.r, ad22.i = z__1.i;
/*<             ABI22 = AD22 - U12*AD21 >*/
            z__2.r = u12.r * ad21.r - u12.i * ad21.i, z__2.i = u12.r * ad21.i
                    + u12.i * ad21.r;
            z__1.r = ad22.r - z__2.r, z__1.i = ad22.i - z__2.i;
            abi22.r = z__1.r, abi22.i = z__1.i;

/*<             T1 = HALF*( AD11+ABI22 ) >*/
            z__2.r = ad11.r + abi22.r, z__2.i = ad11.i + abi22.i;
            z__1.r = z__2.r * .5, z__1.i = z__2.i * .5;
            t1.r = z__1.r, t1.i = z__1.i;
/*<             RTDISC = SQRT( T1**2+AD12*AD21-AD11*AD22 ) >*/
            pow_zi(&z__4, &t1, &c__2);
            z__5.r = ad12.r * ad21.r - ad12.i * ad21.i, z__5.i = ad12.r *
                    ad21.i + ad12.i * ad21.r;
            z__3.r = z__4.r + z__5.r, z__3.i = z__4.i + z__5.i;
            z__6.r = ad11.r * ad22.r - ad11.i * ad22.i, z__6.i = ad11.r *
                    ad22.i + ad11.i * ad22.r;
            z__2.r = z__3.r - z__6.r, z__2.i = z__3.i - z__6.i;
            z_sqrt(&z__1, &z__2);
            rtdisc.r = z__1.r, rtdisc.i = z__1.i;
/*<    >*/
            z__1.r = t1.r - abi22.r, z__1.i = t1.i - abi22.i;
            z__2.r = t1.r - abi22.r, z__2.i = t1.i - abi22.i;
            temp = z__1.r * rtdisc.r + d_imag(&z__2) * d_imag(&rtdisc);
/*<             IF( TEMP.LE.ZERO ) THEN >*/
            if (temp <= 0.) {
/*<                SHIFT = T1 + RTDISC >*/
                z__1.r = t1.r + rtdisc.r, z__1.i = t1.i + rtdisc.i;
                shift.r = z__1.r, shift.i = z__1.i;
/*<             ELSE >*/
            } else {
/*<                SHIFT = T1 - RTDISC >*/
                z__1.r = t1.r - rtdisc.r, z__1.i = t1.i - rtdisc.i;
                shift.r = z__1.r, shift.i = z__1.i;
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {

/*           Exceptional shift.  Chosen for no particularly good reason. */

/*<    >*/
            i__2 = ilast - 1 + ilast * h_dim1;
            z__4.r = ascale * h__[i__2].r, z__4.i = ascale * h__[i__2].i;
            i__3 = ilast - 1 + (ilast - 1) * t_dim1;
            z__5.r = bscale * t[i__3].r, z__5.i = bscale * t[i__3].i;
            z_div(&z__3, &z__4, &z__5);
            d_cnjg(&z__2, &z__3);
            z__1.r = eshift.r + z__2.r, z__1.i = eshift.i + z__2.i;
            eshift.r = z__1.r, eshift.i = z__1.i;
/*<             SHIFT = ESHIFT >*/
            shift.r = eshift.r, shift.i = eshift.i;
/*<          END IF >*/
        }

/*        Now check for two consecutive small subdiagonals. */

/*<          DO 80 J = ILAST - 1, IFIRST + 1, -1 >*/
        i__2 = ifirst + 1;
        for (j = ilast - 1; j >= i__2; --j) {
/*<             ISTART = J >*/
            istart = j;
/*<             CTEMP = ASCALE*H( J, J ) - SHIFT*( BSCALE*T( J, J ) ) >*/
            i__3 = j + j * h_dim1;
            z__2.r = ascale * h__[i__3].r, z__2.i = ascale * h__[i__3].i;
            i__4 = j + j * t_dim1;
            z__4.r = bscale * t[i__4].r, z__4.i = bscale * t[i__4].i;
            z__3.r = shift.r * z__4.r - shift.i * z__4.i, z__3.i = shift.r *
                    z__4.i + shift.i * z__4.r;
            z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
            ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<             TEMP = ABS1( CTEMP ) >*/
            temp = (d__1 = ctemp.r, abs(d__1)) + (d__2 = d_imag(&ctemp), abs(
                    d__2));
/*<             TEMP2 = ASCALE*ABS1( H( J+1, J ) ) >*/
            i__3 = j + 1 + j * h_dim1;
            temp2 = ascale * ((d__1 = h__[i__3].r, abs(d__1)) + (d__2 =
                    d_imag(&h__[j + 1 + j * h_dim1]), abs(d__2)));
/*<             TEMPR = MAX( TEMP, TEMP2 ) >*/
            tempr = max(temp,temp2);
/*<             IF( TEMPR.LT.ONE .AND. TEMPR.NE.ZERO ) THEN >*/
            if (tempr < 1. && tempr != 0.) {
/*<                TEMP = TEMP / TEMPR >*/
                temp /= tempr;
/*<                TEMP2 = TEMP2 / TEMPR >*/
                temp2 /= tempr;
/*<             END IF >*/
            }
/*<    >*/
            i__3 = j + (j - 1) * h_dim1;
            if (((d__1 = h__[i__3].r, abs(d__1)) + (d__2 = d_imag(&h__[j + (j
                    - 1) * h_dim1]), abs(d__2))) * temp2 <= temp * atol) {
                goto L90;
            }
/*<    80    CONTINUE >*/
/* L80: */
        }

/*<          ISTART = IFIRST >*/
        istart = ifirst;
/*<    >*/
        i__2 = ifirst + ifirst * h_dim1;
        z__2.r = ascale * h__[i__2].r, z__2.i = ascale * h__[i__2].i;
        i__3 = ifirst + ifirst * t_dim1;
        z__4.r = bscale * t[i__3].r, z__4.i = bscale * t[i__3].i;
        z__3.r = shift.r * z__4.r - shift.i * z__4.i, z__3.i = shift.r *
                z__4.i + shift.i * z__4.r;
        z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
        ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<    90    CONTINUE >*/
L90:

/*        Do an implicit-shift QZ sweep. */

/*        Initial Q */

/*<          CTEMP2 = ASCALE*H( ISTART+1, ISTART ) >*/
        i__2 = istart + 1 + istart * h_dim1;
        z__1.r = ascale * h__[i__2].r, z__1.i = ascale * h__[i__2].i;
        ctemp2.r = z__1.r, ctemp2.i = z__1.i;
/*<          CALL ZLARTG( CTEMP, CTEMP2, C, S, CTEMP3 ) >*/
        zlartg_(&ctemp, &ctemp2, &c__, &s, &ctemp3);

/*        Sweep */

/*<          DO 150 J = ISTART, ILAST - 1 >*/
        i__2 = ilast - 1;
        for (j = istart; j <= i__2; ++j) {
/*<             IF( J.GT.ISTART ) THEN >*/
            if (j > istart) {
/*<                CTEMP = H( J, J-1 ) >*/
                i__3 = j + (j - 1) * h_dim1;
                ctemp.r = h__[i__3].r, ctemp.i = h__[i__3].i;
/*<                CALL ZLARTG( CTEMP, H( J+1, J-1 ), C, S, H( J, J-1 ) ) >*/
                zlartg_(&ctemp, &h__[j + 1 + (j - 1) * h_dim1], &c__, &s, &
                        h__[j + (j - 1) * h_dim1]);
/*<                H( J+1, J-1 ) = CZERO >*/
                i__3 = j + 1 + (j - 1) * h_dim1;
                h__[i__3].r = 0., h__[i__3].i = 0.;
/*<             END IF >*/
            }

/*<             DO 100 JC = J, ILASTM >*/
            i__3 = ilastm;
            for (jc = j; jc <= i__3; ++jc) {
/*<                CTEMP = C*H( J, JC ) + S*H( J+1, JC ) >*/
                i__4 = j + jc * h_dim1;
                z__2.r = c__ * h__[i__4].r, z__2.i = c__ * h__[i__4].i;
                i__5 = j + 1 + jc * h_dim1;
                z__3.r = s.r * h__[i__5].r - s.i * h__[i__5].i, z__3.i = s.r *
                         h__[i__5].i + s.i * h__[i__5].r;
                z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<                H( J+1, JC ) = -DCONJG( S )*H( J, JC ) + C*H( J+1, JC ) >*/
                i__4 = j + 1 + jc * h_dim1;
                d_cnjg(&z__4, &s);
                z__3.r = -z__4.r, z__3.i = -z__4.i;
                i__5 = j + jc * h_dim1;
                z__2.r = z__3.r * h__[i__5].r - z__3.i * h__[i__5].i, z__2.i =
                         z__3.r * h__[i__5].i + z__3.i * h__[i__5].r;
                i__6 = j + 1 + jc * h_dim1;
                z__5.r = c__ * h__[i__6].r, z__5.i = c__ * h__[i__6].i;
                z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
                h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
/*<                H( J, JC ) = CTEMP >*/
                i__4 = j + jc * h_dim1;
                h__[i__4].r = ctemp.r, h__[i__4].i = ctemp.i;
/*<                CTEMP2 = C*T( J, JC ) + S*T( J+1, JC ) >*/
                i__4 = j + jc * t_dim1;
                z__2.r = c__ * t[i__4].r, z__2.i = c__ * t[i__4].i;
                i__5 = j + 1 + jc * t_dim1;
                z__3.r = s.r * t[i__5].r - s.i * t[i__5].i, z__3.i = s.r * t[
                        i__5].i + s.i * t[i__5].r;
                z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                ctemp2.r = z__1.r, ctemp2.i = z__1.i;
/*<                T( J+1, JC ) = -DCONJG( S )*T( J, JC ) + C*T( J+1, JC ) >*/
                i__4 = j + 1 + jc * t_dim1;
                d_cnjg(&z__4, &s);
                z__3.r = -z__4.r, z__3.i = -z__4.i;
                i__5 = j + jc * t_dim1;
                z__2.r = z__3.r * t[i__5].r - z__3.i * t[i__5].i, z__2.i =
                        z__3.r * t[i__5].i + z__3.i * t[i__5].r;
                i__6 = j + 1 + jc * t_dim1;
                z__5.r = c__ * t[i__6].r, z__5.i = c__ * t[i__6].i;
                z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
                t[i__4].r = z__1.r, t[i__4].i = z__1.i;
/*<                T( J, JC ) = CTEMP2 >*/
                i__4 = j + jc * t_dim1;
                t[i__4].r = ctemp2.r, t[i__4].i = ctemp2.i;
/*<   100       CONTINUE >*/
/* L100: */
            }
/*<             IF( ILQ ) THEN >*/
            if (ilq) {
/*<                DO 110 JR = 1, N >*/
                i__3 = *n;
                for (jr = 1; jr <= i__3; ++jr) {
/*<                   CTEMP = C*Q( JR, J ) + DCONJG( S )*Q( JR, J+1 ) >*/
                    i__4 = jr + j * q_dim1;
                    z__2.r = c__ * q[i__4].r, z__2.i = c__ * q[i__4].i;
                    d_cnjg(&z__4, &s);
                    i__5 = jr + (j + 1) * q_dim1;
                    z__3.r = z__4.r * q[i__5].r - z__4.i * q[i__5].i, z__3.i =
                             z__4.r * q[i__5].i + z__4.i * q[i__5].r;
                    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                    ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<                   Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 ) >*/
                    i__4 = jr + (j + 1) * q_dim1;
                    z__3.r = -s.r, z__3.i = -s.i;
                    i__5 = jr + j * q_dim1;
                    z__2.r = z__3.r * q[i__5].r - z__3.i * q[i__5].i, z__2.i =
                             z__3.r * q[i__5].i + z__3.i * q[i__5].r;
                    i__6 = jr + (j + 1) * q_dim1;
                    z__4.r = c__ * q[i__6].r, z__4.i = c__ * q[i__6].i;
                    z__1.r = z__2.r + z__4.r, z__1.i = z__2.i + z__4.i;
                    q[i__4].r = z__1.r, q[i__4].i = z__1.i;
/*<                   Q( JR, J ) = CTEMP >*/
                    i__4 = jr + j * q_dim1;
                    q[i__4].r = ctemp.r, q[i__4].i = ctemp.i;
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<             END IF >*/
            }

/*<             CTEMP = T( J+1, J+1 ) >*/
            i__3 = j + 1 + (j + 1) * t_dim1;
            ctemp.r = t[i__3].r, ctemp.i = t[i__3].i;
/*<             CALL ZLARTG( CTEMP, T( J+1, J ), C, S, T( J+1, J+1 ) ) >*/
            zlartg_(&ctemp, &t[j + 1 + j * t_dim1], &c__, &s, &t[j + 1 + (j +
                    1) * t_dim1]);
/*<             T( J+1, J ) = CZERO >*/
            i__3 = j + 1 + j * t_dim1;
            t[i__3].r = 0., t[i__3].i = 0.;

/*<             DO 120 JR = IFRSTM, MIN( J+2, ILAST ) >*/
/* Computing MIN */
            i__4 = j + 2;
            i__3 = min(i__4,ilast);
            for (jr = ifrstm; jr <= i__3; ++jr) {
/*<                CTEMP = C*H( JR, J+1 ) + S*H( JR, J ) >*/
                i__4 = jr + (j + 1) * h_dim1;
                z__2.r = c__ * h__[i__4].r, z__2.i = c__ * h__[i__4].i;
                i__5 = jr + j * h_dim1;
                z__3.r = s.r * h__[i__5].r - s.i * h__[i__5].i, z__3.i = s.r *
                         h__[i__5].i + s.i * h__[i__5].r;
                z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<                H( JR, J ) = -DCONJG( S )*H( JR, J+1 ) + C*H( JR, J ) >*/
                i__4 = jr + j * h_dim1;
                d_cnjg(&z__4, &s);
                z__3.r = -z__4.r, z__3.i = -z__4.i;
                i__5 = jr + (j + 1) * h_dim1;
                z__2.r = z__3.r * h__[i__5].r - z__3.i * h__[i__5].i, z__2.i =
                         z__3.r * h__[i__5].i + z__3.i * h__[i__5].r;
                i__6 = jr + j * h_dim1;
                z__5.r = c__ * h__[i__6].r, z__5.i = c__ * h__[i__6].i;
                z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
                h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
/*<                H( JR, J+1 ) = CTEMP >*/
                i__4 = jr + (j + 1) * h_dim1;
                h__[i__4].r = ctemp.r, h__[i__4].i = ctemp.i;
/*<   120       CONTINUE >*/
/* L120: */
            }
/*<             DO 130 JR = IFRSTM, J >*/
            i__3 = j;
            for (jr = ifrstm; jr <= i__3; ++jr) {
/*<                CTEMP = C*T( JR, J+1 ) + S*T( JR, J ) >*/
                i__4 = jr + (j + 1) * t_dim1;
                z__2.r = c__ * t[i__4].r, z__2.i = c__ * t[i__4].i;
                i__5 = jr + j * t_dim1;
                z__3.r = s.r * t[i__5].r - s.i * t[i__5].i, z__3.i = s.r * t[
                        i__5].i + s.i * t[i__5].r;
                z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<                T( JR, J ) = -DCONJG( S )*T( JR, J+1 ) + C*T( JR, J ) >*/
                i__4 = jr + j * t_dim1;
                d_cnjg(&z__4, &s);
                z__3.r = -z__4.r, z__3.i = -z__4.i;
                i__5 = jr + (j + 1) * t_dim1;
                z__2.r = z__3.r * t[i__5].r - z__3.i * t[i__5].i, z__2.i =
                        z__3.r * t[i__5].i + z__3.i * t[i__5].r;
                i__6 = jr + j * t_dim1;
                z__5.r = c__ * t[i__6].r, z__5.i = c__ * t[i__6].i;
                z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
                t[i__4].r = z__1.r, t[i__4].i = z__1.i;
/*<                T( JR, J+1 ) = CTEMP >*/
                i__4 = jr + (j + 1) * t_dim1;
                t[i__4].r = ctemp.r, t[i__4].i = ctemp.i;
/*<   130       CONTINUE >*/
/* L130: */
            }
/*<             IF( ILZ ) THEN >*/
            if (ilz) {
/*<                DO 140 JR = 1, N >*/
                i__3 = *n;
                for (jr = 1; jr <= i__3; ++jr) {
/*<                   CTEMP = C*Z( JR, J+1 ) + S*Z( JR, J ) >*/
                    i__4 = jr + (j + 1) * z_dim1;
                    z__2.r = c__ * z__[i__4].r, z__2.i = c__ * z__[i__4].i;
                    i__5 = jr + j * z_dim1;
                    z__3.r = s.r * z__[i__5].r - s.i * z__[i__5].i, z__3.i =
                            s.r * z__[i__5].i + s.i * z__[i__5].r;
                    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                    ctemp.r = z__1.r, ctemp.i = z__1.i;
/*<                   Z( JR, J ) = -DCONJG( S )*Z( JR, J+1 ) + C*Z( JR, J ) >*/
                    i__4 = jr + j * z_dim1;
                    d_cnjg(&z__4, &s);
                    z__3.r = -z__4.r, z__3.i = -z__4.i;
                    i__5 = jr + (j + 1) * z_dim1;
                    z__2.r = z__3.r * z__[i__5].r - z__3.i * z__[i__5].i,
                            z__2.i = z__3.r * z__[i__5].i + z__3.i * z__[i__5]
                            .r;
                    i__6 = jr + j * z_dim1;
                    z__5.r = c__ * z__[i__6].r, z__5.i = c__ * z__[i__6].i;
                    z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
                    z__[i__4].r = z__1.r, z__[i__4].i = z__1.i;
/*<                   Z( JR, J+1 ) = CTEMP >*/
                    i__4 = jr + (j + 1) * z_dim1;
                    z__[i__4].r = ctemp.r, z__[i__4].i = ctemp.i;
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<             END IF >*/
            }
/*<   150    CONTINUE >*/
/* L150: */
        }

/*<   160    CONTINUE >*/
L160:

/*<   170 CONTINUE >*/
/* L170: */
        ;
    }

/*     Drop-through = non-convergence */

/*<   180 CONTINUE >*/
L180:
/*<       INFO = ILAST >*/
    *info = ilast;
/*<       GO TO 210 >*/
    goto L210;

/*     Successful completion of all QZ steps */

/*<   190 CONTINUE >*/
L190:

/*     Set Eigenvalues 1:ILO-1 */

/*<       DO 200 J = 1, ILO - 1 >*/
    i__1 = *ilo - 1;
    for (j = 1; j <= i__1; ++j) {
/*<          ABSB = ABS( T( J, J ) ) >*/
        absb = z_abs(&t[j + j * t_dim1]);
/*<          IF( ABSB.GT.SAFMIN ) THEN >*/
        if (absb > safmin) {
/*<             SIGNBC = DCONJG( T( J, J ) / ABSB ) >*/
            i__2 = j + j * t_dim1;
            z__2.r = t[i__2].r / absb, z__2.i = t[i__2].i / absb;
            d_cnjg(&z__1, &z__2);
            signbc.r = z__1.r, signbc.i = z__1.i;
/*<             T( J, J ) = ABSB >*/
            i__2 = j + j * t_dim1;
            t[i__2].r = absb, t[i__2].i = 0.;
/*<             IF( ILSCHR ) THEN >*/
            if (ilschr) {
/*<                CALL ZSCAL( J-1, SIGNBC, T( 1, J ), 1 ) >*/
                i__2 = j - 1;
                zscal_(&i__2, &signbc, &t[j * t_dim1 + 1], &c__1);
/*<                CALL ZSCAL( J, SIGNBC, H( 1, J ), 1 ) >*/
                zscal_(&j, &signbc, &h__[j * h_dim1 + 1], &c__1);
/*<             ELSE >*/
            } else {
/*<                H( J, J ) = H( J, J )*SIGNBC >*/
                i__2 = j + j * h_dim1;
                i__3 = j + j * h_dim1;
                z__1.r = h__[i__3].r * signbc.r - h__[i__3].i * signbc.i,
                        z__1.i = h__[i__3].r * signbc.i + h__[i__3].i *
                        signbc.r;
                h__[i__2].r = z__1.r, h__[i__2].i = z__1.i;
/*<             END IF >*/
            }
/*<    >*/
            if (ilz) {
                zscal_(n, &signbc, &z__[j * z_dim1 + 1], &c__1);
            }
/*<          ELSE >*/
        } else {
/*<             T( J, J ) = CZERO >*/
            i__2 = j + j * t_dim1;
            t[i__2].r = 0., t[i__2].i = 0.;
/*<          END IF >*/
        }
/*<          ALPHA( J ) = H( J, J ) >*/
        i__2 = j;
        i__3 = j + j * h_dim1;
        alpha[i__2].r = h__[i__3].r, alpha[i__2].i = h__[i__3].i;
/*<          BETA( J ) = T( J, J ) >*/
        i__2 = j;
        i__3 = j + j * t_dim1;
        beta[i__2].r = t[i__3].r, beta[i__2].i = t[i__3].i;
/*<   200 CONTINUE >*/
/* L200: */
    }

/*     Normal Termination */

/*<       INFO = 0 >*/
    *info = 0;

/*     Exit (other than argument error) -- return optimal workspace size */

/*<   210 CONTINUE >*/
L210:
/*<       WORK( 1 ) = DCMPLX( N ) >*/
    z__1.r = (doublereal) (*n), z__1.i = 0.;
    work[1].r = z__1.r, work[1].i = z__1.i;
/*<       RETURN >*/
    return 0;

/*     End of ZHGEQZ */

/*<       END >*/
} /* zhgeqz_ */

#ifdef __cplusplus
        }
#endif
