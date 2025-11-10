/* lapack/complex16/zgehrd.f -- translated by f2c (version 20050501).
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

static doublecomplex c_b2 = {1.,0.};
static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;
static integer c__65 = 65;

/*<       SUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO ) >*/
/* Subroutine */ int zgehrd_(integer *n, integer *ilo, integer *ihi,
        doublecomplex *a, integer *lda, doublecomplex *tau, doublecomplex *
        work, integer *lwork, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublecomplex z__1;

    /* Local variables */
    integer i__;
    doublecomplex t[4160]       /* was [65][64] */;
    integer ib;
    doublecomplex ei;
    integer nb, nh, nx=0, iws, nbmin, iinfo;
    extern /* Subroutine */ int zgemm_(char *, char *, integer *, integer *,
            integer *, doublecomplex *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, ftnlen, ftnlen), zgehd2_(integer *, integer *, integer
            *, doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *), xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zlarfb_(char *, char *, char *, char *,
            integer *, integer *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, ftnlen, ftnlen, ftnlen, ftnlen),
            zlahrd_(integer *, integer *, integer *, doublecomplex *, integer
            *, doublecomplex *, doublecomplex *, integer *, doublecomplex *,
            integer *);
    integer ldwork, lwkopt;
    logical lquery;


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            IHI, ILO, INFO, LDA, LWORK, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEHRD reduces a complex general matrix A to upper Hessenberg form H */
/*  by a unitary similarity transformation:  Q' * A * Q = H . */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          It is assumed that A is already upper triangular in rows */
/*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally */
/*          set by a previous call to ZGEBAL; otherwise they should be */
/*          set to 1 and N respectively. See Further Details. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the N-by-N general matrix to be reduced. */
/*          On exit, the upper triangle and the first subdiagonal of A */
/*          are overwritten with the upper Hessenberg matrix H, and the */
/*          elements below the first subdiagonal, with the array TAU, */
/*          represent the unitary matrix Q as a product of elementary */
/*          reflectors. See Further Details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  TAU     (output) COMPLEX*16 array, dimension (N-1) */
/*          The scalar factors of the elementary reflectors (see Further */
/*          Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to */
/*          zero. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The length of the array WORK.  LWORK >= max(1,N). */
/*          For optimum performance LWORK >= N*NB, where NB is the */
/*          optimal blocksize. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of (ihi-ilo) elementary */
/*  reflectors */

/*     Q = H(ilo) H(ilo+1) . . . H(ihi-1). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a complex scalar, and v is a complex vector with */
/*  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on */
/*  exit in A(i+2:ihi,i), and tau in TAU(i). */

/*  The contents of A are illustrated by the following example, with */
/*  n = 7, ilo = 2 and ihi = 6: */

/*  on entry,                        on exit, */

/*  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a ) */
/*  (     a   a   a   a   a   a )    (      a   h   h   h   h   a ) */
/*  (     a   a   a   a   a   a )    (      h   h   h   h   h   h ) */
/*  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h ) */
/*  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h ) */
/*  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h ) */
/*  (                         a )    (                          a ) */

/*  where a denotes an element of the original matrix A, h denotes a */
/*  modified element of the upper Hessenberg matrix H, and vi denotes an */
/*  element of the vector defining H(i). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER            NBMAX, LDT >*/
/*<       PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 ) >*/
/*<       COMPLEX*16         ZERO, ONE >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY >*/
/*<    >*/
/*<       COMPLEX*16         EI >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       COMPLEX*16         T( LDT, NBMAX ) >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZGEHD2, ZGEMM, ZLAHRD, ZLARFB >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            ILAENV >*/
/*<       EXTERNAL           ILAENV >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --tau;
    --work;

    /* Function Body */
    *info = 0;
/*<       NB = MIN( NBMAX, ILAENV( 1, 'ZGEHRD', ' ', N, ILO, IHI, -1 ) ) >*/
/* Computing MIN */
    i__1 = 64, i__2 = ilaenv_(&c__1, "ZGEHRD", " ", n, ilo, ihi, &c_n1, (
            ftnlen)6, (ftnlen)1);
    nb = min(i__1,i__2);
/*<       LWKOPT = N*NB >*/
    lwkopt = *n * nb;
/*<       WORK( 1 ) = LWKOPT >*/
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       IF( N.LT.0 ) THEN >*/
    if (*n < 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN >*/
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN >*/
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,*n) && ! lquery) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEHRD', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEHRD", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Set elements 1:ILO-1 and IHI:N-1 of TAU to zero */

/*<       DO 10 I = 1, ILO - 1 >*/
    i__1 = *ilo - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          TAU( I ) = ZERO >*/
        i__2 = i__;
        tau[i__2].r = 0., tau[i__2].i = 0.;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       DO 20 I = MAX( 1, IHI ), N - 1 >*/
    i__1 = *n - 1;
    for (i__ = max(1,*ihi); i__ <= i__1; ++i__) {
/*<          TAU( I ) = ZERO >*/
        i__2 = i__;
        tau[i__2].r = 0., tau[i__2].i = 0.;
/*<    20 CONTINUE >*/
/* L20: */
    }

/*     Quick return if possible */

/*<       NH = IHI - ILO + 1 >*/
    nh = *ihi - *ilo + 1;
/*<       IF( NH.LE.1 ) THEN >*/
    if (nh <= 1) {
/*<          WORK( 1 ) = 1 >*/
        work[1].r = 1., work[1].i = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       NBMIN = 2 >*/
    nbmin = 2;
/*<       IWS = 1 >*/
    iws = 1;
/*<       IF( NB.GT.1 .AND. NB.LT.NH ) THEN >*/
    if (nb > 1 && nb < nh) {

/*        Determine when to cross over from blocked to unblocked code */
/*        (last block is always handled by unblocked code). */

/*<          NX = MAX( NB, ILAENV( 3, 'ZGEHRD', ' ', N, ILO, IHI, -1 ) ) >*/
/* Computing MAX */
        i__1 = nb, i__2 = ilaenv_(&c__3, "ZGEHRD", " ", n, ilo, ihi, &c_n1, (
                ftnlen)6, (ftnlen)1);
        nx = max(i__1,i__2);
/*<          IF( NX.LT.NH ) THEN >*/
        if (nx < nh) {

/*           Determine if workspace is large enough for blocked code. */

/*<             IWS = N*NB >*/
            iws = *n * nb;
/*<             IF( LWORK.LT.IWS ) THEN >*/
            if (*lwork < iws) {

/*              Not enough workspace to use optimal NB:  determine the */
/*              minimum value of NB, and reduce NB or force use of */
/*              unblocked code. */

/*<    >*/
/* Computing MAX */
                i__1 = 2, i__2 = ilaenv_(&c__2, "ZGEHRD", " ", n, ilo, ihi, &
                        c_n1, (ftnlen)6, (ftnlen)1);
                nbmin = max(i__1,i__2);
/*<                IF( LWORK.GE.N*NBMIN ) THEN >*/
                if (*lwork >= *n * nbmin) {
/*<                   NB = LWORK / N >*/
                    nb = *lwork / *n;
/*<                ELSE >*/
                } else {
/*<                   NB = 1 >*/
                    nb = 1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       LDWORK = N >*/
    ldwork = *n;

/*<       IF( NB.LT.NBMIN .OR. NB.GE.NH ) THEN >*/
    if (nb < nbmin || nb >= nh) {

/*        Use unblocked code below */

/*<          I = ILO >*/
        i__ = *ilo;

/*<       ELSE >*/
    } else {

/*        Use blocked code */

/*<          DO 30 I = ILO, IHI - 1 - NX, NB >*/
        i__1 = *ihi - 1 - nx;
        i__2 = nb;
        for (i__ = *ilo; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<             IB = MIN( NB, IHI-I ) >*/
/* Computing MIN */
            i__3 = nb, i__4 = *ihi - i__;
            ib = min(i__3,i__4);

/*           Reduce columns i:i+ib-1 to Hessenberg form, returning the */
/*           matrices V and T of the block reflector H = I - V*T*V' */
/*           which performs the reduction, and also the matrix Y = A*V*T */

/*<    >*/
            zlahrd_(ihi, &i__, &ib, &a[i__ * a_dim1 + 1], lda, &tau[i__], t, &
                    c__65, &work[1], &ldwork);

/*           Apply the block reflector H to A(1:ihi,i+ib:ihi) from the */
/*           right, computing  A := A - Y * V'. V(i+ib,ib-1) must be set */
/*           to 1. */

/*<             EI = A( I+IB, I+IB-1 ) >*/
            i__3 = i__ + ib + (i__ + ib - 1) * a_dim1;
            ei.r = a[i__3].r, ei.i = a[i__3].i;
/*<             A( I+IB, I+IB-1 ) = ONE >*/
            i__3 = i__ + ib + (i__ + ib - 1) * a_dim1;
            a[i__3].r = 1., a[i__3].i = 0.;
/*<    >*/
            i__3 = *ihi - i__ - ib + 1;
            z__1.r = -1., z__1.i = -0.;
            zgemm_("No transpose", "Conjugate transpose", ihi, &i__3, &ib, &
                    z__1, &work[1], &ldwork, &a[i__ + ib + i__ * a_dim1], lda,
                     &c_b2, &a[(i__ + ib) * a_dim1 + 1], lda, (ftnlen)12, (
                    ftnlen)19);
/*<             A( I+IB, I+IB-1 ) = EI >*/
            i__3 = i__ + ib + (i__ + ib - 1) * a_dim1;
            a[i__3].r = ei.r, a[i__3].i = ei.i;

/*           Apply the block reflector H to A(i+1:ihi,i+ib:n) from the */
/*           left */

/*<    >*/
            i__3 = *ihi - i__;
            i__4 = *n - i__ - ib + 1;
            zlarfb_("Left", "Conjugate transpose", "Forward", "Columnwise", &
                    i__3, &i__4, &ib, &a[i__ + 1 + i__ * a_dim1], lda, t, &
                    c__65, &a[i__ + 1 + (i__ + ib) * a_dim1], lda, &work[1], &
                    ldwork, (ftnlen)4, (ftnlen)19, (ftnlen)7, (ftnlen)10);
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<       END IF >*/
    }

/*     Use unblocked code to reduce the rest of the matrix */

/*<       CALL ZGEHD2( N, I, IHI, A, LDA, TAU, WORK, IINFO ) >*/
    zgehd2_(n, &i__, ihi, &a[a_offset], lda, &tau[1], &work[1], &iinfo);
/*<       WORK( 1 ) = IWS >*/
    work[1].r = (doublereal) iws, work[1].i = 0.;

/*<       RETURN >*/
    return 0;

/*     End of ZGEHRD */

/*<       END >*/
} /* zgehrd_ */

#ifdef __cplusplus
        }
#endif
