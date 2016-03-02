/* lapack/complex16/zgeqrf.f -- translated by f2c (version 20090411).
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
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;

/*<       SUBROUTINE ZGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO ) >*/
/* Subroutine */ int zgeqrf_(integer *m, integer *n, doublecomplex *a,
        integer *lda, doublecomplex *tau, doublecomplex *work, integer *lwork,
         integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    integer i__, k, ib, nb, nx, iws, nbmin, iinfo;
    extern /* Subroutine */ int zgeqr2_(integer *, integer *, doublecomplex *,
             integer *, doublecomplex *, doublecomplex *, integer *), xerbla_(
            char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zlarfb_(char *, char *, char *, char *,
            integer *, integer *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, ftnlen, ftnlen, ftnlen, ftnlen);
    integer ldwork;
    extern /* Subroutine */ int zlarft_(char *, char *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, ftnlen, ftnlen);
    integer lwkopt;
    logical lquery;


/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, LDA, LWORK, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEQRF computes a QR factorization of a complex M-by-N matrix A: */
/*  A = Q * R. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the M-by-N matrix A. */
/*          On exit, the elements on and above the diagonal of the array */
/*          contain the min(M,N)-by-N upper trapezoidal matrix R (R is */
/*          upper triangular if m >= n); the elements below the diagonal, */
/*          with the array TAU, represent the unitary matrix Q as a */
/*          product of min(m,n) elementary reflectors (see Further */
/*          Details). */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  TAU     (output) COMPLEX*16 array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors (see Further */
/*          Details). */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK)) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,N). */
/*          For optimum performance LWORK >= N*NB, where NB is */
/*          the optimal blocksize. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of elementary reflectors */

/*     Q = H(1) H(2) . . . H(k), where k = min(m,n). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a complex scalar, and v is a complex vector with */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i), */
/*  and tau in TAU(i). */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY >*/
/*<    >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZGEQR2, ZLARFB, ZLARFT >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            ILAENV >*/
/*<       EXTERNAL           ILAENV >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --tau;
    --work;

    /* Function Body */
    *info = 0;
/*<       NB = ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 ) >*/
    nb = ilaenv_(&c__1, "ZGEQRF", " ", m, n, &c_n1, &c_n1, (ftnlen)6, (ftnlen)
            1);
/*<       LWKOPT = N*NB >*/
    lwkopt = *n * nb;
/*<       WORK( 1 ) = LWKOPT >*/
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
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
/*<       ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,*n) && ! lquery) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEQRF', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEQRF", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       K = MIN( M, N ) >*/
    k = min(*m,*n);
/*<       IF( K.EQ.0 ) THEN >*/
    if (k == 0) {
/*<          WORK( 1 ) = 1 >*/
        work[1].r = 1., work[1].i = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       NBMIN = 2 >*/
    nbmin = 2;
/*<       NX = 0 >*/
    nx = 0;
/*<       IWS = N >*/
    iws = *n;
/*<       IF( NB.GT.1 .AND. NB.LT.K ) THEN >*/
    if (nb > 1 && nb < k) {

/*        Determine when to cross over from blocked to unblocked code. */

/*<          NX = MAX( 0, ILAENV( 3, 'ZGEQRF', ' ', M, N, -1, -1 ) ) >*/
/* Computing MAX */
        i__1 = 0, i__2 = ilaenv_(&c__3, "ZGEQRF", " ", m, n, &c_n1, &c_n1, (
                ftnlen)6, (ftnlen)1);
        nx = max(i__1,i__2);
/*<          IF( NX.LT.K ) THEN >*/
        if (nx < k) {

/*           Determine if workspace is large enough for blocked code. */

/*<             LDWORK = N >*/
            ldwork = *n;
/*<             IWS = LDWORK*NB >*/
            iws = ldwork * nb;
/*<             IF( LWORK.LT.IWS ) THEN >*/
            if (*lwork < iws) {

/*              Not enough workspace to use optimal NB:  reduce NB and */
/*              determine the minimum value of NB. */

/*<                NB = LWORK / LDWORK >*/
                nb = *lwork / ldwork;
/*<    >*/
/* Computing MAX */
                i__1 = 2, i__2 = ilaenv_(&c__2, "ZGEQRF", " ", m, n, &c_n1, &
                        c_n1, (ftnlen)6, (ftnlen)1);
                nbmin = max(i__1,i__2);
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN >*/
    if (nb >= nbmin && nb < k && nx < k) {

/*        Use blocked code initially */

/*<          DO 10 I = 1, K - NX, NB >*/
        i__1 = k - nx;
        i__2 = nb;
        for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<             IB = MIN( K-I+1, NB ) >*/
/* Computing MIN */
            i__3 = k - i__ + 1;
            ib = min(i__3,nb);

/*           Compute the QR factorization of the current block */
/*           A(i:m,i:i+ib-1) */

/*<    >*/
            i__3 = *m - i__ + 1;
            zgeqr2_(&i__3, &ib, &a[i__ + i__ * a_dim1], lda, &tau[i__], &work[
                    1], &iinfo);
/*<             IF( I+IB.LE.N ) THEN >*/
            if (i__ + ib <= *n) {

/*              Form the triangular factor of the block reflector */
/*              H = H(i) H(i+1) . . . H(i+ib-1) */

/*<    >*/
                i__3 = *m - i__ + 1;
                zlarft_("Forward", "Columnwise", &i__3, &ib, &a[i__ + i__ *
                        a_dim1], lda, &tau[i__], &work[1], &ldwork, (ftnlen)7,
                         (ftnlen)10);

/*              Apply H' to A(i:m,i+ib:n) from the left */

/*<    >*/
                i__3 = *m - i__ + 1;
                i__4 = *n - i__ - ib + 1;
                zlarfb_("Left", "Conjugate transpose", "Forward", "Columnwise"
                        , &i__3, &i__4, &ib, &a[i__ + i__ * a_dim1], lda, &
                        work[1], &ldwork, &a[i__ + (i__ + ib) * a_dim1], lda,
                        &work[ib + 1], &ldwork, (ftnlen)4, (ftnlen)19, (
                        ftnlen)7, (ftnlen)10);
/*<             END IF >*/
            }
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<       ELSE >*/
    } else {
/*<          I = 1 >*/
        i__ = 1;
/*<       END IF >*/
    }

/*     Use unblocked code to factor the last or only block. */

/*<    >*/
    if (i__ <= k) {
        i__2 = *m - i__ + 1;
        i__1 = *n - i__ + 1;
        zgeqr2_(&i__2, &i__1, &a[i__ + i__ * a_dim1], lda, &tau[i__], &work[1]
                , &iinfo);
    }

/*<       WORK( 1 ) = IWS >*/
    work[1].r = (doublereal) iws, work[1].i = 0.;
/*<       RETURN >*/
    return 0;

/*     End of ZGEQRF */

/*<       END >*/
} /* zgeqrf_ */

#ifdef __cplusplus
        }
#endif
