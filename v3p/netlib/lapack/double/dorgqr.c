/* lapack/double/dorgqr.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO ) >*/
/* Subroutine */ int dorgqr_(integer *m, integer *n, integer *k, doublereal *
        a, integer *lda, doublereal *tau, doublereal *work, integer *lwork,
        integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, l, ib, nb, ki=0, kk, nx, iws, nbmin, iinfo;
    extern /* Subroutine */ int dorg2r_(integer *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *),
            dlarfb_(char *, char *, char *, char *, integer *, integer *,
            integer *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, integer *, ftnlen, ftnlen,
            ftnlen, ftnlen), dlarft_(char *, char *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *,
            ftnlen, ftnlen), xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    integer ldwork, lwkopt;
    logical lquery;


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, K, LDA, LWORK, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DORGQR generates an M-by-N real matrix Q with orthonormal columns, */
/*  which is defined as the first N columns of a product of K elementary */
/*  reflectors of order M */

/*        Q  =  H(1) H(2) . . . H(k) */

/*  as returned by DGEQRF. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix Q. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix Q. M >= N >= 0. */

/*  K       (input) INTEGER */
/*          The number of elementary reflectors whose product defines the */
/*          matrix Q. N >= K >= 0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the i-th column must contain the vector which */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as */
/*          returned by DGEQRF in the first k columns of its array */
/*          argument A. */
/*          On exit, the M-by-N matrix Q. */

/*  LDA     (input) INTEGER */
/*          The first dimension of the array A. LDA >= max(1,M). */

/*  TAU     (input) DOUBLE PRECISION array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by DGEQRF. */

/*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. LWORK >= max(1,N). */
/*          For optimum performance LWORK >= N*NB, where NB is the */
/*          optimal blocksize. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument has an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY >*/
/*<    >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLARFB, DLARFT, DORG2R, XERBLA >*/
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
/*<       NB = ILAENV( 1, 'DORGQR', ' ', M, N, K, -1 ) >*/
    nb = ilaenv_(&c__1, "DORGQR", " ", m, n, k, &c_n1, (ftnlen)6, (ftnlen)1);
/*<       LWKOPT = MAX( 1, N )*NB >*/
    lwkopt = max(1,*n) * nb;
/*<       WORK( 1 ) = LWKOPT >*/
    work[1] = (doublereal) lwkopt;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       IF( M.LT.0 ) THEN >*/
    if (*m < 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 .OR. N.GT.M ) THEN >*/
    } else if (*n < 0 || *n > *m) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( K.LT.0 .OR. K.GT.N ) THEN >*/
    } else if (*k < 0 || *k > *n) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
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
/*<          CALL XERBLA( 'DORGQR', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DORGQR", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       IF( N.LE.0 ) THEN >*/
    if (*n <= 0) {
/*<          WORK( 1 ) = 1 >*/
        work[1] = 1.;
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
    if (nb > 1 && nb < *k) {

/*        Determine when to cross over from blocked to unblocked code. */

/*<          NX = MAX( 0, ILAENV( 3, 'DORGQR', ' ', M, N, K, -1 ) ) >*/
/* Computing MAX */
        i__1 = 0, i__2 = ilaenv_(&c__3, "DORGQR", " ", m, n, k, &c_n1, (
                ftnlen)6, (ftnlen)1);
        nx = max(i__1,i__2);
/*<          IF( NX.LT.K ) THEN >*/
        if (nx < *k) {

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
/*<                NBMIN = MAX( 2, ILAENV( 2, 'DORGQR', ' ', M, N, K, -1 ) ) >*/
/* Computing MAX */
                i__1 = 2, i__2 = ilaenv_(&c__2, "DORGQR", " ", m, n, k, &c_n1,
                         (ftnlen)6, (ftnlen)1);
                nbmin = max(i__1,i__2);
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN >*/
    if (nb >= nbmin && nb < *k && nx < *k) {

/*        Use blocked code after the last block. */
/*        The first kk columns are handled by the block method. */

/*<          KI = ( ( K-NX-1 ) / NB )*NB >*/
        ki = (*k - nx - 1) / nb * nb;
/*<          KK = MIN( K, KI+NB ) >*/
/* Computing MIN */
        i__1 = *k, i__2 = ki + nb;
        kk = min(i__1,i__2);

/*        Set A(1:kk,kk+1:n) to zero. */

/*<          DO 20 J = KK + 1, N >*/
        i__1 = *n;
        for (j = kk + 1; j <= i__1; ++j) {
/*<             DO 10 I = 1, KK >*/
            i__2 = kk;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ZERO >*/
                a[i__ + j * a_dim1] = 0.;
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       ELSE >*/
    } else {
/*<          KK = 0 >*/
        kk = 0;
/*<       END IF >*/
    }

/*     Use unblocked code for the last or only block. */

/*<    >*/
    if (kk < *n) {
        i__1 = *m - kk;
        i__2 = *n - kk;
        i__3 = *k - kk;
        dorg2r_(&i__1, &i__2, &i__3, &a[kk + 1 + (kk + 1) * a_dim1], lda, &
                tau[kk + 1], &work[1], &iinfo);
    }

/*<       IF( KK.GT.0 ) THEN >*/
    if (kk > 0) {

/*        Use blocked code */

/*<          DO 50 I = KI + 1, 1, -NB >*/
        i__1 = -nb;
        for (i__ = ki + 1; i__1 < 0 ? i__ >= 1 : i__ <= 1; i__ += i__1) {
/*<             IB = MIN( NB, K-I+1 ) >*/
/* Computing MIN */
            i__2 = nb, i__3 = *k - i__ + 1;
            ib = min(i__2,i__3);
/*<             IF( I+IB.LE.N ) THEN >*/
            if (i__ + ib <= *n) {

/*              Form the triangular factor of the block reflector */
/*              H = H(i) H(i+1) . . . H(i+ib-1) */

/*<    >*/
                i__2 = *m - i__ + 1;
                dlarft_("Forward", "Columnwise", &i__2, &ib, &a[i__ + i__ *
                        a_dim1], lda, &tau[i__], &work[1], &ldwork, (ftnlen)7,
                         (ftnlen)10);

/*              Apply H to A(i:m,i+ib:n) from the left */

/*<    >*/
                i__2 = *m - i__ + 1;
                i__3 = *n - i__ - ib + 1;
                dlarfb_("Left", "No transpose", "Forward", "Columnwise", &
                        i__2, &i__3, &ib, &a[i__ + i__ * a_dim1], lda, &work[
                        1], &ldwork, &a[i__ + (i__ + ib) * a_dim1], lda, &
                        work[ib + 1], &ldwork, (ftnlen)4, (ftnlen)12, (ftnlen)
                        7, (ftnlen)10);
/*<             END IF >*/
            }

/*           Apply H to rows i:m of current block */

/*<    >*/
            i__2 = *m - i__ + 1;
            dorg2r_(&i__2, &ib, &ib, &a[i__ + i__ * a_dim1], lda, &tau[i__], &
                    work[1], &iinfo);

/*           Set rows 1:i-1 of current block to zero */

/*<             DO 40 J = I, I + IB - 1 >*/
            i__2 = i__ + ib - 1;
            for (j = i__; j <= i__2; ++j) {
/*<                DO 30 L = 1, I - 1 >*/
                i__3 = i__ - 1;
                for (l = 1; l <= i__3; ++l) {
/*<                   A( L, J ) = ZERO >*/
                    a[l + j * a_dim1] = 0.;
/*<    30          CONTINUE >*/
/* L30: */
                }
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<    50    CONTINUE >*/
/* L50: */
        }
/*<       END IF >*/
    }

/*<       WORK( 1 ) = IWS >*/
    work[1] = (doublereal) iws;
/*<       RETURN >*/
    return 0;

/*     End of DORGQR */

/*<       END >*/
} /* dorgqr_ */

#ifdef __cplusplus
        }
#endif
