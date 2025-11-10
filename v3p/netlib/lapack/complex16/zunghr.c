/* lapack/complex16/zunghr.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO ) >*/
/* Subroutine */ int zunghr_(integer *n, integer *ilo, integer *ihi,
        doublecomplex *a, integer *lda, doublecomplex *tau, doublecomplex *
        work, integer *lwork, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    integer i__, j, nb, nh, iinfo;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    integer lwkopt=0;
    logical lquery;
    extern /* Subroutine */ int zungqr_(integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, integer *);


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

/*  ZUNGHR generates a complex unitary matrix Q which is defined as the */
/*  product of IHI-ILO elementary reflectors of order N, as returned by */
/*  ZGEHRD: */

/*  Q = H(ilo) H(ilo+1) . . . H(ihi-1). */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix Q. N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          ILO and IHI must have the same values as in the previous call */
/*          of ZGEHRD. Q is equal to the unit matrix except in the */
/*          submatrix Q(ilo+1:ihi,ilo+1:ihi). */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the vectors which define the elementary reflectors, */
/*          as returned by ZGEHRD. */
/*          On exit, the N-by-N unitary matrix Q. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,N). */

/*  TAU     (input) COMPLEX*16 array, dimension (N-1) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by ZGEHRD. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. LWORK >= IHI-ILO. */
/*          For optimum performance LWORK >= (IHI-ILO)*NB, where NB is */
/*          the optimal blocksize. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ZERO, ONE >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY >*/
/*<       INTEGER            I, IINFO, J, LWKOPT, NB, NH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZUNGQR >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            ILAENV >*/
/*<       EXTERNAL           ILAENV >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
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
/*<       NH = IHI - ILO >*/
    nh = *ihi - *ilo;
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
/*<       ELSE IF( LWORK.LT.MAX( 1, NH ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,nh) && ! lquery) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       END IF >*/
    }

/*<       IF( INFO.EQ.0 ) THEN >*/
    if (*info == 0) {
/*<          NB = ILAENV( 1, 'ZUNGQR', ' ', NH, NH, NH, -1 ) >*/
        nb = ilaenv_(&c__1, "ZUNGQR", " ", &nh, &nh, &nh, &c_n1, (ftnlen)6, (
                ftnlen)1);
/*<          LWKOPT = MAX( 1, NH )*NB >*/
        lwkopt = max(1,nh) * nb;
/*<          WORK( 1 ) = LWKOPT >*/
        work[1].r = (doublereal) lwkopt, work[1].i = 0.;
/*<       END IF >*/
    }

/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZUNGHR', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZUNGHR", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       IF( N.EQ.0 ) THEN >*/
    if (*n == 0) {
/*<          WORK( 1 ) = 1 >*/
        work[1].r = 1., work[1].i = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Shift the vectors which define the elementary reflectors one */
/*     column to the right, and set the first ilo and the last n-ihi */
/*     rows and columns to those of the unit matrix */

/*<       DO 40 J = IHI, ILO + 1, -1 >*/
    i__1 = *ilo + 1;
    for (j = *ihi; j >= i__1; --j) {
/*<          DO 10 I = 1, J - 1 >*/
        i__2 = j - 1;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             A( I, J ) = ZERO >*/
            i__3 = i__ + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          DO 20 I = J + 1, IHI >*/
        i__2 = *ihi;
        for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<             A( I, J ) = A( I, J-1 ) >*/
            i__3 = i__ + j * a_dim1;
            i__4 = i__ + (j - 1) * a_dim1;
            a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<          DO 30 I = IHI + 1, N >*/
        i__2 = *n;
        for (i__ = *ihi + 1; i__ <= i__2; ++i__) {
/*<             A( I, J ) = ZERO >*/
            i__3 = i__ + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<       DO 60 J = 1, ILO >*/
    i__1 = *ilo;
    for (j = 1; j <= i__1; ++j) {
/*<          DO 50 I = 1, N >*/
        i__2 = *n;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             A( I, J ) = ZERO >*/
            i__3 = i__ + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/*<    50    CONTINUE >*/
/* L50: */
        }
/*<          A( J, J ) = ONE >*/
        i__2 = j + j * a_dim1;
        a[i__2].r = 1., a[i__2].i = 0.;
/*<    60 CONTINUE >*/
/* L60: */
    }
/*<       DO 80 J = IHI + 1, N >*/
    i__1 = *n;
    for (j = *ihi + 1; j <= i__1; ++j) {
/*<          DO 70 I = 1, N >*/
        i__2 = *n;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<             A( I, J ) = ZERO >*/
            i__3 = i__ + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/*<    70    CONTINUE >*/
/* L70: */
        }
/*<          A( J, J ) = ONE >*/
        i__2 = j + j * a_dim1;
        a[i__2].r = 1., a[i__2].i = 0.;
/*<    80 CONTINUE >*/
/* L80: */
    }

/*<       IF( NH.GT.0 ) THEN >*/
    if (nh > 0) {

/*        Generate Q(ilo+1:ihi,ilo+1:ihi) */

/*<    >*/
        zungqr_(&nh, &nh, &nh, &a[*ilo + 1 + (*ilo + 1) * a_dim1], lda, &tau[*
                ilo], &work[1], lwork, &iinfo);
/*<       END IF >*/
    }
/*<       WORK( 1 ) = LWKOPT >*/
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;
/*<       RETURN >*/
    return 0;

/*     End of ZUNGHR */

/*<       END >*/
} /* zunghr_ */

#ifdef __cplusplus
        }
#endif
