/* lapack/double/dgghrd.f -- translated by f2c (version 20050501).
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

static doublereal c_b10 = 0.;
static doublereal c_b11 = 1.;
static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int dgghrd_(char *compq, char *compz, integer *n, integer *
        ilo, integer *ihi, doublereal *a, integer *lda, doublereal *b,
        integer *ldb, doublereal *q, integer *ldq, doublereal *z__, integer *
        ldz, integer *info, ftnlen compq_len, ftnlen compz_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1,
            z_offset, i__1, i__2, i__3;

    /* Local variables */
    doublereal c__, s;
    logical ilq=0, ilz=0;
    integer jcol;
    doublereal temp;
    extern /* Subroutine */ int drot_(integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *);
    integer jrow;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *,
            doublereal *, doublereal *, doublereal *, integer *, ftnlen),
            dlartg_(doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *), xerbla_(char *, integer *, ftnlen);
    integer icompq, icompz;
    (void)compq_len;
    (void)compz_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          COMPQ, COMPZ >*/
/*<       INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGGHRD reduces a pair of real matrices (A,B) to generalized upper */
/*  Hessenberg form using orthogonal transformations, where A is a */
/*  general matrix and B is upper triangular:  Q' * A * Z = H and */
/*  Q' * B * Z = T, where H is upper Hessenberg, T is upper triangular, */
/*  and Q and Z are orthogonal, and ' means transpose. */

/*  The orthogonal matrices Q and Z are determined as products of Givens */
/*  rotations.  They may either be formed explicitly, or they may be */
/*  postmultiplied into input matrices Q1 and Z1, so that */

/*       Q1 * A * Z1' = (Q1*Q) * H * (Z1*Z)' */
/*       Q1 * B * Z1' = (Q1*Q) * T * (Z1*Z)' */

/*  Arguments */
/*  ========= */

/*  COMPQ   (input) CHARACTER*1 */
/*          = 'N': do not compute Q; */
/*          = 'I': Q is initialized to the unit matrix, and the */
/*                 orthogonal matrix Q is returned; */
/*          = 'V': Q must contain an orthogonal matrix Q1 on entry, */
/*                 and the product Q1*Q is returned. */

/*  COMPZ   (input) CHARACTER*1 */
/*          = 'N': do not compute Z; */
/*          = 'I': Z is initialized to the unit matrix, and the */
/*                 orthogonal matrix Z is returned; */
/*          = 'V': Z must contain an orthogonal matrix Z1 on entry, */
/*                 and the product Z1*Z is returned. */

/*  N       (input) INTEGER */
/*          The order of the matrices A and B.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          It is assumed that A is already upper triangular in rows and */
/*          columns 1:ILO-1 and IHI+1:N.  ILO and IHI are normally set */
/*          by a previous call to DGGBAL; otherwise they should be set */
/*          to 1 and N respectively. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N) */
/*          On entry, the N-by-N general matrix to be reduced. */
/*          On exit, the upper triangle and the first subdiagonal of A */
/*          are overwritten with the upper Hessenberg matrix H, and the */
/*          rest is set to zero. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N) */
/*          On entry, the N-by-N upper triangular matrix B. */
/*          On exit, the upper triangular matrix T = Q' B Z.  The */
/*          elements below the diagonal are set to zero. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B.  LDB >= max(1,N). */

/*  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N) */
/*          If COMPQ='N':  Q is not referenced. */
/*          If COMPQ='I':  on entry, Q need not be set, and on exit it */
/*                         contains the orthogonal matrix Q, where Q' */
/*                         is the product of the Givens transformations */
/*                         which are applied to A and B on the left. */
/*          If COMPQ='V':  on entry, Q must contain an orthogonal matrix */
/*                         Q1, and on exit this is overwritten by Q1*Q. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. */
/*          LDQ >= N if COMPQ='V' or 'I'; LDQ >= 1 otherwise. */

/*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N) */
/*          If COMPZ='N':  Z is not referenced. */
/*          If COMPZ='I':  on entry, Z need not be set, and on exit it */
/*                         contains the orthogonal matrix Z, which is */
/*                         the product of the Givens transformations */
/*                         which are applied to A and B on the right. */
/*          If COMPZ='V':  on entry, Z must contain an orthogonal matrix */
/*                         Z1, and on exit this is overwritten by Z1*Z. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z. */
/*          LDZ >= N if COMPZ='V' or 'I'; LDZ >= 1 otherwise. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit. */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  This routine reduces A to Hessenberg and B to triangular form by */
/*  an unblocked reduction, as described in _Matrix_Computations_, */
/*  by Golub and Van Loan (Johns Hopkins Press.) */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            ILQ, ILZ >*/
/*<       INTEGER            ICOMPQ, ICOMPZ, JCOL, JROW >*/
/*<       DOUBLE PRECISION   C, S, TEMP >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLARTG, DLASET, DROT, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode COMPQ */

/*<       IF( LSAME( COMPQ, 'N' ) ) THEN >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
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

/*     Decode COMPZ */

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

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    *info = 0;
/*<       IF( ICOMPQ.LE.0 ) THEN >*/
    if (icompq <= 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ICOMPZ.LE.0 ) THEN >*/
    } else if (icompz <= 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( ILO.LT.1 ) THEN >*/
    } else if (*ilo < 1) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN >*/
    } else if (*ihi > *n || *ihi < *ilo - 1) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldb < max(1,*n)) {
/*<          INFO = -9 >*/
        *info = -9;
/*<       ELSE IF( ( ILQ .AND. LDQ.LT.N ) .OR. LDQ.LT.1 ) THEN >*/
    } else if ((ilq && *ldq < *n) || *ldq < 1) {
/*<          INFO = -11 >*/
        *info = -11;
/*<       ELSE IF( ( ILZ .AND. LDZ.LT.N ) .OR. LDZ.LT.1 ) THEN >*/
    } else if ((ilz && *ldz < *n) || *ldz < 1) {
/*<          INFO = -13 >*/
        *info = -13;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DGGHRD', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DGGHRD", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Initialize Q and Z if desired. */

/*<    >*/
    if (icompq == 3) {
        dlaset_("Full", n, n, &c_b10, &c_b11, &q[q_offset], ldq, (ftnlen)4);
    }
/*<    >*/
    if (icompz == 3) {
        dlaset_("Full", n, n, &c_b10, &c_b11, &z__[z_offset], ldz, (ftnlen)4);
    }

/*     Quick return if possible */

/*<    >*/
    if (*n <= 1) {
        return 0;
    }

/*     Zero out lower triangle of B */

/*<       DO 20 JCOL = 1, N - 1 >*/
    i__1 = *n - 1;
    for (jcol = 1; jcol <= i__1; ++jcol) {
/*<          DO 10 JROW = JCOL + 1, N >*/
        i__2 = *n;
        for (jrow = jcol + 1; jrow <= i__2; ++jrow) {
/*<             B( JROW, JCOL ) = ZERO >*/
            b[jrow + jcol * b_dim1] = 0.;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<    20 CONTINUE >*/
/* L20: */
    }

/*     Reduce A and B */

/*<       DO 40 JCOL = ILO, IHI - 2 >*/
    i__1 = *ihi - 2;
    for (jcol = *ilo; jcol <= i__1; ++jcol) {

/*<          DO 30 JROW = IHI, JCOL + 2, -1 >*/
        i__2 = jcol + 2;
        for (jrow = *ihi; jrow >= i__2; --jrow) {

/*           Step 1: rotate rows JROW-1, JROW to kill A(JROW,JCOL) */

/*<             TEMP = A( JROW-1, JCOL ) >*/
            temp = a[jrow - 1 + jcol * a_dim1];
/*<    >*/
            dlartg_(&temp, &a[jrow + jcol * a_dim1], &c__, &s, &a[jrow - 1 +
                    jcol * a_dim1]);
/*<             A( JROW, JCOL ) = ZERO >*/
            a[jrow + jcol * a_dim1] = 0.;
/*<    >*/
            i__3 = *n - jcol;
            drot_(&i__3, &a[jrow - 1 + (jcol + 1) * a_dim1], lda, &a[jrow + (
                    jcol + 1) * a_dim1], lda, &c__, &s);
/*<    >*/
            i__3 = *n + 2 - jrow;
            drot_(&i__3, &b[jrow - 1 + (jrow - 1) * b_dim1], ldb, &b[jrow + (
                    jrow - 1) * b_dim1], ldb, &c__, &s);
/*<    >*/
            if (ilq) {
                drot_(n, &q[(jrow - 1) * q_dim1 + 1], &c__1, &q[jrow * q_dim1
                        + 1], &c__1, &c__, &s);
            }

/*           Step 2: rotate columns JROW, JROW-1 to kill B(JROW,JROW-1) */

/*<             TEMP = B( JROW, JROW ) >*/
            temp = b[jrow + jrow * b_dim1];
/*<    >*/
            dlartg_(&temp, &b[jrow + (jrow - 1) * b_dim1], &c__, &s, &b[jrow
                    + jrow * b_dim1]);
/*<             B( JROW, JROW-1 ) = ZERO >*/
            b[jrow + (jrow - 1) * b_dim1] = 0.;
/*<             CALL DROT( IHI, A( 1, JROW ), 1, A( 1, JROW-1 ), 1, C, S ) >*/
            drot_(ihi, &a[jrow * a_dim1 + 1], &c__1, &a[(jrow - 1) * a_dim1 +
                    1], &c__1, &c__, &s);
/*<    >*/
            i__3 = jrow - 1;
            drot_(&i__3, &b[jrow * b_dim1 + 1], &c__1, &b[(jrow - 1) * b_dim1
                    + 1], &c__1, &c__, &s);
/*<    >*/
            if (ilz) {
                drot_(n, &z__[jrow * z_dim1 + 1], &c__1, &z__[(jrow - 1) *
                        z_dim1 + 1], &c__1, &c__, &s);
            }
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }

/*<       RETURN >*/
    return 0;

/*     End of DGGHRD */

/*<       END >*/
} /* dgghrd_ */

#ifdef __cplusplus
        }
#endif
