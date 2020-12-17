/* lapack/double/dggbal.f -- translated by f2c (version 20050501).
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
static doublereal c_b34 = 10.;
static doublereal c_b70 = .5;

/*<    >*/
/* Subroutine */ int dggbal_(char *job, integer *n, doublereal *a, integer *
        lda, doublereal *b, integer *ldb, integer *ilo, integer *ihi,
        doublereal *lscale, doublereal *rscale, doublereal *work, integer *
        info, ftnlen job_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double d_lg10(doublereal *), d_sign(doublereal *, doublereal *), pow_di(
            doublereal *, integer *);

    /* Local variables */
    integer i__, j, k, l, m;
    doublereal t;
    integer jc;
    doublereal ta, tb, tc;
    integer ir;
    doublereal ew;
    integer it, nr, ip1, jp1, lm1;
    doublereal cab, rab, ewc, cor, sum;
    integer nrp2, icab, lcab;
    doublereal beta, coef;
    integer irab, lrab;
    doublereal basl, cmax;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    doublereal coef2, coef5, gamma, alpha;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal sfmin, sfmax;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    integer iflow;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    integer kount;
    extern doublereal dlamch_(char *, ftnlen);
    doublereal pgamma=0;
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    integer lsfmin, lsfmax;
    (void)job_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOB >*/
/*<       INTEGER            IHI, ILO, INFO, LDA, LDB, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGGBAL balances a pair of general real matrices (A,B).  This */
/*  involves, first, permuting A and B by similarity transformations to */
/*  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N */
/*  elements on the diagonal; and second, applying a diagonal similarity */
/*  transformation to rows and columns ILO to IHI to make the rows */
/*  and columns as close in norm as possible. Both steps are optional. */

/*  Balancing may reduce the 1-norm of the matrices, and improve the */
/*  accuracy of the computed eigenvalues and/or eigenvectors in the */
/*  generalized eigenvalue problem A*x = lambda*B*x. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          Specifies the operations to be performed on A and B: */
/*          = 'N':  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0 */
/*                  and RSCALE(I) = 1.0 for i = 1,...,N. */
/*          = 'P':  permute only; */
/*          = 'S':  scale only; */
/*          = 'B':  both permute and scale. */

/*  N       (input) INTEGER */
/*          The order of the matrices A and B.  N >= 0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the input matrix A. */
/*          On exit,  A is overwritten by the balanced matrix. */
/*          If JOB = 'N', A is not referenced. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,N). */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) */
/*          On entry, the input matrix B. */
/*          On exit,  B is overwritten by the balanced matrix. */
/*          If JOB = 'N', B is not referenced. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDB >= max(1,N). */

/*  ILO     (output) INTEGER */
/*  IHI     (output) INTEGER */
/*          ILO and IHI are set to integers such that on exit */
/*          A(i,j) = 0 and B(i,j) = 0 if i > j and */
/*          j = 1,...,ILO-1 or i = IHI+1,...,N. */
/*          If JOB = 'N' or 'S', ILO = 1 and IHI = N. */

/*  LSCALE  (output) DOUBLE PRECISION array, dimension (N) */
/*          Details of the permutations and scaling factors applied */
/*          to the left side of A and B.  If P(j) is the index of the */
/*          row interchanged with row j, and D(j) */
/*          is the scaling factor applied to row j, then */
/*            LSCALE(j) = P(j)    for J = 1,...,ILO-1 */
/*                      = D(j)    for J = ILO,...,IHI */
/*                      = P(j)    for J = IHI+1,...,N. */
/*          The order in which the interchanges are made is N to IHI+1, */
/*          then 1 to ILO-1. */

/*  RSCALE  (output) DOUBLE PRECISION array, dimension (N) */
/*          Details of the permutations and scaling factors applied */
/*          to the right side of A and B.  If P(j) is the index of the */
/*          column interchanged with column j, and D(j) */
/*          is the scaling factor applied to column j, then */
/*            LSCALE(j) = P(j)    for J = 1,...,ILO-1 */
/*                      = D(j)    for J = ILO,...,IHI */
/*                      = P(j)    for J = IHI+1,...,N. */
/*          The order in which the interchanges are made is N to IHI+1, */
/*          then 1 to ILO-1. */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (6*N) */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  See R.C. WARD, Balancing the generalized eigenvalue problem, */
/*                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, HALF, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 ) >*/
/*<       DOUBLE PRECISION   THREE, SCLFAC >*/
/*<       PARAMETER          ( THREE = 3.0D+0, SCLFAC = 1.0D+1 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<    >*/
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IDAMAX >*/
/*<       DOUBLE PRECISION   DDOT, DLAMCH >*/
/*<       EXTERNAL           LSAME, IDAMAX, DDOT, DLAMCH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DAXPY, DSCAL, DSWAP, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, INT, LOG10, MAX, MIN, SIGN >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    --lscale;
    --rscale;
    --work;

    /* Function Body */
    *info = 0;
/*<    >*/
    if (! lsame_(job, "N", (ftnlen)1, (ftnlen)1) && ! lsame_(job, "P", (
            ftnlen)1, (ftnlen)1) && ! lsame_(job, "S", (ftnlen)1, (ftnlen)1)
            && ! lsame_(job, "B", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldb < max(1,*n)) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DGGBAL', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DGGBAL", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       K = 1 >*/
    k = 1;
/*<       L = N >*/
    l = *n;

/*     Quick return if possible */

/*<    >*/
    if (*n == 0) {
        return 0;
    }

/*<       IF( LSAME( JOB, 'N' ) ) THEN >*/
    if (lsame_(job, "N", (ftnlen)1, (ftnlen)1)) {
/*<          ILO = 1 >*/
        *ilo = 1;
/*<          IHI = N >*/
        *ihi = *n;
/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             LSCALE( I ) = ONE >*/
            lscale[i__] = 1.;
/*<             RSCALE( I ) = ONE >*/
            rscale[i__] = 1.;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       IF( K.EQ.L ) THEN >*/
    if (k == l) {
/*<          ILO = 1 >*/
        *ilo = 1;
/*<          IHI = 1 >*/
        *ihi = 1;
/*<          LSCALE( 1 ) = ONE >*/
        lscale[1] = 1.;
/*<          RSCALE( 1 ) = ONE >*/
        rscale[1] = 1.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<    >*/
    if (lsame_(job, "S", (ftnlen)1, (ftnlen)1)) {
        goto L190;
    }

/*<       GO TO 30 >*/
    goto L30;

/*     Permute the matrices A and B to isolate the eigenvalues. */

/*     Find row with one nonzero in columns 1 through L */

/*<    20 CONTINUE >*/
L20:
/*<       L = LM1 >*/
    l = lm1;
/*<    >*/
    if (l != 1) {
        goto L30;
    }

/*<       RSCALE( 1 ) = 1 >*/
    rscale[1] = 1.;
/*<       LSCALE( 1 ) = 1 >*/
    lscale[1] = 1.;
/*<       GO TO 190 >*/
    goto L190;

/*<    30 CONTINUE >*/
L30:
/*<       LM1 = L - 1 >*/
    lm1 = l - 1;
/*<       DO 80 I = L, 1, -1 >*/
    for (i__ = l; i__ >= 1; --i__) {
/*<          DO 40 J = 1, LM1 >*/
        i__1 = lm1;
        for (j = 1; j <= i__1; ++j) {
/*<             JP1 = J + 1 >*/
            jp1 = j + 1;
/*<    >*/
            if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.) {
                goto L50;
            }
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<          J = L >*/
        j = l;
/*<          GO TO 70 >*/
        goto L70;

/*<    50    CONTINUE >*/
L50:
/*<          DO 60 J = JP1, L >*/
        i__1 = l;
        for (j = jp1; j <= i__1; ++j) {
/*<    >*/
            if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.) {
                goto L80;
            }
/*<    60    CONTINUE >*/
/* L60: */
        }
/*<          J = JP1 - 1 >*/
        j = jp1 - 1;

/*<    70    CONTINUE >*/
L70:
/*<          M = L >*/
        m = l;
/*<          IFLOW = 1 >*/
        iflow = 1;
/*<          GO TO 160 >*/
        goto L160;
/*<    80 CONTINUE >*/
L80:
        ;
    }
/*<       GO TO 100 >*/
    goto L100;

/*     Find column with one nonzero in rows K through N */

/*<    90 CONTINUE >*/
L90:
/*<       K = K + 1 >*/
    ++k;

/*<   100 CONTINUE >*/
L100:
/*<       DO 150 J = K, L >*/
    i__1 = l;
    for (j = k; j <= i__1; ++j) {
/*<          DO 110 I = K, LM1 >*/
        i__2 = lm1;
        for (i__ = k; i__ <= i__2; ++i__) {
/*<             IP1 = I + 1 >*/
            ip1 = i__ + 1;
/*<    >*/
            if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.) {
                goto L120;
            }
/*<   110    CONTINUE >*/
/* L110: */
        }
/*<          I = L >*/
        i__ = l;
/*<          GO TO 140 >*/
        goto L140;
/*<   120    CONTINUE >*/
L120:
/*<          DO 130 I = IP1, L >*/
        i__2 = l;
        for (i__ = ip1; i__ <= i__2; ++i__) {
/*<    >*/
            if (a[i__ + j * a_dim1] != 0. || b[i__ + j * b_dim1] != 0.) {
                goto L150;
            }
/*<   130    CONTINUE >*/
/* L130: */
        }
/*<          I = IP1 - 1 >*/
        i__ = ip1 - 1;
/*<   140    CONTINUE >*/
L140:
/*<          M = K >*/
        m = k;
/*<          IFLOW = 2 >*/
        iflow = 2;
/*<          GO TO 160 >*/
        goto L160;
/*<   150 CONTINUE >*/
L150:
        ;
    }
/*<       GO TO 190 >*/
    goto L190;

/*     Permute rows M and I */

/*<   160 CONTINUE >*/
L160:
/*<       LSCALE( M ) = I >*/
    lscale[m] = (doublereal) i__;
/*<    >*/
    if (i__ == m) {
        goto L170;
    }
/*<       CALL DSWAP( N-K+1, A( I, K ), LDA, A( M, K ), LDA ) >*/
    i__1 = *n - k + 1;
    dswap_(&i__1, &a[i__ + k * a_dim1], lda, &a[m + k * a_dim1], lda);
/*<       CALL DSWAP( N-K+1, B( I, K ), LDB, B( M, K ), LDB ) >*/
    i__1 = *n - k + 1;
    dswap_(&i__1, &b[i__ + k * b_dim1], ldb, &b[m + k * b_dim1], ldb);

/*     Permute columns M and J */

/*<   170 CONTINUE >*/
L170:
/*<       RSCALE( M ) = J >*/
    rscale[m] = (doublereal) j;
/*<    >*/
    if (j == m) {
        goto L180;
    }
/*<       CALL DSWAP( L, A( 1, J ), 1, A( 1, M ), 1 ) >*/
    dswap_(&l, &a[j * a_dim1 + 1], &c__1, &a[m * a_dim1 + 1], &c__1);
/*<       CALL DSWAP( L, B( 1, J ), 1, B( 1, M ), 1 ) >*/
    dswap_(&l, &b[j * b_dim1 + 1], &c__1, &b[m * b_dim1 + 1], &c__1);

/*<   180 CONTINUE >*/
L180:
/*<       GO TO ( 20, 90 )IFLOW >*/
    switch (iflow) {
        case 1:  goto L20;
        case 2:  goto L90;
    }

/*<   190 CONTINUE >*/
L190:
/*<       ILO = K >*/
    *ilo = k;
/*<       IHI = L >*/
    *ihi = l;

/*<    >*/
    if (*ilo == *ihi) {
        return 0;
    }

/*<    >*/
    if (lsame_(job, "P", (ftnlen)1, (ftnlen)1)) {
        return 0;
    }

/*     Balance the submatrix in rows ILO to IHI. */

/*<       NR = IHI - ILO + 1 >*/
    nr = *ihi - *ilo + 1;
/*<       DO 200 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          RSCALE( I ) = ZERO >*/
        rscale[i__] = 0.;
/*<          LSCALE( I ) = ZERO >*/
        lscale[i__] = 0.;

/*<          WORK( I ) = ZERO >*/
        work[i__] = 0.;
/*<          WORK( I+N ) = ZERO >*/
        work[i__ + *n] = 0.;
/*<          WORK( I+2*N ) = ZERO >*/
        work[i__ + (*n << 1)] = 0.;
/*<          WORK( I+3*N ) = ZERO >*/
        work[i__ + *n * 3] = 0.;
/*<          WORK( I+4*N ) = ZERO >*/
        work[i__ + (*n << 2)] = 0.;
/*<          WORK( I+5*N ) = ZERO >*/
        work[i__ + *n * 5] = 0.;
/*<   200 CONTINUE >*/
/* L200: */
    }

/*     Compute right side vector in resulting linear equations */

/*<       BASL = LOG10( SCLFAC ) >*/
    basl = d_lg10(&c_b34);
/*<       DO 240 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          DO 230 J = ILO, IHI >*/
        i__2 = *ihi;
        for (j = *ilo; j <= i__2; ++j) {
/*<             TB = B( I, J ) >*/
            tb = b[i__ + j * b_dim1];
/*<             TA = A( I, J ) >*/
            ta = a[i__ + j * a_dim1];
/*<    >*/
            if (ta == 0.) {
                goto L210;
            }
/*<             TA = LOG10( ABS( TA ) ) / BASL >*/
            d__1 = abs(ta);
            ta = d_lg10(&d__1) / basl;
/*<   210       CONTINUE >*/
L210:
/*<    >*/
            if (tb == 0.) {
                goto L220;
            }
/*<             TB = LOG10( ABS( TB ) ) / BASL >*/
            d__1 = abs(tb);
            tb = d_lg10(&d__1) / basl;
/*<   220       CONTINUE >*/
L220:
/*<             WORK( I+4*N ) = WORK( I+4*N ) - TA - TB >*/
            work[i__ + (*n << 2)] = work[i__ + (*n << 2)] - ta - tb;
/*<             WORK( J+5*N ) = WORK( J+5*N ) - TA - TB >*/
            work[j + *n * 5] = work[j + *n * 5] - ta - tb;
/*<   230    CONTINUE >*/
/* L230: */
        }
/*<   240 CONTINUE >*/
/* L240: */
    }

/*<       COEF = ONE / DBLE( 2*NR ) >*/
    coef = 1. / (doublereal) (nr << 1);
/*<       COEF2 = COEF*COEF >*/
    coef2 = coef * coef;
/*<       COEF5 = HALF*COEF2 >*/
    coef5 = coef2 * .5;
/*<       NRP2 = NR + 2 >*/
    nrp2 = nr + 2;
/*<       BETA = ZERO >*/
    beta = 0.;
/*<       IT = 1 >*/
    it = 1;

/*     Start generalized conjugate gradient iteration */

/*<   250 CONTINUE >*/
L250:

/*<    >*/
    gamma = ddot_(&nr, &work[*ilo + (*n << 2)], &c__1, &work[*ilo + (*n << 2)]
            , &c__1) + ddot_(&nr, &work[*ilo + *n * 5], &c__1, &work[*ilo + *
            n * 5], &c__1);

/*<       EW = ZERO >*/
    ew = 0.;
/*<       EWC = ZERO >*/
    ewc = 0.;
/*<       DO 260 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          EW = EW + WORK( I+4*N ) >*/
        ew += work[i__ + (*n << 2)];
/*<          EWC = EWC + WORK( I+5*N ) >*/
        ewc += work[i__ + *n * 5];
/*<   260 CONTINUE >*/
/* L260: */
    }

/*<       GAMMA = COEF*GAMMA - COEF2*( EW**2+EWC**2 ) - COEF5*( EW-EWC )**2 >*/
/* Computing 2nd power */
    d__1 = ew;
/* Computing 2nd power */
    d__2 = ewc;
/* Computing 2nd power */
    d__3 = ew - ewc;
    gamma = coef * gamma - coef2 * (d__1 * d__1 + d__2 * d__2) - coef5 * (
            d__3 * d__3);
/*<    >*/
    if (gamma == 0.) {
        goto L350;
    }
/*<    >*/
    if (it != 1) {
        beta = gamma / pgamma;
    }
/*<       T = COEF5*( EWC-THREE*EW ) >*/
    t = coef5 * (ewc - ew * 3.);
/*<       TC = COEF5*( EW-THREE*EWC ) >*/
    tc = coef5 * (ew - ewc * 3.);

/*<       CALL DSCAL( NR, BETA, WORK( ILO ), 1 ) >*/
    dscal_(&nr, &beta, &work[*ilo], &c__1);
/*<       CALL DSCAL( NR, BETA, WORK( ILO+N ), 1 ) >*/
    dscal_(&nr, &beta, &work[*ilo + *n], &c__1);

/*<       CALL DAXPY( NR, COEF, WORK( ILO+4*N ), 1, WORK( ILO+N ), 1 ) >*/
    daxpy_(&nr, &coef, &work[*ilo + (*n << 2)], &c__1, &work[*ilo + *n], &
            c__1);
/*<       CALL DAXPY( NR, COEF, WORK( ILO+5*N ), 1, WORK( ILO ), 1 ) >*/
    daxpy_(&nr, &coef, &work[*ilo + *n * 5], &c__1, &work[*ilo], &c__1);

/*<       DO 270 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          WORK( I ) = WORK( I ) + TC >*/
        work[i__] += tc;
/*<          WORK( I+N ) = WORK( I+N ) + T >*/
        work[i__ + *n] += t;
/*<   270 CONTINUE >*/
/* L270: */
    }

/*     Apply matrix to vector */

/*<       DO 300 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          KOUNT = 0 >*/
        kount = 0;
/*<          SUM = ZERO >*/
        sum = 0.;
/*<          DO 290 J = ILO, IHI >*/
        i__2 = *ihi;
        for (j = *ilo; j <= i__2; ++j) {
/*<    >*/
            if (a[i__ + j * a_dim1] == 0.) {
                goto L280;
            }
/*<             KOUNT = KOUNT + 1 >*/
            ++kount;
/*<             SUM = SUM + WORK( J ) >*/
            sum += work[j];
/*<   280       CONTINUE >*/
L280:
/*<    >*/
            if (b[i__ + j * b_dim1] == 0.) {
                goto L290;
            }
/*<             KOUNT = KOUNT + 1 >*/
            ++kount;
/*<             SUM = SUM + WORK( J ) >*/
            sum += work[j];
/*<   290    CONTINUE >*/
L290:
            ;
        }
/*<          WORK( I+2*N ) = DBLE( KOUNT )*WORK( I+N ) + SUM >*/
        work[i__ + (*n << 1)] = (doublereal) kount * work[i__ + *n] + sum;
/*<   300 CONTINUE >*/
/* L300: */
    }

/*<       DO 330 J = ILO, IHI >*/
    i__1 = *ihi;
    for (j = *ilo; j <= i__1; ++j) {
/*<          KOUNT = 0 >*/
        kount = 0;
/*<          SUM = ZERO >*/
        sum = 0.;
/*<          DO 320 I = ILO, IHI >*/
        i__2 = *ihi;
        for (i__ = *ilo; i__ <= i__2; ++i__) {
/*<    >*/
            if (a[i__ + j * a_dim1] == 0.) {
                goto L310;
            }
/*<             KOUNT = KOUNT + 1 >*/
            ++kount;
/*<             SUM = SUM + WORK( I+N ) >*/
            sum += work[i__ + *n];
/*<   310       CONTINUE >*/
L310:
/*<    >*/
            if (b[i__ + j * b_dim1] == 0.) {
                goto L320;
            }
/*<             KOUNT = KOUNT + 1 >*/
            ++kount;
/*<             SUM = SUM + WORK( I+N ) >*/
            sum += work[i__ + *n];
/*<   320    CONTINUE >*/
L320:
            ;
        }
/*<          WORK( J+3*N ) = DBLE( KOUNT )*WORK( J ) + SUM >*/
        work[j + *n * 3] = (doublereal) kount * work[j] + sum;
/*<   330 CONTINUE >*/
/* L330: */
    }

/*<    >*/
    sum = ddot_(&nr, &work[*ilo + *n], &c__1, &work[*ilo + (*n << 1)], &c__1)
            + ddot_(&nr, &work[*ilo], &c__1, &work[*ilo + *n * 3], &c__1);
/*<       ALPHA = GAMMA / SUM >*/
    alpha = gamma / sum;

/*     Determine correction to current iteration */

/*<       CMAX = ZERO >*/
    cmax = 0.;
/*<       DO 340 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          COR = ALPHA*WORK( I+N ) >*/
        cor = alpha * work[i__ + *n];
/*<    >*/
        if (abs(cor) > cmax) {
            cmax = abs(cor);
        }
/*<          LSCALE( I ) = LSCALE( I ) + COR >*/
        lscale[i__] += cor;
/*<          COR = ALPHA*WORK( I ) >*/
        cor = alpha * work[i__];
/*<    >*/
        if (abs(cor) > cmax) {
            cmax = abs(cor);
        }
/*<          RSCALE( I ) = RSCALE( I ) + COR >*/
        rscale[i__] += cor;
/*<   340 CONTINUE >*/
/* L340: */
    }
/*<    >*/
    if (cmax < .5) {
        goto L350;
    }

/*<       CALL DAXPY( NR, -ALPHA, WORK( ILO+2*N ), 1, WORK( ILO+4*N ), 1 ) >*/
    d__1 = -alpha;
    daxpy_(&nr, &d__1, &work[*ilo + (*n << 1)], &c__1, &work[*ilo + (*n << 2)]
            , &c__1);
/*<       CALL DAXPY( NR, -ALPHA, WORK( ILO+3*N ), 1, WORK( ILO+5*N ), 1 ) >*/
    d__1 = -alpha;
    daxpy_(&nr, &d__1, &work[*ilo + *n * 3], &c__1, &work[*ilo + *n * 5], &
            c__1);

/*<       PGAMMA = GAMMA >*/
    pgamma = gamma;
/*<       IT = IT + 1 >*/
    ++it;
/*<    >*/
    if (it <= nrp2) {
        goto L250;
    }

/*     End generalized conjugate gradient iteration */

/*<   350 CONTINUE >*/
L350:
/*<       SFMIN = DLAMCH( 'S' ) >*/
    sfmin = dlamch_("S", (ftnlen)1);
/*<       SFMAX = ONE / SFMIN >*/
    sfmax = 1. / sfmin;
/*<       LSFMIN = INT( LOG10( SFMIN ) / BASL+ONE ) >*/
    lsfmin = (integer) (d_lg10(&sfmin) / basl + 1.);
/*<       LSFMAX = INT( LOG10( SFMAX ) / BASL ) >*/
    lsfmax = (integer) (d_lg10(&sfmax) / basl);
/*<       DO 360 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          IRAB = IDAMAX( N-ILO+1, A( I, ILO ), LDA ) >*/
        i__2 = *n - *ilo + 1;
        irab = idamax_(&i__2, &a[i__ + *ilo * a_dim1], lda);
/*<          RAB = ABS( A( I, IRAB+ILO-1 ) ) >*/
        rab = (d__1 = a[i__ + (irab + *ilo - 1) * a_dim1], abs(d__1));
/*<          IRAB = IDAMAX( N-ILO+1, B( I, ILO ), LDA ) >*/
        i__2 = *n - *ilo + 1;
        irab = idamax_(&i__2, &b[i__ + *ilo * b_dim1], lda);
/*<          RAB = MAX( RAB, ABS( B( I, IRAB+ILO-1 ) ) ) >*/
/* Computing MAX */
        d__2 = rab, d__3 = (d__1 = b[i__ + (irab + *ilo - 1) * b_dim1], abs(
                d__1));
        rab = max(d__2,d__3);
/*<          LRAB = INT( LOG10( RAB+SFMIN ) / BASL+ONE ) >*/
        d__1 = rab + sfmin;
        lrab = (integer) (d_lg10(&d__1) / basl + 1.);
/*<          IR = LSCALE( I ) + SIGN( HALF, LSCALE( I ) ) >*/
        ir = (integer) (lscale[i__] + d_sign(&c_b70, &lscale[i__]));
/*<          IR = MIN( MAX( IR, LSFMIN ), LSFMAX, LSFMAX-LRAB ) >*/
/* Computing MIN */
        i__2 = max(ir,lsfmin), i__2 = min(i__2,lsfmax), i__3 = lsfmax - lrab;
        ir = min(i__2,i__3);
/*<          LSCALE( I ) = SCLFAC**IR >*/
        lscale[i__] = pow_di(&c_b34, &ir);
/*<          ICAB = IDAMAX( IHI, A( 1, I ), 1 ) >*/
        icab = idamax_(ihi, &a[i__ * a_dim1 + 1], &c__1);
/*<          CAB = ABS( A( ICAB, I ) ) >*/
        cab = (d__1 = a[icab + i__ * a_dim1], abs(d__1));
/*<          ICAB = IDAMAX( IHI, B( 1, I ), 1 ) >*/
        icab = idamax_(ihi, &b[i__ * b_dim1 + 1], &c__1);
/*<          CAB = MAX( CAB, ABS( B( ICAB, I ) ) ) >*/
/* Computing MAX */
        d__2 = cab, d__3 = (d__1 = b[icab + i__ * b_dim1], abs(d__1));
        cab = max(d__2,d__3);
/*<          LCAB = INT( LOG10( CAB+SFMIN ) / BASL+ONE ) >*/
        d__1 = cab + sfmin;
        lcab = (integer) (d_lg10(&d__1) / basl + 1.);
/*<          JC = RSCALE( I ) + SIGN( HALF, RSCALE( I ) ) >*/
        jc = (integer) (rscale[i__] + d_sign(&c_b70, &rscale[i__]));
/*<          JC = MIN( MAX( JC, LSFMIN ), LSFMAX, LSFMAX-LCAB ) >*/
/* Computing MIN */
        i__2 = max(jc,lsfmin), i__2 = min(i__2,lsfmax), i__3 = lsfmax - lcab;
        jc = min(i__2,i__3);
/*<          RSCALE( I ) = SCLFAC**JC >*/
        rscale[i__] = pow_di(&c_b34, &jc);
/*<   360 CONTINUE >*/
/* L360: */
    }

/*     Row scaling of matrices A and B */

/*<       DO 370 I = ILO, IHI >*/
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
/*<          CALL DSCAL( N-ILO+1, LSCALE( I ), A( I, ILO ), LDA ) >*/
        i__2 = *n - *ilo + 1;
        dscal_(&i__2, &lscale[i__], &a[i__ + *ilo * a_dim1], lda);
/*<          CALL DSCAL( N-ILO+1, LSCALE( I ), B( I, ILO ), LDB ) >*/
        i__2 = *n - *ilo + 1;
        dscal_(&i__2, &lscale[i__], &b[i__ + *ilo * b_dim1], ldb);
/*<   370 CONTINUE >*/
/* L370: */
    }

/*     Column scaling of matrices A and B */

/*<       DO 380 J = ILO, IHI >*/
    i__1 = *ihi;
    for (j = *ilo; j <= i__1; ++j) {
/*<          CALL DSCAL( IHI, RSCALE( J ), A( 1, J ), 1 ) >*/
        dscal_(ihi, &rscale[j], &a[j * a_dim1 + 1], &c__1);
/*<          CALL DSCAL( IHI, RSCALE( J ), B( 1, J ), 1 ) >*/
        dscal_(ihi, &rscale[j], &b[j * b_dim1 + 1], &c__1);
/*<   380 CONTINUE >*/
/* L380: */
    }

/*<       RETURN >*/
    return 0;

/*     End of DGGBAL */

/*<       END >*/
} /* dggbal_ */

#ifdef __cplusplus
        }
#endif
