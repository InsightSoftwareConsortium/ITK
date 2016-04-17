/* lapack/double/dlatrs.f -- translated by f2c (version 20050501).
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
static doublereal c_b36 = .5;

/*<    >*/
/* Subroutine */ int dlatrs_(char *uplo, char *trans, char *diag, char *
        normin, integer *n, doublereal *a, integer *lda, doublereal *x,
        doublereal *scale, doublereal *cnorm, integer *info, ftnlen uplo_len,
        ftnlen trans_len, ftnlen diag_len, ftnlen normin_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    integer i__, j;
    doublereal xj, rec, tjj;
    integer jinc;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    doublereal xbnd;
    integer imax;
    doublereal tmax, tjjs=0, xmax, grow, sumj;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal tscal, uscal;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    integer jlast;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    logical upper;
    extern /* Subroutine */ int dtrsv_(char *, char *, char *, integer *,
            doublereal *, integer *, doublereal *, integer *, ftnlen, ftnlen,
            ftnlen);
    extern doublereal dlamch_(char *, ftnlen);
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    doublereal bignum;
    logical notran;
    integer jfirst;
    doublereal smlnum;
    logical nounit;
    (void)uplo_len;
    (void)trans_len;
    (void)diag_len;
    (void)normin_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          DIAG, NORMIN, TRANS, UPLO >*/
/*<       INTEGER            INFO, LDA, N >*/
/*<       DOUBLE PRECISION   SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), CNORM( * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLATRS solves one of the triangular systems */

/*     A *x = s*b  or  A'*x = s*b */

/*  with scaling to prevent overflow.  Here A is an upper or lower */
/*  triangular matrix, A' denotes the transpose of A, x and b are */
/*  n-element vectors, and s is a scaling factor, usually less than */
/*  or equal to 1, chosen so that the components of x will be less than */
/*  the overflow threshold.  If the unscaled problem will not cause */
/*  overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A */
/*  is singular (A(j,j) = 0 for some j), then s is set to 0 and a */
/*  non-trivial solution to A*x = 0 is returned. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          Specifies whether the matrix A is upper or lower triangular. */
/*          = 'U':  Upper triangular */
/*          = 'L':  Lower triangular */

/*  TRANS   (input) CHARACTER*1 */
/*          Specifies the operation applied to A. */
/*          = 'N':  Solve A * x = s*b  (No transpose) */
/*          = 'T':  Solve A'* x = s*b  (Transpose) */
/*          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose) */

/*  DIAG    (input) CHARACTER*1 */
/*          Specifies whether or not the matrix A is unit triangular. */
/*          = 'N':  Non-unit triangular */
/*          = 'U':  Unit triangular */

/*  NORMIN  (input) CHARACTER*1 */
/*          Specifies whether CNORM has been set or not. */
/*          = 'Y':  CNORM contains the column norms on entry */
/*          = 'N':  CNORM is not set on entry.  On exit, the norms will */
/*                  be computed and stored in CNORM. */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0. */

/*  A       (input) DOUBLE PRECISION array, dimension (LDA,N) */
/*          The triangular matrix A.  If UPLO = 'U', the leading n by n */
/*          upper triangular part of the array A contains the upper */
/*          triangular matrix, and the strictly lower triangular part of */
/*          A is not referenced.  If UPLO = 'L', the leading n by n lower */
/*          triangular part of the array A contains the lower triangular */
/*          matrix, and the strictly upper triangular part of A is not */
/*          referenced.  If DIAG = 'U', the diagonal elements of A are */
/*          also not referenced and are assumed to be 1. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max (1,N). */

/*  X       (input/output) DOUBLE PRECISION array, dimension (N) */
/*          On entry, the right hand side b of the triangular system. */
/*          On exit, X is overwritten by the solution vector x. */

/*  SCALE   (output) DOUBLE PRECISION */
/*          The scaling factor s for the triangular system */
/*             A * x = s*b  or  A'* x = s*b. */
/*          If SCALE = 0, the matrix A is singular or badly scaled, and */
/*          the vector x is an exact or approximate solution to A*x = 0. */

/*  CNORM   (input or output) DOUBLE PRECISION array, dimension (N) */

/*          If NORMIN = 'Y', CNORM is an input argument and CNORM(j) */
/*          contains the norm of the off-diagonal part of the j-th column */
/*          of A.  If TRANS = 'N', CNORM(j) must be greater than or equal */
/*          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j) */
/*          must be greater than or equal to the 1-norm. */

/*          If NORMIN = 'N', CNORM is an output argument and CNORM(j) */
/*          returns the 1-norm of the offdiagonal part of the j-th column */
/*          of A. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -k, the k-th argument had an illegal value */

/*  Further Details */
/*  ======= ======= */

/*  A rough bound on x is computed; if that is less than overflow, DTRSV */
/*  is called, otherwise, specific code is used which checks for possible */
/*  overflow or divide-by-zero at every operation. */

/*  A columnwise scheme is used for solving A*x = b.  The basic algorithm */
/*  if A is lower triangular is */

/*       x[1:n] := b[1:n] */
/*       for j = 1, ..., n */
/*            x(j) := x(j) / A(j,j) */
/*            x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j] */
/*       end */

/*  Define bounds on the components of x after j iterations of the loop: */
/*     M(j) = bound on x[1:j] */
/*     G(j) = bound on x[j+1:n] */
/*  Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}. */

/*  Then for iteration j+1 we have */
/*     M(j+1) <= G(j) / | A(j+1,j+1) | */
/*     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] | */
/*            <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | ) */

/*  where CNORM(j+1) is greater than or equal to the infinity-norm of */
/*  column j+1 of A, not counting the diagonal.  Hence */

/*     G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | ) */
/*                  1<=i<=j */
/*  and */

/*     |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| ) */
/*                                   1<=i< j */

/*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine DTRSV if the */
/*  reciprocal of the largest M(j), j=1,..,n, is larger than */
/*  max(underflow, 1/overflow). */

/*  The bound on x(j) is also used to determine when a step in the */
/*  columnwise method can be performed without fear of overflow.  If */
/*  the computed bound is greater than a large constant, x is scaled to */
/*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to */
/*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found. */

/*  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic */
/*  algorithm for A upper triangular is */

/*       for j = 1, ..., n */
/*            x(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A(j,j) */
/*       end */

/*  We simultaneously compute two bounds */
/*       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j */
/*       M(j) = bound on x(i), 1<=i<=j */

/*  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we */
/*  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1. */
/*  Then the bound on x(j) is */

/*       M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) | */

/*            <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| ) */
/*                      1<=i<=j */

/*  and we can safely call DTRSV if 1/M(n) and 1/G(n) are both greater */
/*  than max(underflow, 1/overflow). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, HALF, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            NOTRAN, NOUNIT, UPPER >*/
/*<       INTEGER            I, IMAX, J, JFIRST, JINC, JLAST >*/
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IDAMAX >*/
/*<       DOUBLE PRECISION   DASUM, DDOT, DLAMCH >*/
/*<       EXTERNAL           LSAME, IDAMAX, DASUM, DDOT, DLAMCH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DAXPY, DSCAL, DTRSV, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --x;
    --cnorm;

    /* Function Body */
    *info = 0;
/*<       UPPER = LSAME( UPLO, 'U' ) >*/
    upper = lsame_(uplo, "U", (ftnlen)1, (ftnlen)1);
/*<       NOTRAN = LSAME( TRANS, 'N' ) >*/
    notran = lsame_(trans, "N", (ftnlen)1, (ftnlen)1);
/*<       NOUNIT = LSAME( DIAG, 'N' ) >*/
    nounit = lsame_(diag, "N", (ftnlen)1, (ftnlen)1);

/*     Test the input parameters. */

/*<       IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN >*/
    if (! upper && ! lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<    >*/
    } else if (! notran && ! lsame_(trans, "T", (ftnlen)1, (ftnlen)1) && !
            lsame_(trans, "C", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN >*/
    } else if (! nounit && ! lsame_(diag, "U", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -3 >*/
        *info = -3;
/*<    >*/
    } else if (! lsame_(normin, "Y", (ftnlen)1, (ftnlen)1) && ! lsame_(normin,
             "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DLATRS', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DLATRS", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n == 0) {
        return 0;
    }

/*     Determine machine dependent parameters to control overflow. */

/*<       SMLNUM = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' ) >*/
    smlnum = dlamch_("Safe minimum", (ftnlen)12) / dlamch_("Precision", (
            ftnlen)9);
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       SCALE = ONE >*/
    *scale = 1.;

/*<       IF( LSAME( NORMIN, 'N' ) ) THEN >*/
    if (lsame_(normin, "N", (ftnlen)1, (ftnlen)1)) {

/*        Compute the 1-norm of each column, not including the diagonal. */

/*<          IF( UPPER ) THEN >*/
        if (upper) {

/*           A is upper triangular. */

/*<             DO 10 J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                CNORM( J ) = DASUM( J-1, A( 1, J ), 1 ) >*/
                i__2 = j - 1;
                cnorm[j] = dasum_(&i__2, &a[j * a_dim1 + 1], &c__1);
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<          ELSE >*/
        } else {

/*           A is lower triangular. */

/*<             DO 20 J = 1, N - 1 >*/
            i__1 = *n - 1;
            for (j = 1; j <= i__1; ++j) {
/*<                CNORM( J ) = DASUM( N-J, A( J+1, J ), 1 ) >*/
                i__2 = *n - j;
                cnorm[j] = dasum_(&i__2, &a[j + 1 + j * a_dim1], &c__1);
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<             CNORM( N ) = ZERO >*/
            cnorm[*n] = 0.;
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*     Scale the column norms by TSCAL if the maximum element in CNORM is */
/*     greater than BIGNUM. */

/*<       IMAX = IDAMAX( N, CNORM, 1 ) >*/
    imax = idamax_(n, &cnorm[1], &c__1);
/*<       TMAX = CNORM( IMAX ) >*/
    tmax = cnorm[imax];
/*<       IF( TMAX.LE.BIGNUM ) THEN >*/
    if (tmax <= bignum) {
/*<          TSCAL = ONE >*/
        tscal = 1.;
/*<       ELSE >*/
    } else {
/*<          TSCAL = ONE / ( SMLNUM*TMAX ) >*/
        tscal = 1. / (smlnum * tmax);
/*<          CALL DSCAL( N, TSCAL, CNORM, 1 ) >*/
        dscal_(n, &tscal, &cnorm[1], &c__1);
/*<       END IF >*/
    }

/*     Compute a bound on the computed solution vector to see if the */
/*     Level 2 BLAS routine DTRSV can be used. */

/*<       J = IDAMAX( N, X, 1 ) >*/
    j = idamax_(n, &x[1], &c__1);
/*<       XMAX = ABS( X( J ) ) >*/
    xmax = (d__1 = x[j], abs(d__1));
/*<       XBND = XMAX >*/
    xbnd = xmax;
/*<       IF( NOTRAN ) THEN >*/
    if (notran) {

/*        Compute the growth in A * x = b. */

/*<          IF( UPPER ) THEN >*/
        if (upper) {
/*<             JFIRST = N >*/
            jfirst = *n;
/*<             JLAST = 1 >*/
            jlast = 1;
/*<             JINC = -1 >*/
            jinc = -1;
/*<          ELSE >*/
        } else {
/*<             JFIRST = 1 >*/
            jfirst = 1;
/*<             JLAST = N >*/
            jlast = *n;
/*<             JINC = 1 >*/
            jinc = 1;
/*<          END IF >*/
        }

/*<          IF( TSCAL.NE.ONE ) THEN >*/
        if (tscal != 1.) {
/*<             GROW = ZERO >*/
            grow = 0.;
/*<             GO TO 50 >*/
            goto L50;
/*<          END IF >*/
        }

/*<          IF( NOUNIT ) THEN >*/
        if (nounit) {

/*           A is non-unit triangular. */

/*           Compute GROW = 1/G(j) and XBND = 1/M(j). */
/*           Initially, G(0) = max{x(i), i=1,...,n}. */

/*<             GROW = ONE / MAX( XBND, SMLNUM ) >*/
            grow = 1. / max(xbnd,smlnum);
/*<             XBND = GROW >*/
            xbnd = grow;
/*<             DO 30 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L50;
                }

/*              M(j) = G(j-1) / abs(A(j,j)) */

/*<                TJJ = ABS( A( J, J ) ) >*/
                tjj = (d__1 = a[j + j * a_dim1], abs(d__1));
/*<                XBND = MIN( XBND, MIN( ONE, TJJ )*GROW ) >*/
/* Computing MIN */
                d__1 = xbnd, d__2 = min(1.,tjj) * grow;
                xbnd = min(d__1,d__2);
/*<                IF( TJJ+CNORM( J ).GE.SMLNUM ) THEN >*/
                if (tjj + cnorm[j] >= smlnum) {

/*                 G(j) = G(j-1)*( 1 + CNORM(j) / abs(A(j,j)) ) */

/*<                   GROW = GROW*( TJJ / ( TJJ+CNORM( J ) ) ) >*/
                    grow *= tjj / (tjj + cnorm[j]);
/*<                ELSE >*/
                } else {

/*                 G(j) could overflow, set GROW to 0. */

/*<                   GROW = ZERO >*/
                    grow = 0.;
/*<                END IF >*/
                }
/*<    30       CONTINUE >*/
/* L30: */
            }
/*<             GROW = XBND >*/
            grow = xbnd;
/*<          ELSE >*/
        } else {

/*           A is unit triangular. */

/*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}. */

/*<             GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) ) >*/
/* Computing MIN */
            d__1 = 1., d__2 = 1. / max(xbnd,smlnum);
            grow = min(d__1,d__2);
/*<             DO 40 J = JFIRST, JLAST, JINC >*/
            i__2 = jlast;
            i__1 = jinc;
            for (j = jfirst; i__1 < 0 ? j >= i__2 : j <= i__2; j += i__1) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L50;
                }

/*              G(j) = G(j-1)*( 1 + CNORM(j) ) */

/*<                GROW = GROW*( ONE / ( ONE+CNORM( J ) ) ) >*/
                grow *= 1. / (cnorm[j] + 1.);
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<          END IF >*/
        }
/*<    50    CONTINUE >*/
L50:

/*<       ELSE >*/
        ;
    } else {

/*        Compute the growth in A' * x = b. */

/*<          IF( UPPER ) THEN >*/
        if (upper) {
/*<             JFIRST = 1 >*/
            jfirst = 1;
/*<             JLAST = N >*/
            jlast = *n;
/*<             JINC = 1 >*/
            jinc = 1;
/*<          ELSE >*/
        } else {
/*<             JFIRST = N >*/
            jfirst = *n;
/*<             JLAST = 1 >*/
            jlast = 1;
/*<             JINC = -1 >*/
            jinc = -1;
/*<          END IF >*/
        }

/*<          IF( TSCAL.NE.ONE ) THEN >*/
        if (tscal != 1.) {
/*<             GROW = ZERO >*/
            grow = 0.;
/*<             GO TO 80 >*/
            goto L80;
/*<          END IF >*/
        }

/*<          IF( NOUNIT ) THEN >*/
        if (nounit) {

/*           A is non-unit triangular. */

/*           Compute GROW = 1/G(j) and XBND = 1/M(j). */
/*           Initially, M(0) = max{x(i), i=1,...,n}. */

/*<             GROW = ONE / MAX( XBND, SMLNUM ) >*/
            grow = 1. / max(xbnd,smlnum);
/*<             XBND = GROW >*/
            xbnd = grow;
/*<             DO 60 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L80;
                }

/*              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) ) */

/*<                XJ = ONE + CNORM( J ) >*/
                xj = cnorm[j] + 1.;
/*<                GROW = MIN( GROW, XBND / XJ ) >*/
/* Computing MIN */
                d__1 = grow, d__2 = xbnd / xj;
                grow = min(d__1,d__2);

/*              M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j)) */

/*<                TJJ = ABS( A( J, J ) ) >*/
                tjj = (d__1 = a[j + j * a_dim1], abs(d__1));
/*<    >*/
                if (xj > tjj) {
                    xbnd *= tjj / xj;
                }
/*<    60       CONTINUE >*/
/* L60: */
            }
/*<             GROW = MIN( GROW, XBND ) >*/
            grow = min(grow,xbnd);
/*<          ELSE >*/
        } else {

/*           A is unit triangular. */

/*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}. */

/*<             GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) ) >*/
/* Computing MIN */
            d__1 = 1., d__2 = 1. / max(xbnd,smlnum);
            grow = min(d__1,d__2);
/*<             DO 70 J = JFIRST, JLAST, JINC >*/
            i__2 = jlast;
            i__1 = jinc;
            for (j = jfirst; i__1 < 0 ? j >= i__2 : j <= i__2; j += i__1) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L80;
                }

/*              G(j) = ( 1 + CNORM(j) )*G(j-1) */

/*<                XJ = ONE + CNORM( J ) >*/
                xj = cnorm[j] + 1.;
/*<                GROW = GROW / XJ >*/
                grow /= xj;
/*<    70       CONTINUE >*/
/* L70: */
            }
/*<          END IF >*/
        }
/*<    80    CONTINUE >*/
L80:
/*<       END IF >*/
        ;
    }

/*<       IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN >*/
    if (grow * tscal > smlnum) {

/*        Use the Level 2 BLAS solve if the reciprocal of the bound on */
/*        elements of X is not too small. */

/*<          CALL DTRSV( UPLO, TRANS, DIAG, N, A, LDA, X, 1 ) >*/
        dtrsv_(uplo, trans, diag, n, &a[a_offset], lda, &x[1], &c__1, (ftnlen)
                1, (ftnlen)1, (ftnlen)1);
/*<       ELSE >*/
    } else {

/*        Use a Level 1 BLAS solve, scaling intermediate results. */

/*<          IF( XMAX.GT.BIGNUM ) THEN >*/
        if (xmax > bignum) {

/*           Scale X so that its components are less than or equal to */
/*           BIGNUM in absolute value. */

/*<             SCALE = BIGNUM / XMAX >*/
            *scale = bignum / xmax;
/*<             CALL DSCAL( N, SCALE, X, 1 ) >*/
            dscal_(n, scale, &x[1], &c__1);
/*<             XMAX = BIGNUM >*/
            xmax = bignum;
/*<          END IF >*/
        }

/*<          IF( NOTRAN ) THEN >*/
        if (notran) {

/*           Solve A * x = b */

/*<             DO 110 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Compute x(j) = b(j) / A(j,j), scaling x if necessary. */

/*<                XJ = ABS( X( J ) ) >*/
                xj = (d__1 = x[j], abs(d__1));
/*<                IF( NOUNIT ) THEN >*/
                if (nounit) {
/*<                   TJJS = A( J, J )*TSCAL >*/
                    tjjs = a[j + j * a_dim1] * tscal;
/*<                ELSE >*/
                } else {
/*<                   TJJS = TSCAL >*/
                    tjjs = tscal;
/*<    >*/
                    if (tscal == 1.) {
                        goto L100;
                    }
/*<                END IF >*/
                }
/*<                TJJ = ABS( TJJS ) >*/
                tjj = abs(tjjs);
/*<                IF( TJJ.GT.SMLNUM ) THEN >*/
                if (tjj > smlnum) {

/*                    abs(A(j,j)) > SMLNUM: */

/*<                   IF( TJJ.LT.ONE ) THEN >*/
                    if (tjj < 1.) {
/*<                      IF( XJ.GT.TJJ*BIGNUM ) THEN >*/
                        if (xj > tjj * bignum) {

/*                          Scale x by 1/b(j). */

/*<                         REC = ONE / XJ >*/
                            rec = 1. / xj;
/*<                         CALL DSCAL( N, REC, X, 1 ) >*/
                            dscal_(n, &rec, &x[1], &c__1);
/*<                         SCALE = SCALE*REC >*/
                            *scale *= rec;
/*<                         XMAX = XMAX*REC >*/
                            xmax *= rec;
/*<                      END IF >*/
                        }
/*<                   END IF >*/
                    }
/*<                   X( J ) = X( J ) / TJJS >*/
                    x[j] /= tjjs;
/*<                   XJ = ABS( X( J ) ) >*/
                    xj = (d__1 = x[j], abs(d__1));
/*<                ELSE IF( TJJ.GT.ZERO ) THEN >*/
                } else if (tjj > 0.) {

/*                    0 < abs(A(j,j)) <= SMLNUM: */

/*<                   IF( XJ.GT.TJJ*BIGNUM ) THEN >*/
                    if (xj > tjj * bignum) {

/*                       Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM */
/*                       to avoid overflow when dividing by A(j,j). */

/*<                      REC = ( TJJ*BIGNUM ) / XJ >*/
                        rec = tjj * bignum / xj;
/*<                      IF( CNORM( J ).GT.ONE ) THEN >*/
                        if (cnorm[j] > 1.) {

/*                          Scale by 1/CNORM(j) to avoid overflow when */
/*                          multiplying x(j) times column j. */

/*<                         REC = REC / CNORM( J ) >*/
                            rec /= cnorm[j];
/*<                      END IF >*/
                        }
/*<                      CALL DSCAL( N, REC, X, 1 ) >*/
                        dscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                      XMAX = XMAX*REC >*/
                        xmax *= rec;
/*<                   END IF >*/
                    }
/*<                   X( J ) = X( J ) / TJJS >*/
                    x[j] /= tjjs;
/*<                   XJ = ABS( X( J ) ) >*/
                    xj = (d__1 = x[j], abs(d__1));
/*<                ELSE >*/
                } else {

/*                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
/*                    scale = 0, and compute a solution to A*x = 0. */

/*<                   DO 90 I = 1, N >*/
                    i__3 = *n;
                    for (i__ = 1; i__ <= i__3; ++i__) {
/*<                      X( I ) = ZERO >*/
                        x[i__] = 0.;
/*<    90             CONTINUE >*/
/* L90: */
                    }
/*<                   X( J ) = ONE >*/
                    x[j] = 1.;
/*<                   XJ = ONE >*/
                    xj = 1.;
/*<                   SCALE = ZERO >*/
                    *scale = 0.;
/*<                   XMAX = ZERO >*/
                    xmax = 0.;
/*<                END IF >*/
                }
/*<   100          CONTINUE >*/
L100:

/*              Scale x if necessary to avoid overflow when adding a */
/*              multiple of column j of A. */

/*<                IF( XJ.GT.ONE ) THEN >*/
                if (xj > 1.) {
/*<                   REC = ONE / XJ >*/
                    rec = 1. / xj;
/*<                   IF( CNORM( J ).GT.( BIGNUM-XMAX )*REC ) THEN >*/
                    if (cnorm[j] > (bignum - xmax) * rec) {

/*                    Scale x by 1/(2*abs(x(j))). */

/*<                      REC = REC*HALF >*/
                        rec *= .5;
/*<                      CALL DSCAL( N, REC, X, 1 ) >*/
                        dscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                   END IF >*/
                    }
/*<                ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN >*/
                } else if (xj * cnorm[j] > bignum - xmax) {

/*                 Scale x by 1/2. */

/*<                   CALL DSCAL( N, HALF, X, 1 ) >*/
                    dscal_(n, &c_b36, &x[1], &c__1);
/*<                   SCALE = SCALE*HALF >*/
                    *scale *= .5;
/*<                END IF >*/
                }

/*<                IF( UPPER ) THEN >*/
                if (upper) {
/*<                   IF( J.GT.1 ) THEN >*/
                    if (j > 1) {

/*                    Compute the update */
/*                       x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j) */

/*<    >*/
                        i__3 = j - 1;
                        d__1 = -x[j] * tscal;
                        daxpy_(&i__3, &d__1, &a[j * a_dim1 + 1], &c__1, &x[1],
                                 &c__1);
/*<                      I = IDAMAX( J-1, X, 1 ) >*/
                        i__3 = j - 1;
                        i__ = idamax_(&i__3, &x[1], &c__1);
/*<                      XMAX = ABS( X( I ) ) >*/
                        xmax = (d__1 = x[i__], abs(d__1));
/*<                   END IF >*/
                    }
/*<                ELSE >*/
                } else {
/*<                   IF( J.LT.N ) THEN >*/
                    if (j < *n) {

/*                    Compute the update */
/*                       x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j) */

/*<    >*/
                        i__3 = *n - j;
                        d__1 = -x[j] * tscal;
                        daxpy_(&i__3, &d__1, &a[j + 1 + j * a_dim1], &c__1, &
                                x[j + 1], &c__1);
/*<                      I = J + IDAMAX( N-J, X( J+1 ), 1 ) >*/
                        i__3 = *n - j;
                        i__ = j + idamax_(&i__3, &x[j + 1], &c__1);
/*<                      XMAX = ABS( X( I ) ) >*/
                        xmax = (d__1 = x[i__], abs(d__1));
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }
/*<   110       CONTINUE >*/
/* L110: */
            }

/*<          ELSE >*/
        } else {

/*           Solve A' * x = b */

/*<             DO 160 J = JFIRST, JLAST, JINC >*/
            i__2 = jlast;
            i__1 = jinc;
            for (j = jfirst; i__1 < 0 ? j >= i__2 : j <= i__2; j += i__1) {

/*              Compute x(j) = b(j) - sum A(k,j)*x(k). */
/*                                    k<>j */

/*<                XJ = ABS( X( J ) ) >*/
                xj = (d__1 = x[j], abs(d__1));
/*<                USCAL = TSCAL >*/
                uscal = tscal;
/*<                REC = ONE / MAX( XMAX, ONE ) >*/
                rec = 1. / max(xmax,1.);
/*<                IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN >*/
                if (cnorm[j] > (bignum - xj) * rec) {

/*                 If x(j) could overflow, scale x by 1/(2*XMAX). */

/*<                   REC = REC*HALF >*/
                    rec *= .5;
/*<                   IF( NOUNIT ) THEN >*/
                    if (nounit) {
/*<                      TJJS = A( J, J )*TSCAL >*/
                        tjjs = a[j + j * a_dim1] * tscal;
/*<                   ELSE >*/
                    } else {
/*<                      TJJS = TSCAL >*/
                        tjjs = tscal;
/*<                   END IF >*/
                    }
/*<                   TJJ = ABS( TJJS ) >*/
                    tjj = abs(tjjs);
/*<                   IF( TJJ.GT.ONE ) THEN >*/
                    if (tjj > 1.) {

/*                       Divide by A(j,j) when scaling x if A(j,j) > 1. */

/*<                      REC = MIN( ONE, REC*TJJ ) >*/
/* Computing MIN */
                        d__1 = 1., d__2 = rec * tjj;
                        rec = min(d__1,d__2);
/*<                      USCAL = USCAL / TJJS >*/
                        uscal /= tjjs;
/*<                   END IF >*/
                    }
/*<                   IF( REC.LT.ONE ) THEN >*/
                    if (rec < 1.) {
/*<                      CALL DSCAL( N, REC, X, 1 ) >*/
                        dscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                      XMAX = XMAX*REC >*/
                        xmax *= rec;
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }

/*<                SUMJ = ZERO >*/
                sumj = 0.;
/*<                IF( USCAL.EQ.ONE ) THEN >*/
                if (uscal == 1.) {

/*                 If the scaling needed for A in the dot product is 1, */
/*                 call DDOT to perform the dot product. */

/*<                   IF( UPPER ) THEN >*/
                    if (upper) {
/*<                      SUMJ = DDOT( J-1, A( 1, J ), 1, X, 1 ) >*/
                        i__3 = j - 1;
                        sumj = ddot_(&i__3, &a[j * a_dim1 + 1], &c__1, &x[1],
                                &c__1);
/*<                   ELSE IF( J.LT.N ) THEN >*/
                    } else if (j < *n) {
/*<                      SUMJ = DDOT( N-J, A( J+1, J ), 1, X( J+1 ), 1 ) >*/
                        i__3 = *n - j;
                        sumj = ddot_(&i__3, &a[j + 1 + j * a_dim1], &c__1, &x[
                                j + 1], &c__1);
/*<                   END IF >*/
                    }
/*<                ELSE >*/
                } else {

/*                 Otherwise, use in-line code for the dot product. */

/*<                   IF( UPPER ) THEN >*/
                    if (upper) {
/*<                      DO 120 I = 1, J - 1 >*/
                        i__3 = j - 1;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         SUMJ = SUMJ + ( A( I, J )*USCAL )*X( I ) >*/
                            sumj += a[i__ + j * a_dim1] * uscal * x[i__];
/*<   120                CONTINUE >*/
/* L120: */
                        }
/*<                   ELSE IF( J.LT.N ) THEN >*/
                    } else if (j < *n) {
/*<                      DO 130 I = J + 1, N >*/
                        i__3 = *n;
                        for (i__ = j + 1; i__ <= i__3; ++i__) {
/*<                         SUMJ = SUMJ + ( A( I, J )*USCAL )*X( I ) >*/
                            sumj += a[i__ + j * a_dim1] * uscal * x[i__];
/*<   130                CONTINUE >*/
/* L130: */
                        }
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }

/*<                IF( USCAL.EQ.TSCAL ) THEN >*/
                if (uscal == tscal) {

/*                 Compute x(j) := ( x(j) - sumj ) / A(j,j) if 1/A(j,j) */
/*                 was not used to scale the dotproduct. */

/*<                   X( J ) = X( J ) - SUMJ >*/
                    x[j] -= sumj;
/*<                   XJ = ABS( X( J ) ) >*/
                    xj = (d__1 = x[j], abs(d__1));
/*<                   IF( NOUNIT ) THEN >*/
                    if (nounit) {
/*<                      TJJS = A( J, J )*TSCAL >*/
                        tjjs = a[j + j * a_dim1] * tscal;
/*<                   ELSE >*/
                    } else {
/*<                      TJJS = TSCAL >*/
                        tjjs = tscal;
/*<    >*/
                        if (tscal == 1.) {
                            goto L150;
                        }
/*<                   END IF >*/
                    }

/*                    Compute x(j) = x(j) / A(j,j), scaling if necessary. */

/*<                   TJJ = ABS( TJJS ) >*/
                    tjj = abs(tjjs);
/*<                   IF( TJJ.GT.SMLNUM ) THEN >*/
                    if (tjj > smlnum) {

/*                       abs(A(j,j)) > SMLNUM: */

/*<                      IF( TJJ.LT.ONE ) THEN >*/
                        if (tjj < 1.) {
/*<                         IF( XJ.GT.TJJ*BIGNUM ) THEN >*/
                            if (xj > tjj * bignum) {

/*                             Scale X by 1/abs(x(j)). */

/*<                            REC = ONE / XJ >*/
                                rec = 1. / xj;
/*<                            CALL DSCAL( N, REC, X, 1 ) >*/
                                dscal_(n, &rec, &x[1], &c__1);
/*<                            SCALE = SCALE*REC >*/
                                *scale *= rec;
/*<                            XMAX = XMAX*REC >*/
                                xmax *= rec;
/*<                         END IF >*/
                            }
/*<                      END IF >*/
                        }
/*<                      X( J ) = X( J ) / TJJS >*/
                        x[j] /= tjjs;
/*<                   ELSE IF( TJJ.GT.ZERO ) THEN >*/
                    } else if (tjj > 0.) {

/*                       0 < abs(A(j,j)) <= SMLNUM: */

/*<                      IF( XJ.GT.TJJ*BIGNUM ) THEN >*/
                        if (xj > tjj * bignum) {

/*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM. */

/*<                         REC = ( TJJ*BIGNUM ) / XJ >*/
                            rec = tjj * bignum / xj;
/*<                         CALL DSCAL( N, REC, X, 1 ) >*/
                            dscal_(n, &rec, &x[1], &c__1);
/*<                         SCALE = SCALE*REC >*/
                            *scale *= rec;
/*<                         XMAX = XMAX*REC >*/
                            xmax *= rec;
/*<                      END IF >*/
                        }
/*<                      X( J ) = X( J ) / TJJS >*/
                        x[j] /= tjjs;
/*<                   ELSE >*/
                    } else {

/*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
/*                       scale = 0, and compute a solution to A'*x = 0. */

/*<                      DO 140 I = 1, N >*/
                        i__3 = *n;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         X( I ) = ZERO >*/
                            x[i__] = 0.;
/*<   140                CONTINUE >*/
/* L140: */
                        }
/*<                      X( J ) = ONE >*/
                        x[j] = 1.;
/*<                      SCALE = ZERO >*/
                        *scale = 0.;
/*<                      XMAX = ZERO >*/
                        xmax = 0.;
/*<                   END IF >*/
                    }
/*<   150             CONTINUE >*/
L150:
/*<                ELSE >*/
                    ;
                } else {

/*                 Compute x(j) := x(j) / A(j,j)  - sumj if the dot */
/*                 product has already been divided by 1/A(j,j). */

/*<                   X( J ) = X( J ) / TJJS - SUMJ >*/
                    x[j] = x[j] / tjjs - sumj;
/*<                END IF >*/
                }
/*<                XMAX = MAX( XMAX, ABS( X( J ) ) ) >*/
/* Computing MAX */
                d__2 = xmax, d__3 = (d__1 = x[j], abs(d__1));
                xmax = max(d__2,d__3);
/*<   160       CONTINUE >*/
/* L160: */
            }
/*<          END IF >*/
        }
/*<          SCALE = SCALE / TSCAL >*/
        *scale /= tscal;
/*<       END IF >*/
    }

/*     Scale the column norms by 1/TSCAL for return. */

/*<       IF( TSCAL.NE.ONE ) THEN >*/
    if (tscal != 1.) {
/*<          CALL DSCAL( N, ONE / TSCAL, CNORM, 1 ) >*/
        d__1 = 1. / tscal;
        dscal_(n, &d__1, &cnorm[1], &c__1);
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DLATRS */

/*<       END >*/
} /* dlatrs_ */

#ifdef __cplusplus
        }
#endif
