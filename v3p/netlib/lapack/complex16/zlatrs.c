/* lapack/complex16/zlatrs.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int zlatrs_(char *uplo, char *trans, char *diag, char *
        normin, integer *n, doublecomplex *a, integer *lda, doublecomplex *x,
        doublereal *scale, doublereal *cnorm, integer *info, ftnlen uplo_len,
        ftnlen trans_len, ftnlen diag_len, ftnlen normin_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    double d_imag(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j;
    doublereal xj, rec, tjj;
    integer jinc;
    doublereal xbnd;
    integer imax;
    doublereal tmax;
    doublecomplex tjjs;
    doublereal xmax, grow;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal tscal;
    doublecomplex uscal;
    integer jlast;
    doublecomplex csumj;
    extern /* Double Complex */ VOID zdotc_(doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *);
    logical upper;
    extern /* Double Complex */ VOID zdotu_(doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zaxpy_(integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *), ztrsv_(
            char *, char *, char *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, ftnlen, ftnlen, ftnlen), dlabad_(
            doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), zdscal_(
            integer *, doublereal *, doublecomplex *, integer *);
    doublereal bignum;
    extern integer izamax_(integer *, doublecomplex *, integer *);
    extern /* Double Complex */ VOID zladiv_(doublecomplex *, doublecomplex *,
             doublecomplex *);
    logical notran;
    integer jfirst;
    extern doublereal dzasum_(integer *, doublecomplex *, integer *);
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
/*<       DOUBLE PRECISION   CNORM( * ) >*/
/*<       COMPLEX*16         A( LDA, * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLATRS solves one of the triangular systems */

/*     A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b, */

/*  with scaling to prevent overflow.  Here A is an upper or lower */
/*  triangular matrix, A**T denotes the transpose of A, A**H denotes the */
/*  conjugate transpose of A, x and b are n-element vectors, and s is a */
/*  scaling factor, usually less than or equal to 1, chosen so that the */
/*  components of x will be less than the overflow threshold.  If the */
/*  unscaled problem will not cause overflow, the Level 2 BLAS routine */
/*  ZTRSV is called. If the matrix A is singular (A(j,j) = 0 for some j), */
/*  then s is set to 0 and a non-trivial solution to A*x = 0 is returned. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          Specifies whether the matrix A is upper or lower triangular. */
/*          = 'U':  Upper triangular */
/*          = 'L':  Lower triangular */

/*  TRANS   (input) CHARACTER*1 */
/*          Specifies the operation applied to A. */
/*          = 'N':  Solve A * x = s*b     (No transpose) */
/*          = 'T':  Solve A**T * x = s*b  (Transpose) */
/*          = 'C':  Solve A**H * x = s*b  (Conjugate transpose) */

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

/*  A       (input) COMPLEX*16 array, dimension (LDA,N) */
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

/*  X       (input/output) COMPLEX*16 array, dimension (N) */
/*          On entry, the right hand side b of the triangular system. */
/*          On exit, X is overwritten by the solution vector x. */

/*  SCALE   (output) DOUBLE PRECISION */
/*          The scaling factor s for the triangular system */
/*             A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b. */
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

/*  A rough bound on x is computed; if that is less than overflow, ZTRSV */
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

/*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine ZTRSV if the */
/*  reciprocal of the largest M(j), j=1,..,n, is larger than */
/*  max(underflow, 1/overflow). */

/*  The bound on x(j) is also used to determine when a step in the */
/*  columnwise method can be performed without fear of overflow.  If */
/*  the computed bound is greater than a large constant, x is scaled to */
/*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to */
/*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found. */

/*  Similarly, a row-wise scheme is used to solve A**T *x = b  or */
/*  A**H *x = b.  The basic algorithm for A upper triangular is */

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

/*  and we can safely call ZTRSV if 1/M(n) and 1/G(n) are both greater */
/*  than max(underflow, 1/overflow). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, HALF, ONE, TWO >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            NOTRAN, NOUNIT, UPPER >*/
/*<       INTEGER            I, IMAX, J, JFIRST, JINC, JLAST >*/
/*<    >*/
/*<       COMPLEX*16         CSUMJ, TJJS, USCAL, ZDUM >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IDAMAX, IZAMAX >*/
/*<       DOUBLE PRECISION   DLAMCH, DZASUM >*/
/*<       COMPLEX*16         ZDOTC, ZDOTU, ZLADIV >*/
/*<    >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DSCAL, XERBLA, ZAXPY, ZDSCAL, ZTRSV >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   CABS1, CABS2 >*/
/*     .. */
/*     .. Statement Function definitions .. */
/*<       CABS1( ZDUM ) = ABS( DBLE( ZDUM ) ) + ABS( DIMAG( ZDUM ) ) >*/
/*<    >*/
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
/*<          CALL XERBLA( 'ZLATRS', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZLATRS", &i__1, (ftnlen)6);
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

/*<       SMLNUM = DLAMCH( 'Safe minimum' ) >*/
    smlnum = dlamch_("Safe minimum", (ftnlen)12);
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       CALL DLABAD( SMLNUM, BIGNUM ) >*/
    dlabad_(&smlnum, &bignum);
/*<       SMLNUM = SMLNUM / DLAMCH( 'Precision' ) >*/
    smlnum /= dlamch_("Precision", (ftnlen)9);
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
/*<                CNORM( J ) = DZASUM( J-1, A( 1, J ), 1 ) >*/
                i__2 = j - 1;
                cnorm[j] = dzasum_(&i__2, &a[j * a_dim1 + 1], &c__1);
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<          ELSE >*/
        } else {

/*           A is lower triangular. */

/*<             DO 20 J = 1, N - 1 >*/
            i__1 = *n - 1;
            for (j = 1; j <= i__1; ++j) {
/*<                CNORM( J ) = DZASUM( N-J, A( J+1, J ), 1 ) >*/
                i__2 = *n - j;
                cnorm[j] = dzasum_(&i__2, &a[j + 1 + j * a_dim1], &c__1);
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
/*     greater than BIGNUM/2. */

/*<       IMAX = IDAMAX( N, CNORM, 1 ) >*/
    imax = idamax_(n, &cnorm[1], &c__1);
/*<       TMAX = CNORM( IMAX ) >*/
    tmax = cnorm[imax];
/*<       IF( TMAX.LE.BIGNUM*HALF ) THEN >*/
    if (tmax <= bignum * .5) {
/*<          TSCAL = ONE >*/
        tscal = 1.;
/*<       ELSE >*/
    } else {
/*<          TSCAL = HALF / ( SMLNUM*TMAX ) >*/
        tscal = .5 / (smlnum * tmax);
/*<          CALL DSCAL( N, TSCAL, CNORM, 1 ) >*/
        dscal_(n, &tscal, &cnorm[1], &c__1);
/*<       END IF >*/
    }

/*     Compute a bound on the computed solution vector to see if the */
/*     Level 2 BLAS routine ZTRSV can be used. */

/*<       XMAX = ZERO >*/
    xmax = 0.;
/*<       DO 30 J = 1, N >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          XMAX = MAX( XMAX, CABS2( X( J ) ) ) >*/
/* Computing MAX */
        i__2 = j;
        d__3 = xmax, d__4 = (d__1 = x[i__2].r / 2., abs(d__1)) + (d__2 =
                d_imag(&x[j]) / 2., abs(d__2));
        xmax = max(d__3,d__4);
/*<    30 CONTINUE >*/
/* L30: */
    }
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
/*<             GO TO 60 >*/
            goto L60;
/*<          END IF >*/
        }

/*<          IF( NOUNIT ) THEN >*/
        if (nounit) {

/*           A is non-unit triangular. */

/*           Compute GROW = 1/G(j) and XBND = 1/M(j). */
/*           Initially, G(0) = max{x(i), i=1,...,n}. */

/*<             GROW = HALF / MAX( XBND, SMLNUM ) >*/
            grow = .5 / max(xbnd,smlnum);
/*<             XBND = GROW >*/
            xbnd = grow;
/*<             DO 40 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L60;
                }

/*<                TJJS = A( J, J ) >*/
                i__3 = j + j * a_dim1;
                tjjs.r = a[i__3].r, tjjs.i = a[i__3].i;
/*<                TJJ = CABS1( TJJS ) >*/
                tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs), abs(
                        d__2));

/*<                IF( TJJ.GE.SMLNUM ) THEN >*/
                if (tjj >= smlnum) {

/*                 M(j) = G(j-1) / abs(A(j,j)) */

/*<                   XBND = MIN( XBND, MIN( ONE, TJJ )*GROW ) >*/
/* Computing MIN */
                    d__1 = xbnd, d__2 = min(1.,tjj) * grow;
                    xbnd = min(d__1,d__2);
/*<                ELSE >*/
                } else {

/*                 M(j) could overflow, set XBND to 0. */

/*<                   XBND = ZERO >*/
                    xbnd = 0.;
/*<                END IF >*/
                }

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
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<             GROW = XBND >*/
            grow = xbnd;
/*<          ELSE >*/
        } else {

/*           A is unit triangular. */

/*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}. */

/*<             GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) ) >*/
/* Computing MIN */
            d__1 = 1., d__2 = .5 / max(xbnd,smlnum);
            grow = min(d__1,d__2);
/*<             DO 50 J = JFIRST, JLAST, JINC >*/
            i__2 = jlast;
            i__1 = jinc;
            for (j = jfirst; i__1 < 0 ? j >= i__2 : j <= i__2; j += i__1) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L60;
                }

/*              G(j) = G(j-1)*( 1 + CNORM(j) ) */

/*<                GROW = GROW*( ONE / ( ONE+CNORM( J ) ) ) >*/
                grow *= 1. / (cnorm[j] + 1.);
/*<    50       CONTINUE >*/
/* L50: */
            }
/*<          END IF >*/
        }
/*<    60    CONTINUE >*/
L60:

/*<       ELSE >*/
        ;
    } else {

/*        Compute the growth in A**T * x = b  or  A**H * x = b. */

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
/*<             GO TO 90 >*/
            goto L90;
/*<          END IF >*/
        }

/*<          IF( NOUNIT ) THEN >*/
        if (nounit) {

/*           A is non-unit triangular. */

/*           Compute GROW = 1/G(j) and XBND = 1/M(j). */
/*           Initially, M(0) = max{x(i), i=1,...,n}. */

/*<             GROW = HALF / MAX( XBND, SMLNUM ) >*/
            grow = .5 / max(xbnd,smlnum);
/*<             XBND = GROW >*/
            xbnd = grow;
/*<             DO 70 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L90;
                }

/*              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) ) */

/*<                XJ = ONE + CNORM( J ) >*/
                xj = cnorm[j] + 1.;
/*<                GROW = MIN( GROW, XBND / XJ ) >*/
/* Computing MIN */
                d__1 = grow, d__2 = xbnd / xj;
                grow = min(d__1,d__2);

/*<                TJJS = A( J, J ) >*/
                i__3 = j + j * a_dim1;
                tjjs.r = a[i__3].r, tjjs.i = a[i__3].i;
/*<                TJJ = CABS1( TJJS ) >*/
                tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs), abs(
                        d__2));

/*<                IF( TJJ.GE.SMLNUM ) THEN >*/
                if (tjj >= smlnum) {

/*                 M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j)) */

/*<    >*/
                    if (xj > tjj) {
                        xbnd *= tjj / xj;
                    }
/*<                ELSE >*/
                } else {

/*                 M(j) could overflow, set XBND to 0. */

/*<                   XBND = ZERO >*/
                    xbnd = 0.;
/*<                END IF >*/
                }
/*<    70       CONTINUE >*/
/* L70: */
            }
/*<             GROW = MIN( GROW, XBND ) >*/
            grow = min(grow,xbnd);
/*<          ELSE >*/
        } else {

/*           A is unit triangular. */

/*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}. */

/*<             GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) ) >*/
/* Computing MIN */
            d__1 = 1., d__2 = .5 / max(xbnd,smlnum);
            grow = min(d__1,d__2);
/*<             DO 80 J = JFIRST, JLAST, JINC >*/
            i__2 = jlast;
            i__1 = jinc;
            for (j = jfirst; i__1 < 0 ? j >= i__2 : j <= i__2; j += i__1) {

/*              Exit the loop if the growth factor is too small. */

/*<    >*/
                if (grow <= smlnum) {
                    goto L90;
                }

/*              G(j) = ( 1 + CNORM(j) )*G(j-1) */

/*<                XJ = ONE + CNORM( J ) >*/
                xj = cnorm[j] + 1.;
/*<                GROW = GROW / XJ >*/
                grow /= xj;
/*<    80       CONTINUE >*/
/* L80: */
            }
/*<          END IF >*/
        }
/*<    90    CONTINUE >*/
L90:
/*<       END IF >*/
        ;
    }

/*<       IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN >*/
    if (grow * tscal > smlnum) {

/*        Use the Level 2 BLAS solve if the reciprocal of the bound on */
/*        elements of X is not too small. */

/*<          CALL ZTRSV( UPLO, TRANS, DIAG, N, A, LDA, X, 1 ) >*/
        ztrsv_(uplo, trans, diag, n, &a[a_offset], lda, &x[1], &c__1, (ftnlen)
                1, (ftnlen)1, (ftnlen)1);
/*<       ELSE >*/
    } else {

/*        Use a Level 1 BLAS solve, scaling intermediate results. */

/*<          IF( XMAX.GT.BIGNUM*HALF ) THEN >*/
        if (xmax > bignum * .5) {

/*           Scale X so that its components are less than or equal to */
/*           BIGNUM in absolute value. */

/*<             SCALE = ( BIGNUM*HALF ) / XMAX >*/
            *scale = bignum * .5 / xmax;
/*<             CALL ZDSCAL( N, SCALE, X, 1 ) >*/
            zdscal_(n, scale, &x[1], &c__1);
/*<             XMAX = BIGNUM >*/
            xmax = bignum;
/*<          ELSE >*/
        } else {
/*<             XMAX = XMAX*TWO >*/
            xmax *= 2.;
/*<          END IF >*/
        }

/*<          IF( NOTRAN ) THEN >*/
        if (notran) {

/*           Solve A * x = b */

/*<             DO 120 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Compute x(j) = b(j) / A(j,j), scaling x if necessary. */

/*<                XJ = CABS1( X( J ) ) >*/
                i__3 = j;
                xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j]),
                        abs(d__2));
/*<                IF( NOUNIT ) THEN >*/
                if (nounit) {
/*<                   TJJS = A( J, J )*TSCAL >*/
                    i__3 = j + j * a_dim1;
                    z__1.r = tscal * a[i__3].r, z__1.i = tscal * a[i__3].i;
                    tjjs.r = z__1.r, tjjs.i = z__1.i;
/*<                ELSE >*/
                } else {
/*<                   TJJS = TSCAL >*/
                    tjjs.r = tscal, tjjs.i = 0.;
/*<    >*/
                    if (tscal == 1.) {
                        goto L110;
                    }
/*<                END IF >*/
                }
/*<                TJJ = CABS1( TJJS ) >*/
                tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs), abs(
                        d__2));
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
/*<                         CALL ZDSCAL( N, REC, X, 1 ) >*/
                            zdscal_(n, &rec, &x[1], &c__1);
/*<                         SCALE = SCALE*REC >*/
                            *scale *= rec;
/*<                         XMAX = XMAX*REC >*/
                            xmax *= rec;
/*<                      END IF >*/
                        }
/*<                   END IF >*/
                    }
/*<                   X( J ) = ZLADIV( X( J ), TJJS ) >*/
                    i__3 = j;
                    zladiv_(&z__1, &x[j], &tjjs);
                    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   XJ = CABS1( X( J ) ) >*/
                    i__3 = j;
                    xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j])
                            , abs(d__2));
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
/*<                      CALL ZDSCAL( N, REC, X, 1 ) >*/
                        zdscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                      XMAX = XMAX*REC >*/
                        xmax *= rec;
/*<                   END IF >*/
                    }
/*<                   X( J ) = ZLADIV( X( J ), TJJS ) >*/
                    i__3 = j;
                    zladiv_(&z__1, &x[j], &tjjs);
                    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   XJ = CABS1( X( J ) ) >*/
                    i__3 = j;
                    xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j])
                            , abs(d__2));
/*<                ELSE >*/
                } else {

/*                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
/*                    scale = 0, and compute a solution to A*x = 0. */

/*<                   DO 100 I = 1, N >*/
                    i__3 = *n;
                    for (i__ = 1; i__ <= i__3; ++i__) {
/*<                      X( I ) = ZERO >*/
                        i__4 = i__;
                        x[i__4].r = 0., x[i__4].i = 0.;
/*<   100             CONTINUE >*/
/* L100: */
                    }
/*<                   X( J ) = ONE >*/
                    i__3 = j;
                    x[i__3].r = 1., x[i__3].i = 0.;
/*<                   XJ = ONE >*/
                    xj = 1.;
/*<                   SCALE = ZERO >*/
                    *scale = 0.;
/*<                   XMAX = ZERO >*/
                    xmax = 0.;
/*<                END IF >*/
                }
/*<   110          CONTINUE >*/
L110:

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
/*<                      CALL ZDSCAL( N, REC, X, 1 ) >*/
                        zdscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                   END IF >*/
                    }
/*<                ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN >*/
                } else if (xj * cnorm[j] > bignum - xmax) {

/*                 Scale x by 1/2. */

/*<                   CALL ZDSCAL( N, HALF, X, 1 ) >*/
                    zdscal_(n, &c_b36, &x[1], &c__1);
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
                        i__4 = j;
                        z__2.r = -x[i__4].r, z__2.i = -x[i__4].i;
                        z__1.r = tscal * z__2.r, z__1.i = tscal * z__2.i;
                        zaxpy_(&i__3, &z__1, &a[j * a_dim1 + 1], &c__1, &x[1],
                                 &c__1);
/*<                      I = IZAMAX( J-1, X, 1 ) >*/
                        i__3 = j - 1;
                        i__ = izamax_(&i__3, &x[1], &c__1);
/*<                      XMAX = CABS1( X( I ) ) >*/
                        i__3 = i__;
                        xmax = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(
                                &x[i__]), abs(d__2));
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
                        i__4 = j;
                        z__2.r = -x[i__4].r, z__2.i = -x[i__4].i;
                        z__1.r = tscal * z__2.r, z__1.i = tscal * z__2.i;
                        zaxpy_(&i__3, &z__1, &a[j + 1 + j * a_dim1], &c__1, &
                                x[j + 1], &c__1);
/*<                      I = J + IZAMAX( N-J, X( J+1 ), 1 ) >*/
                        i__3 = *n - j;
                        i__ = j + izamax_(&i__3, &x[j + 1], &c__1);
/*<                      XMAX = CABS1( X( I ) ) >*/
                        i__3 = i__;
                        xmax = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(
                                &x[i__]), abs(d__2));
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }
/*<   120       CONTINUE >*/
/* L120: */
            }

/*<          ELSE IF( LSAME( TRANS, 'T' ) ) THEN >*/
        } else if (lsame_(trans, "T", (ftnlen)1, (ftnlen)1)) {

/*           Solve A**T * x = b */

/*<             DO 170 J = JFIRST, JLAST, JINC >*/
            i__2 = jlast;
            i__1 = jinc;
            for (j = jfirst; i__1 < 0 ? j >= i__2 : j <= i__2; j += i__1) {

/*              Compute x(j) = b(j) - sum A(k,j)*x(k). */
/*                                    k<>j */

/*<                XJ = CABS1( X( J ) ) >*/
                i__3 = j;
                xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j]),
                        abs(d__2));
/*<                USCAL = TSCAL >*/
                uscal.r = tscal, uscal.i = 0.;
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
                        i__3 = j + j * a_dim1;
                        z__1.r = tscal * a[i__3].r, z__1.i = tscal * a[i__3]
                                .i;
                        tjjs.r = z__1.r, tjjs.i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      TJJS = TSCAL >*/
                        tjjs.r = tscal, tjjs.i = 0.;
/*<                   END IF >*/
                    }
/*<                   TJJ = CABS1( TJJS ) >*/
                    tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs),
                            abs(d__2));
/*<                   IF( TJJ.GT.ONE ) THEN >*/
                    if (tjj > 1.) {

/*                       Divide by A(j,j) when scaling x if A(j,j) > 1. */

/*<                      REC = MIN( ONE, REC*TJJ ) >*/
/* Computing MIN */
                        d__1 = 1., d__2 = rec * tjj;
                        rec = min(d__1,d__2);
/*<                      USCAL = ZLADIV( USCAL, TJJS ) >*/
                        zladiv_(&z__1, &uscal, &tjjs);
                        uscal.r = z__1.r, uscal.i = z__1.i;
/*<                   END IF >*/
                    }
/*<                   IF( REC.LT.ONE ) THEN >*/
                    if (rec < 1.) {
/*<                      CALL ZDSCAL( N, REC, X, 1 ) >*/
                        zdscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                      XMAX = XMAX*REC >*/
                        xmax *= rec;
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }

/*<                CSUMJ = ZERO >*/
                csumj.r = 0., csumj.i = 0.;
/*<                IF( USCAL.EQ.DCMPLX( ONE ) ) THEN >*/
                if (uscal.r == 1. && uscal.i == 0.) {

/*                 If the scaling needed for A in the dot product is 1, */
/*                 call ZDOTU to perform the dot product. */

/*<                   IF( UPPER ) THEN >*/
                    if (upper) {
/*<                      CSUMJ = ZDOTU( J-1, A( 1, J ), 1, X, 1 ) >*/
                        i__3 = j - 1;
                        zdotu_(&z__1, &i__3, &a[j * a_dim1 + 1], &c__1, &x[1],
                                 &c__1);
                        csumj.r = z__1.r, csumj.i = z__1.i;
/*<                   ELSE IF( J.LT.N ) THEN >*/
                    } else if (j < *n) {
/*<                      CSUMJ = ZDOTU( N-J, A( J+1, J ), 1, X( J+1 ), 1 ) >*/
                        i__3 = *n - j;
                        zdotu_(&z__1, &i__3, &a[j + 1 + j * a_dim1], &c__1, &
                                x[j + 1], &c__1);
                        csumj.r = z__1.r, csumj.i = z__1.i;
/*<                   END IF >*/
                    }
/*<                ELSE >*/
                } else {

/*                 Otherwise, use in-line code for the dot product. */

/*<                   IF( UPPER ) THEN >*/
                    if (upper) {
/*<                      DO 130 I = 1, J - 1 >*/
                        i__3 = j - 1;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I ) >*/
                            i__4 = i__ + j * a_dim1;
                            z__3.r = a[i__4].r * uscal.r - a[i__4].i *
                                    uscal.i, z__3.i = a[i__4].r * uscal.i + a[
                                    i__4].i * uscal.r;
                            i__5 = i__;
                            z__2.r = z__3.r * x[i__5].r - z__3.i * x[i__5].i,
                                    z__2.i = z__3.r * x[i__5].i + z__3.i * x[
                                    i__5].r;
                            z__1.r = csumj.r + z__2.r, z__1.i = csumj.i +
                                    z__2.i;
                            csumj.r = z__1.r, csumj.i = z__1.i;
/*<   130                CONTINUE >*/
/* L130: */
                        }
/*<                   ELSE IF( J.LT.N ) THEN >*/
                    } else if (j < *n) {
/*<                      DO 140 I = J + 1, N >*/
                        i__3 = *n;
                        for (i__ = j + 1; i__ <= i__3; ++i__) {
/*<                         CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I ) >*/
                            i__4 = i__ + j * a_dim1;
                            z__3.r = a[i__4].r * uscal.r - a[i__4].i *
                                    uscal.i, z__3.i = a[i__4].r * uscal.i + a[
                                    i__4].i * uscal.r;
                            i__5 = i__;
                            z__2.r = z__3.r * x[i__5].r - z__3.i * x[i__5].i,
                                    z__2.i = z__3.r * x[i__5].i + z__3.i * x[
                                    i__5].r;
                            z__1.r = csumj.r + z__2.r, z__1.i = csumj.i +
                                    z__2.i;
                            csumj.r = z__1.r, csumj.i = z__1.i;
/*<   140                CONTINUE >*/
/* L140: */
                        }
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }

/*<                IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN >*/
                z__1.r = tscal, z__1.i = 0.;
                if (uscal.r == z__1.r && uscal.i == z__1.i) {

/*                 Compute x(j) := ( x(j) - CSUMJ ) / A(j,j) if 1/A(j,j) */
/*                 was not used to scale the dotproduct. */

/*<                   X( J ) = X( J ) - CSUMJ >*/
                    i__3 = j;
                    i__4 = j;
                    z__1.r = x[i__4].r - csumj.r, z__1.i = x[i__4].i -
                            csumj.i;
                    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   XJ = CABS1( X( J ) ) >*/
                    i__3 = j;
                    xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j])
                            , abs(d__2));
/*<                   IF( NOUNIT ) THEN >*/
                    if (nounit) {
/*<                      TJJS = A( J, J )*TSCAL >*/
                        i__3 = j + j * a_dim1;
                        z__1.r = tscal * a[i__3].r, z__1.i = tscal * a[i__3]
                                .i;
                        tjjs.r = z__1.r, tjjs.i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      TJJS = TSCAL >*/
                        tjjs.r = tscal, tjjs.i = 0.;
/*<    >*/
                        if (tscal == 1.) {
                            goto L160;
                        }
/*<                   END IF >*/
                    }

/*                    Compute x(j) = x(j) / A(j,j), scaling if necessary. */

/*<                   TJJ = CABS1( TJJS ) >*/
                    tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs),
                            abs(d__2));
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
/*<                            CALL ZDSCAL( N, REC, X, 1 ) >*/
                                zdscal_(n, &rec, &x[1], &c__1);
/*<                            SCALE = SCALE*REC >*/
                                *scale *= rec;
/*<                            XMAX = XMAX*REC >*/
                                xmax *= rec;
/*<                         END IF >*/
                            }
/*<                      END IF >*/
                        }
/*<                      X( J ) = ZLADIV( X( J ), TJJS ) >*/
                        i__3 = j;
                        zladiv_(&z__1, &x[j], &tjjs);
                        x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   ELSE IF( TJJ.GT.ZERO ) THEN >*/
                    } else if (tjj > 0.) {

/*                       0 < abs(A(j,j)) <= SMLNUM: */

/*<                      IF( XJ.GT.TJJ*BIGNUM ) THEN >*/
                        if (xj > tjj * bignum) {

/*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM. */

/*<                         REC = ( TJJ*BIGNUM ) / XJ >*/
                            rec = tjj * bignum / xj;
/*<                         CALL ZDSCAL( N, REC, X, 1 ) >*/
                            zdscal_(n, &rec, &x[1], &c__1);
/*<                         SCALE = SCALE*REC >*/
                            *scale *= rec;
/*<                         XMAX = XMAX*REC >*/
                            xmax *= rec;
/*<                      END IF >*/
                        }
/*<                      X( J ) = ZLADIV( X( J ), TJJS ) >*/
                        i__3 = j;
                        zladiv_(&z__1, &x[j], &tjjs);
                        x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {

/*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
/*                       scale = 0 and compute a solution to A**T *x = 0. */

/*<                      DO 150 I = 1, N >*/
                        i__3 = *n;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         X( I ) = ZERO >*/
                            i__4 = i__;
                            x[i__4].r = 0., x[i__4].i = 0.;
/*<   150                CONTINUE >*/
/* L150: */
                        }
/*<                      X( J ) = ONE >*/
                        i__3 = j;
                        x[i__3].r = 1., x[i__3].i = 0.;
/*<                      SCALE = ZERO >*/
                        *scale = 0.;
/*<                      XMAX = ZERO >*/
                        xmax = 0.;
/*<                   END IF >*/
                    }
/*<   160             CONTINUE >*/
L160:
/*<                ELSE >*/
                    ;
                } else {

/*                 Compute x(j) := x(j) / A(j,j) - CSUMJ if the dot */
/*                 product has already been divided by 1/A(j,j). */

/*<                   X( J ) = ZLADIV( X( J ), TJJS ) - CSUMJ >*/
                    i__3 = j;
                    zladiv_(&z__2, &x[j], &tjjs);
                    z__1.r = z__2.r - csumj.r, z__1.i = z__2.i - csumj.i;
                    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                END IF >*/
                }
/*<                XMAX = MAX( XMAX, CABS1( X( J ) ) ) >*/
/* Computing MAX */
                i__3 = j;
                d__3 = xmax, d__4 = (d__1 = x[i__3].r, abs(d__1)) + (d__2 =
                        d_imag(&x[j]), abs(d__2));
                xmax = max(d__3,d__4);
/*<   170       CONTINUE >*/
/* L170: */
            }

/*<          ELSE >*/
        } else {

/*           Solve A**H * x = b */

/*<             DO 220 J = JFIRST, JLAST, JINC >*/
            i__1 = jlast;
            i__2 = jinc;
            for (j = jfirst; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {

/*              Compute x(j) = b(j) - sum A(k,j)*x(k). */
/*                                    k<>j */

/*<                XJ = CABS1( X( J ) ) >*/
                i__3 = j;
                xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j]),
                        abs(d__2));
/*<                USCAL = TSCAL >*/
                uscal.r = tscal, uscal.i = 0.;
/*<                REC = ONE / MAX( XMAX, ONE ) >*/
                rec = 1. / max(xmax,1.);
/*<                IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN >*/
                if (cnorm[j] > (bignum - xj) * rec) {

/*                 If x(j) could overflow, scale x by 1/(2*XMAX). */

/*<                   REC = REC*HALF >*/
                    rec *= .5;
/*<                   IF( NOUNIT ) THEN >*/
                    if (nounit) {
/*<                      TJJS = DCONJG( A( J, J ) )*TSCAL >*/
                        d_cnjg(&z__2, &a[j + j * a_dim1]);
                        z__1.r = tscal * z__2.r, z__1.i = tscal * z__2.i;
                        tjjs.r = z__1.r, tjjs.i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      TJJS = TSCAL >*/
                        tjjs.r = tscal, tjjs.i = 0.;
/*<                   END IF >*/
                    }
/*<                   TJJ = CABS1( TJJS ) >*/
                    tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs),
                            abs(d__2));
/*<                   IF( TJJ.GT.ONE ) THEN >*/
                    if (tjj > 1.) {

/*                       Divide by A(j,j) when scaling x if A(j,j) > 1. */

/*<                      REC = MIN( ONE, REC*TJJ ) >*/
/* Computing MIN */
                        d__1 = 1., d__2 = rec * tjj;
                        rec = min(d__1,d__2);
/*<                      USCAL = ZLADIV( USCAL, TJJS ) >*/
                        zladiv_(&z__1, &uscal, &tjjs);
                        uscal.r = z__1.r, uscal.i = z__1.i;
/*<                   END IF >*/
                    }
/*<                   IF( REC.LT.ONE ) THEN >*/
                    if (rec < 1.) {
/*<                      CALL ZDSCAL( N, REC, X, 1 ) >*/
                        zdscal_(n, &rec, &x[1], &c__1);
/*<                      SCALE = SCALE*REC >*/
                        *scale *= rec;
/*<                      XMAX = XMAX*REC >*/
                        xmax *= rec;
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }

/*<                CSUMJ = ZERO >*/
                csumj.r = 0., csumj.i = 0.;
/*<                IF( USCAL.EQ.DCMPLX( ONE ) ) THEN >*/
                if (uscal.r == 1. && uscal.i == 0.) {

/*                 If the scaling needed for A in the dot product is 1, */
/*                 call ZDOTC to perform the dot product. */

/*<                   IF( UPPER ) THEN >*/
                    if (upper) {
/*<                      CSUMJ = ZDOTC( J-1, A( 1, J ), 1, X, 1 ) >*/
                        i__3 = j - 1;
                        zdotc_(&z__1, &i__3, &a[j * a_dim1 + 1], &c__1, &x[1],
                                 &c__1);
                        csumj.r = z__1.r, csumj.i = z__1.i;
/*<                   ELSE IF( J.LT.N ) THEN >*/
                    } else if (j < *n) {
/*<                      CSUMJ = ZDOTC( N-J, A( J+1, J ), 1, X( J+1 ), 1 ) >*/
                        i__3 = *n - j;
                        zdotc_(&z__1, &i__3, &a[j + 1 + j * a_dim1], &c__1, &
                                x[j + 1], &c__1);
                        csumj.r = z__1.r, csumj.i = z__1.i;
/*<                   END IF >*/
                    }
/*<                ELSE >*/
                } else {

/*                 Otherwise, use in-line code for the dot product. */

/*<                   IF( UPPER ) THEN >*/
                    if (upper) {
/*<                      DO 180 I = 1, J - 1 >*/
                        i__3 = j - 1;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<    >*/
                            d_cnjg(&z__4, &a[i__ + j * a_dim1]);
                            z__3.r = z__4.r * uscal.r - z__4.i * uscal.i,
                                    z__3.i = z__4.r * uscal.i + z__4.i *
                                    uscal.r;
                            i__4 = i__;
                            z__2.r = z__3.r * x[i__4].r - z__3.i * x[i__4].i,
                                    z__2.i = z__3.r * x[i__4].i + z__3.i * x[
                                    i__4].r;
                            z__1.r = csumj.r + z__2.r, z__1.i = csumj.i +
                                    z__2.i;
                            csumj.r = z__1.r, csumj.i = z__1.i;
/*<   180                CONTINUE >*/
/* L180: */
                        }
/*<                   ELSE IF( J.LT.N ) THEN >*/
                    } else if (j < *n) {
/*<                      DO 190 I = J + 1, N >*/
                        i__3 = *n;
                        for (i__ = j + 1; i__ <= i__3; ++i__) {
/*<    >*/
                            d_cnjg(&z__4, &a[i__ + j * a_dim1]);
                            z__3.r = z__4.r * uscal.r - z__4.i * uscal.i,
                                    z__3.i = z__4.r * uscal.i + z__4.i *
                                    uscal.r;
                            i__4 = i__;
                            z__2.r = z__3.r * x[i__4].r - z__3.i * x[i__4].i,
                                    z__2.i = z__3.r * x[i__4].i + z__3.i * x[
                                    i__4].r;
                            z__1.r = csumj.r + z__2.r, z__1.i = csumj.i +
                                    z__2.i;
                            csumj.r = z__1.r, csumj.i = z__1.i;
/*<   190                CONTINUE >*/
/* L190: */
                        }
/*<                   END IF >*/
                    }
/*<                END IF >*/
                }

/*<                IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN >*/
                z__1.r = tscal, z__1.i = 0.;
                if (uscal.r == z__1.r && uscal.i == z__1.i) {

/*                 Compute x(j) := ( x(j) - CSUMJ ) / A(j,j) if 1/A(j,j) */
/*                 was not used to scale the dotproduct. */

/*<                   X( J ) = X( J ) - CSUMJ >*/
                    i__3 = j;
                    i__4 = j;
                    z__1.r = x[i__4].r - csumj.r, z__1.i = x[i__4].i -
                            csumj.i;
                    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   XJ = CABS1( X( J ) ) >*/
                    i__3 = j;
                    xj = (d__1 = x[i__3].r, abs(d__1)) + (d__2 = d_imag(&x[j])
                            , abs(d__2));
/*<                   IF( NOUNIT ) THEN >*/
                    if (nounit) {
/*<                      TJJS = DCONJG( A( J, J ) )*TSCAL >*/
                        d_cnjg(&z__2, &a[j + j * a_dim1]);
                        z__1.r = tscal * z__2.r, z__1.i = tscal * z__2.i;
                        tjjs.r = z__1.r, tjjs.i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      TJJS = TSCAL >*/
                        tjjs.r = tscal, tjjs.i = 0.;
/*<    >*/
                        if (tscal == 1.) {
                            goto L210;
                        }
/*<                   END IF >*/
                    }

/*                    Compute x(j) = x(j) / A(j,j), scaling if necessary. */

/*<                   TJJ = CABS1( TJJS ) >*/
                    tjj = (d__1 = tjjs.r, abs(d__1)) + (d__2 = d_imag(&tjjs),
                            abs(d__2));
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
/*<                            CALL ZDSCAL( N, REC, X, 1 ) >*/
                                zdscal_(n, &rec, &x[1], &c__1);
/*<                            SCALE = SCALE*REC >*/
                                *scale *= rec;
/*<                            XMAX = XMAX*REC >*/
                                xmax *= rec;
/*<                         END IF >*/
                            }
/*<                      END IF >*/
                        }
/*<                      X( J ) = ZLADIV( X( J ), TJJS ) >*/
                        i__3 = j;
                        zladiv_(&z__1, &x[j], &tjjs);
                        x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   ELSE IF( TJJ.GT.ZERO ) THEN >*/
                    } else if (tjj > 0.) {

/*                       0 < abs(A(j,j)) <= SMLNUM: */

/*<                      IF( XJ.GT.TJJ*BIGNUM ) THEN >*/
                        if (xj > tjj * bignum) {

/*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM. */

/*<                         REC = ( TJJ*BIGNUM ) / XJ >*/
                            rec = tjj * bignum / xj;
/*<                         CALL ZDSCAL( N, REC, X, 1 ) >*/
                            zdscal_(n, &rec, &x[1], &c__1);
/*<                         SCALE = SCALE*REC >*/
                            *scale *= rec;
/*<                         XMAX = XMAX*REC >*/
                            xmax *= rec;
/*<                      END IF >*/
                        }
/*<                      X( J ) = ZLADIV( X( J ), TJJS ) >*/
                        i__3 = j;
                        zladiv_(&z__1, &x[j], &tjjs);
                        x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {

/*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and */
/*                       scale = 0 and compute a solution to A**H *x = 0. */

/*<                      DO 200 I = 1, N >*/
                        i__3 = *n;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         X( I ) = ZERO >*/
                            i__4 = i__;
                            x[i__4].r = 0., x[i__4].i = 0.;
/*<   200                CONTINUE >*/
/* L200: */
                        }
/*<                      X( J ) = ONE >*/
                        i__3 = j;
                        x[i__3].r = 1., x[i__3].i = 0.;
/*<                      SCALE = ZERO >*/
                        *scale = 0.;
/*<                      XMAX = ZERO >*/
                        xmax = 0.;
/*<                   END IF >*/
                    }
/*<   210             CONTINUE >*/
L210:
/*<                ELSE >*/
                    ;
                } else {

/*                 Compute x(j) := x(j) / A(j,j) - CSUMJ if the dot */
/*                 product has already been divided by 1/A(j,j). */

/*<                   X( J ) = ZLADIV( X( J ), TJJS ) - CSUMJ >*/
                    i__3 = j;
                    zladiv_(&z__2, &x[j], &tjjs);
                    z__1.r = z__2.r - csumj.r, z__1.i = z__2.i - csumj.i;
                    x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<                END IF >*/
                }
/*<                XMAX = MAX( XMAX, CABS1( X( J ) ) ) >*/
/* Computing MAX */
                i__3 = j;
                d__3 = xmax, d__4 = (d__1 = x[i__3].r, abs(d__1)) + (d__2 =
                        d_imag(&x[j]), abs(d__2));
                xmax = max(d__3,d__4);
/*<   220       CONTINUE >*/
/* L220: */
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

/*     End of ZLATRS */

/*<       END >*/
} /* zlatrs_ */

#ifdef __cplusplus
        }
#endif
