/* dsptrf.f -- translated by f2c (version 20060506).
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

/*<       SUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO ) >*/
/* Subroutine */ int dsptrf_(char *uplo, integer *n, doublereal *ap, integer *ipiv,
integer *info, ftnlen uplo_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal c__;
    integer j, k;
    doublereal s, t, r1, r2;
    integer kc, kk, kp, kx, knc, kpc=0, npp, imax=0, jmax;
    extern /* Subroutine */ int drot_(integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *), dspr_(char *
            , integer *, doublereal *, doublereal *, integer *, doublereal *,
            ftnlen);
    doublereal alpha;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    integer kstep;
    logical upper;
    extern /* Subroutine */ int dlaev2_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    doublereal absakk;
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    doublereal colmax, rowmax;
    (void)uplo_len;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          UPLO >*/
/*<       INTEGER            INFO, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IPIV( * ) >*/
/*<       DOUBLE PRECISION   AP( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DSPTRF computes the factorization of a real symmetric matrix A stored */
/*  in packed format using the Bunch-Kaufman diagonal pivoting method: */

/*     A = U*D*U**T  or  A = L*D*L**T */

/*  where U (or L) is a product of permutation and unit upper (lower) */
/*  triangular matrices, and D is symmetric and block diagonal with */
/*  1-by-1 and 2-by-2 diagonal blocks. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          = 'U':  Upper triangle of A is stored; */
/*          = 'L':  Lower triangle of A is stored. */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0. */

/*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2) */
/*          On entry, the upper or lower triangle of the symmetric matrix */
/*          A, packed columnwise in a linear array.  The j-th column of A */
/*          is stored in the array AP as follows: */
/*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j; */
/*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n. */

/*          On exit, the block diagonal matrix D and the multipliers used */
/*          to obtain the factor U or L, stored as a packed triangular */
/*          matrix overwriting A (see below for further details). */

/*  IPIV    (output) INTEGER array, dimension (N) */
/*          Details of the interchanges and the block structure of D. */
/*          If IPIV(k) > 0, then rows and columns k and IPIV(k) were */
/*          interchanged and D(k,k) is a 1-by-1 diagonal block. */
/*          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and */
/*          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k) */
/*          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) = */
/*          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were */
/*          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block. */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */
/*          > 0: if INFO = i, D(i,i) is exactly zero.  The factorization */
/*               has been completed, but the block diagonal matrix D is */
/*               exactly singular, and division by zero will occur if it */
/*               is used to solve a system of equations. */

/*  Further Details */
/*  =============== */

/*  If UPLO = 'U', then A = U*D*U', where */
/*     U = P(n)*U(n)* ... *P(k)U(k)* ..., */
/*  i.e., U is a product of terms P(k)*U(k), where k decreases from n to */
/*  1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1 */
/*  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as */
/*  defined by IPIV(k), and U(k) is a unit upper triangular matrix, such */
/*  that if the diagonal block D(k) is of order s (s = 1 or 2), then */

/*             (   I    v    0   )   k-s */
/*     U(k) =  (   0    I    0   )   s */
/*             (   0    0    I   )   n-k */
/*                k-s   s   n-k */

/*  If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k). */
/*  If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k), */
/*  and A(k,k), and v overwrites A(1:k-2,k-1:k). */

/*  If UPLO = 'L', then A = L*D*L', where */
/*     L = P(1)*L(1)* ... *P(k)*L(k)* ..., */
/*  i.e., L is a product of terms P(k)*L(k), where k increases from 1 to */
/*  n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1 */
/*  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as */
/*  defined by IPIV(k), and L(k) is a unit lower triangular matrix, such */
/*  that if the diagonal block D(k) is of order s (s = 1 or 2), then */

/*             (   I    0     0   )  k-1 */
/*     L(k) =  (   0    I     0   )  s */
/*             (   0    v     I   )  n-k-s+1 */
/*                k-1   s  n-k-s+1 */

/*  If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k). */
/*  If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k), */
/*  and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*<       DOUBLE PRECISION   EIGHT, SEVTEN >*/
/*<       PARAMETER          ( EIGHT = 8.0D+0, SEVTEN = 17.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            UPPER >*/
/*<    >*/
/*<       DOUBLE PRECISION   ABSAKK, ALPHA, C, COLMAX, R1, R2, ROWMAX, S, T >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IDAMAX >*/
/*<       EXTERNAL           LSAME, IDAMAX >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLAEV2, DROT, DSCAL, DSPR, DSWAP, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --ipiv;
    --ap;

    /* Function Body */
    *info = 0;
/*<       UPPER = LSAME( UPLO, 'U' ) >*/
    upper = lsame_(uplo, "U", (ftnlen)1, (ftnlen)1);
/*<       IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN >*/
    if (! upper && ! lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DSPTRF', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DSPTRF", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Initialize ALPHA for use in choosing pivot block size. */

/*<       ALPHA = ( ONE+SQRT( SEVTEN ) ) / EIGHT >*/
    alpha = (sqrt(17.) + 1.) / 8.;

/*<       IF( UPPER ) THEN >*/
    if (upper) {

/*        Factorize A as U*D*U' using the upper triangle of A */

/*        K is the main loop index, decreasing from N to 1 in steps of */
/*        1 or 2 */

/*<          K = N >*/
        k = *n;
/*<          KC = ( N-1 )*N / 2 + 1 >*/
        kc = (*n - 1) * *n / 2 + 1;
/*<    10    CONTINUE >*/
L10:
/*<          KNC = KC >*/
        knc = kc;

/*        If K < 1, exit from loop */

/*<    >*/
        if (k < 1) {
            goto L70;
        }
/*<          KSTEP = 1 >*/
        kstep = 1;

/*        Determine rows and columns to be interchanged and whether */
/*        a 1-by-1 or 2-by-2 pivot block will be used */

/*<          ABSAKK = ABS( AP( KC+K-1 ) ) >*/
        absakk = (d__1 = ap[kc + k - 1], abs(d__1));

/*        IMAX is the row-index of the largest off-diagonal element in */
/*        column K, and COLMAX is its absolute value */

/*<          IF( K.GT.1 ) THEN >*/
        if (k > 1) {
/*<             IMAX = IDAMAX( K-1, AP( KC ), 1 ) >*/
            i__1 = k - 1;
            imax = idamax_(&i__1, &ap[kc], &c__1);
/*<             COLMAX = ABS( AP( KC+IMAX-1 ) ) >*/
            colmax = (d__1 = ap[kc + imax - 1], abs(d__1));
/*<          ELSE >*/
        } else {
/*<             COLMAX = ZERO >*/
            colmax = 0.;
/*<          END IF >*/
        }

/*<          IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN >*/
        if (max(absakk,colmax) == 0.) {

/*           Column K is zero: set INFO and continue */

/*<    >*/
            if (*info == 0) {
                *info = k;
            }
/*<             KP = K >*/
            kp = k;
/*<          ELSE >*/
        } else {
/*<             IF( ABSAKK.GE.ALPHA*COLMAX ) THEN >*/
            if (absakk >= alpha * colmax) {

/*              no interchange, use 1-by-1 pivot block */

/*<                KP = K >*/
                kp = k;
/*<             ELSE >*/
            } else {

/*              JMAX is the column-index of the largest off-diagonal */
/*              element in row IMAX, and ROWMAX is its absolute value */

/*<                ROWMAX = ZERO >*/
                rowmax = 0.;
/*<                JMAX = IMAX >*/
//                jmax = imax;
/*<                KX = IMAX*( IMAX+1 ) / 2 + IMAX >*/
                kx = imax * (imax + 1) / 2 + imax;
/*<                DO 20 J = IMAX + 1, K >*/
                i__1 = k;
                for (j = imax + 1; j <= i__1; ++j) {
/*<                   IF( ABS( AP( KX ) ).GT.ROWMAX ) THEN >*/
                    if ((d__1 = ap[kx], abs(d__1)) > rowmax) {
/*<                      ROWMAX = ABS( AP( KX ) ) >*/
                        rowmax = (d__1 = ap[kx], abs(d__1));
/*<                      JMAX = J >*/
//                        jmax = j;
/*<                   END IF >*/
                    }
/*<                   KX = KX + J >*/
                    kx += j;
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<                KPC = ( IMAX-1 )*IMAX / 2 + 1 >*/
                kpc = (imax - 1) * imax / 2 + 1;
/*<                IF( IMAX.GT.1 ) THEN >*/
                if (imax > 1) {
/*<                   JMAX = IDAMAX( IMAX-1, AP( KPC ), 1 ) >*/
                    i__1 = imax - 1;
                    jmax = idamax_(&i__1, &ap[kpc], &c__1);
/*<                   ROWMAX = MAX( ROWMAX, ABS( AP( KPC+JMAX-1 ) ) ) >*/
/* Computing MAX */
                    d__2 = rowmax, d__3 = (d__1 = ap[kpc + jmax - 1], abs(
                            d__1));
                    rowmax = max(d__2,d__3);
/*<                END IF >*/
                }

/*<                IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN >*/
                if (absakk >= alpha * colmax * (colmax / rowmax)) {

/*                 no interchange, use 1-by-1 pivot block */

/*<                   KP = K >*/
                    kp = k;
/*<                ELSE IF( ABS( AP( KPC+IMAX-1 ) ).GE.ALPHA*ROWMAX ) THEN >*/
                } else if ((d__1 = ap[kpc + imax - 1], abs(d__1)) >= alpha *
                        rowmax) {

/*                 interchange rows and columns K and IMAX, use 1-by-1 */
/*                 pivot block */

/*<                   KP = IMAX >*/
                    kp = imax;
/*<                ELSE >*/
                } else {

/*                 interchange rows and columns K-1 and IMAX, use 2-by-2 */
/*                 pivot block */

/*<                   KP = IMAX >*/
                    kp = imax;
/*<                   KSTEP = 2 >*/
                    kstep = 2;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*<             KK = K - KSTEP + 1 >*/
            kk = k - kstep + 1;
/*<    >*/
            if (kstep == 2) {
                knc = knc - k + 1;
            }
/*<             IF( KP.NE.KK ) THEN >*/
            if (kp != kk) {

/*              Interchange rows and columns KK and KP in the leading */
/*              submatrix A(1:k,1:k) */

/*<                CALL DSWAP( KP-1, AP( KNC ), 1, AP( KPC ), 1 ) >*/
                i__1 = kp - 1;
                dswap_(&i__1, &ap[knc], &c__1, &ap[kpc], &c__1);
/*<                KX = KPC + KP - 1 >*/
                kx = kpc + kp - 1;
/*<                DO 30 J = KP + 1, KK - 1 >*/
                i__1 = kk - 1;
                for (j = kp + 1; j <= i__1; ++j) {
/*<                   KX = KX + J - 1 >*/
                    kx = kx + j - 1;
/*<                   T = AP( KNC+J-1 ) >*/
                    t = ap[knc + j - 1];
/*<                   AP( KNC+J-1 ) = AP( KX ) >*/
                    ap[knc + j - 1] = ap[kx];
/*<                   AP( KX ) = T >*/
                    ap[kx] = t;
/*<    30          CONTINUE >*/
/* L30: */
                }
/*<                T = AP( KNC+KK-1 ) >*/
                t = ap[knc + kk - 1];
/*<                AP( KNC+KK-1 ) = AP( KPC+KP-1 ) >*/
                ap[knc + kk - 1] = ap[kpc + kp - 1];
/*<                AP( KPC+KP-1 ) = T >*/
                ap[kpc + kp - 1] = t;
/*<                IF( KSTEP.EQ.2 ) THEN >*/
                if (kstep == 2) {
/*<                   T = AP( KC+K-2 ) >*/
                    t = ap[kc + k - 2];
/*<                   AP( KC+K-2 ) = AP( KC+KP-1 ) >*/
                    ap[kc + k - 2] = ap[kc + kp - 1];
/*<                   AP( KC+KP-1 ) = T >*/
                    ap[kc + kp - 1] = t;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*           Update the leading submatrix */

/*<             IF( KSTEP.EQ.1 ) THEN >*/
            if (kstep == 1) {

/*              1-by-1 pivot block D(k): column k now holds */

/*              W(k) = U(k)*D(k) */

/*              where U(k) is the k-th column of U */

/*              Perform a rank-1 update of A(1:k-1,1:k-1) as */

/*              A := A - U(k)*D(k)*U(k)' = A - W(k)*1/D(k)*W(k)' */

/*<                R1 = ONE / AP( KC+K-1 ) >*/
                r1 = 1. / ap[kc + k - 1];
/*<                CALL DSPR( UPLO, K-1, -R1, AP( KC ), 1, AP ) >*/
                i__1 = k - 1;
                d__1 = -r1;
                dspr_(uplo, &i__1, &d__1, &ap[kc], &c__1, &ap[1], (ftnlen)1);

/*              Store U(k) in column k */

/*<                CALL DSCAL( K-1, R1, AP( KC ), 1 ) >*/
                i__1 = k - 1;
                dscal_(&i__1, &r1, &ap[kc], &c__1);
/*<             ELSE >*/
            } else {

/*              2-by-2 pivot block D(k): columns k and k-1 now hold */

/*              ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k) */

/*              where U(k) and U(k-1) are the k-th and (k-1)-th columns */
/*              of U */

/*              Perform a rank-2 update of A(1:k-2,1:k-2) as */

/*              A := A - ( U(k-1) U(k) )*D(k)*( U(k-1) U(k) )' */
/*                 = A - ( W(k-1) W(k) )*inv(D(k))*( W(k-1) W(k) )' */

/*              Convert this to two rank-1 updates by using the eigen- */
/*              decomposition of D(k) */

/*<    >*/
                dlaev2_(&ap[kc - 1], &ap[kc + k - 2], &ap[kc + k - 1], &r1, &
                        r2, &c__, &s);
/*<                R1 = ONE / R1 >*/
                r1 = 1. / r1;
/*<                R2 = ONE / R2 >*/
                r2 = 1. / r2;
/*<                CALL DROT( K-2, AP( KNC ), 1, AP( KC ), 1, C, S ) >*/
                i__1 = k - 2;
                drot_(&i__1, &ap[knc], &c__1, &ap[kc], &c__1, &c__, &s);
/*<                CALL DSPR( UPLO, K-2, -R1, AP( KNC ), 1, AP ) >*/
                i__1 = k - 2;
                d__1 = -r1;
                dspr_(uplo, &i__1, &d__1, &ap[knc], &c__1, &ap[1], (ftnlen)1);
/*<                CALL DSPR( UPLO, K-2, -R2, AP( KC ), 1, AP ) >*/
                i__1 = k - 2;
                d__1 = -r2;
                dspr_(uplo, &i__1, &d__1, &ap[kc], &c__1, &ap[1], (ftnlen)1);

/*              Store U(k) and U(k-1) in columns k and k-1 */

/*<                CALL DSCAL( K-2, R1, AP( KNC ), 1 ) >*/
                i__1 = k - 2;
                dscal_(&i__1, &r1, &ap[knc], &c__1);
/*<                CALL DSCAL( K-2, R2, AP( KC ), 1 ) >*/
                i__1 = k - 2;
                dscal_(&i__1, &r2, &ap[kc], &c__1);
/*<                CALL DROT( K-2, AP( KNC ), 1, AP( KC ), 1, C, -S ) >*/
                i__1 = k - 2;
                d__1 = -s;
                drot_(&i__1, &ap[knc], &c__1, &ap[kc], &c__1, &c__, &d__1);
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*        Store details of the interchanges in IPIV */

/*<          IF( KSTEP.EQ.1 ) THEN >*/
        if (kstep == 1) {
/*<             IPIV( K ) = KP >*/
            ipiv[k] = kp;
/*<          ELSE >*/
        } else {
/*<             IPIV( K ) = -KP >*/
            ipiv[k] = -kp;
/*<             IPIV( K-1 ) = -KP >*/
            ipiv[k - 1] = -kp;
/*<          END IF >*/
        }

/*        Decrease K and return to the start of the main loop */

/*<          K = K - KSTEP >*/
        k -= kstep;
/*<          KC = KNC - K >*/
        kc = knc - k;
/*<          GO TO 10 >*/
        goto L10;

/*<       ELSE >*/
    } else {

/*        Factorize A as L*D*L' using the lower triangle of A */

/*        K is the main loop index, increasing from 1 to N in steps of */
/*        1 or 2 */

/*<          K = 1 >*/
        k = 1;
/*<          KC = 1 >*/
        kc = 1;
/*<          NPP = N*( N+1 ) / 2 >*/
        npp = *n * (*n + 1) / 2;
/*<    40    CONTINUE >*/
L40:
/*<          KNC = KC >*/
        knc = kc;

/*        If K > N, exit from loop */

/*<    >*/
        if (k > *n) {
            goto L70;
        }
/*<          KSTEP = 1 >*/
        kstep = 1;

/*        Determine rows and columns to be interchanged and whether */
/*        a 1-by-1 or 2-by-2 pivot block will be used */

/*<          ABSAKK = ABS( AP( KC ) ) >*/
        absakk = (d__1 = ap[kc], abs(d__1));

/*        IMAX is the row-index of the largest off-diagonal element in */
/*        column K, and COLMAX is its absolute value */

/*<          IF( K.LT.N ) THEN >*/
        if (k < *n) {
/*<             IMAX = K + IDAMAX( N-K, AP( KC+1 ), 1 ) >*/
            i__1 = *n - k;
            imax = k + idamax_(&i__1, &ap[kc + 1], &c__1);
/*<             COLMAX = ABS( AP( KC+IMAX-K ) ) >*/
            colmax = (d__1 = ap[kc + imax - k], abs(d__1));
/*<          ELSE >*/
        } else {
/*<             COLMAX = ZERO >*/
            colmax = 0.;
/*<          END IF >*/
        }

/*<          IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN >*/
        if (max(absakk,colmax) == 0.) {

/*           Column K is zero: set INFO and continue */

/*<    >*/
            if (*info == 0) {
                *info = k;
            }
/*<             KP = K >*/
            kp = k;
/*<          ELSE >*/
        } else {
/*<             IF( ABSAKK.GE.ALPHA*COLMAX ) THEN >*/
            if (absakk >= alpha * colmax) {

/*              no interchange, use 1-by-1 pivot block */

/*<                KP = K >*/
                kp = k;
/*<             ELSE >*/
            } else {

/*              JMAX is the column-index of the largest off-diagonal */
/*              element in row IMAX, and ROWMAX is its absolute value */

/*<                ROWMAX = ZERO >*/
                rowmax = 0.;
/*<                KX = KC + IMAX - K >*/
                kx = kc + imax - k;
/*<                DO 50 J = K, IMAX - 1 >*/
                i__1 = imax - 1;
                for (j = k; j <= i__1; ++j) {
/*<                   IF( ABS( AP( KX ) ).GT.ROWMAX ) THEN >*/
                    if ((d__1 = ap[kx], abs(d__1)) > rowmax) {
/*<                      ROWMAX = ABS( AP( KX ) ) >*/
                        rowmax = (d__1 = ap[kx], abs(d__1));
/*<                      JMAX = J >*/
//                        jmax = j;
/*<                   END IF >*/
                    }
/*<                   KX = KX + N - J >*/
                    kx = kx + *n - j;
/*<    50          CONTINUE >*/
/* L50: */
                }
/*<                KPC = NPP - ( N-IMAX+1 )*( N-IMAX+2 ) / 2 + 1 >*/
                kpc = npp - (*n - imax + 1) * (*n - imax + 2) / 2 + 1;
/*<                IF( IMAX.LT.N ) THEN >*/
                if (imax < *n) {
/*<                   JMAX = IMAX + IDAMAX( N-IMAX, AP( KPC+1 ), 1 ) >*/
                    i__1 = *n - imax;
                    jmax = imax + idamax_(&i__1, &ap[kpc + 1], &c__1);
/*<                   ROWMAX = MAX( ROWMAX, ABS( AP( KPC+JMAX-IMAX ) ) ) >*/
/* Computing MAX */
                    d__2 = rowmax, d__3 = (d__1 = ap[kpc + jmax - imax], abs(
                            d__1));
                    rowmax = max(d__2,d__3);
/*<                END IF >*/
                }

/*<                IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN >*/
                if (absakk >= alpha * colmax * (colmax / rowmax)) {

/*                 no interchange, use 1-by-1 pivot block */

/*<                   KP = K >*/
                    kp = k;
/*<                ELSE IF( ABS( AP( KPC ) ).GE.ALPHA*ROWMAX ) THEN >*/
                } else if ((d__1 = ap[kpc], abs(d__1)) >= alpha * rowmax) {

/*                 interchange rows and columns K and IMAX, use 1-by-1 */
/*                 pivot block */

/*<                   KP = IMAX >*/
                    kp = imax;
/*<                ELSE >*/
                } else {

/*                 interchange rows and columns K+1 and IMAX, use 2-by-2 */
/*                 pivot block */

/*<                   KP = IMAX >*/
                    kp = imax;
/*<                   KSTEP = 2 >*/
                    kstep = 2;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*<             KK = K + KSTEP - 1 >*/
            kk = k + kstep - 1;
/*<    >*/
            if (kstep == 2) {
                knc = knc + *n - k + 1;
            }
/*<             IF( KP.NE.KK ) THEN >*/
            if (kp != kk) {

/*              Interchange rows and columns KK and KP in the trailing */
/*              submatrix A(k:n,k:n) */

/*<    >*/
                if (kp < *n) {
                    i__1 = *n - kp;
                    dswap_(&i__1, &ap[knc + kp - kk + 1], &c__1, &ap[kpc + 1],
                             &c__1);
                }
/*<                KX = KNC + KP - KK >*/
                kx = knc + kp - kk;
/*<                DO 60 J = KK + 1, KP - 1 >*/
                i__1 = kp - 1;
                for (j = kk + 1; j <= i__1; ++j) {
/*<                   KX = KX + N - J + 1 >*/
                    kx = kx + *n - j + 1;
/*<                   T = AP( KNC+J-KK ) >*/
                    t = ap[knc + j - kk];
/*<                   AP( KNC+J-KK ) = AP( KX ) >*/
                    ap[knc + j - kk] = ap[kx];
/*<                   AP( KX ) = T >*/
                    ap[kx] = t;
/*<    60          CONTINUE >*/
/* L60: */
                }
/*<                T = AP( KNC ) >*/
                t = ap[knc];
/*<                AP( KNC ) = AP( KPC ) >*/
                ap[knc] = ap[kpc];
/*<                AP( KPC ) = T >*/
                ap[kpc] = t;
/*<                IF( KSTEP.EQ.2 ) THEN >*/
                if (kstep == 2) {
/*<                   T = AP( KC+1 ) >*/
                    t = ap[kc + 1];
/*<                   AP( KC+1 ) = AP( KC+KP-K ) >*/
                    ap[kc + 1] = ap[kc + kp - k];
/*<                   AP( KC+KP-K ) = T >*/
                    ap[kc + kp - k] = t;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*           Update the trailing submatrix */

/*<             IF( KSTEP.EQ.1 ) THEN >*/
            if (kstep == 1) {

/*              1-by-1 pivot block D(k): column k now holds */

/*              W(k) = L(k)*D(k) */

/*              where L(k) is the k-th column of L */

/*<                IF( K.LT.N ) THEN >*/
                if (k < *n) {

/*                 Perform a rank-1 update of A(k+1:n,k+1:n) as */

/*                 A := A - L(k)*D(k)*L(k)' = A - W(k)*(1/D(k))*W(k)' */

/*<                   R1 = ONE / AP( KC ) >*/
                    r1 = 1. / ap[kc];
/*<    >*/
                    i__1 = *n - k;
                    d__1 = -r1;
                    dspr_(uplo, &i__1, &d__1, &ap[kc + 1], &c__1, &ap[kc + *n
                            - k + 1], (ftnlen)1);

/*                 Store L(k) in column K */

/*<                   CALL DSCAL( N-K, R1, AP( KC+1 ), 1 ) >*/
                    i__1 = *n - k;
                    dscal_(&i__1, &r1, &ap[kc + 1], &c__1);
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {

/*              2-by-2 pivot block D(k): columns K and K+1 now hold */

/*              ( W(k) W(k+1) ) = ( L(k) L(k+1) )*D(k) */

/*              where L(k) and L(k+1) are the k-th and (k+1)-th columns */
/*              of L */

/*<                IF( K.LT.N-1 ) THEN >*/
                if (k < *n - 1) {

/*                 Perform a rank-2 update of A(k+2:n,k+2:n) as */

/*                 A := A - ( L(k) L(k+1) )*D(k)*( L(k) L(k+1) )' */
/*                    = A - ( W(k) W(k+1) )*inv(D(k))*( W(k) W(k+1) )' */

/*                 Convert this to two rank-1 updates by using the eigen- */
/*                 decomposition of D(k) */

/*<    >*/
                    dlaev2_(&ap[kc], &ap[kc + 1], &ap[knc], &r1, &r2, &c__, &
                            s);
/*<                   R1 = ONE / R1 >*/
                    r1 = 1. / r1;
/*<                   R2 = ONE / R2 >*/
                    r2 = 1. / r2;
/*<    >*/
                    i__1 = *n - k - 1;
                    drot_(&i__1, &ap[kc + 2], &c__1, &ap[knc + 1], &c__1, &
                            c__, &s);
/*<    >*/
                    i__1 = *n - k - 1;
                    d__1 = -r1;
                    dspr_(uplo, &i__1, &d__1, &ap[kc + 2], &c__1, &ap[knc + *
                            n - k], (ftnlen)1);
/*<    >*/
                    i__1 = *n - k - 1;
                    d__1 = -r2;
                    dspr_(uplo, &i__1, &d__1, &ap[knc + 1], &c__1, &ap[knc + *
                            n - k], (ftnlen)1);

/*                 Store L(k) and L(k+1) in columns k and k+1 */

/*<                   CALL DSCAL( N-K-1, R1, AP( KC+2 ), 1 ) >*/
                    i__1 = *n - k - 1;
                    dscal_(&i__1, &r1, &ap[kc + 2], &c__1);
/*<                   CALL DSCAL( N-K-1, R2, AP( KNC+1 ), 1 ) >*/
                    i__1 = *n - k - 1;
                    dscal_(&i__1, &r2, &ap[knc + 1], &c__1);
/*<    >*/
                    i__1 = *n - k - 1;
                    d__1 = -s;
                    drot_(&i__1, &ap[kc + 2], &c__1, &ap[knc + 1], &c__1, &
                            c__, &d__1);
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*        Store details of the interchanges in IPIV */

/*<          IF( KSTEP.EQ.1 ) THEN >*/
        if (kstep == 1) {
/*<             IPIV( K ) = KP >*/
            ipiv[k] = kp;
/*<          ELSE >*/
        } else {
/*<             IPIV( K ) = -KP >*/
            ipiv[k] = -kp;
/*<             IPIV( K+1 ) = -KP >*/
            ipiv[k + 1] = -kp;
/*<          END IF >*/
        }

/*        Increase K and return to the start of the main loop */

/*<          K = K + KSTEP >*/
        k += kstep;
/*<          KC = KNC + N - K + 2 >*/
        kc = knc + *n - k + 2;
/*<          GO TO 40 >*/
        goto L40;

/*<       END IF >*/
    }

/*<    70 CONTINUE >*/
L70:
/*<       RETURN >*/
    return 0;

/*     End of DSPTRF */

/*<       END >*/
} /* dsptrf_ */

#ifdef __cplusplus
        }
#endif
