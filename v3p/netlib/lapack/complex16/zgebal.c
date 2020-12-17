/* lapack/complex16/zgebal.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO ) >*/
/* Subroutine */ int zgebal_(char *job, integer *n, doublecomplex *a, integer
        *lda, integer *ilo, integer *ihi, doublereal *scale, integer *info,
        ftnlen job_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_imag(doublecomplex *), z_abs(doublecomplex *);

    /* Local variables */
    doublereal c__, f, g;
    integer i__, j, k, l, m;
    doublereal r__, s, ca, ra;
    integer ica, ira, iexc;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zswap_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *);
    doublereal sfmin1, sfmin2, sfmax1, sfmax2;
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), zdscal_(
            integer *, doublereal *, doublecomplex *, integer *);
    extern integer izamax_(integer *, doublecomplex *, integer *);
    logical noconv;
    (void)job_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOB >*/
/*<       INTEGER            IHI, ILO, INFO, LDA, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   SCALE( * ) >*/
/*<       COMPLEX*16         A( LDA, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEBAL balances a general complex matrix A.  This involves, first, */
/*  permuting A by a similarity transformation to isolate eigenvalues */
/*  in the first 1 to ILO-1 and last IHI+1 to N elements on the */
/*  diagonal; and second, applying a diagonal similarity transformation */
/*  to rows and columns ILO to IHI to make the rows and columns as */
/*  close in norm as possible.  Both steps are optional. */

/*  Balancing may reduce the 1-norm of the matrix, and improve the */
/*  accuracy of the computed eigenvalues and/or eigenvectors. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          Specifies the operations to be performed on A: */
/*          = 'N':  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0 */
/*                  for i = 1,...,N; */
/*          = 'P':  permute only; */
/*          = 'S':  scale only; */
/*          = 'B':  both permute and scale. */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the input matrix A. */
/*          On exit,  A is overwritten by the balanced matrix. */
/*          If JOB = 'N', A is not referenced. */
/*          See Further Details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  ILO     (output) INTEGER */
/*  IHI     (output) INTEGER */
/*          ILO and IHI are set to integers such that on exit */
/*          A(i,j) = 0 if i > j and j = 1,...,ILO-1 or I = IHI+1,...,N. */
/*          If JOB = 'N' or 'S', ILO = 1 and IHI = N. */

/*  SCALE   (output) DOUBLE PRECISION array, dimension (N) */
/*          Details of the permutations and scaling factors applied to */
/*          A.  If P(j) is the index of the row and column interchanged */
/*          with row and column j and D(j) is the scaling factor */
/*          applied to row and column j, then */
/*          SCALE(j) = P(j)    for j = 1,...,ILO-1 */
/*                   = D(j)    for j = ILO,...,IHI */
/*                   = P(j)    for j = IHI+1,...,N. */
/*          The order in which the interchanges are made is N to IHI+1, */
/*          then 1 to ILO-1. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit. */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  The permutations consist of row and column interchanges which put */
/*  the matrix in the form */

/*             ( T1   X   Y  ) */
/*     P A P = (  0   B   Z  ) */
/*             (  0   0   T2 ) */

/*  where T1 and T2 are upper triangular matrices whose eigenvalues lie */
/*  along the diagonal.  The column indices ILO and IHI mark the starting */
/*  and ending columns of the submatrix B. Balancing consists of applying */
/*  a diagonal similarity transformation inv(D) * B * D to make the */
/*  1-norms of each row of B and its corresponding column nearly equal. */
/*  The output matrix is */

/*     ( T1     X*D          Y    ) */
/*     (  0  inv(D)*B*D  inv(D)*Z ). */
/*     (  0      0           T2   ) */

/*  Information about the permutations P and the diagonal matrix D is */
/*  returned in the vector SCALE. */

/*  This subroutine is based on the EISPACK routine CBAL. */

/*  Modified by Tzu-Yi Chen, Computer Science Division, University of */
/*    California at Berkeley, USA */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*<       DOUBLE PRECISION   SCLFAC >*/
/*<       PARAMETER          ( SCLFAC = 0.8D+1 ) >*/
/*<       DOUBLE PRECISION   FACTOR >*/
/*<       PARAMETER          ( FACTOR = 0.95D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            NOCONV >*/
/*<       INTEGER            I, ICA, IEXC, IRA, J, K, L, M >*/
/*<    >*/
/*<       COMPLEX*16         CDUM >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IZAMAX >*/
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           LSAME, IZAMAX, DLAMCH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZDSCAL, ZSWAP >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DIMAG, MAX, MIN >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   CABS1 >*/
/*     .. */
/*     .. Statement Function definitions .. */
/*<       CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --scale;

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
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEBAL', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEBAL", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       K = 1 >*/
    k = 1;
/*<       L = N >*/
    l = *n;

/*<    >*/
    if (*n == 0) {
        goto L210;
    }

/*<       IF( LSAME( JOB, 'N' ) ) THEN >*/
    if (lsame_(job, "N", (ftnlen)1, (ftnlen)1)) {
/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             SCALE( I ) = ONE >*/
            scale[i__] = 1.;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          GO TO 210 >*/
        goto L210;
/*<       END IF >*/
    }

/*<    >*/
    if (lsame_(job, "S", (ftnlen)1, (ftnlen)1)) {
        goto L120;
    }

/*     Permutation to isolate eigenvalues if possible */

/*<       GO TO 50 >*/
    goto L50;

/*     Row and column exchange. */

/*<    20 CONTINUE >*/
L20:
/*<       SCALE( M ) = J >*/
    scale[m] = (doublereal) j;
/*<    >*/
    if (j == m) {
        goto L30;
    }

/*<       CALL ZSWAP( L, A( 1, J ), 1, A( 1, M ), 1 ) >*/
    zswap_(&l, &a[j * a_dim1 + 1], &c__1, &a[m * a_dim1 + 1], &c__1);
/*<       CALL ZSWAP( N-K+1, A( J, K ), LDA, A( M, K ), LDA ) >*/
    i__1 = *n - k + 1;
    zswap_(&i__1, &a[j + k * a_dim1], lda, &a[m + k * a_dim1], lda);

/*<    30 CONTINUE >*/
L30:
/*<       GO TO ( 40, 80 )IEXC >*/
    switch (iexc) {
        case 1:  goto L40;
        case 2:  goto L80;
    }

/*     Search for rows isolating an eigenvalue and push them down. */

/*<    40 CONTINUE >*/
L40:
/*<    >*/
    if (l == 1) {
        goto L210;
    }
/*<       L = L - 1 >*/
    --l;

/*<    50 CONTINUE >*/
L50:
/*<       DO 70 J = L, 1, -1 >*/
    for (j = l; j >= 1; --j) {

/*<          DO 60 I = 1, L >*/
        i__1 = l;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<    >*/
            if (i__ == j) {
                goto L60;
            }
/*<    >*/
            i__2 = j + i__ * a_dim1;
            if (a[i__2].r != 0. || d_imag(&a[j + i__ * a_dim1]) != 0.) {
                goto L70;
            }
/*<    60    CONTINUE >*/
L60:
            ;
        }

/*<          M = L >*/
        m = l;
/*<          IEXC = 1 >*/
        iexc = 1;
/*<          GO TO 20 >*/
        goto L20;
/*<    70 CONTINUE >*/
L70:
        ;
    }

/*<       GO TO 90 >*/
    goto L90;

/*     Search for columns isolating an eigenvalue and push them left. */

/*<    80 CONTINUE >*/
L80:
/*<       K = K + 1 >*/
    ++k;

/*<    90 CONTINUE >*/
L90:
/*<       DO 110 J = K, L >*/
    i__1 = l;
    for (j = k; j <= i__1; ++j) {

/*<          DO 100 I = K, L >*/
        i__2 = l;
        for (i__ = k; i__ <= i__2; ++i__) {
/*<    >*/
            if (i__ == j) {
                goto L100;
            }
/*<    >*/
            i__3 = i__ + j * a_dim1;
            if (a[i__3].r != 0. || d_imag(&a[i__ + j * a_dim1]) != 0.) {
                goto L110;
            }
/*<   100    CONTINUE >*/
L100:
            ;
        }

/*<          M = K >*/
        m = k;
/*<          IEXC = 2 >*/
        iexc = 2;
/*<          GO TO 20 >*/
        goto L20;
/*<   110 CONTINUE >*/
L110:
        ;
    }

/*<   120 CONTINUE >*/
L120:
/*<       DO 130 I = K, L >*/
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/*<          SCALE( I ) = ONE >*/
        scale[i__] = 1.;
/*<   130 CONTINUE >*/
/* L130: */
    }

/*<    >*/
    if (lsame_(job, "P", (ftnlen)1, (ftnlen)1)) {
        goto L210;
    }

/*     Balance the submatrix in rows K to L. */

/*     Iterative loop for norm reduction */

/*<       SFMIN1 = DLAMCH( 'S' ) / DLAMCH( 'P' ) >*/
    sfmin1 = dlamch_("S", (ftnlen)1) / dlamch_("P", (ftnlen)1);
/*<       SFMAX1 = ONE / SFMIN1 >*/
    sfmax1 = 1. / sfmin1;
/*<       SFMIN2 = SFMIN1*SCLFAC >*/
    sfmin2 = sfmin1 * 8.;
/*<       SFMAX2 = ONE / SFMIN2 >*/
    sfmax2 = 1. / sfmin2;
/*<   140 CONTINUE >*/
L140:
/*<       NOCONV = .FALSE. >*/
    noconv = FALSE_;

/*<       DO 200 I = K, L >*/
    i__1 = l;
    for (i__ = k; i__ <= i__1; ++i__) {
/*<          C = ZERO >*/
        c__ = 0.;
/*<          R = ZERO >*/
        r__ = 0.;

/*<          DO 150 J = K, L >*/
        i__2 = l;
        for (j = k; j <= i__2; ++j) {
/*<    >*/
            if (j == i__) {
                goto L150;
            }
/*<             C = C + CABS1( A( J, I ) ) >*/
            i__3 = j + i__ * a_dim1;
            c__ += (d__1 = a[i__3].r, abs(d__1)) + (d__2 = d_imag(&a[j + i__ *
                     a_dim1]), abs(d__2));
/*<             R = R + CABS1( A( I, J ) ) >*/
            i__3 = i__ + j * a_dim1;
            r__ += (d__1 = a[i__3].r, abs(d__1)) + (d__2 = d_imag(&a[i__ + j *
                     a_dim1]), abs(d__2));
/*<   150    CONTINUE >*/
L150:
            ;
        }
/*<          ICA = IZAMAX( L, A( 1, I ), 1 ) >*/
        ica = izamax_(&l, &a[i__ * a_dim1 + 1], &c__1);
/*<          CA = ABS( A( ICA, I ) ) >*/
        ca = z_abs(&a[ica + i__ * a_dim1]);
/*<          IRA = IZAMAX( N-K+1, A( I, K ), LDA ) >*/
        i__2 = *n - k + 1;
        ira = izamax_(&i__2, &a[i__ + k * a_dim1], lda);
/*<          RA = ABS( A( I, IRA+K-1 ) ) >*/
        ra = z_abs(&a[i__ + (ira + k - 1) * a_dim1]);

/*        Guard against zero C or R due to underflow. */

/*<    >*/
        if (c__ == 0. || r__ == 0.) {
            goto L200;
        }
/*<          G = R / SCLFAC >*/
        g = r__ / 8.;
/*<          F = ONE >*/
        f = 1.;
/*<          S = C + R >*/
        s = c__ + r__;
/*<   160    CONTINUE >*/
L160:
/*<    >*/
/* Computing MAX */
        d__1 = max(f,c__);
/* Computing MIN */
        d__2 = min(r__,g);
        if (c__ >= g || max(d__1,ca) >= sfmax2 || min(d__2,ra) <= sfmin2) {
            goto L170;
        }
/*<          F = F*SCLFAC >*/
        f *= 8.;
/*<          C = C*SCLFAC >*/
        c__ *= 8.;
/*<          CA = CA*SCLFAC >*/
        ca *= 8.;
/*<          R = R / SCLFAC >*/
        r__ /= 8.;
/*<          G = G / SCLFAC >*/
        g /= 8.;
/*<          RA = RA / SCLFAC >*/
        ra /= 8.;
/*<          GO TO 160 >*/
        goto L160;

/*<   170    CONTINUE >*/
L170:
/*<          G = C / SCLFAC >*/
        g = c__ / 8.;
/*<   180    CONTINUE >*/
L180:
/*<    >*/
/* Computing MIN */
        d__1 = min(f,c__), d__1 = min(d__1,g);
        if (g < r__ || max(r__,ra) >= sfmax2 || min(d__1,ca) <= sfmin2) {
            goto L190;
        }
/*<          F = F / SCLFAC >*/
        f /= 8.;
/*<          C = C / SCLFAC >*/
        c__ /= 8.;
/*<          G = G / SCLFAC >*/
        g /= 8.;
/*<          CA = CA / SCLFAC >*/
        ca /= 8.;
/*<          R = R*SCLFAC >*/
        r__ *= 8.;
/*<          RA = RA*SCLFAC >*/
        ra *= 8.;
/*<          GO TO 180 >*/
        goto L180;

/*        Now balance. */

/*<   190    CONTINUE >*/
L190:
/*<    >*/
        if (c__ + r__ >= s * .95) {
            goto L200;
        }
/*<          IF( F.LT.ONE .AND. SCALE( I ).LT.ONE ) THEN >*/
        if (f < 1. && scale[i__] < 1.) {
/*<    >*/
            if (f * scale[i__] <= sfmin1) {
                goto L200;
            }
/*<          END IF >*/
        }
/*<          IF( F.GT.ONE .AND. SCALE( I ).GT.ONE ) THEN >*/
        if (f > 1. && scale[i__] > 1.) {
/*<    >*/
            if (scale[i__] >= sfmax1 / f) {
                goto L200;
            }
/*<          END IF >*/
        }
/*<          G = ONE / F >*/
        g = 1. / f;
/*<          SCALE( I ) = SCALE( I )*F >*/
        scale[i__] *= f;
/*<          NOCONV = .TRUE. >*/
        noconv = TRUE_;

/*<          CALL ZDSCAL( N-K+1, G, A( I, K ), LDA ) >*/
        i__2 = *n - k + 1;
        zdscal_(&i__2, &g, &a[i__ + k * a_dim1], lda);
/*<          CALL ZDSCAL( L, F, A( 1, I ), 1 ) >*/
        zdscal_(&l, &f, &a[i__ * a_dim1 + 1], &c__1);

/*<   200 CONTINUE >*/
L200:
        ;
    }

/*<    >*/
    if (noconv) {
        goto L140;
    }

/*<   210 CONTINUE >*/
L210:
/*<       ILO = K >*/
    *ilo = k;
/*<       IHI = L >*/
    *ihi = l;

/*<       RETURN >*/
    return 0;

/*     End of ZGEBAL */

/*<       END >*/
} /* zgebal_ */

#ifdef __cplusplus
        }
#endif
