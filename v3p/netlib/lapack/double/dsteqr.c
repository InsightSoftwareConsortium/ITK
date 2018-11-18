/* lapack/double/dsteqr.f -- translated by f2c (version 20090411).
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

static doublereal c_b9 = 0.;
static doublereal c_b10 = 1.;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__2 = 2;

/*<       SUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO ) >*/
/* Subroutine */ int dsteqr_(char *compz, integer *n, doublereal *d__,
        doublereal *e, doublereal *z__, integer *ldz, doublereal *work,
        integer *info, ftnlen compz_len)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal b, c__, f, g;
    integer i__, j, k, l, m;
    doublereal p, r__, s;
    integer l1, ii, mm, lm1, mm1, nm1;
    doublereal rt1, rt2, eps;
    integer lsv;
    doublereal tst, eps2;
    integer lend, jtot;
    extern /* Subroutine */ int dlae2_(doublereal *, doublereal *, doublereal
            *, doublereal *, doublereal *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dlasr_(char *, char *, char *, integer *,
            integer *, doublereal *, doublereal *, doublereal *, integer *,
            ftnlen, ftnlen, ftnlen);
    doublereal anorm;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *), dlaev2_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    integer lendm1, lendp1;
    extern doublereal dlapy2_(doublereal *, doublereal *), dlamch_(char *,
            ftnlen);
    integer iscale;
    extern /* Subroutine */ int dlascl_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, integer *, doublereal *,
            integer *, integer *, ftnlen), dlaset_(char *, integer *, integer
            *, doublereal *, doublereal *, doublereal *, integer *, ftnlen);
    doublereal safmin;
    extern /* Subroutine */ int dlartg_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *);
    doublereal safmax;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    extern doublereal dlanst_(char *, integer *, doublereal *, doublereal *,
            ftnlen);
    extern /* Subroutine */ int dlasrt_(char *, integer *, doublereal *,
            integer *, ftnlen);
    integer lendsv;
    doublereal ssfmin;
    integer nmaxit, icompz;
    doublereal ssfmax;


/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          COMPZ >*/
/*<       INTEGER            INFO, LDZ, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DSTEQR computes all eigenvalues and, optionally, eigenvectors of a */
/*  symmetric tridiagonal matrix using the implicit QL or QR method. */
/*  The eigenvectors of a full or band symmetric matrix can also be found */
/*  if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to */
/*  tridiagonal form. */

/*  Arguments */
/*  ========= */

/*  COMPZ   (input) CHARACTER*1 */
/*          = 'N':  Compute eigenvalues only. */
/*          = 'V':  Compute eigenvalues and eigenvectors of the original */
/*                  symmetric matrix.  On entry, Z must contain the */
/*                  orthogonal matrix used to reduce the original matrix */
/*                  to tridiagonal form. */
/*          = 'I':  Compute eigenvalues and eigenvectors of the */
/*                  tridiagonal matrix.  Z is initialized to the identity */
/*                  matrix. */

/*  N       (input) INTEGER */
/*          The order of the matrix.  N >= 0. */

/*  D       (input/output) DOUBLE PRECISION array, dimension (N) */
/*          On entry, the diagonal elements of the tridiagonal matrix. */
/*          On exit, if INFO = 0, the eigenvalues in ascending order. */

/*  E       (input/output) DOUBLE PRECISION array, dimension (N-1) */
/*          On entry, the (n-1) subdiagonal elements of the tridiagonal */
/*          matrix. */
/*          On exit, E has been destroyed. */

/*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N) */
/*          On entry, if  COMPZ = 'V', then Z contains the orthogonal */
/*          matrix used in the reduction to tridiagonal form. */
/*          On exit, if INFO = 0, then if  COMPZ = 'V', Z contains the */
/*          orthonormal eigenvectors of the original symmetric matrix, */
/*          and if COMPZ = 'I', Z contains the orthonormal eigenvectors */
/*          of the symmetric tridiagonal matrix. */
/*          If COMPZ = 'N', then Z is not referenced. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z.  LDZ >= 1, and if */
/*          eigenvectors are desired, then  LDZ >= max(1,N). */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2)) */
/*          If COMPZ = 'N', then WORK is not referenced. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */
/*          > 0:  the algorithm has failed to find all the eigenvalues in */
/*                a total of 30*N iterations; if INFO = i, then i */
/*                elements of E have not converged to zero; on exit, D */
/*                and E contain the elements of a symmetric tridiagonal */
/*                matrix which is orthogonally similar to the original */
/*                matrix. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE, TWO, THREE >*/
/*<        >*/
/*<       INTEGER            MAXIT >*/
/*<       PARAMETER          ( MAXIT = 30 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<        >*/
/*<        >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2 >*/
/*<       EXTERNAL           LSAME, DLAMCH, DLANST, DLAPY2 >*/
/*     .. */
/*     .. External Subroutines .. */
/*<        >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, SIGN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --d__;
    --e;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --work;

    /* Function Body */
    *info = 0;

/*<       IF( LSAME( COMPZ, 'N' ) ) THEN >*/
    if (lsame_(compz, "N", (ftnlen)1, (ftnlen)1)) {
/*<          ICOMPZ = 0 >*/
        icompz = 0;
/*<       ELSE IF( LSAME( COMPZ, 'V' ) ) THEN >*/
    } else if (lsame_(compz, "V", (ftnlen)1, (ftnlen)1)) {
/*<          ICOMPZ = 1 >*/
        icompz = 1;
/*<       ELSE IF( LSAME( COMPZ, 'I' ) ) THEN >*/
    } else if (lsame_(compz, "I", (ftnlen)1, (ftnlen)1)) {
/*<          ICOMPZ = 2 >*/
        icompz = 2;
/*<       ELSE >*/
    } else {
/*<          ICOMPZ = -1 >*/
        icompz = -1;
/*<       END IF >*/
    }
/*<       IF( ICOMPZ.LT.0 ) THEN >*/
    if (icompz < 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<        >*/
    } else if (*ldz < 1 || (icompz > 0 && *ldz < max(1,*n))) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DSTEQR', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DSTEQR", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<        >*/
    if (*n == 0) {
        return 0;
    }

/*<       IF( N.EQ.1 ) THEN >*/
    if (*n == 1) {
/*<        >*/
        if (icompz == 2) {
            z__[z_dim1 + 1] = 1.;
        }
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Determine the unit roundoff and over/underflow thresholds. */

/*<       EPS = DLAMCH( 'E' ) >*/
    eps = dlamch_("E", (ftnlen)1);
/*<       EPS2 = EPS**2 >*/
/* Computing 2nd power */
    d__1 = eps;
    eps2 = d__1 * d__1;
/*<       SAFMIN = DLAMCH( 'S' ) >*/
    safmin = dlamch_("S", (ftnlen)1);
/*<       SAFMAX = ONE / SAFMIN >*/
    safmax = 1. / safmin;
/*<       SSFMAX = SQRT( SAFMAX ) / THREE >*/
    ssfmax = sqrt(safmax) / 3.;
/*<       SSFMIN = SQRT( SAFMIN ) / EPS2 >*/
    ssfmin = sqrt(safmin) / eps2;

/*     Compute the eigenvalues and eigenvectors of the tridiagonal */
/*     matrix. */

/*<        >*/
    if (icompz == 2) {
        dlaset_("Full", n, n, &c_b9, &c_b10, &z__[z_offset], ldz, (ftnlen)4);
    }

/*<       NMAXIT = N*MAXIT >*/
    nmaxit = *n * 30;
/*<       JTOT = 0 >*/
    jtot = 0;

/*     Determine where the matrix splits and choose QL or QR iteration */
/*     for each block, according to whether top or bottom diagonal */
/*     element is smaller. */

/*<       L1 = 1 >*/
    l1 = 1;
/*<       NM1 = N - 1 >*/
    nm1 = *n - 1;

/*<    10 CONTINUE >*/
L10:
/*<        >*/
    if (l1 > *n) {
        goto L160;
    }
/*<        >*/
    if (l1 > 1) {
        e[l1 - 1] = 0.;
    }
/*<       IF( L1.LE.NM1 ) THEN >*/
    if (l1 <= nm1) {
/*<          DO 20 M = L1, NM1 >*/
        i__1 = nm1;
        for (m = l1; m <= i__1; ++m) {
/*<             TST = ABS( E( M ) ) >*/
            tst = (d__1 = e[m], abs(d__1));
/*<        >*/
            if (tst == 0.) {
                goto L30;
            }
/*<        >*/
            if (tst <= sqrt((d__1 = d__[m], abs(d__1))) * sqrt((d__2 = d__[m
                    + 1], abs(d__2))) * eps) {
/*<                E( M ) = ZERO >*/
                e[m] = 0.;
/*<                GO TO 30 >*/
                goto L30;
/*<             END IF >*/
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       END IF >*/
    }
/*<       M = N >*/
    m = *n;

/*<    30 CONTINUE >*/
L30:
/*<       L = L1 >*/
    l = l1;
/*<       LSV = L >*/
    lsv = l;
/*<       LEND = M >*/
    lend = m;
/*<       LENDSV = LEND >*/
    lendsv = lend;
/*<       L1 = M + 1 >*/
    l1 = m + 1;
/*<        >*/
    if (lend == l) {
        goto L10;
    }

/*     Scale submatrix in rows and columns L to LEND */

/*<       ANORM = DLANST( 'I', LEND-L+1, D( L ), E( L ) ) >*/
    i__1 = lend - l + 1;
    anorm = dlanst_("I", &i__1, &d__[l], &e[l], (ftnlen)1);
/*<       ISCALE = 0 >*/
    iscale = 0;
/*<        >*/
    if (anorm == 0.) {
        goto L10;
    }
/*<       IF( ANORM.GT.SSFMAX ) THEN >*/
    if (anorm > ssfmax) {
/*<          ISCALE = 1 >*/
        iscale = 1;
/*<        >*/
        i__1 = lend - l + 1;
        dlascl_("G", &c__0, &c__0, &anorm, &ssfmax, &i__1, &c__1, &d__[l], n,
                info, (ftnlen)1);
/*<        >*/
        i__1 = lend - l;
        dlascl_("G", &c__0, &c__0, &anorm, &ssfmax, &i__1, &c__1, &e[l], n,
                info, (ftnlen)1);
/*<       ELSE IF( ANORM.LT.SSFMIN ) THEN >*/
    } else if (anorm < ssfmin) {
/*<          ISCALE = 2 >*/
        iscale = 2;
/*<        >*/
        i__1 = lend - l + 1;
        dlascl_("G", &c__0, &c__0, &anorm, &ssfmin, &i__1, &c__1, &d__[l], n,
                info, (ftnlen)1);
/*<        >*/
        i__1 = lend - l;
        dlascl_("G", &c__0, &c__0, &anorm, &ssfmin, &i__1, &c__1, &e[l], n,
                info, (ftnlen)1);
/*<       END IF >*/
    }

/*     Choose between QL and QR iteration */

/*<       IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN >*/
    if ((d__1 = d__[lend], abs(d__1)) < (d__2 = d__[l], abs(d__2))) {
/*<          LEND = LSV >*/
        lend = lsv;
/*<          L = LENDSV >*/
        l = lendsv;
/*<       END IF >*/
    }

/*<       IF( LEND.GT.L ) THEN >*/
    if (lend > l) {

/*        QL Iteration */

/*        Look for small subdiagonal element. */

/*<    40    CONTINUE >*/
L40:
/*<          IF( L.NE.LEND ) THEN >*/
        if (l != lend) {
/*<             LENDM1 = LEND - 1 >*/
            lendm1 = lend - 1;
/*<             DO 50 M = L, LENDM1 >*/
            i__1 = lendm1;
            for (m = l; m <= i__1; ++m) {
/*<                TST = ABS( E( M ) )**2 >*/
/* Computing 2nd power */
                d__2 = (d__1 = e[m], abs(d__1));
                tst = d__2 * d__2;
/*<        >*/
                if (tst <= eps2 * (d__1 = d__[m], abs(d__1)) * (d__2 = d__[m
                        + 1], abs(d__2)) + safmin) {
                    goto L60;
                }
/*<    50       CONTINUE >*/
/* L50: */
            }
/*<          END IF >*/
        }

/*<          M = LEND >*/
        m = lend;

/*<    60    CONTINUE >*/
L60:
/*<        >*/
        if (m < lend) {
            e[m] = 0.;
        }
/*<          P = D( L ) >*/
        p = d__[l];
/*<        >*/
        if (m == l) {
            goto L80;
        }

/*        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2 */
/*        to compute its eigensystem. */

/*<          IF( M.EQ.L+1 ) THEN >*/
        if (m == l + 1) {
/*<             IF( ICOMPZ.GT.0 ) THEN >*/
            if (icompz > 0) {
/*<                CALL DLAEV2( D( L ), E( L ), D( L+1 ), RT1, RT2, C, S ) >*/
                dlaev2_(&d__[l], &e[l], &d__[l + 1], &rt1, &rt2, &c__, &s);
/*<                WORK( L ) = C >*/
                work[l] = c__;
/*<                WORK( N-1+L ) = S >*/
                work[*n - 1 + l] = s;
/*<        >*/
                dlasr_("R", "V", "B", n, &c__2, &work[l], &work[*n - 1 + l], &
                        z__[l * z_dim1 + 1], ldz, (ftnlen)1, (ftnlen)1, (
                        ftnlen)1);
/*<             ELSE >*/
            } else {
/*<                CALL DLAE2( D( L ), E( L ), D( L+1 ), RT1, RT2 ) >*/
                dlae2_(&d__[l], &e[l], &d__[l + 1], &rt1, &rt2);
/*<             END IF >*/
            }
/*<             D( L ) = RT1 >*/
            d__[l] = rt1;
/*<             D( L+1 ) = RT2 >*/
            d__[l + 1] = rt2;
/*<             E( L ) = ZERO >*/
            e[l] = 0.;
/*<             L = L + 2 >*/
            l += 2;
/*<        >*/
            if (l <= lend) {
                goto L40;
            }
/*<             GO TO 140 >*/
            goto L140;
/*<          END IF >*/
        }

/*<        >*/
        if (jtot == nmaxit) {
            goto L140;
        }
/*<          JTOT = JTOT + 1 >*/
        ++jtot;

/*        Form shift. */

/*<          G = ( D( L+1 )-P ) / ( TWO*E( L ) ) >*/
        g = (d__[l + 1] - p) / (e[l] * 2.);
/*<          R = DLAPY2( G, ONE ) >*/
        r__ = dlapy2_(&g, &c_b10);
/*<          G = D( M ) - P + ( E( L ) / ( G+SIGN( R, G ) ) ) >*/
        g = d__[m] - p + e[l] / (g + d_sign(&r__, &g));

/*<          S = ONE >*/
        s = 1.;
/*<          C = ONE >*/
        c__ = 1.;
/*<          P = ZERO >*/
        p = 0.;

/*        Inner loop */

/*<          MM1 = M - 1 >*/
        mm1 = m - 1;
/*<          DO 70 I = MM1, L, -1 >*/
        i__1 = l;
        for (i__ = mm1; i__ >= i__1; --i__) {
/*<             F = S*E( I ) >*/
            f = s * e[i__];
/*<             B = C*E( I ) >*/
            b = c__ * e[i__];
/*<             CALL DLARTG( G, F, C, S, R ) >*/
            dlartg_(&g, &f, &c__, &s, &r__);
/*<        >*/
            if (i__ != m - 1) {
                e[i__ + 1] = r__;
            }
/*<             G = D( I+1 ) - P >*/
            g = d__[i__ + 1] - p;
/*<             R = ( D( I )-G )*S + TWO*C*B >*/
            r__ = (d__[i__] - g) * s + c__ * 2. * b;
/*<             P = S*R >*/
            p = s * r__;
/*<             D( I+1 ) = G + P >*/
            d__[i__ + 1] = g + p;
/*<             G = C*R - B >*/
            g = c__ * r__ - b;

/*           If eigenvectors are desired, then save rotations. */

/*<             IF( ICOMPZ.GT.0 ) THEN >*/
            if (icompz > 0) {
/*<                WORK( I ) = C >*/
                work[i__] = c__;
/*<                WORK( N-1+I ) = -S >*/
                work[*n - 1 + i__] = -s;
/*<             END IF >*/
            }

/*<    70    CONTINUE >*/
/* L70: */
        }

/*        If eigenvectors are desired, then apply saved rotations. */

/*<          IF( ICOMPZ.GT.0 ) THEN >*/
        if (icompz > 0) {
/*<             MM = M - L + 1 >*/
            mm = m - l + 1;
/*<        >*/
            dlasr_("R", "V", "B", n, &mm, &work[l], &work[*n - 1 + l], &z__[l
                    * z_dim1 + 1], ldz, (ftnlen)1, (ftnlen)1, (ftnlen)1);
/*<          END IF >*/
        }

/*<          D( L ) = D( L ) - P >*/
        d__[l] -= p;
/*<          E( L ) = G >*/
        e[l] = g;
/*<          GO TO 40 >*/
        goto L40;

/*        Eigenvalue found. */

/*<    80    CONTINUE >*/
L80:
/*<          D( L ) = P >*/
        d__[l] = p;

/*<          L = L + 1 >*/
        ++l;
/*<        >*/
        if (l <= lend) {
            goto L40;
        }
/*<          GO TO 140 >*/
        goto L140;

/*<       ELSE >*/
    } else {

/*        QR Iteration */

/*        Look for small superdiagonal element. */

/*<    90    CONTINUE >*/
L90:
/*<          IF( L.NE.LEND ) THEN >*/
        if (l != lend) {
/*<             LENDP1 = LEND + 1 >*/
            lendp1 = lend + 1;
/*<             DO 100 M = L, LENDP1, -1 >*/
            i__1 = lendp1;
            for (m = l; m >= i__1; --m) {
/*<                TST = ABS( E( M-1 ) )**2 >*/
/* Computing 2nd power */
                d__2 = (d__1 = e[m - 1], abs(d__1));
                tst = d__2 * d__2;
/*<        >*/
                if (tst <= eps2 * (d__1 = d__[m], abs(d__1)) * (d__2 = d__[m
                        - 1], abs(d__2)) + safmin) {
                    goto L110;
                }
/*<   100       CONTINUE >*/
/* L100: */
            }
/*<          END IF >*/
        }

/*<          M = LEND >*/
        m = lend;

/*<   110    CONTINUE >*/
L110:
/*<        >*/
        if (m > lend) {
            e[m - 1] = 0.;
        }
/*<          P = D( L ) >*/
        p = d__[l];
/*<        >*/
        if (m == l) {
            goto L130;
        }

/*        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2 */
/*        to compute its eigensystem. */

/*<          IF( M.EQ.L-1 ) THEN >*/
        if (m == l - 1) {
/*<             IF( ICOMPZ.GT.0 ) THEN >*/
            if (icompz > 0) {
/*<                CALL DLAEV2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2, C, S ) >*/
                dlaev2_(&d__[l - 1], &e[l - 1], &d__[l], &rt1, &rt2, &c__, &s)
                        ;
/*<                WORK( M ) = C >*/
                work[m] = c__;
/*<                WORK( N-1+M ) = S >*/
                work[*n - 1 + m] = s;
/*<        >*/
                dlasr_("R", "V", "F", n, &c__2, &work[m], &work[*n - 1 + m], &
                        z__[(l - 1) * z_dim1 + 1], ldz, (ftnlen)1, (ftnlen)1,
                        (ftnlen)1);
/*<             ELSE >*/
            } else {
/*<                CALL DLAE2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2 ) >*/
                dlae2_(&d__[l - 1], &e[l - 1], &d__[l], &rt1, &rt2);
/*<             END IF >*/
            }
/*<             D( L-1 ) = RT1 >*/
            d__[l - 1] = rt1;
/*<             D( L ) = RT2 >*/
            d__[l] = rt2;
/*<             E( L-1 ) = ZERO >*/
            e[l - 1] = 0.;
/*<             L = L - 2 >*/
            l += -2;
/*<        >*/
            if (l >= lend) {
                goto L90;
            }
/*<             GO TO 140 >*/
            goto L140;
/*<          END IF >*/
        }

/*<        >*/
        if (jtot == nmaxit) {
            goto L140;
        }
/*<          JTOT = JTOT + 1 >*/
        ++jtot;

/*        Form shift. */

/*<          G = ( D( L-1 )-P ) / ( TWO*E( L-1 ) ) >*/
        g = (d__[l - 1] - p) / (e[l - 1] * 2.);
/*<          R = DLAPY2( G, ONE ) >*/
        r__ = dlapy2_(&g, &c_b10);
/*<          G = D( M ) - P + ( E( L-1 ) / ( G+SIGN( R, G ) ) ) >*/
        g = d__[m] - p + e[l - 1] / (g + d_sign(&r__, &g));

/*<          S = ONE >*/
        s = 1.;
/*<          C = ONE >*/
        c__ = 1.;
/*<          P = ZERO >*/
        p = 0.;

/*        Inner loop */

/*<          LM1 = L - 1 >*/
        lm1 = l - 1;
/*<          DO 120 I = M, LM1 >*/
        i__1 = lm1;
        for (i__ = m; i__ <= i__1; ++i__) {
/*<             F = S*E( I ) >*/
            f = s * e[i__];
/*<             B = C*E( I ) >*/
            b = c__ * e[i__];
/*<             CALL DLARTG( G, F, C, S, R ) >*/
            dlartg_(&g, &f, &c__, &s, &r__);
/*<        >*/
            if (i__ != m) {
                e[i__ - 1] = r__;
            }
/*<             G = D( I ) - P >*/
            g = d__[i__] - p;
/*<             R = ( D( I+1 )-G )*S + TWO*C*B >*/
            r__ = (d__[i__ + 1] - g) * s + c__ * 2. * b;
/*<             P = S*R >*/
            p = s * r__;
/*<             D( I ) = G + P >*/
            d__[i__] = g + p;
/*<             G = C*R - B >*/
            g = c__ * r__ - b;

/*           If eigenvectors are desired, then save rotations. */

/*<             IF( ICOMPZ.GT.0 ) THEN >*/
            if (icompz > 0) {
/*<                WORK( I ) = C >*/
                work[i__] = c__;
/*<                WORK( N-1+I ) = S >*/
                work[*n - 1 + i__] = s;
/*<             END IF >*/
            }

/*<   120    CONTINUE >*/
/* L120: */
        }

/*        If eigenvectors are desired, then apply saved rotations. */

/*<          IF( ICOMPZ.GT.0 ) THEN >*/
        if (icompz > 0) {
/*<             MM = L - M + 1 >*/
            mm = l - m + 1;
/*<        >*/
            dlasr_("R", "V", "F", n, &mm, &work[m], &work[*n - 1 + m], &z__[m
                    * z_dim1 + 1], ldz, (ftnlen)1, (ftnlen)1, (ftnlen)1);
/*<          END IF >*/
        }

/*<          D( L ) = D( L ) - P >*/
        d__[l] -= p;
/*<          E( LM1 ) = G >*/
        e[lm1] = g;
/*<          GO TO 90 >*/
        goto L90;

/*        Eigenvalue found. */

/*<   130    CONTINUE >*/
L130:
/*<          D( L ) = P >*/
        d__[l] = p;

/*<          L = L - 1 >*/
        --l;
/*<        >*/
        if (l >= lend) {
            goto L90;
        }
/*<          GO TO 140 >*/
        goto L140;

/*<       END IF >*/
    }

/*     Undo scaling if necessary */

/*<   140 CONTINUE >*/
L140:
/*<       IF( ISCALE.EQ.1 ) THEN >*/
    if (iscale == 1) {
/*<        >*/
        i__1 = lendsv - lsv + 1;
        dlascl_("G", &c__0, &c__0, &ssfmax, &anorm, &i__1, &c__1, &d__[lsv],
                n, info, (ftnlen)1);
/*<        >*/
        i__1 = lendsv - lsv;
        dlascl_("G", &c__0, &c__0, &ssfmax, &anorm, &i__1, &c__1, &e[lsv], n,
                info, (ftnlen)1);
/*<       ELSE IF( ISCALE.EQ.2 ) THEN >*/
    } else if (iscale == 2) {
/*<        >*/
        i__1 = lendsv - lsv + 1;
        dlascl_("G", &c__0, &c__0, &ssfmin, &anorm, &i__1, &c__1, &d__[lsv],
                n, info, (ftnlen)1);
/*<        >*/
        i__1 = lendsv - lsv;
        dlascl_("G", &c__0, &c__0, &ssfmin, &anorm, &i__1, &c__1, &e[lsv], n,
                info, (ftnlen)1);
/*<       END IF >*/
    }

/*     Check for no convergence to an eigenvalue after a total */
/*     of N*MAXIT iterations. */

/*<        >*/
    if (jtot < nmaxit) {
        goto L10;
    }
/*<       DO 150 I = 1, N - 1 >*/
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<        >*/
        if (e[i__] != 0.) {
            ++(*info);
        }
/*<   150 CONTINUE >*/
/* L150: */
    }
/*<       GO TO 190 >*/
    goto L190;

/*     Order eigenvalues and eigenvectors. */

/*<   160 CONTINUE >*/
L160:
/*<       IF( ICOMPZ.EQ.0 ) THEN >*/
    if (icompz == 0) {

/*        Use Quick Sort */

/*<          CALL DLASRT( 'I', N, D, INFO ) >*/
        dlasrt_("I", n, &d__[1], info, (ftnlen)1);

/*<       ELSE >*/
    } else {

/*        Use Selection Sort to minimize swaps of eigenvectors */

/*<          DO 180 II = 2, N >*/
        i__1 = *n;
        for (ii = 2; ii <= i__1; ++ii) {
/*<             I = II - 1 >*/
            i__ = ii - 1;
/*<             K = I >*/
            k = i__;
/*<             P = D( I ) >*/
            p = d__[i__];
/*<             DO 170 J = II, N >*/
            i__2 = *n;
            for (j = ii; j <= i__2; ++j) {
/*<                IF( D( J ).LT.P ) THEN >*/
                if (d__[j] < p) {
/*<                   K = J >*/
                    k = j;
/*<                   P = D( J ) >*/
                    p = d__[j];
/*<                END IF >*/
                }
/*<   170       CONTINUE >*/
/* L170: */
            }
/*<             IF( K.NE.I ) THEN >*/
            if (k != i__) {
/*<                D( K ) = D( I ) >*/
                d__[k] = d__[i__];
/*<                D( I ) = P >*/
                d__[i__] = p;
/*<                CALL DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 ) >*/
                dswap_(n, &z__[i__ * z_dim1 + 1], &c__1, &z__[k * z_dim1 + 1],
                         &c__1);
/*<             END IF >*/
            }
/*<   180    CONTINUE >*/
/* L180: */
        }
/*<       END IF >*/
    }

/*<   190 CONTINUE >*/
L190:
/*<       RETURN >*/
    return 0;

/*     End of DSTEQR */

/*<       END >*/
} /* dsteqr_ */

#ifdef __cplusplus
        }
#endif
