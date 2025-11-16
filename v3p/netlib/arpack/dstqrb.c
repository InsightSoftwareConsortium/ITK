/* arpack/dstqrb.f -- translated by f2c (version 20090411).
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

static integer c__0 = 0;
static integer c__1 = 1;
static doublereal c_b31 = 1.;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dstqrb */

/* \Description: */
/*  Computes all eigenvalues and the last component of the eigenvectors */
/*  of a symmetric tridiagonal matrix using the implicit QL or QR method. */

/*  This is mostly a modification of the LAPACK routine dsteqr. */
/*  See Remarks. */

/* \Usage: */
/*  call dstqrb */
/*     ( N, D, E, Z, WORK, INFO ) */

/* \Arguments */
/*  N       Integer.  (INPUT) */
/*          The number of rows and columns in the matrix.  N >= 0. */

/*  D       Double precision array, dimension (N).  (INPUT/OUTPUT) */
/*          On entry, D contains the diagonal elements of the */
/*          tridiagonal matrix. */
/*          On exit, D contains the eigenvalues, in ascending order. */
/*          If an error exit is made, the eigenvalues are correct */
/*          for indices 1,2,...,INFO-1, but they are unordered and */
/*          may not be the smallest eigenvalues of the matrix. */

/*  E       Double precision array, dimension (N-1).  (INPUT/OUTPUT) */
/*          On entry, E contains the subdiagonal elements of the */
/*          tridiagonal matrix in positions 1 through N-1. */
/*          On exit, E has been destroyed. */

/*  Z       Double precision array, dimension (N).  (OUTPUT) */
/*          On exit, Z contains the last row of the orthonormal */
/*          eigenvector matrix of the symmetric tridiagonal matrix. */
/*          If an error exit is made, Z contains the last row of the */
/*          eigenvector matrix associated with the stored eigenvalues. */

/*  WORK    Double precision array, dimension (max(1,2*N-2)).  (WORKSPACE) */
/*          Workspace used in accumulating the transformation for */
/*          computing the last components of the eigenvectors. */

/*  INFO    Integer.  (OUTPUT) */
/*          = 0:  normal return. */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          > 0:  if INFO = +i, the i-th eigenvalue has not converged */
/*                              after a total of  30*N  iterations. */

/* \Remarks */
/*  1. None. */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Local variables: */
/*     xxxxxx  real */

/* \Routines called: */
/*     daxpy   Level 1 BLAS that computes a vector triad. */
/*     dcopy   Level 1 BLAS that copies one vector to another. */
/*     dswap   Level 1 BLAS that swaps the contents of two vectors. */
/*     lsame   LAPACK character comparison routine. */
/*     dlae2   LAPACK routine that computes the eigenvalues of a 2-by-2 */
/*             symmetric matrix. */
/*     dlaev2  LAPACK routine that eigendecomposition of a 2-by-2 symmetric */
/*             matrix. */
/*     dlamch  LAPACK routine that determines machine constants. */
/*     dlanst  LAPACK routine that computes the norm of a matrix. */
/*     dlapy2  LAPACK routine to compute sqrt(x**2+y**2) carefully. */
/*     dlartg  LAPACK Givens rotation construction routine. */
/*     dlascl  LAPACK routine for careful scaling of a matrix. */
/*     dlaset  LAPACK matrix initialization routine. */
/*     dlasr   LAPACK routine that applies an orthogonal transformation to */
/*             a matrix. */
/*     dlasrt  LAPACK sorting routine. */
/*     dsteqr  LAPACK routine that computes eigenvalues and eigenvectors */
/*             of a symmetric tridiagonal matrix. */
/*     xerbla  LAPACK error handler routine. */

/* \Authors */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \SCCS Information: @(#) */
/* FILE: stqrb.F   SID: 2.5   DATE OF SID: 8/27/96   RELEASE: 2 */

/* \Remarks */
/*     1. Starting with version 2.5, this routine is a modified version */
/*        of LAPACK version 2.0 subroutine SSTEQR. No lines are deleted, */
/*        only commeted out and new lines inserted. */
/*        All lines commented out have "c$$$" at the beginning. */
/*        Note that the LAPACK version 1.0 subroutine SSTEQR contained */
/*        bugs. */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<       subroutine dstqrb ( n, d, e, z, work, info ) >*/
/* Subroutine */ int dstqrb_(integer *n, doublereal *d__, doublereal *e,
        doublereal *z__, doublereal *work, integer *info)
{
    /* System generated locals */
    integer i__1, i__2;
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
            *, doublereal *, doublereal *), dlasr_(char *, char *, char *,
            integer *, integer *, doublereal *, doublereal *, doublereal *,
            integer *, ftnlen, ftnlen, ftnlen);
    doublereal anorm;
    extern /* Subroutine */ int dlaev2_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    integer lendm1, lendp1;
    extern doublereal dlapy2_(doublereal *, doublereal *), dlamch_(char *,
            ftnlen);
    integer iscale;
    extern /* Subroutine */ int dlascl_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, integer *, doublereal *,
            integer *, integer *, ftnlen);
    doublereal safmin;
    extern /* Subroutine */ int dlartg_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *);
    doublereal safmax;
    extern doublereal dlanst_(char *, integer *, doublereal *, doublereal *,
            ftnlen);
    extern /* Subroutine */ int dlasrt_(char *, integer *, doublereal *,
            integer *, ftnlen);
    integer lendsv, nmaxit, icompz;
    doublereal ssfmax, ssfmin;


/*     %------------------% */
/*     | Scalar Arguments | */
/*     %------------------% */

/*<       integer    info, n >*/

/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*<        >*/

/*     .. parameters .. */
/*<        >*/
/*<        >*/
/*<       integer            maxit >*/
/*<       parameter          ( maxit = 30 ) >*/
/*     .. */
/*     .. local scalars .. */
/*<        >*/
/*<        >*/
/*     .. */
/*     .. external functions .. */
/*<       logical            lsame >*/
/*<        >*/
/*<       external           lsame, dlamch, dlanst, dlapy2 >*/
/*     .. */
/*     .. external subroutines .. */
/*<        >*/
/*     .. */
/*     .. intrinsic functions .. */
/*<       intrinsic          abs, max, sign, sqrt >*/
/*     .. */
/*     .. executable statements .. */

/*     test the input parameters. */

/*<       info = 0 >*/
    /* Parameter adjustments */
    --work;
    --z__;
    --e;
    --d__;

    /* Function Body */
    *info = 0;

/* $$$      IF( LSAME( COMPZ, 'N' ) ) THEN */
/* $$$         ICOMPZ = 0 */
/* $$$      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN */
/* $$$         ICOMPZ = 1 */
/* $$$      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN */
/* $$$         ICOMPZ = 2 */
/* $$$      ELSE */
/* $$$         ICOMPZ = -1 */
/* $$$      END IF */
/* $$$      IF( ICOMPZ.LT.0 ) THEN */
/* $$$         INFO = -1 */
/* $$$      ELSE IF( N.LT.0 ) THEN */
/* $$$         INFO = -2 */
/* $$$      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1, */
/* $$$     $         N ) ) ) THEN */
/* $$$         INFO = -6 */
/* $$$      END IF */
/* $$$      IF( INFO.NE.0 ) THEN */
/* $$$         CALL XERBLA( 'SSTEQR', -INFO ) */
/* $$$         RETURN */
/* $$$      END IF */

/*    *** New starting with version 2.5 *** */

/*<       icompz = 2 >*/
    icompz = 2;
/*    ************************************* */

/*     quick return if possible */

/*<        >*/
    if (*n == 0) {
        return 0;
    }

/*<       if( n.eq.1 ) then >*/
    if (*n == 1) {
/*<          if( icompz.eq.2 )  z( 1 ) = one >*/
        if (icompz == 2) {
            z__[1] = 1.;
        }
/*<          return >*/
        return 0;
/*<       end if >*/
    }

/*     determine the unit roundoff and over/underflow thresholds. */

/*<       eps = dlamch( 'e' ) >*/
    eps = dlamch_("e", (ftnlen)1);
/*<       eps2 = eps**2 >*/
/* Computing 2nd power */
    d__1 = eps;
    eps2 = d__1 * d__1;
/*<       safmin = dlamch( 's' ) >*/
    safmin = dlamch_("s", (ftnlen)1);
/*<       safmax = one / safmin >*/
    safmax = 1. / safmin;
/*<       ssfmax = sqrt( safmax ) / three >*/
    ssfmax = sqrt(safmax) / 3.;
/*<       ssfmin = sqrt( safmin ) / eps2 >*/
    ssfmin = sqrt(safmin) / eps2;

/*     compute the eigenvalues and eigenvectors of the tridiagonal */
/*     matrix. */

/* $$      if( icompz.eq.2 ) */
/* $$$     $   call dlaset( 'full', n, n, zero, one, z, ldz ) */

/*     *** New starting with version 2.5 *** */

/*<       if ( icompz .eq. 2 ) then >*/
    if (icompz == 2) {
/*<          do 5 j = 1, n-1 >*/
        i__1 = *n - 1;
        for (j = 1; j <= i__1; ++j) {
/*<             z(j) = zero >*/
            z__[j] = 0.;
/*<   5      continue >*/
/* L5: */
        }
/*<          z( n ) = one >*/
        z__[*n] = 1.;
/*<       end if >*/
    }
/*     ************************************* */

/*<       nmaxit = n*maxit >*/
    nmaxit = *n * 30;
/*<       jtot = 0 >*/
    jtot = 0;

/*     determine where the matrix splits and choose ql or qr iteration */
/*     for each block, according to whether top or bottom diagonal */
/*     element is smaller. */

/*<       l1 = 1 >*/
    l1 = 1;
/*<       nm1 = n - 1 >*/
    nm1 = *n - 1;

/*<    10 continue >*/
L10:
/*<        >*/
    if (l1 > *n) {
        goto L160;
    }
/*<        >*/
    if (l1 > 1) {
        e[l1 - 1] = 0.;
    }
/*<       if( l1.le.nm1 ) then >*/
    if (l1 <= nm1) {
/*<          do 20 m = l1, nm1 >*/
        i__1 = nm1;
        for (m = l1; m <= i__1; ++m) {
/*<             tst = abs( e( m ) ) >*/
            tst = (d__1 = e[m], abs(d__1));
/*<        >*/
            if (tst == 0.) {
                goto L30;
            }
/*<        >*/
            if (tst <= sqrt((d__1 = d__[m], abs(d__1))) * sqrt((d__2 = d__[m
                    + 1], abs(d__2))) * eps) {
/*<                e( m ) = zero >*/
                e[m] = 0.;
/*<                go to 30 >*/
                goto L30;
/*<             end if >*/
            }
/*<    20    continue >*/
/* L20: */
        }
/*<       end if >*/
    }
/*<       m = n >*/
    m = *n;

/*<    30 continue >*/
L30:
/*<       l = l1 >*/
    l = l1;
/*<       lsv = l >*/
    lsv = l;
/*<       lend = m >*/
    lend = m;
/*<       lendsv = lend >*/
    lendsv = lend;
/*<       l1 = m + 1 >*/
    l1 = m + 1;
/*<        >*/
    if (lend == l) {
        goto L10;
    }

/*     scale submatrix in rows and columns l to lend */

/*<       anorm = dlanst( 'i', lend-l+1, d( l ), e( l ) ) >*/
    i__1 = lend - l + 1;
    anorm = dlanst_("i", &i__1, &d__[l], &e[l], (ftnlen)1);
/*<       iscale = 0 >*/
    iscale = 0;
/*<        >*/
    if (anorm == 0.) {
        goto L10;
    }
/*<       if( anorm.gt.ssfmax ) then >*/
    if (anorm > ssfmax) {
/*<          iscale = 1 >*/
        iscale = 1;
/*<        >*/
        i__1 = lend - l + 1;
        dlascl_("g", &c__0, &c__0, &anorm, &ssfmax, &i__1, &c__1, &d__[l], n,
                info, (ftnlen)1);
/*<        >*/
        i__1 = lend - l;
        dlascl_("g", &c__0, &c__0, &anorm, &ssfmax, &i__1, &c__1, &e[l], n,
                info, (ftnlen)1);
/*<       else if( anorm.lt.ssfmin ) then >*/
    } else if (anorm < ssfmin) {
/*<          iscale = 2 >*/
        iscale = 2;
/*<        >*/
        i__1 = lend - l + 1;
        dlascl_("g", &c__0, &c__0, &anorm, &ssfmin, &i__1, &c__1, &d__[l], n,
                info, (ftnlen)1);
/*<        >*/
        i__1 = lend - l;
        dlascl_("g", &c__0, &c__0, &anorm, &ssfmin, &i__1, &c__1, &e[l], n,
                info, (ftnlen)1);
/*<       end if >*/
    }

/*     choose between ql and qr iteration */

/*<       if( abs( d( lend ) ).lt.abs( d( l ) ) ) then >*/
    if ((d__1 = d__[lend], abs(d__1)) < (d__2 = d__[l], abs(d__2))) {
/*<          lend = lsv >*/
        lend = lsv;
/*<          l = lendsv >*/
        l = lendsv;
/*<       end if >*/
    }

/*<       if( lend.gt.l ) then >*/
    if (lend > l) {

/*        ql iteration */

/*        look for small subdiagonal element. */

/*<    40    continue >*/
L40:
/*<          if( l.ne.lend ) then >*/
        if (l != lend) {
/*<             lendm1 = lend - 1 >*/
            lendm1 = lend - 1;
/*<             do 50 m = l, lendm1 >*/
            i__1 = lendm1;
            for (m = l; m <= i__1; ++m) {
/*<                tst = abs( e( m ) )**2 >*/
/* Computing 2nd power */
                d__2 = (d__1 = e[m], abs(d__1));
                tst = d__2 * d__2;
/*<        >*/
                if (tst <= eps2 * (d__1 = d__[m], abs(d__1)) * (d__2 = d__[m
                        + 1], abs(d__2)) + safmin) {
                    goto L60;
                }
/*<    50       continue >*/
/* L50: */
            }
/*<          end if >*/
        }

/*<          m = lend >*/
        m = lend;

/*<    60    continue >*/
L60:
/*<        >*/
        if (m < lend) {
            e[m] = 0.;
        }
/*<          p = d( l ) >*/
        p = d__[l];
/*<        >*/
        if (m == l) {
            goto L80;
        }

/*        if remaining matrix is 2-by-2, use dlae2 or dlaev2 */
/*        to compute its eigensystem. */

/*<          if( m.eq.l+1 ) then >*/
        if (m == l + 1) {
/*<             if( icompz.gt.0 ) then >*/
            if (icompz > 0) {
/*<                call dlaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s ) >*/
                dlaev2_(&d__[l], &e[l], &d__[l + 1], &rt1, &rt2, &c__, &s);
/*<                work( l ) = c >*/
                work[l] = c__;
/*<                work( n-1+l ) = s >*/
                work[*n - 1 + l] = s;
/* $$$               call dlasr( 'r', 'v', 'b', n, 2, work( l ), */
/* $$$     $                     work( n-1+l ), z( 1, l ), ldz ) */

/*              *** New starting with version 2.5 *** */

/*<                tst      = z(l+1) >*/
                tst = z__[l + 1];
/*<                z(l+1) = c*tst - s*z(l) >*/
                z__[l + 1] = c__ * tst - s * z__[l];
/*<                z(l)   = s*tst + c*z(l) >*/
                z__[l] = s * tst + c__ * z__[l];
/*              ************************************* */
/*<             else >*/
            } else {
/*<                call dlae2( d( l ), e( l ), d( l+1 ), rt1, rt2 ) >*/
                dlae2_(&d__[l], &e[l], &d__[l + 1], &rt1, &rt2);
/*<             end if >*/
            }
/*<             d( l ) = rt1 >*/
            d__[l] = rt1;
/*<             d( l+1 ) = rt2 >*/
            d__[l + 1] = rt2;
/*<             e( l ) = zero >*/
            e[l] = 0.;
/*<             l = l + 2 >*/
            l += 2;
/*<        >*/
            if (l <= lend) {
                goto L40;
            }
/*<             go to 140 >*/
            goto L140;
/*<          end if >*/
        }

/*<        >*/
        if (jtot == nmaxit) {
            goto L140;
        }
/*<          jtot = jtot + 1 >*/
        ++jtot;

/*        form shift. */

/*<          g = ( d( l+1 )-p ) / ( two*e( l ) ) >*/
        g = (d__[l + 1] - p) / (e[l] * 2.);
/*<          r = dlapy2( g, one ) >*/
        r__ = dlapy2_(&g, &c_b31);
/*<          g = d( m ) - p + ( e( l ) / ( g+sign( r, g ) ) ) >*/
        g = d__[m] - p + e[l] / (g + d_sign(&r__, &g));

/*<          s = one >*/
        s = 1.;
/*<          c = one >*/
        c__ = 1.;
/*<          p = zero >*/
        p = 0.;

/*        inner loop */

/*<          mm1 = m - 1 >*/
        mm1 = m - 1;
/*<          do 70 i = mm1, l, -1 >*/
        i__1 = l;
        for (i__ = mm1; i__ >= i__1; --i__) {
/*<             f = s*e( i ) >*/
            f = s * e[i__];
/*<             b = c*e( i ) >*/
            b = c__ * e[i__];
/*<             call dlartg( g, f, c, s, r ) >*/
            dlartg_(&g, &f, &c__, &s, &r__);
/*<        >*/
            if (i__ != m - 1) {
                e[i__ + 1] = r__;
            }
/*<             g = d( i+1 ) - p >*/
            g = d__[i__ + 1] - p;
/*<             r = ( d( i )-g )*s + two*c*b >*/
            r__ = (d__[i__] - g) * s + c__ * 2. * b;
/*<             p = s*r >*/
            p = s * r__;
/*<             d( i+1 ) = g + p >*/
            d__[i__ + 1] = g + p;
/*<             g = c*r - b >*/
            g = c__ * r__ - b;

/*           if eigenvectors are desired, then save rotations. */

/*<             if( icompz.gt.0 ) then >*/
            if (icompz > 0) {
/*<                work( i ) = c >*/
                work[i__] = c__;
/*<                work( n-1+i ) = -s >*/
                work[*n - 1 + i__] = -s;
/*<             end if >*/
            }

/*<    70    continue >*/
/* L70: */
        }

/*        if eigenvectors are desired, then apply saved rotations. */

/*<          if( icompz.gt.0 ) then >*/
        if (icompz > 0) {
/*<             mm = m - l + 1 >*/
            mm = m - l + 1;
/* $$$            call dlasr( 'r', 'v', 'b', n, mm, work( l ), work( n-1+l ), */
/* $$$     $                  z( 1, l ), ldz ) */

/*             *** New starting with version 2.5 *** */

/*<        >*/
            dlasr_("r", "v", "b", &c__1, &mm, &work[l], &work[*n - 1 + l], &
                    z__[l], &c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
/*             ************************************* */
/*<          end if >*/
        }

/*<          d( l ) = d( l ) - p >*/
        d__[l] -= p;
/*<          e( l ) = g >*/
        e[l] = g;
/*<          go to 40 >*/
        goto L40;

/*        eigenvalue found. */

/*<    80    continue >*/
L80:
/*<          d( l ) = p >*/
        d__[l] = p;

/*<          l = l + 1 >*/
        ++l;
/*<        >*/
        if (l <= lend) {
            goto L40;
        }
/*<          go to 140 >*/
        goto L140;

/*<       else >*/
    } else {

/*        qr iteration */

/*        look for small superdiagonal element. */

/*<    90    continue >*/
L90:
/*<          if( l.ne.lend ) then >*/
        if (l != lend) {
/*<             lendp1 = lend + 1 >*/
            lendp1 = lend + 1;
/*<             do 100 m = l, lendp1, -1 >*/
            i__1 = lendp1;
            for (m = l; m >= i__1; --m) {
/*<                tst = abs( e( m-1 ) )**2 >*/
/* Computing 2nd power */
                d__2 = (d__1 = e[m - 1], abs(d__1));
                tst = d__2 * d__2;
/*<        >*/
                if (tst <= eps2 * (d__1 = d__[m], abs(d__1)) * (d__2 = d__[m
                        - 1], abs(d__2)) + safmin) {
                    goto L110;
                }
/*<   100       continue >*/
/* L100: */
            }
/*<          end if >*/
        }

/*<          m = lend >*/
        m = lend;

/*<   110    continue >*/
L110:
/*<        >*/
        if (m > lend) {
            e[m - 1] = 0.;
        }
/*<          p = d( l ) >*/
        p = d__[l];
/*<        >*/
        if (m == l) {
            goto L130;
        }

/*        if remaining matrix is 2-by-2, use dlae2 or dlaev2 */
/*        to compute its eigensystem. */

/*<          if( m.eq.l-1 ) then >*/
        if (m == l - 1) {
/*<             if( icompz.gt.0 ) then >*/
            if (icompz > 0) {
/*<                call dlaev2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2, c, s ) >*/
                dlaev2_(&d__[l - 1], &e[l - 1], &d__[l], &rt1, &rt2, &c__, &s)
                        ;
/* $$$               work( m ) = c */
/* $$$               work( n-1+m ) = s */
/* $$$               call dlasr( 'r', 'v', 'f', n, 2, work( m ), */
/* $$$     $                     work( n-1+m ), z( 1, l-1 ), ldz ) */

/*               *** New starting with version 2.5 *** */

/*<                 tst      = z(l) >*/
                tst = z__[l];
/*<                 z(l)   = c*tst - s*z(l-1) >*/
                z__[l] = c__ * tst - s * z__[l - 1];
/*<                 z(l-1) = s*tst + c*z(l-1) >*/
                z__[l - 1] = s * tst + c__ * z__[l - 1];
/*               ************************************* */
/*<             else >*/
            } else {
/*<                call dlae2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2 ) >*/
                dlae2_(&d__[l - 1], &e[l - 1], &d__[l], &rt1, &rt2);
/*<             end if >*/
            }
/*<             d( l-1 ) = rt1 >*/
            d__[l - 1] = rt1;
/*<             d( l ) = rt2 >*/
            d__[l] = rt2;
/*<             e( l-1 ) = zero >*/
            e[l - 1] = 0.;
/*<             l = l - 2 >*/
            l += -2;
/*<        >*/
            if (l >= lend) {
                goto L90;
            }
/*<             go to 140 >*/
            goto L140;
/*<          end if >*/
        }

/*<        >*/
        if (jtot == nmaxit) {
            goto L140;
        }
/*<          jtot = jtot + 1 >*/
        ++jtot;

/*        form shift. */

/*<          g = ( d( l-1 )-p ) / ( two*e( l-1 ) ) >*/
        g = (d__[l - 1] - p) / (e[l - 1] * 2.);
/*<          r = dlapy2( g, one ) >*/
        r__ = dlapy2_(&g, &c_b31);
/*<          g = d( m ) - p + ( e( l-1 ) / ( g+sign( r, g ) ) ) >*/
        g = d__[m] - p + e[l - 1] / (g + d_sign(&r__, &g));

/*<          s = one >*/
        s = 1.;
/*<          c = one >*/
        c__ = 1.;
/*<          p = zero >*/
        p = 0.;

/*        inner loop */

/*<          lm1 = l - 1 >*/
        lm1 = l - 1;
/*<          do 120 i = m, lm1 >*/
        i__1 = lm1;
        for (i__ = m; i__ <= i__1; ++i__) {
/*<             f = s*e( i ) >*/
            f = s * e[i__];
/*<             b = c*e( i ) >*/
            b = c__ * e[i__];
/*<             call dlartg( g, f, c, s, r ) >*/
            dlartg_(&g, &f, &c__, &s, &r__);
/*<        >*/
            if (i__ != m) {
                e[i__ - 1] = r__;
            }
/*<             g = d( i ) - p >*/
            g = d__[i__] - p;
/*<             r = ( d( i+1 )-g )*s + two*c*b >*/
            r__ = (d__[i__ + 1] - g) * s + c__ * 2. * b;
/*<             p = s*r >*/
            p = s * r__;
/*<             d( i ) = g + p >*/
            d__[i__] = g + p;
/*<             g = c*r - b >*/
            g = c__ * r__ - b;

/*           if eigenvectors are desired, then save rotations. */

/*<             if( icompz.gt.0 ) then >*/
            if (icompz > 0) {
/*<                work( i ) = c >*/
                work[i__] = c__;
/*<                work( n-1+i ) = s >*/
                work[*n - 1 + i__] = s;
/*<             end if >*/
            }

/*<   120    continue >*/
/* L120: */
        }

/*        if eigenvectors are desired, then apply saved rotations. */

/*<          if( icompz.gt.0 ) then >*/
        if (icompz > 0) {
/*<             mm = l - m + 1 >*/
            mm = l - m + 1;
/* $$$            call dlasr( 'r', 'v', 'f', n, mm, work( m ), work( n-1+m ), */
/* $$$     $                  z( 1, m ), ldz ) */

/*           *** New starting with version 2.5 *** */

/*<        >*/
            dlasr_("r", "v", "f", &c__1, &mm, &work[m], &work[*n - 1 + m], &
                    z__[m], &c__1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
/*           ************************************* */
/*<          end if >*/
        }

/*<          d( l ) = d( l ) - p >*/
        d__[l] -= p;
/*<          e( lm1 ) = g >*/
        e[lm1] = g;
/*<          go to 90 >*/
        goto L90;

/*        eigenvalue found. */

/*<   130    continue >*/
L130:
/*<          d( l ) = p >*/
        d__[l] = p;

/*<          l = l - 1 >*/
        --l;
/*<        >*/
        if (l >= lend) {
            goto L90;
        }
/*<          go to 140 >*/
        goto L140;

/*<       end if >*/
    }

/*     undo scaling if necessary */

/*<   140 continue >*/
L140:
/*<       if( iscale.eq.1 ) then >*/
    if (iscale == 1) {
/*<        >*/
        i__1 = lendsv - lsv + 1;
        dlascl_("g", &c__0, &c__0, &ssfmax, &anorm, &i__1, &c__1, &d__[lsv],
                n, info, (ftnlen)1);
/*<        >*/
        i__1 = lendsv - lsv;
        dlascl_("g", &c__0, &c__0, &ssfmax, &anorm, &i__1, &c__1, &e[lsv], n,
                info, (ftnlen)1);
/*<       else if( iscale.eq.2 ) then >*/
    } else if (iscale == 2) {
/*<        >*/
        i__1 = lendsv - lsv + 1;
        dlascl_("g", &c__0, &c__0, &ssfmin, &anorm, &i__1, &c__1, &d__[lsv],
                n, info, (ftnlen)1);
/*<        >*/
        i__1 = lendsv - lsv;
        dlascl_("g", &c__0, &c__0, &ssfmin, &anorm, &i__1, &c__1, &e[lsv], n,
                info, (ftnlen)1);
/*<       end if >*/
    }

/*     check for no convergence to an eigenvalue after a total */
/*     of n*maxit iterations. */

/*<        >*/
    if (jtot < nmaxit) {
        goto L10;
    }
/*<       do 150 i = 1, n - 1 >*/
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<        >*/
        if (e[i__] != 0.) {
            ++(*info);
        }
/*<   150 continue >*/
/* L150: */
    }
/*<       go to 190 >*/
    goto L190;

/*     order eigenvalues and eigenvectors. */

/*<   160 continue >*/
L160:
/*<       if( icompz.eq.0 ) then >*/
    if (icompz == 0) {

/*        use quick sort */

/*<          call dlasrt( 'i', n, d, info ) >*/
        dlasrt_("i", n, &d__[1], info, (ftnlen)1);

/*<       else >*/
    } else {

/*        use selection sort to minimize swaps of eigenvectors */

/*<          do 180 ii = 2, n >*/
        i__1 = *n;
        for (ii = 2; ii <= i__1; ++ii) {
/*<             i = ii - 1 >*/
            i__ = ii - 1;
/*<             k = i >*/
            k = i__;
/*<             p = d( i ) >*/
            p = d__[i__];
/*<             do 170 j = ii, n >*/
            i__2 = *n;
            for (j = ii; j <= i__2; ++j) {
/*<                if( d( j ).lt.p ) then >*/
                if (d__[j] < p) {
/*<                   k = j >*/
                    k = j;
/*<                   p = d( j ) >*/
                    p = d__[j];
/*<                end if >*/
                }
/*<   170       continue >*/
/* L170: */
            }
/*<             if( k.ne.i ) then >*/
            if (k != i__) {
/*<                d( k ) = d( i ) >*/
                d__[k] = d__[i__];
/*<                d( i ) = p >*/
                d__[i__] = p;
/* $$$               call dswap( n, z( 1, i ), 1, z( 1, k ), 1 ) */
/*           *** New starting with version 2.5 *** */

/*<                p    = z(k) >*/
                p = z__[k];
/*<                z(k) = z(i) >*/
                z__[k] = z__[i__];
/*<                z(i) = p >*/
                z__[i__] = p;
/*           ************************************* */
/*<             end if >*/
            }
/*<   180    continue >*/
/* L180: */
        }
/*<       end if >*/
    }

/*<   190 continue >*/
L190:
/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dstqrb | */
/*     %---------------% */

/*<       end >*/
} /* dstqrb_ */

#ifdef __cplusplus
        }
#endif
