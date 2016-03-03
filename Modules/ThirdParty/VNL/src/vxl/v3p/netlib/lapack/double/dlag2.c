/* lapack/double/dlag2.f -- translated by f2c (version 20050501).
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

/*<    >*/
/* Subroutine */ int dlag2_(doublereal *a, integer *lda, doublereal *b,
        integer *ldb, doublereal *safmin, doublereal *scale1, doublereal *
        scale2, doublereal *wr1, doublereal *wr2, doublereal *wi)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal r__, c1, c2, c3, c4, c5, s1, s2, a11, a12, a21, a22, b11, b12,
            b22, pp, qq, ss, as11, as12, as22, sum, abi22, diff, bmin, wbig,
            wabs, wdet, binv11, binv22, discr, anorm, bnorm, bsize, shift,
            rtmin, rtmax, wsize, ascale, bscale, wscale, safmax, wsmall;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            LDA, LDB >*/
/*<       DOUBLE PRECISION   SAFMIN, SCALE1, SCALE2, WI, WR1, WR2 >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAG2 computes the eigenvalues of a 2 x 2 generalized eigenvalue */
/*  problem  A - w B, with scaling as necessary to avoid over-/underflow. */

/*  The scaling factor "s" results in a modified eigenvalue equation */

/*      s A - w B */

/*  where  s  is a non-negative scaling factor chosen so that  w,  w B, */
/*  and  s A  do not overflow and, if possible, do not underflow, either. */

/*  Arguments */
/*  ========= */

/*  A       (input) DOUBLE PRECISION array, dimension (LDA, 2) */
/*          On entry, the 2 x 2 matrix A.  It is assumed that its 1-norm */
/*          is less than 1/SAFMIN.  Entries less than */
/*          sqrt(SAFMIN)*norm(A) are subject to being treated as zero. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= 2. */

/*  B       (input) DOUBLE PRECISION array, dimension (LDB, 2) */
/*          On entry, the 2 x 2 upper triangular matrix B.  It is */
/*          assumed that the one-norm of B is less than 1/SAFMIN.  The */
/*          diagonals should be at least sqrt(SAFMIN) times the largest */
/*          element of B (in absolute value); if a diagonal is smaller */
/*          than that, then  +/- sqrt(SAFMIN) will be used instead of */
/*          that diagonal. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B.  LDB >= 2. */

/*  SAFMIN  (input) DOUBLE PRECISION */
/*          The smallest positive number s.t. 1/SAFMIN does not */
/*          overflow.  (This should always be DLAMCH('S') -- it is an */
/*          argument in order to avoid having to call DLAMCH frequently.) */

/*  SCALE1  (output) DOUBLE PRECISION */
/*          A scaling factor used to avoid over-/underflow in the */
/*          eigenvalue equation which defines the first eigenvalue.  If */
/*          the eigenvalues are complex, then the eigenvalues are */
/*          ( WR1  +/-  WI i ) / SCALE1  (which may lie outside the */
/*          exponent range of the machine), SCALE1=SCALE2, and SCALE1 */
/*          will always be positive.  If the eigenvalues are real, then */
/*          the first (real) eigenvalue is  WR1 / SCALE1 , but this may */
/*          overflow or underflow, and in fact, SCALE1 may be zero or */
/*          less than the underflow threshhold if the exact eigenvalue */
/*          is sufficiently large. */

/*  SCALE2  (output) DOUBLE PRECISION */
/*          A scaling factor used to avoid over-/underflow in the */
/*          eigenvalue equation which defines the second eigenvalue.  If */
/*          the eigenvalues are complex, then SCALE2=SCALE1.  If the */
/*          eigenvalues are real, then the second (real) eigenvalue is */
/*          WR2 / SCALE2 , but this may overflow or underflow, and in */
/*          fact, SCALE2 may be zero or less than the underflow */
/*          threshhold if the exact eigenvalue is sufficiently large. */

/*  WR1     (output) DOUBLE PRECISION */
/*          If the eigenvalue is real, then WR1 is SCALE1 times the */
/*          eigenvalue closest to the (2,2) element of A B**(-1).  If the */
/*          eigenvalue is complex, then WR1=WR2 is SCALE1 times the real */
/*          part of the eigenvalues. */

/*  WR2     (output) DOUBLE PRECISION */
/*          If the eigenvalue is real, then WR2 is SCALE2 times the */
/*          other eigenvalue.  If the eigenvalue is complex, then */
/*          WR1=WR2 is SCALE1 times the real part of the eigenvalues. */

/*  WI      (output) DOUBLE PRECISION */
/*          If the eigenvalue is real, then WI is zero.  If the */
/*          eigenvalue is complex, then WI is SCALE1 times the imaginary */
/*          part of the eigenvalues.  WI will always be non-negative. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE, TWO >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 ) >*/
/*<       DOUBLE PRECISION   HALF >*/
/*<       PARAMETER          ( HALF = ONE / TWO ) >*/
/*<       DOUBLE PRECISION   FUZZY1 >*/
/*<       PARAMETER          ( FUZZY1 = ONE+1.0D-5 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SIGN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       RTMIN = SQRT( SAFMIN ) >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;

    /* Function Body */
    rtmin = sqrt(*safmin);
/*<       RTMAX = ONE / RTMIN >*/
    rtmax = 1. / rtmin;
/*<       SAFMAX = ONE / SAFMIN >*/
    safmax = 1. / *safmin;

/*     Scale A */

/*<    >*/
/* Computing MAX */
    d__5 = (d__1 = a[a_dim1 + 1], abs(d__1)) + (d__2 = a[a_dim1 + 2], abs(
            d__2)), d__6 = (d__3 = a[(a_dim1 << 1) + 1], abs(d__3)) + (d__4 =
            a[(a_dim1 << 1) + 2], abs(d__4)), d__5 = max(d__5,d__6);
    anorm = max(d__5,*safmin);
/*<       ASCALE = ONE / ANORM >*/
    ascale = 1. / anorm;
/*<       A11 = ASCALE*A( 1, 1 ) >*/
    a11 = ascale * a[a_dim1 + 1];
/*<       A21 = ASCALE*A( 2, 1 ) >*/
    a21 = ascale * a[a_dim1 + 2];
/*<       A12 = ASCALE*A( 1, 2 ) >*/
    a12 = ascale * a[(a_dim1 << 1) + 1];
/*<       A22 = ASCALE*A( 2, 2 ) >*/
    a22 = ascale * a[(a_dim1 << 1) + 2];

/*     Perturb B if necessary to insure non-singularity */

/*<       B11 = B( 1, 1 ) >*/
    b11 = b[b_dim1 + 1];
/*<       B12 = B( 1, 2 ) >*/
    b12 = b[(b_dim1 << 1) + 1];
/*<       B22 = B( 2, 2 ) >*/
    b22 = b[(b_dim1 << 1) + 2];
/*<       BMIN = RTMIN*MAX( ABS( B11 ), ABS( B12 ), ABS( B22 ), RTMIN ) >*/
/* Computing MAX */
    d__1 = abs(b11), d__2 = abs(b12), d__1 = max(d__1,d__2), d__2 = abs(b22),
            d__1 = max(d__1,d__2);
    bmin = rtmin * max(d__1,rtmin);
/*<    >*/
    if (abs(b11) < bmin) {
        b11 = d_sign(&bmin, &b11);
    }
/*<    >*/
    if (abs(b22) < bmin) {
        b22 = d_sign(&bmin, &b22);
    }

/*     Scale B */

/*<       BNORM = MAX( ABS( B11 ), ABS( B12 )+ABS( B22 ), SAFMIN ) >*/
/* Computing MAX */
    d__1 = abs(b11), d__2 = abs(b12) + abs(b22), d__1 = max(d__1,d__2);
    bnorm = max(d__1,*safmin);
/*<       BSIZE = MAX( ABS( B11 ), ABS( B22 ) ) >*/
/* Computing MAX */
    d__1 = abs(b11), d__2 = abs(b22);
    bsize = max(d__1,d__2);
/*<       BSCALE = ONE / BSIZE >*/
    bscale = 1. / bsize;
/*<       B11 = B11*BSCALE >*/
    b11 *= bscale;
/*<       B12 = B12*BSCALE >*/
    b12 *= bscale;
/*<       B22 = B22*BSCALE >*/
    b22 *= bscale;

/*     Compute larger eigenvalue by method described by C. van Loan */

/*     ( AS is A shifted by -SHIFT*B ) */

/*<       BINV11 = ONE / B11 >*/
    binv11 = 1. / b11;
/*<       BINV22 = ONE / B22 >*/
    binv22 = 1. / b22;
/*<       S1 = A11*BINV11 >*/
    s1 = a11 * binv11;
/*<       S2 = A22*BINV22 >*/
    s2 = a22 * binv22;
/*<       IF( ABS( S1 ).LE.ABS( S2 ) ) THEN >*/
    if (abs(s1) <= abs(s2)) {
/*<          AS12 = A12 - S1*B12 >*/
        as12 = a12 - s1 * b12;
/*<          AS22 = A22 - S1*B22 >*/
        as22 = a22 - s1 * b22;
/*<          SS = A21*( BINV11*BINV22 ) >*/
        ss = a21 * (binv11 * binv22);
/*<          ABI22 = AS22*BINV22 - SS*B12 >*/
        abi22 = as22 * binv22 - ss * b12;
/*<          PP = HALF*ABI22 >*/
        pp = abi22 * .5;
/*<          SHIFT = S1 >*/
        shift = s1;
/*<       ELSE >*/
    } else {
/*<          AS12 = A12 - S2*B12 >*/
        as12 = a12 - s2 * b12;
/*<          AS11 = A11 - S2*B11 >*/
        as11 = a11 - s2 * b11;
/*<          SS = A21*( BINV11*BINV22 ) >*/
        ss = a21 * (binv11 * binv22);
/*<          ABI22 = -SS*B12 >*/
        abi22 = -ss * b12;
/*<          PP = HALF*( AS11*BINV11+ABI22 ) >*/
        pp = (as11 * binv11 + abi22) * .5;
/*<          SHIFT = S2 >*/
        shift = s2;
/*<       END IF >*/
    }
/*<       QQ = SS*AS12 >*/
    qq = ss * as12;
/*<       IF( ABS( PP*RTMIN ).GE.ONE ) THEN >*/
    if ((d__1 = pp * rtmin, abs(d__1)) >= 1.) {
/*<          DISCR = ( RTMIN*PP )**2 + QQ*SAFMIN >*/
/* Computing 2nd power */
        d__1 = rtmin * pp;
        discr = d__1 * d__1 + qq * *safmin;
/*<          R = SQRT( ABS( DISCR ) )*RTMAX >*/
        r__ = sqrt((abs(discr))) * rtmax;
/*<       ELSE >*/
    } else {
/*<          IF( PP**2+ABS( QQ ).LE.SAFMIN ) THEN >*/
/* Computing 2nd power */
        d__1 = pp;
        if (d__1 * d__1 + abs(qq) <= *safmin) {
/*<             DISCR = ( RTMAX*PP )**2 + QQ*SAFMAX >*/
/* Computing 2nd power */
            d__1 = rtmax * pp;
            discr = d__1 * d__1 + qq * safmax;
/*<             R = SQRT( ABS( DISCR ) )*RTMIN >*/
            r__ = sqrt((abs(discr))) * rtmin;
/*<          ELSE >*/
        } else {
/*<             DISCR = PP**2 + QQ >*/
/* Computing 2nd power */
            d__1 = pp;
            discr = d__1 * d__1 + qq;
/*<             R = SQRT( ABS( DISCR ) ) >*/
            r__ = sqrt((abs(discr)));
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*     Note: the test of R in the following IF is to cover the case when */
/*           DISCR is small and negative and is flushed to zero during */
/*           the calculation of R.  On machines which have a consistent */
/*           flush-to-zero threshhold and handle numbers above that */
/*           threshhold correctly, it would not be necessary. */

/*<       IF( DISCR.GE.ZERO .OR. R.EQ.ZERO ) THEN >*/
    if (discr >= 0. || r__ == 0.) {
/*<          SUM = PP + SIGN( R, PP ) >*/
        sum = pp + d_sign(&r__, &pp);
/*<          DIFF = PP - SIGN( R, PP ) >*/
        diff = pp - d_sign(&r__, &pp);
/*<          WBIG = SHIFT + SUM >*/
        wbig = shift + sum;

/*        Compute smaller eigenvalue */

/*<          WSMALL = SHIFT + DIFF >*/
        wsmall = shift + diff;
/*<          IF( HALF*ABS( WBIG ).GT.MAX( ABS( WSMALL ), SAFMIN ) ) THEN >*/
/* Computing MAX */
        d__1 = abs(wsmall);
        if (abs(wbig) * .5 > max(d__1,*safmin)) {
/*<             WDET = ( A11*A22-A12*A21 )*( BINV11*BINV22 ) >*/
            wdet = (a11 * a22 - a12 * a21) * (binv11 * binv22);
/*<             WSMALL = WDET / WBIG >*/
            wsmall = wdet / wbig;
/*<          END IF >*/
        }

/*        Choose (real) eigenvalue closest to 2,2 element of A*B**(-1) */
/*        for WR1. */

/*<          IF( PP.GT.ABI22 ) THEN >*/
        if (pp > abi22) {
/*<             WR1 = MIN( WBIG, WSMALL ) >*/
            *wr1 = min(wbig,wsmall);
/*<             WR2 = MAX( WBIG, WSMALL ) >*/
            *wr2 = max(wbig,wsmall);
/*<          ELSE >*/
        } else {
/*<             WR1 = MAX( WBIG, WSMALL ) >*/
            *wr1 = max(wbig,wsmall);
/*<             WR2 = MIN( WBIG, WSMALL ) >*/
            *wr2 = min(wbig,wsmall);
/*<          END IF >*/
        }
/*<          WI = ZERO >*/
        *wi = 0.;
/*<       ELSE >*/
    } else {

/*        Complex eigenvalues */

/*<          WR1 = SHIFT + PP >*/
        *wr1 = shift + pp;
/*<          WR2 = WR1 >*/
        *wr2 = *wr1;
/*<          WI = R >*/
        *wi = r__;
/*<       END IF >*/
    }

/*     Further scaling to avoid underflow and overflow in computing */
/*     SCALE1 and overflow in computing w*B. */

/*     This scale factor (WSCALE) is bounded from above using C1 and C2, */
/*     and from below using C3 and C4. */
/*        C1 implements the condition  s A  must never overflow. */
/*        C2 implements the condition  w B  must never overflow. */
/*        C3, with C2, */
/*           implement the condition that s A - w B must never overflow. */
/*        C4 implements the condition  s    should not underflow. */
/*        C5 implements the condition  max(s,|w|) should be at least 2. */

/*<       C1 = BSIZE*( SAFMIN*MAX( ONE, ASCALE ) ) >*/
    c1 = bsize * (*safmin * max(1.,ascale));
/*<       C2 = SAFMIN*MAX( ONE, BNORM ) >*/
    c2 = *safmin * max(1.,bnorm);
/*<       C3 = BSIZE*SAFMIN >*/
    c3 = bsize * *safmin;
/*<       IF( ASCALE.LE.ONE .AND. BSIZE.LE.ONE ) THEN >*/
    if (ascale <= 1. && bsize <= 1.) {
/*<          C4 = MIN( ONE, ( ASCALE / SAFMIN )*BSIZE ) >*/
/* Computing MIN */
        d__1 = 1., d__2 = ascale / *safmin * bsize;
        c4 = min(d__1,d__2);
/*<       ELSE >*/
    } else {
/*<          C4 = ONE >*/
        c4 = 1.;
/*<       END IF >*/
    }
/*<       IF( ASCALE.LE.ONE .OR. BSIZE.LE.ONE ) THEN >*/
    if (ascale <= 1. || bsize <= 1.) {
/*<          C5 = MIN( ONE, ASCALE*BSIZE ) >*/
/* Computing MIN */
        d__1 = 1., d__2 = ascale * bsize;
        c5 = min(d__1,d__2);
/*<       ELSE >*/
    } else {
/*<          C5 = ONE >*/
        c5 = 1.;
/*<       END IF >*/
    }

/*     Scale first eigenvalue */

/*<       WABS = ABS( WR1 ) + ABS( WI ) >*/
    wabs = abs(*wr1) + abs(*wi);
/*<    >*/
/* Computing MAX */
/* Computing MIN */
    d__3 = c4, d__4 = max(wabs,c5) * .5;
    d__1 = max(*safmin,c1), d__2 = (wabs * c2 + c3) * 1.0000100000000001,
            d__1 = max(d__1,d__2), d__2 = min(d__3,d__4);
    wsize = max(d__1,d__2);
/*<       IF( WSIZE.NE.ONE ) THEN >*/
    if (wsize != 1.) {
/*<          WSCALE = ONE / WSIZE >*/
        wscale = 1. / wsize;
/*<          IF( WSIZE.GT.ONE ) THEN >*/
        if (wsize > 1.) {
/*<    >*/
            *scale1 = max(ascale,bsize) * wscale * min(ascale,bsize);
/*<          ELSE >*/
        } else {
/*<    >*/
            *scale1 = min(ascale,bsize) * wscale * max(ascale,bsize);
/*<          END IF >*/
        }
/*<          WR1 = WR1*WSCALE >*/
        *wr1 *= wscale;
/*<          IF( WI.NE.ZERO ) THEN >*/
        if (*wi != 0.) {
/*<             WI = WI*WSCALE >*/
            *wi *= wscale;
/*<             WR2 = WR1 >*/
            *wr2 = *wr1;
/*<             SCALE2 = SCALE1 >*/
            *scale2 = *scale1;
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          SCALE1 = ASCALE*BSIZE >*/
        *scale1 = ascale * bsize;
/*<          SCALE2 = SCALE1 >*/
        *scale2 = *scale1;
/*<       END IF >*/
    }

/*     Scale second eigenvalue (if real) */

/*<       IF( WI.EQ.ZERO ) THEN >*/
    if (*wi == 0.) {
/*<    >*/
/* Computing MAX */
/* Computing MIN */
/* Computing MAX */
        d__5 = abs(*wr2);
        d__3 = c4, d__4 = max(d__5,c5) * .5;
        d__1 = max(*safmin,c1), d__2 = (abs(*wr2) * c2 + c3) *
                1.0000100000000001, d__1 = max(d__1,d__2), d__2 = min(d__3,
                d__4);
        wsize = max(d__1,d__2);
/*<          IF( WSIZE.NE.ONE ) THEN >*/
        if (wsize != 1.) {
/*<             WSCALE = ONE / WSIZE >*/
            wscale = 1. / wsize;
/*<             IF( WSIZE.GT.ONE ) THEN >*/
            if (wsize > 1.) {
/*<    >*/
                *scale2 = max(ascale,bsize) * wscale * min(ascale,bsize);
/*<             ELSE >*/
            } else {
/*<    >*/
                *scale2 = min(ascale,bsize) * wscale * max(ascale,bsize);
/*<             END IF >*/
            }
/*<             WR2 = WR2*WSCALE >*/
            *wr2 *= wscale;
/*<          ELSE >*/
        } else {
/*<             SCALE2 = ASCALE*BSIZE >*/
            *scale2 = ascale * bsize;
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*     End of DLAG2 */

/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dlag2_ */

#ifdef __cplusplus
        }
#endif
