#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Subroutine */ void dlag2_(a, lda, b, ldb, safmin, scale1, scale2, wr1, wr2, wi)
doublereal *a;
integer *lda;
doublereal *b;
integer *ldb;
doublereal *safmin, *scale1, *scale2, *wr1, *wr2, *wi;
{
    /* Local variables */
    static doublereal diff, bmin, wbig, wabs, wdet, r, binv11, binv22,
            discr, anorm, bnorm, bsize, shift, c1, c2, c3, c4, c5, rtmin,
            rtmax, wsize, s1, s2, a11, a12, a21, a22, b11, b12, b22, ascale,
            bscale, pp, qq, ss, wscale, safmax, wsmall, as11, as12, as22, sum,
             abi22;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLAG2 computes the eigenvalues of a 2 x 2 generalized eigenvalue      */
/*  problem  A - w B, with scaling as necessary to avoid over-/underflow. */
/*                                                                        */
/*  The scaling factor "s" results in a modified eigenvalue equation      */
/*                                                                        */
/*      s A - w B                                                         */
/*                                                                        */
/*  where  s  is a non-negative scaling factor chosen so that  w,  w B,   */
/*  and  s A  do not overflow and, if possible, do not underflow, either. */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  A       (input) DOUBLE PRECISION array, dimension (LDA, 2)            */
/*          On entry, the 2 x 2 matrix A.  It is assumed that its 1-norm  */
/*          is less than 1/SAFMIN.  Entries less than                     */
/*          sqrt(SAFMIN)*norm(A) are subject to being treated as zero.    */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= 2.              */
/*                                                                        */
/*  B       (input) DOUBLE PRECISION array, dimension (LDB, 2)            */
/*          On entry, the 2 x 2 upper triangular matrix B.  It is         */
/*          assumed that the one-norm of B is less than 1/SAFMIN.  The    */
/*          diagonals should be at least sqrt(SAFMIN) times the largest   */
/*          element of B (in absolute value); if a diagonal is smaller    */
/*          than that, then  +/- sqrt(SAFMIN) will be used instead of     */
/*          that diagonal.                                                */
/*                                                                        */
/*  LDB     (input) INTEGER                                               */
/*          The leading dimension of the array B.  LDB >= 2.              */
/*                                                                        */
/*  SAFMIN  (input) DOUBLE PRECISION                                      */
/*          The smallest positive number s.t. 1/SAFMIN does not           */
/*          overflow.  (This should always be DLAMCH('S') -- it is an     */
/*          argument in order to avoid having to call DLAMCH frequently.) */
/*                                                                        */
/*  SCALE1  (output) DOUBLE PRECISION                                     */
/*          A scaling factor used to avoid over-/underflow in the         */
/*          eigenvalue equation which defines the first eigenvalue.  If   */
/*          the eigenvalues are complex, then the eigenvalues are         */
/*          ( WR1  +/-  WI i ) / SCALE1  (which may lie outside the       */
/*          exponent range of the machine), SCALE1=SCALE2, and SCALE1     */
/*          will always be positive.  If the eigenvalues are real, then   */
/*          the first (real) eigenvalue is  WR1 / SCALE1 , but this may   */
/*          overflow or underflow, and in fact, SCALE1 may be zero or     */
/*          less than the underflow threshhold if the exact eigenvalue    */
/*          is sufficiently large.                                        */
/*                                                                        */
/*  SCALE2  (output) DOUBLE PRECISION                                     */
/*          A scaling factor used to avoid over-/underflow in the         */
/*          eigenvalue equation which defines the second eigenvalue.  If  */
/*          the eigenvalues are complex, then SCALE2=SCALE1.  If the      */
/*          eigenvalues are real, then the second (real) eigenvalue is    */
/*          WR2 / SCALE2 , but this may overflow or underflow, and in     */
/*          fact, SCALE2 may be zero or less than the underflow           */
/*          threshhold if the exact eigenvalue is sufficiently large.     */
/*                                                                        */
/*  WR1     (output) DOUBLE PRECISION                                     */
/*          If the eigenvalue is real, then WR1 is SCALE1 times the       */
/*          eigenvalue closest to the (2,2) element of A B**(-1).  If the */
/*          eigenvalue is complex, then WR1=WR2 is SCALE1 times the real  */
/*          part of the eigenvalues.                                      */
/*                                                                        */
/*  WR2     (output) DOUBLE PRECISION                                     */
/*          If the eigenvalue is real, then WR2 is SCALE2 times the       */
/*          other eigenvalue.  If the eigenvalue is complex, then         */
/*          WR1=WR2 is SCALE1 times the real part of the eigenvalues.     */
/*                                                                        */
/*  WI      (output) DOUBLE PRECISION                                     */
/*          If the eigenvalue is real, then WI is zero.  If the           */
/*          eigenvalue is complex, then WI is SCALE1 times the imaginary  */
/*          part of the eigenvalues.  WI will always be non-negative.     */
/*                                                                        */
/*  ===================================================================== */

    rtmin = sqrt(*safmin);
    rtmax = 1. / rtmin;
    safmax = 1. / *safmin;

/*     Scale A */

    anorm = abs(a[0]) + abs(a[1]),
    anorm = max(anorm, abs(a[*lda]) + abs(a[*lda + 1]));
    anorm = max(anorm, *safmin);
    ascale = 1. / anorm;
    a11 = ascale * a[0];
    a21 = ascale * a[1];
    a12 = ascale * a[*lda];
    a22 = ascale * a[*lda + 1];

/*     Perturb B if necessary to insure non-singularity */

    b11 = b[0];
    b12 = b[*ldb];
    b22 = b[*ldb + 1];
    bmin = rtmin * max(max(max(abs(b11),abs(b12)),abs(b22)),rtmin);
    if (abs(b11) < bmin) {
        b11 = d_sign(&bmin, &b11);
    }
    if (abs(b22) < bmin) {
        b22 = d_sign(&bmin, &b22);
    }

/*     Scale B */

    bnorm = max(max(abs(b11), abs(b12)+abs(b22)), *safmin);
    bsize = max(abs(b11),abs(b22));
    bscale = 1. / bsize;
    b11 *= bscale;
    b12 *= bscale;
    b22 *= bscale;

/*     Compute larger eigenvalue by method described by C. van Loan */

/*     ( AS is A shifted by -SHIFT*B ) */

    binv11 = 1. / b11;
    binv22 = 1. / b22;
    s1 = a11 * binv11;
    s2 = a22 * binv22;
    if (abs(s1) <= abs(s2)) {
        as12 = a12 - s1 * b12;
        as22 = a22 - s1 * b22;
        ss = a21 * (binv11 * binv22);
        abi22 = as22 * binv22 - ss * b12;
        pp = abi22 * .5;
        shift = s1;
    } else {
        as12 = a12 - s2 * b12;
        as11 = a11 - s2 * b11;
        ss = a21 * (binv11 * binv22);
        abi22 = -ss * b12;
        pp = (as11 * binv11 + abi22) * .5;
        shift = s2;
    }
    qq = ss * as12;
    if (abs(pp * rtmin) >= 1.) {
        discr = rtmin * pp; discr = discr * discr + qq * *safmin;
        r = sqrt((abs(discr))) * rtmax;
    } else {
        if (pp * pp + abs(qq) <= *safmin) {
            discr = rtmax * pp; discr = discr * discr + qq * safmax;
            r = sqrt((abs(discr))) * rtmin;
        } else {
            discr = pp * pp + qq;
            r = sqrt((abs(discr)));
        }
    }

/*     Note: the test of R in the following IF is to cover the case when */
/*           DISCR is small and negative and is flushed to zero during */
/*           the calculation of R.  On machines which have a consistent */
/*           flush-to-zero threshhold and handle numbers above that */
/*           threshhold correctly, it would not be necessary. */

    if (discr >= 0. || r == 0.) {
        sum = pp + d_sign(&r, &pp);
        diff = pp - d_sign(&r, &pp);
        wbig = shift + sum;

/*        Compute smaller eigenvalue */

        wsmall = shift + diff;
        if (abs(wbig) * .5 > max(abs(wsmall),*safmin)) {
            wdet = (a11 * a22 - a12 * a21) * (binv11 * binv22);
            wsmall = wdet / wbig;
        }

/*        Choose (real) eigenvalue closest to 2,2 element of A*B**(-1) */
/*        for WR1. */

        if (pp > abi22) {
            *wr1 = min(wbig,wsmall);
            *wr2 = max(wbig,wsmall);
        } else {
            *wr1 = max(wbig,wsmall);
            *wr2 = min(wbig,wsmall);
        }
        *wi = 0.;
    } else {

/*        Complex eigenvalues */

        *wr1 = shift + pp;
        *wr2 = *wr1;
        *wi = r;
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

    c1 = bsize * (*safmin * max(1.,ascale));
    c2 = *safmin * max(1.,bnorm);
    c3 = bsize * *safmin;
    if (ascale <= 1. && bsize <= 1.) {
        c4 = min(1., ascale / *safmin * bsize);
    } else {
        c4 = 1.;
    }
    if (ascale <= 1. || bsize <= 1.) {
        c5 = min(1., ascale * bsize);
    } else {
        c5 = 1.;
    }

/*     Scale first eigenvalue */

    wabs = abs(*wr1) + abs(*wi);
    wsize = min(c4, max(wabs,c5) * .5);
    wsize = max(max(max(*safmin,c1), (wabs * c2 + c3) * 1.0000100000000001), wsize);
    if (wsize != 1.) {
        wscale = 1. / wsize;
        if (wsize > 1.) {
            *scale1 = max(ascale,bsize) * wscale * min(ascale,bsize);
        } else {
            *scale1 = min(ascale,bsize) * wscale * max(ascale,bsize);
        }
        *wr1 *= wscale;
        if (*wi != 0.) {
            *wi *= wscale;
            *wr2 = *wr1;
            *scale2 = *scale1;
        }
    } else {
        *scale1 = ascale * bsize;
        *scale2 = *scale1;
    }

/*     Scale second eigenvalue (if real) */

    if (*wi == 0.) {
        wsize = min(c4, max(abs(*wr2),c5) * .5);
        wsize = max(max(max(*safmin,c1),(abs(*wr2) * c2 + c3) * 1.0000100000000001), wsize);
        if (wsize != 1.) {
            wscale = 1. / wsize;
            if (wsize > 1.) {
                *scale2 = max(ascale,bsize) * wscale * min(ascale,bsize);
            } else {
                *scale2 = min(ascale,bsize) * wscale * max(ascale,bsize);
            }
            *wr2 *= wscale;
        } else {
            *scale2 = ascale * bsize;
        }
    }
} /* dlag2_ */
