#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, Oct 2003: manual optimisation and clean-up */

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

/* Subroutine */ void dlagv2_(a, lda, b, ldb, alphar, alphai, beta, csl, snl, csr, snr)
doublereal *a;
integer *lda;
doublereal *b;
integer *ldb;
doublereal *alphar, *alphai, *beta, *csl, *snl, *csr, *snr;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal r, t, anorm, bnorm, h1, h2, h3, scale1, scale2;
    static doublereal ascale, bscale;
    static doublereal wi, qq, rr, safmin;
    static doublereal wr1, wr2, ulp;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLAGV2 computes the Generalized Schur factorization of a real 2-by-2  */
/*  matrix pencil (A,B) where B is upper triangular. This routine         */
/*  computes orthogonal (rotation) matrices given by CSL, SNL and CSR,    */
/*  SNR such that                                                         */
/*                                                                        */
/*  1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0   */
/*     types), then                                                       */
/*                                                                        */
/*     [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]             */
/*     [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]             */
/*                                                                        */
/*     [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]             */
/*     [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],            */
/*                                                                        */
/*  2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,   */
/*     then                                                               */
/*                                                                        */
/*     [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]             */
/*     [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]             */
/*                                                                        */
/*     [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]             */
/*     [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]             */
/*                                                                        */
/*     where b11 >= b22 > 0.                                              */
/*                                                                        */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, 2)     */
/*          On entry, the 2 x 2 matrix A.                                 */
/*          On exit, A is overwritten by the ``A-part'' of the            */
/*          generalized Schur form.                                       */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          THe leading dimension of the array A.  LDA >= 2.              */
/*                                                                        */
/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, 2)     */
/*          On entry, the upper triangular 2 x 2 matrix B.                */
/*          On exit, B is overwritten by the ``B-part'' of the            */
/*          generalized Schur form.                                       */
/*                                                                        */
/*  LDB     (input) INTEGER                                               */
/*          THe leading dimension of the array B.  LDB >= 2.              */
/*                                                                        */
/*  ALPHAR  (output) DOUBLE PRECISION array, dimension (2)                */
/*  ALPHAI  (output) DOUBLE PRECISION array, dimension (2)                */
/*  BETA    (output) DOUBLE PRECISION array, dimension (2)                */
/*          (ALPHAR(k)+i*ALPHAI(k))/BETA(k) are the eigenvalues of the    */
/*          pencil (A,B), k=1,2, i = sqrt(-1).  Note that BETA(k) may     */
/*          be zero.                                                      */
/*                                                                        */
/*  CSL     (output) DOUBLE PRECISION                                     */
/*          The cosine of the left rotation matrix.                       */
/*                                                                        */
/*  SNL     (output) DOUBLE PRECISION                                     */
/*          The sine of the left rotation matrix.                         */
/*                                                                        */
/*  CSR     (output) DOUBLE PRECISION                                     */
/*          The cosine of the right rotation matrix.                      */
/*                                                                        */
/*  SNR     (output) DOUBLE PRECISION                                     */
/*          The sine of the right rotation matrix.                        */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  Based on contributions by                                             */
/*     Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA      */
/*                                                                        */
/*  ===================================================================== */

    safmin = dlamch_("S");
    ulp = dlamch_("P");

    /* Scale A */

    anorm = max(max(abs(a[0]) + abs(a[1]), abs(a[*lda]) + abs(a[*lda + 1])), safmin);
    ascale = 1. / anorm;
    a[0] *= ascale;
    a[1] *= ascale;
    a[*lda] *= ascale;
    a[*lda + 1] *= ascale;

    /* Scale B */

    bnorm = max(max(abs(b[0]), abs(b[*ldb]) + abs(b[*ldb + 1])), safmin);
    bscale = 1. / bnorm;
    b[0] *= bscale;
    b[*ldb] *= bscale;
    b[*ldb + 1] *= bscale;

    /* Check if A can be deflated */

    if (abs(a[1]) <= ulp) {
        *csl = 1.;
        *snl = 0.;
        *csr = 1.;
        *snr = 0.;
        a[1] = 0.;
        b[1] = 0.;
    }

    /* Check if B is singular */

    else if (abs(b[0]) <= ulp) {
        dlartg_(a, a+1, csl, snl, &r);
        *csr = 1.;
        *snr = 0.;
        drot_(&c__2, a, lda, a+1, lda, csl, snl);
        drot_(&c__2, b, ldb, b+1, ldb, csl, snl);
        a[1] = 0.;
        b[0] = 0.;
        b[1] = 0.;
    }
    else if (abs(b[*ldb + 1]) <= ulp) {
        dlartg_(&a[*lda + 1], a+1, csr, snr, &t);
        *snr = -(*snr);
        drot_(&c__2, a, &c__1, &a[*lda], &c__1, csr, snr);
        drot_(&c__2, b, &c__1, &b[*ldb], &c__1, csr, snr);
        *csl = 1.;
        *snl = 0.;
        a[1] = 0.;
        b[1] = 0.;
        b[*ldb + 1] = 0.;
    }
    else
    {
      /* B is nonsingular, first compute the eigenvalues of (A,B) */
        dlag2_(a, lda, b, ldb, &safmin, &scale1, &scale2, &wr1, &wr2, &wi);

        if (wi == 0.)
        {
            /* two real eigenvalues, compute s*A-w*B */
            h1 = scale1 * a[0] - wr1 * b[0];
            h2 = scale1 * a[*lda    ] - wr1 * b[*ldb    ];
            h3 = scale1 * a[*lda + 1] - wr1 * b[*ldb + 1];

            rr = dlapy2_(&h1, &h2);
            d__1 = scale1 * a[1];
            qq = dlapy2_(&d__1, &h3);

            if (rr > qq) /* find right rotation matrix to zero 1,1 element of (sA - wB) */
                dlartg_(&h2, &h1, csr, snr, &t);
            else         /* find right rotation matrix to zero 2,1 element of (sA - wB) */
            {
                d__1 = scale1 * a[1];
                dlartg_(&h3, &d__1, csr, snr, &t);
            }

            *snr = -(*snr);
            drot_(&c__2, a, &c__1, &a[*lda], &c__1, csr, snr);
            drot_(&c__2, b, &c__1, &b[*ldb], &c__1, csr, snr);

            /* compute inf norms of A and B */

            h1 = max(abs(a[0]) + abs(a[*lda]),
                     abs(a[1]) + abs(a[*lda + 1]));
            h2 = max(abs(b[0]) + abs(b[*ldb]),
                     abs(b[1]) + abs(b[*ldb + 1]));

            if (scale1 * h1 >= abs(wr1) * h2) /* find left rotation matrix Q to zero out B(2,1) */
                dlartg_(b, b+1, csl, snl, &r);
            else                              /* find left rotation matrix Q to zero out A(2,1) */
                dlartg_(a, a+1, csl, snl, &r);

            drot_(&c__2, a, lda, a+1, lda, csl, snl);
            drot_(&c__2, b, ldb, b+1, ldb, csl, snl);

            a[1] = 0.;
            b[1] = 0.;
        }
        else
        {
            /* a pair of complex conjugate eigenvalues */
            /* first compute the SVD of the matrix B */

            dlasv2_(b, &b[*ldb], &b[*ldb + 1], &r, &t, snr, csr, snl, csl);

            /* Form (A,B) := Q(A,B)Z' where Q is left rotation matrix and */
            /* Z is right rotation matrix computed from DLASV2 */

            drot_(&c__2, a, lda, a+1, lda, csl, snl);
            drot_(&c__2, b, ldb, b+1, ldb, csl, snl);
            drot_(&c__2, a, &c__1, &a[*lda], &c__1, csr, snr);
            drot_(&c__2, b, &c__1, &b[*ldb], &c__1, csr, snr);

            b[1] = 0.;
            b[*ldb] = 0.;
        }
    }

    /* Unscaling */

    a[0] *= anorm;
    a[1] *= anorm;
    a[*lda] *= anorm;
    a[*lda + 1] *= anorm;
    b[0] *= bnorm;
    b[1] *= bnorm;
    b[*ldb] *= bnorm;
    b[*ldb + 1] *= bnorm;

    if (wi == 0.) {
        alphar[0] = a[0];
        alphar[1] = a[*lda + 1];
        alphai[0] = 0.;
        alphai[1] = 0.;
        beta[0] = b[0];
        beta[1] = b[*ldb + 1];
    }
    else {
        alphar[0] = anorm * wr1 / scale1 / bnorm;
        alphai[0] = anorm * wi / scale1 / bnorm;
        alphar[1] = alphar[0];
        alphai[1] = -alphai[0];
        beta[0] = 1.;
        beta[1] = 1.;
    }
} /* dlagv2_ */
