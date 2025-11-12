/* lapack/single/slags2.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int slags2_(logical *upper, real *a1, real *a2, real *a3,
        real *b1, real *b2, real *b3, real *csu, real *snu, real *csv, real *
        snv, real *csq, real *snq)
{
    /* System generated locals */
    real r__1;

    /* Local variables */
    real a, b, c__, d__, r__, s1, s2, ua11, ua12, ua21, ua22, vb11, vb12,
            vb21, vb22, csl, csr, snl, snr, aua11, aua12, aua21, aua22, avb11,
             avb12, avb21, avb22, ua11r, ua22r, vb11r, vb22r;
    extern /* Subroutine */ int slasv2_(real *, real *, real *, real *, real *
            , real *, real *, real *, real *), slartg_(real *, real *, real *,
             real *, real *);


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            UPPER >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such */
/*  that if ( UPPER ) then */

/*            U'*A*Q = U'*( A1 A2 )*Q = ( x  0  ) */
/*                        ( 0  A3 )     ( x  x  ) */
/*  and */
/*            V'*B*Q = V'*( B1 B2 )*Q = ( x  0  ) */
/*                        ( 0  B3 )     ( x  x  ) */

/*  or if ( .NOT.UPPER ) then */

/*            U'*A*Q = U'*( A1 0  )*Q = ( x  x  ) */
/*                        ( A2 A3 )     ( 0  x  ) */
/*  and */
/*            V'*B*Q = V'*( B1 0  )*Q = ( x  x  ) */
/*                        ( B2 B3 )     ( 0  x  ) */

/*  The rows of the transformed A and B are parallel, where */

/*    U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ ) */
/*        ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ ) */

/*  Z' denotes the transpose of Z. */


/*  Arguments */
/*  ========= */

/*  UPPER   (input) LOGICAL */
/*          = .TRUE.: the input matrices A and B are upper triangular. */
/*          = .FALSE.: the input matrices A and B are lower triangular. */

/*  A1      (input) REAL */
/*  A2      (input) REAL */
/*  A3      (input) REAL */
/*          On entry, A1, A2 and A3 are elements of the input 2-by-2 */
/*          upper (lower) triangular matrix A. */

/*  B1      (input) REAL */
/*  B2      (input) REAL */
/*  B3      (input) REAL */
/*          On entry, B1, B2 and B3 are elements of the input 2-by-2 */
/*          upper (lower) triangular matrix B. */

/*  CSU     (output) REAL */
/*  SNU     (output) REAL */
/*          The desired orthogonal matrix U. */

/*  CSV     (output) REAL */
/*  SNV     (output) REAL */
/*          The desired orthogonal matrix V. */

/*  CSQ     (output) REAL */
/*  SNQ     (output) REAL */
/*          The desired orthogonal matrix Q. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<    >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SLARTG, SLASV2 >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( UPPER ) THEN >*/
    if (*upper) {

/*        Input matrices A and B are upper triangular matrices */

/*        Form matrix C = A*adj(B) = ( a b ) */
/*                                   ( 0 d ) */

/*<          A = A1*B3 >*/
        a = *a1 * *b3;
/*<          D = A3*B1 >*/
        d__ = *a3 * *b1;
/*<          B = A2*B1 - A1*B2 >*/
        b = *a2 * *b1 - *a1 * *b2;

/*        The SVD of real 2-by-2 triangular C */

/*         ( CSL -SNL )*( A B )*(  CSR  SNR ) = ( R 0 ) */
/*         ( SNL  CSL ) ( 0 D ) ( -SNR  CSR )   ( 0 T ) */

/*<          CALL SLASV2( A, B, D, S1, S2, SNR, CSR, SNL, CSL ) >*/
        slasv2_(&a, &b, &d__, &s1, &s2, &snr, &csr, &snl, &csl);

/*<    >*/
        if (dabs(csl) >= dabs(snl) || dabs(csr) >= dabs(snr)) {

/*           Compute the (1,1) and (1,2) elements of U'*A and V'*B, */
/*           and (1,2) element of |U|'*|A| and |V|'*|B|. */

/*<             UA11R = CSL*A1 >*/
            ua11r = csl * *a1;
/*<             UA12 = CSL*A2 + SNL*A3 >*/
            ua12 = csl * *a2 + snl * *a3;

/*<             VB11R = CSR*B1 >*/
            vb11r = csr * *b1;
/*<             VB12 = CSR*B2 + SNR*B3 >*/
            vb12 = csr * *b2 + snr * *b3;

/*<             AUA12 = ABS( CSL )*ABS( A2 ) + ABS( SNL )*ABS( A3 ) >*/
            aua12 = dabs(csl) * dabs(*a2) + dabs(snl) * dabs(*a3);
/*<             AVB12 = ABS( CSR )*ABS( B2 ) + ABS( SNR )*ABS( B3 ) >*/
            avb12 = dabs(csr) * dabs(*b2) + dabs(snr) * dabs(*b3);

/*           zero (1,2) elements of U'*A and V'*B */

/*<             IF( ( ABS( UA11R )+ABS( UA12 ) ).NE.ZERO ) THEN >*/
            if (dabs(ua11r) + dabs(ua12) != (float)0.) {
/*<    >*/
                if (aua12 / (dabs(ua11r) + dabs(ua12)) <= avb12 / (dabs(vb11r)
                         + dabs(vb12))) {
/*<                   CALL SLARTG( -UA11R, UA12, CSQ, SNQ, R ) >*/
                    r__1 = -ua11r;
                    slartg_(&r__1, &ua12, csq, snq, &r__);
/*<                ELSE >*/
                } else {
/*<                   CALL SLARTG( -VB11R, VB12, CSQ, SNQ, R ) >*/
                    r__1 = -vb11r;
                    slartg_(&r__1, &vb12, csq, snq, &r__);
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                CALL SLARTG( -VB11R, VB12, CSQ, SNQ, R ) >*/
                r__1 = -vb11r;
                slartg_(&r__1, &vb12, csq, snq, &r__);
/*<             END IF >*/
            }

/*<             CSU = CSL >*/
            *csu = csl;
/*<             SNU = -SNL >*/
            *snu = -snl;
/*<             CSV = CSR >*/
            *csv = csr;
/*<             SNV = -SNR >*/
            *snv = -snr;

/*<          ELSE >*/
        } else {

/*           Compute the (2,1) and (2,2) elements of U'*A and V'*B, */
/*           and (2,2) element of |U|'*|A| and |V|'*|B|. */

/*<             UA21 = -SNL*A1 >*/
            ua21 = -snl * *a1;
/*<             UA22 = -SNL*A2 + CSL*A3 >*/
            ua22 = -snl * *a2 + csl * *a3;

/*<             VB21 = -SNR*B1 >*/
            vb21 = -snr * *b1;
/*<             VB22 = -SNR*B2 + CSR*B3 >*/
            vb22 = -snr * *b2 + csr * *b3;

/*<             AUA22 = ABS( SNL )*ABS( A2 ) + ABS( CSL )*ABS( A3 ) >*/
            aua22 = dabs(snl) * dabs(*a2) + dabs(csl) * dabs(*a3);
/*<             AVB22 = ABS( SNR )*ABS( B2 ) + ABS( CSR )*ABS( B3 ) >*/
            avb22 = dabs(snr) * dabs(*b2) + dabs(csr) * dabs(*b3);

/*           zero (2,2) elements of U'*A and V'*B, and then swap. */

/*<             IF( ( ABS( UA21 )+ABS( UA22 ) ).NE.ZERO ) THEN >*/
            if (dabs(ua21) + dabs(ua22) != (float)0.) {
/*<    >*/
                if (aua22 / (dabs(ua21) + dabs(ua22)) <= avb22 / (dabs(vb21)
                        + dabs(vb22))) {
/*<                   CALL SLARTG( -UA21, UA22, CSQ, SNQ, R ) >*/
                    r__1 = -ua21;
                    slartg_(&r__1, &ua22, csq, snq, &r__);
/*<                ELSE >*/
                } else {
/*<                   CALL SLARTG( -VB21, VB22, CSQ, SNQ, R ) >*/
                    r__1 = -vb21;
                    slartg_(&r__1, &vb22, csq, snq, &r__);
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                CALL SLARTG( -VB21, VB22, CSQ, SNQ, R ) >*/
                r__1 = -vb21;
                slartg_(&r__1, &vb22, csq, snq, &r__);
/*<             END IF >*/
            }

/*<             CSU = SNL >*/
            *csu = snl;
/*<             SNU = CSL >*/
            *snu = csl;
/*<             CSV = SNR >*/
            *csv = snr;
/*<             SNV = CSR >*/
            *snv = csr;

/*<          END IF >*/
        }

/*<       ELSE >*/
    } else {

/*        Input matrices A and B are lower triangular matrices */

/*        Form matrix C = A*adj(B) = ( a 0 ) */
/*                                   ( c d ) */

/*<          A = A1*B3 >*/
        a = *a1 * *b3;
/*<          D = A3*B1 >*/
        d__ = *a3 * *b1;
/*<          C = A2*B3 - A3*B2 >*/
        c__ = *a2 * *b3 - *a3 * *b2;

/*        The SVD of real 2-by-2 triangular C */

/*         ( CSL -SNL )*( A 0 )*(  CSR  SNR ) = ( R 0 ) */
/*         ( SNL  CSL ) ( C D ) ( -SNR  CSR )   ( 0 T ) */

/*<          CALL SLASV2( A, C, D, S1, S2, SNR, CSR, SNL, CSL ) >*/
        slasv2_(&a, &c__, &d__, &s1, &s2, &snr, &csr, &snl, &csl);

/*<    >*/
        if (dabs(csr) >= dabs(snr) || dabs(csl) >= dabs(snl)) {

/*           Compute the (2,1) and (2,2) elements of U'*A and V'*B, */
/*           and (2,1) element of |U|'*|A| and |V|'*|B|. */

/*<             UA21 = -SNR*A1 + CSR*A2 >*/
            ua21 = -snr * *a1 + csr * *a2;
/*<             UA22R = CSR*A3 >*/
            ua22r = csr * *a3;

/*<             VB21 = -SNL*B1 + CSL*B2 >*/
            vb21 = -snl * *b1 + csl * *b2;
/*<             VB22R = CSL*B3 >*/
            vb22r = csl * *b3;

/*<             AUA21 = ABS( SNR )*ABS( A1 ) + ABS( CSR )*ABS( A2 ) >*/
            aua21 = dabs(snr) * dabs(*a1) + dabs(csr) * dabs(*a2);
/*<             AVB21 = ABS( SNL )*ABS( B1 ) + ABS( CSL )*ABS( B2 ) >*/
            avb21 = dabs(snl) * dabs(*b1) + dabs(csl) * dabs(*b2);

/*           zero (2,1) elements of U'*A and V'*B. */

/*<             IF( ( ABS( UA21 )+ABS( UA22R ) ).NE.ZERO ) THEN >*/
            if (dabs(ua21) + dabs(ua22r) != (float)0.) {
/*<    >*/
                if (aua21 / (dabs(ua21) + dabs(ua22r)) <= avb21 / (dabs(vb21)
                        + dabs(vb22r))) {
/*<                   CALL SLARTG( UA22R, UA21, CSQ, SNQ, R ) >*/
                    slartg_(&ua22r, &ua21, csq, snq, &r__);
/*<                ELSE >*/
                } else {
/*<                   CALL SLARTG( VB22R, VB21, CSQ, SNQ, R ) >*/
                    slartg_(&vb22r, &vb21, csq, snq, &r__);
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                CALL SLARTG( VB22R, VB21, CSQ, SNQ, R ) >*/
                slartg_(&vb22r, &vb21, csq, snq, &r__);
/*<             END IF >*/
            }

/*<             CSU = CSR >*/
            *csu = csr;
/*<             SNU = -SNR >*/
            *snu = -snr;
/*<             CSV = CSL >*/
            *csv = csl;
/*<             SNV = -SNL >*/
            *snv = -snl;

/*<          ELSE >*/
        } else {

/*           Compute the (1,1) and (1,2) elements of U'*A and V'*B, */
/*           and (1,1) element of |U|'*|A| and |V|'*|B|. */

/*<             UA11 = CSR*A1 + SNR*A2 >*/
            ua11 = csr * *a1 + snr * *a2;
/*<             UA12 = SNR*A3 >*/
            ua12 = snr * *a3;

/*<             VB11 = CSL*B1 + SNL*B2 >*/
            vb11 = csl * *b1 + snl * *b2;
/*<             VB12 = SNL*B3 >*/
            vb12 = snl * *b3;

/*<             AUA11 = ABS( CSR )*ABS( A1 ) + ABS( SNR )*ABS( A2 ) >*/
            aua11 = dabs(csr) * dabs(*a1) + dabs(snr) * dabs(*a2);
/*<             AVB11 = ABS( CSL )*ABS( B1 ) + ABS( SNL )*ABS( B2 ) >*/
            avb11 = dabs(csl) * dabs(*b1) + dabs(snl) * dabs(*b2);

/*           zero (1,1) elements of U'*A and V'*B, and then swap. */

/*<             IF( ( ABS( UA11 )+ABS( UA12 ) ).NE.ZERO ) THEN >*/
            if (dabs(ua11) + dabs(ua12) != (float)0.) {
/*<    >*/
                if (aua11 / (dabs(ua11) + dabs(ua12)) <= avb11 / (dabs(vb11)
                        + dabs(vb12))) {
/*<                   CALL SLARTG( UA12, UA11, CSQ, SNQ, R ) >*/
                    slartg_(&ua12, &ua11, csq, snq, &r__);
/*<                ELSE >*/
                } else {
/*<                   CALL SLARTG( VB12, VB11, CSQ, SNQ, R ) >*/
                    slartg_(&vb12, &vb11, csq, snq, &r__);
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                CALL SLARTG( VB12, VB11, CSQ, SNQ, R ) >*/
                slartg_(&vb12, &vb11, csq, snq, &r__);
/*<             END IF >*/
            }

/*<             CSU = SNR >*/
            *csu = snr;
/*<             SNU = CSR >*/
            *snu = csr;
/*<             CSV = SNL >*/
            *csv = snl;
/*<             SNV = CSL >*/
            *snv = csl;

/*<          END IF >*/
        }

/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of SLAGS2 */

/*<       END >*/
} /* slags2_ */

#ifdef __cplusplus
        }
#endif
