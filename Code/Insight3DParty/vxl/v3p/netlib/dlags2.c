/* dlags2.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/*<    >*/
/* Subroutine */ int dlags2_(logical *upper, doublereal *a1, doublereal *a2, 
	doublereal *a3, doublereal *b1, doublereal *b2, doublereal *b3, 
	doublereal *csu, doublereal *snu, doublereal *csv, doublereal *snv, 
	doublereal *csq, doublereal *snq)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal aua11, aua12, aua21, aua22, avb12, avb11, avb21, avb22, 
	    ua11r, ua22r, vb11r, vb22r, a, b, c, d, r, s1, s2;
    extern /* Subroutine */ int dlasv2_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), dlartg_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal ua11, ua12, ua21, ua22, vb11, vb12, vb21, vb22, csl, 
	    csr, snl, snr;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            UPPER >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such */
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

/*  A1      (input) DOUBLE PRECISION */
/*  A2      (input) DOUBLE PRECISION */
/*  A3      (input) DOUBLE PRECISION */
/*          On entry, A1, A2 and A3 are elements of the input 2-by-2 */
/*          upper (lower) triangular matrix A. */

/*  B1      (input) DOUBLE PRECISION */
/*  B2      (input) DOUBLE PRECISION */
/*  B3      (input) DOUBLE PRECISION */
/*          On entry, B1, B2 and B3 are elements of the input 2-by-2 */
/*          upper (lower) triangular matrix B. */

/*  CSU     (output) DOUBLE PRECISION */
/*  SNU     (output) DOUBLE PRECISION */
/*          The desired orthogonal matrix U. */

/*  CSV     (output) DOUBLE PRECISION */
/*  SNV     (output) DOUBLE PRECISION */
/*          The desired orthogonal matrix V. */

/*  CSQ     (output) DOUBLE PRECISION */
/*  SNQ     (output) DOUBLE PRECISION */
/*          The desired orthogonal matrix Q. */

/*  ===================================================================== 
*/

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<    >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLARTG, DLASV2 >*/
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
	d = *a3 * *b1;
/*<          B = A2*B1 - A1*B2 >*/
	b = *a2 * *b1 - *a1 * *b2;

/*        The SVD of real 2-by-2 triangular C */

/*         ( CSL -SNL )*( A B )*(  CSR  SNR ) = ( R 0 ) */
/*         ( SNL  CSL ) ( 0 D ) ( -SNR  CSR )   ( 0 T ) */

/*<          CALL DLASV2( A, B, D, S1, S2, SNR, CSR, SNL, CSL ) >*/
	dlasv2_(&a, &b, &d, &s1, &s2, &snr, &csr, &snl, &csl);

/*<    >*/
	if (abs(csl) >= abs(snl) || abs(csr) >= abs(snr)) {

/*           Compute the (1,1) and (1,2) elements of U'*A and V'*B
, */
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
	    aua12 = abs(csl) * abs(*a2) + abs(snl) * abs(*a3);
/*<             AVB12 = ABS( CSR )*ABS( B2 ) + ABS( SNR )*ABS( B3 ) >*/
	    avb12 = abs(csr) * abs(*b2) + abs(snr) * abs(*b3);

/*           zero (1,2) elements of U'*A and V'*B */

/*<             IF( ( ABS( UA11R )+ABS( UA12 ) ).NE.ZERO ) THEN >*/
	    if (abs(ua11r) + abs(ua12) != 0.) {
/*<    >*/
		if (aua12 / (abs(ua11r) + abs(ua12)) <= avb12 / (abs(vb11r) + 
			abs(vb12))) {
/*<                   CALL DLARTG( -UA11R, UA12, CSQ, SNQ, R ) >*/
		    d__1 = -ua11r;
		    dlartg_(&d__1, &ua12, csq, snq, &r);
/*<                ELSE >*/
		} else {
/*<                   CALL DLARTG( -VB11R, VB12, CSQ, SNQ, R ) >*/
		    d__1 = -vb11r;
		    dlartg_(&d__1, &vb12, csq, snq, &r);
/*<                END IF >*/
		}
/*<             ELSE >*/
	    } else {
/*<                CALL DLARTG( -VB11R, VB12, CSQ, SNQ, R ) >*/
		d__1 = -vb11r;
		dlartg_(&d__1, &vb12, csq, snq, &r);
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

/*           Compute the (2,1) and (2,2) elements of U'*A and V'*B
, */
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
	    aua22 = abs(snl) * abs(*a2) + abs(csl) * abs(*a3);
/*<             AVB22 = ABS( SNR )*ABS( B2 ) + ABS( CSR )*ABS( B3 ) >*/
	    avb22 = abs(snr) * abs(*b2) + abs(csr) * abs(*b3);

/*           zero (2,2) elements of U'*A and V'*B, and then swap. 
*/

/*<             IF( ( ABS( UA21 )+ABS( UA22 ) ).NE.ZERO ) THEN >*/
	    if (abs(ua21) + abs(ua22) != 0.) {
/*<    >*/
		if (aua22 / (abs(ua21) + abs(ua22)) <= avb22 / (abs(vb21) + 
			abs(vb22))) {
/*<                   CALL DLARTG( -UA21, UA22, CSQ, SNQ, R ) >*/
		    d__1 = -ua21;
		    dlartg_(&d__1, &ua22, csq, snq, &r);
/*<                ELSE >*/
		} else {
/*<                   CALL DLARTG( -VB21, VB22, CSQ, SNQ, R ) >*/
		    d__1 = -vb21;
		    dlartg_(&d__1, &vb22, csq, snq, &r);
/*<                END IF >*/
		}
/*<             ELSE >*/
	    } else {
/*<                CALL DLARTG( -VB21, VB22, CSQ, SNQ, R ) >*/
		d__1 = -vb21;
		dlartg_(&d__1, &vb22, csq, snq, &r);
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
	d = *a3 * *b1;
/*<          C = A2*B3 - A3*B2 >*/
	c = *a2 * *b3 - *a3 * *b2;

/*        The SVD of real 2-by-2 triangular C */

/*         ( CSL -SNL )*( A 0 )*(  CSR  SNR ) = ( R 0 ) */
/*         ( SNL  CSL ) ( C D ) ( -SNR  CSR )   ( 0 T ) */

/*<          CALL DLASV2( A, C, D, S1, S2, SNR, CSR, SNL, CSL ) >*/
	dlasv2_(&a, &c, &d, &s1, &s2, &snr, &csr, &snl, &csl);

/*<    >*/
	if (abs(csr) >= abs(snr) || abs(csl) >= abs(snl)) {

/*           Compute the (2,1) and (2,2) elements of U'*A and V'*B
, */
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
	    aua21 = abs(snr) * abs(*a1) + abs(csr) * abs(*a2);
/*<             AVB21 = ABS( SNL )*ABS( B1 ) + ABS( CSL )*ABS( B2 ) >*/
	    avb21 = abs(snl) * abs(*b1) + abs(csl) * abs(*b2);

/*           zero (2,1) elements of U'*A and V'*B. */

/*<             IF( ( ABS( UA21 )+ABS( UA22R ) ).NE.ZERO ) THEN >*/
	    if (abs(ua21) + abs(ua22r) != 0.) {
/*<    >*/
		if (aua21 / (abs(ua21) + abs(ua22r)) <= avb21 / (abs(vb21) + 
			abs(vb22r))) {
/*<                   CALL DLARTG( UA22R, UA21, CSQ, SNQ, R ) >*/
		    dlartg_(&ua22r, &ua21, csq, snq, &r);
/*<                ELSE >*/
		} else {
/*<                   CALL DLARTG( VB22R, VB21, CSQ, SNQ, R ) >*/
		    dlartg_(&vb22r, &vb21, csq, snq, &r);
/*<                END IF >*/
		}
/*<             ELSE >*/
	    } else {
/*<                CALL DLARTG( VB22R, VB21, CSQ, SNQ, R ) >*/
		dlartg_(&vb22r, &vb21, csq, snq, &r);
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

/*           Compute the (1,1) and (1,2) elements of U'*A and V'*B
, */
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
	    aua11 = abs(csr) * abs(*a1) + abs(snr) * abs(*a2);
/*<             AVB11 = ABS( CSL )*ABS( B1 ) + ABS( SNL )*ABS( B2 ) >*/
	    avb11 = abs(csl) * abs(*b1) + abs(snl) * abs(*b2);

/*           zero (1,1) elements of U'*A and V'*B, and then swap. 
*/

/*<             IF( ( ABS( UA11 )+ABS( UA12 ) ).NE.ZERO ) THEN >*/
	    if (abs(ua11) + abs(ua12) != 0.) {
/*<    >*/
		if (aua11 / (abs(ua11) + abs(ua12)) <= avb11 / (abs(vb11) + 
			abs(vb12))) {
/*<                   CALL DLARTG( UA12, UA11, CSQ, SNQ, R ) >*/
		    dlartg_(&ua12, &ua11, csq, snq, &r);
/*<                ELSE >*/
		} else {
/*<                   CALL DLARTG( VB12, VB11, CSQ, SNQ, R ) >*/
		    dlartg_(&vb12, &vb11, csq, snq, &r);
/*<                END IF >*/
		}
/*<             ELSE >*/
	    } else {
/*<                CALL DLARTG( VB12, VB11, CSQ, SNQ, R ) >*/
		dlartg_(&vb12, &vb11, csq, snq, &r);
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

/*     End of DLAGS2 */

/*<       END >*/
} /* dlags2_ */

