#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__2 = 2;
static integer c__3 = 3;

/*        SUBROUTINE 'GPFA'                                               */
/*        SELF-SORTING IN-PLACE GENERALIZED PRIME FACTOR (COMPLEX) FFT    */
/*                                                                        */
/*        *** THIS IS THE ALL-FORTRAN VERSION ***                         */
/*            -------------------------------                             */
/*                                                                        */
/*        CALL GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN)                       */
/*                                                                        */
/*        A IS FIRST REAL INPUT/OUTPUT VECTOR                             */
/*        B IS FIRST IMAGINARY INPUT/OUTPUT VECTOR                        */
/*        TRIGS IS A TABLE OF TWIDDLE FACTORS, PRECALCULATED              */
/*              BY CALLING SUBROUTINE 'SETGPFA'                           */
/*        INC IS THE INCREMENT WITHIN EACH DATA VECTOR                    */
/*        JUMP IS THE INCREMENT BETWEEN DATA VECTORS                      */
/*        N IS THE LENGTH OF THE TRANSFORMS:                              */
/*          -----------------------------------                           */
/*            N = (2**IP) * (3**IQ) * (5**IR)                             */
/*          -----------------------------------                           */
/*        LOT IS THE NUMBER OF TRANSFORMS                                 */
/*        ISIGN = +1 FOR FORWARD TRANSFORM                                */
/*              = -1 FOR INVERSE TRANSFORM                                */
/*                                                                        */
/*        WRITTEN BY CLIVE TEMPERTON                                      */
/*        RECHERCHE EN PREVISION NUMERIQUE                                */
/*        ATMOSPHERIC ENVIRONMENT SERVICE, CANADA                         */
/*                                                                        */
/* ---------------------------------------------------------------------- */
/*                                                                        */
/*        DEFINITION OF TRANSFORM                                         */
/*        -----------------------                                         */
/*                                                                        */
/*        X(J) = SUM(K=0,...,N-1)(C(K)*EXP(ISIGN*2*I*J*K*PI/N))           */
/*                                                                        */
/* ---------------------------------------------------------------------  */
/*                                                                        */
/*        FOR A MATHEMATICAL DEVELOPMENT OF THE ALGORITHM USED,           */
/*        SEE:                                                            */
/*                                                                        */
/*        C TEMPERTON : "A GENERALIZED PRIME FACTOR FFT ALGORITHM         */
/*          FOR ANY N = (2**P)(3**Q)(5**R)",                              */
/*          SIAM J. SCI. STAT. COMP., MAY 1992.                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */

/* Subroutine */ void gpfa_(real *a, real *b, const real *trigs, const integer *inc,
                            const integer *jump, const integer *n, const integer *lot,
                            const integer *isign, const integer *npqr, integer *info)
{
    /* Local variables */
    static integer i, ip, iq, ir;

    ip = npqr[0];
    iq = npqr[1];
    ir = npqr[2];

/*     COMPUTE THE TRANSFORM */
/*     --------------------- */
    i = 0;
    if (ip > 0) {
        gpfa2f_(a, b, trigs, inc, jump, n, &ip, lot, isign);
        i += pow_ii(&c__2, &ip) << 1;
    }
    if (iq > 0) {
        gpfa3f_(a, b, &trigs[i], inc, jump, n, &iq, lot, isign);
        i += pow_ii(&c__3, &iq) << 1;
    }
    if (ir > 0) {
        gpfa5f_(a, b, &trigs[i], inc, jump, n, &ir, lot, isign);
    }

    *info = 0;
} /* gpfa_ */
