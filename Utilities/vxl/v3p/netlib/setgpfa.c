#include "f2c.h"
#include "netlib.h"
extern double asin(double), cos(double), sin(double); /* #include <math.h> */

/* Table of constant values */
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__5 = 5;

/*        SUBROUTINE 'SETGPFA' */
/*        SETUP ROUTINE FOR SELF-SORTING IN-PLACE */
/*            GENERALIZED PRIME FACTOR (COMPLEX) FFT [GPFA] */
/*                                                                        */
/*        CALL SETGPFA(TRIGS,N) */
/*                                                                        */
/*        INPUT : */
/*        ----- */
/*        N IS THE LENGTH OF THE TRANSFORMS. N MUST BE OF THE FORM: */
/*          ----------------------------------- */
/*            N = (2**IP) * (3**IQ) * (5**IR) */
/*          ----------------------------------- */
/*                                                                        */
/*        OUTPUT: */
/*        ------ */
/*        TRIGS IS A TABLE OF TWIDDLE FACTORS, */
/*          OF LENGTH 2*IPQR (REAL) WORDS, WHERE: */
/*          -------------------------------------- */
/*            IPQR = (2**IP) + (3**IQ) + (5**IR) */
/*          -------------------------------------- */
/*                                                                        */
/*        WRITTEN BY CLIVE TEMPERTON 1990 */
/*                                                                        */
/* ---------------------------------------------------------------------- */

/* Subroutine */
void setgpfa_(real *trigs, const integer *n, integer *ires, integer *info)
{
    /* Local variables */
    static integer ifac, kink, irot, i, k;
    static real angle, twopi;
    static integer kk, ni, nj[3], ll, nn;
    static real del;

/*     DECOMPOSE N INTO FACTORS 2,3,5 */
/*     ------------------------------ */
    nn = *n;
    ifac = 2;

    for (ll = 0; ll < 3; ++ll) {
        kk = 0;
        while (nn % ifac == 0) {
            ++kk;
            nn /= ifac;
        }
        ires[ll] = kk;
        ifac += ll+1; /* which makes ifac 3 and 5 in the next 2 runs */
    }

    if (nn != 1) {
        *info = -1;
        return;
    }

/*     COMPUTE LIST OF ROTATED TWIDDLE FACTORS */
/*     --------------------------------------- */
    nj[0] = pow_ii(&c__2, ires);
    nj[1] = pow_ii(&c__3, ires+1);
    nj[2] = pow_ii(&c__5, ires+2);

    twopi = (real)asin(1.) * 4.f;
    i = 0;

    for (ll = 0; ll < 3; ++ll) {
        ni = nj[ll];
        if (ni == 1) {
            continue; /* next ll */
        }
        del = twopi / (real) ni;
        irot = *n / ni;
        kink = irot % ni;
        kk = 0;

        for (k = 1; k <= ni; ++k) {
            angle = (real) kk * del;
            trigs[i] = (float)cos(angle);
            trigs[i+1] = (float)sin(angle);

            i += 2;
            kk += kink;
            if (kk > ni) {
                kk -= ni;
            }
        }
    }
    *info = 0;
} /* setgpfa_ */
