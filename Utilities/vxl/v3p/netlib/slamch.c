#include "f2c.h"
#include "netlib.h"
#include <stdio.h>

static void slamc1_(integer *beta, integer *t, logical *rnd, logical *ieee1);
static void slamc2_(integer *beta, integer *t, logical *rnd, real *eps,
                    integer *emin, real *rmin, integer *emax, real *rmax);
static real slamc3_(real *a, real *b);
static void slamc4_(integer *emin, real *start, integer *base);
static void slamc5_(integer *beta, integer *p, integer *emin, logical *ieee, integer *emax, real *rmax);

real slamch_(const char *cmach)
{
/*  -- LAPACK auxiliary routine (version 2.0) --
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
       Courant Institute, Argonne National Lab, and Rice University
       October 31, 1992

    Purpose
    =======

    SLAMCH determines single precision machine parameters.

    Arguments
    =========

    CMACH   (input) CHARACTER*1
            Specifies the value to be returned by SLAMCH:
            = 'E' or 'e',   SLAMCH := eps
            = 'S' or 's ,   SLAMCH := sfmin
            = 'B' or 'b',   SLAMCH := base
            = 'P' or 'p',   SLAMCH := eps*base
            = 'N' or 'n',   SLAMCH := t
            = 'R' or 'r',   SLAMCH := rnd
            = 'M' or 'm',   SLAMCH := emin
            = 'U' or 'u',   SLAMCH := rmin
            = 'L' or 'l',   SLAMCH := emax
            = 'O' or 'o',   SLAMCH := rmax

            where

            eps   = relative machine precision
            sfmin = safe minimum, such that 1/sfmin does not overflow
            base  = base of the machine
            prec  = eps*base
            t     = number of (base) digits in the mantissa
            rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
            emin  = minimum exponent before (gradual) underflow
            rmin  = underflow threshold - base**(emin-1)
            emax  = largest exponent before overflow
            rmax  = overflow threshold  - (base**emax)*(1-eps)

   =====================================================================
*/

    /* Initialized data */
    static logical first = TRUE_;
    /* System generated locals */
    integer i__1;
    /* Local variables */
    static real base;
    static integer beta;
    static real emin, prec, emax;
    static integer imin, imax;
    static logical lrnd;
    static real rmin, rmax, t;
    static real small, sfmin;
    static integer it;
    static real rnd, eps;

    if (first) {
        first = FALSE_;
        slamc2_(&beta, &it, &lrnd, &eps, &imin, &rmin, &imax, &rmax);
        base = (real) beta;
        t = (real) it;
        if (lrnd) {
            rnd = 1.f;
            i__1 = 1 - it;
            eps = pow_ri(&base, &i__1) / 2;
        } else {
            rnd = 0.f;
            i__1 = 1 - it;
            eps = pow_ri(&base, &i__1);
        }
        prec = eps * base;
        emin = (real) imin;
        emax = (real) imax;
        sfmin = rmin;
        small = 1.f / rmax;
        if (small >= sfmin) {
            /* Use SMALL plus a bit, to avoid the possibility of rounding */
            /* causing overflow when computing  1/sfmin. */
            sfmin = small * (eps + 1.f);
        }
    }

    if      (*cmach=='E' || *cmach=='e') return eps;   /* 1.19209e-7f */
    else if (*cmach=='S' || *cmach=='s') return sfmin; /* 1.17549e-38f */
    else if (*cmach=='B' || *cmach=='b') return base;  /* 2.f */
    else if (*cmach=='P' || *cmach=='p') return prec;  /* 2.38419e-7.f */
    else if (*cmach=='N' || *cmach=='n') return t;     /* 24.f */
    else if (*cmach=='R' || *cmach=='r') return rnd;   /* 0.f */
    else if (*cmach=='M' || *cmach=='m') return emin;  /* -125.f */
    else if (*cmach=='U' || *cmach=='u') return rmin;  /* 1.17549e-38f */
    else if (*cmach=='L' || *cmach=='l') return emax;  /* 128.f */
    else if (*cmach=='O' || *cmach=='o') return rmax;  /* 3.40282e38f */
    else return 0.f; /* in case a non-documented argument was passed */
} /* slamch_ */

/* Subroutine */ void slamc1_(integer *beta, integer *t, logical *rnd, logical *ieee1)
{
/*  -- LAPACK auxiliary routine (version 2.0) --
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
       Courant Institute, Argonne National Lab, and Rice University
       October 31, 1992

    Purpose
    =======

    SLAMC1 determines the machine parameters given by BETA, T, RND, and
    IEEE1.

    Arguments
    =========

    BETA    (output) INTEGER
            The base of the machine.

    T       (output) INTEGER
            The number of ( BETA ) digits in the mantissa.

    RND     (output) LOGICAL
            Specifies whether proper rounding  ( RND = .TRUE. )  or
            chopping  ( RND = .FALSE. )  occurs in addition. This may not

            be a reliable guide to the way in which the machine performs
            its arithmetic.

    IEEE1   (output) LOGICAL
            Specifies whether rounding appears to be done in the IEEE
            'round to nearest' style.

    Further Details
    ===============

    The routine is based on the routine  ENVRON  by Malcolm and
    incorporates suggestions by Gentleman and Marovich. See

       Malcolm M. A. (1972) Algorithms to reveal properties of
          floating-point arithmetic. Comms. of the ACM, 15, 949-951.

       Gentleman W. M. and Marovich S. B. (1974) More on algorithms
          that reveal properties of floating point arithmetic units.
          Comms. of the ACM, 17, 276-277.

   =====================================================================
*/

    /* Initialized data */
    static logical first = TRUE_;
    /* System generated locals */
    real r__1, r__2;
    /* Local variables */
    static logical lrnd;
    static real a, b, c, f;
    static integer lbeta;
    static real savec;
    static logical lieee1;
    static real t1, t2;
    static integer lt;
    static real one = 1.f;

    if (first) {
        first = FALSE_;

        /* LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA, IEEE1, T and RND. */

        /* Throughout this routine  we use the function  SLAMC3  to ensure */
        /* that relevant values are  stored and not held in registers, or */
        /* are not affected by optimizers. */

        /* Compute  a = 2.0**m  with the  smallest positive integer m such that */
        /*      fl( a + 1.0 ) = a. */

        a = c = one;

        while (c == one) {
            a *= 2;
            c = slamc3_(&a, &one);
            r__1 = -a;
            c = slamc3_(&c, &r__1);
        }

        /* Now compute  b = 2.0**m  with the smallest positive integer m such that */
        /*  fl( a + b ) .gt. a. */

        b = one;
        c = slamc3_(&a, &b);

        /* The next two lines of code were replaced by Ian Scott from the original line
                     while (c==a) {
          During a optimised build under MSVC, the compiler was using the value of
          C still in a register in while loop test. This is an 80-bit value rather than
          the 64 bit value it uses after saving and loading from memory.
          So the 80 bit precision value was having 1 added, making it a different number
          and so not executing the loop.
          The call to slamc3_ in the loop condition forces the value to 64-bit precision
          as during the previous calculation.
        */
        r__1 = -a;
        while (slamc3_(&c, &r__1) == 0.f) {
            b *= 2;
            c = slamc3_(&a, &b);
        }

        /* Now compute the base.  a and c  are neighbouring floating point */
        /* numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so */
        /* their difference is beta. Adding 0.25 to c is to ensure that it */
        /* is truncated to beta and not ( beta - 1 ). */

        savec = c;
        r__1 = -a;
        c = slamc3_(&c, &r__1);
        lbeta = (integer)(c + 0.25f);

        /* Now determine whether rounding or chopping occurs,  by adding a */
        /* bit  less  than  beta/2  and a  bit  more  than  beta/2  to a. */

        b = (real) lbeta;
        r__1 = b / 2;
        r__2 = -b / 100;
        f = slamc3_(&r__1, &r__2);
        c = slamc3_(&f, &a);
        if (c == a) {
            lrnd = TRUE_;
        } else {
            lrnd = FALSE_;
        }
        r__1 = b / 2;
        r__2 = b / 100;
        f = slamc3_(&r__1, &r__2);
        c = slamc3_(&f, &a);
        if (lrnd && c == a) {
            lrnd = FALSE_;
        }

        /* Try and decide whether rounding is done in the  IEEE  'round to */
        /* nearest' style. B/2 is half a unit in the last place of the two */
        /* numbers A and SAVEC. Furthermore, A is even, i.e. has last bit */
        /* zero, and SAVEC is odd. Thus adding B/2 to A should not  change */
        /* A, but adding B/2 to SAVEC should change SAVEC. */

        r__1 = b / 2;
        t1 = slamc3_(&r__1, &a);
        r__1 = b / 2;
        t2 = slamc3_(&r__1, &savec);
        lieee1 = t1 == a && t2 > savec && lrnd;

        /* Now find  the  mantissa, t.  It should  be the  integer part of */
        /* log to the base beta of a,  however it is safer to determine t */
        /* by powering.  So we find t as the smallest positive integer for */
        /* which */
        /*    fl( beta**t + 1.0 ) = 1.0. */

        lt = 0;
        a = c = one;

        while (c == one) {
            ++lt;
            a *= lbeta;
            c = slamc3_(&a, &one);
            r__1 = -a;
            c = slamc3_(&c, &r__1);
        }
    }

    *beta = lbeta;
    *t = lt;
    *rnd = lrnd;
    *ieee1 = lieee1;
} /* slamc1_ */

/* Subroutine */ void slamc2_(integer *beta, integer *t, logical *rnd,
                              real *eps, integer *emin, real *rmin,
                              integer *emax, real *rmax)
{
/*  -- LAPACK auxiliary routine (version 2.0) --
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
       Courant Institute, Argonne National Lab, and Rice University
       October 31, 1992


    Purpose
    =======

    SLAMC2 determines the machine parameters specified in its argument
    list.

    Arguments
    =========

    BETA    (output) INTEGER
            The base of the machine.

    T       (output) INTEGER
            The number of ( BETA ) digits in the mantissa.

    RND     (output) LOGICAL
            Specifies whether proper rounding  ( RND = .TRUE. )  or
            chopping  ( RND = .FALSE. )  occurs in addition. This may not

            be a reliable guide to the way in which the machine performs

            its arithmetic.

    EPS     (output) REAL
            The smallest positive number such that

               fl( 1.0 - EPS ) .LT. 1.0,

            where fl denotes the computed value.

    EMIN    (output) INTEGER
            The minimum exponent before (gradual) underflow occurs.

    RMIN    (output) REAL
            The smallest normalized number for the machine, given by
            BASE**( EMIN - 1 ), where  BASE  is the floating point value

            of BETA.

    EMAX    (output) INTEGER
            The maximum exponent before overflow occurs.

    RMAX    (output) REAL
            The largest positive number for the machine, given by
            BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point

            value of BETA.

    Further Details
    ===============

    The computation of  EPS  is based on a routine PARANOIA by
    W. Kahan of the University of California at Berkeley.

   =====================================================================
*/

    /* Initialized data */
    static logical first = TRUE_;
    static logical iwarn = FALSE_;
    /* System generated locals */
    integer i__1;
    real r__1, r__2;
    /* Local variables */
    static logical ieee;
    static real half = 0.5f;
    static logical lrnd;
    static real leps, zero = 0.f, a, b, c;
    static integer i, lbeta;
    static real rbase;
    static integer lemin, lemax, gnmin;
    static real small;
    static integer gpmin;
    static real third, lrmin, lrmax, sixth;
    static logical lieee1;
    static integer lt, ngnmin, ngpmin;
    static real one = 1.f;

    if (first) {
        first = FALSE_;

/*        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
          BETA, T, RND, EPS, EMIN and RMIN.

          Throughout this routine  we use the function  SLAMC3  to ensure
          that relevant values are stored  and not held in registers, or
          are not affected by optimizers.

          SLAMC1 returns the parameters  LBETA, LT, LRND and LIEEE1.
*/

        slamc1_(&lbeta, &lt, &lrnd, &lieee1);

        /* Start to find EPS. */

        b = (real) lbeta;
        i__1 = -lt;
        a = pow_ri(&b, &i__1);
        leps = a;

        /* Try some tricks to see whether or not this is the correct  EPS. */

        b = 2.f / 3;
        r__1 = -half;
        sixth = slamc3_(&b, &r__1);
        third = slamc3_(&sixth, &sixth);
        b = slamc3_(&third, &r__1);
        b = slamc3_(&b, &sixth);
        b = abs(b);
        if (b < leps) {
            b = leps;
        }

        leps = one;

        while (leps > b && b > zero) {
            leps = b;
            r__1 = half * leps;
            r__2 = 32.0f * leps * leps;
            c = slamc3_(&r__1, &r__2);
            r__1 = -c;
            c = slamc3_(&half, &r__1);
            b = slamc3_(&half, &c);
            r__1 = -b;
            c = slamc3_(&half, &r__1);
            b = slamc3_(&half, &c);
        }

        if (a < leps) {
            leps = a;
        }

/*        Computation of EPS complete.

          Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
          Keep dividing  A by BETA until (gradual) underflow occurs. This
          is detected when we cannot recover the previous A.
*/

        rbase = one / lbeta;
        small = one;
        for (i = 1; i <= 3; ++i) {
            r__1 = small * rbase;
            small = slamc3_(&r__1, &zero);
        }
        a = slamc3_(&one, &small);
        slamc4_(&ngpmin, &one, &lbeta);
        r__1 = -one;
        slamc4_(&ngnmin, &r__1, &lbeta);
        slamc4_(&gpmin, &a, &lbeta);
        r__1 = -a;
        slamc4_(&gnmin, &r__1, &lbeta);
        ieee = FALSE_;

        if (ngpmin == ngnmin && gpmin == gnmin) {
            if (ngpmin == gpmin) {
                lemin = ngpmin;
                /* ( Non twos-complement machines, no gradual underflow; e.g.,  VAX ) */
            } else if (gpmin - ngpmin == 3) {
                lemin = ngpmin - 1 + lt;
                ieee = TRUE_;
                /* ( Non twos-complement machines, with gradual underflow; e.g., IEEE standard followers ) */
            } else {
                lemin = min(ngpmin,gpmin);
                /* ( A guess; no known machine ) */
                iwarn = TRUE_;
            }
        } else if (ngpmin == gpmin && ngnmin == gnmin) {
            if (abs(ngpmin - ngnmin) == 1) {
                lemin = max(ngpmin,ngnmin);
                /* ( Twos-complement machines, no gradual underflow; e.g., CYBER 205 ) */
            } else {
                lemin = min(ngpmin,ngnmin);
                /* ( A guess; no known machine ) */
                iwarn = TRUE_;
            }
        } else if (abs(ngpmin - ngnmin) == 1 && gpmin == gnmin) {
            if (gpmin - min(ngpmin,ngnmin) == 3) {
                lemin = max(ngpmin,ngnmin) - 1 + lt;
                /* ( Twos-complement machines with gradual underflow; no known machine ) */
            } else {
                lemin = min(ngpmin,ngnmin);
                /* ( A guess; no known machine ) */
                iwarn = TRUE_;
            }
        } else {
            lemin = min(min(min(ngpmin,ngnmin),gpmin),gnmin);
/*         ( A guess; no known machine ) */
            iwarn = TRUE_;
        }
        /* ** Comment out this if block if EMIN is ok */
        if (iwarn) {
            first = TRUE_;
            printf("\n\n WARNING. The value EMIN may be incorrect: - ");
            printf("EMIN = %8i\n",lemin);
            printf("If, after inspection, the value EMIN looks acceptable");
            printf(" please comment out\n the IF block as marked within the");
            printf(" code of routine SLAMC2,\n otherwise supply EMIN");
            printf(" explicitly.\n");
        }
/* **     Assume IEEE arithmetic if we found denormalised  numbers above,
          or if arithmetic seems to round in the  IEEE style,  determined
          in routine SLAMC1. A true IEEE machine should have both  things
          true; however, faulty machines may have one or the other.
*/

        ieee = ieee || lieee1;

/*        Compute  RMIN by successive division by  BETA. We could compute
          RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
          this computation.
*/

        lrmin = one;
        for (i = 1; i <= 1-lemin; ++i) {
            r__1 = lrmin * rbase;
            lrmin = slamc3_(&r__1, &zero);
        }

/*        Finally, call SLAMC5 to compute EMAX and RMAX. */

        slamc5_(&lbeta, &lt, &lemin, &ieee, &lemax, &lrmax);
    }

    *beta = lbeta;
    *t = lt;
    *rnd = lrnd;
    *eps = leps;
    *emin = lemin;
    *rmin = lrmin;
    *emax = lemax;
    *rmax = lrmax;
} /* slamc2_ */


/* Microsoft Visual C++ 2003 produces bad code when the following */
/* routine is optimized.  Turn off the optimization for this one  */
/* routine and turn back on any optimizations after this routine. */
#if defined(_WIN32) || defined(WIN32)
#if (_MSC_VER >= 1310)
#pragma optimize("", off)
#endif
#endif

real slamc3_(real *a, real *b)
{
/*  -- LAPACK auxiliary routine (version 2.0) --
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
       Courant Institute, Argonne National Lab, and Rice University
       October 31, 1992

    Purpose
    =======

    SLAMC3  is intended to force  A  and  B  to be stored prior to doing
    the addition of  A  and  B ,  for use in situations where optimizers
    might hold one of these in a register.

    Arguments
    =========

    A, B    (input) REAL
            The values A and B.

   =====================================================================
*/

    return *a + *b;
} /* slamc3_ */

/* Turn the optimizations back on for Visual Studio .NET 2003 */
#if defined(_WIN32) || defined(WIN32)
#if (_MSC_VER >= 1310)
#pragma optimize("", on)
#endif
#endif

/* Subroutine */ void slamc4_(integer *emin, real *start, integer *base)
{
/*  -- LAPACK auxiliary routine (version 2.0) --
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
       Courant Institute, Argonne National Lab, and Rice University
       October 31, 1992

    Purpose
    =======

    SLAMC4 is a service routine for SLAMC2.

    Arguments
    =========

    EMIN    (output) EMIN
            The minimum exponent before (gradual) underflow, computed by
            setting A = START and dividing by BASE until the previous A
            can not be recovered.

    START   (input) REAL
            The starting point for determining EMIN.

    BASE    (input) INTEGER
            The base of the machine.

   =====================================================================
*/

    /* System generated locals */
    real r__1;
    /* Local variables */
    static real zero = 0.f, a;
    static integer i;
    static real rbase, b1, b2, c1, c2, d1, d2;
    static real one = 1.f;

    a = *start;
    rbase = one / *base;
    *emin = 1;
    r__1 = a * rbase;
    b1 = slamc3_(&r__1, &zero);
    c1 = c2 = d1 = d2 = a;
    while (c1 == a && c2 == a && d1 == a && d2 == a) {
        --(*emin);
        a = b1;
        r__1 = a / *base;
        b1 = slamc3_(&r__1, &zero);
        r__1 = b1 * *base;
        c1 = slamc3_(&r__1, &zero);
        d1 = zero;
        for (i = 1; i <= *base; ++i) {
            d1 += b1;
        }
        r__1 = a * rbase;
        b2 = slamc3_(&r__1, &zero);
        r__1 = b2 / rbase;
        c2 = slamc3_(&r__1, &zero);
        d2 = zero;
        for (i = 1; i <= *base; ++i) {
            d2 += b2;
        }
    }
} /* slamc4_ */

/* Subroutine */ void slamc5_(integer *beta, integer *p, integer *emin,
                              logical *ieee, integer *emax, real *rmax)
{
/*  -- LAPACK auxiliary routine (version 2.0) --
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
       Courant Institute, Argonne National Lab, and Rice University
       October 31, 1992

    Purpose
    =======

    SLAMC5 attempts to compute RMAX, the largest machine floating-point
    number, without overflow.  It assumes that EMAX + abs(EMIN) sum
    approximately to a power of 2.  It will fail on machines where this
    assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
    EMAX = 28718).  It will also fail if the value supplied for EMIN is
    too large (i.e. too close to zero), probably with overflow.

    Arguments
    =========

    BETA    (input) INTEGER
            The base of floating-point arithmetic.

    P       (input) INTEGER
            The number of base BETA digits in the mantissa of a
            floating-point value.

    EMIN    (input) INTEGER
            The minimum exponent before (gradual) underflow.

    IEEE    (input) LOGICAL
            A logical flag specifying whether or not the arithmetic
            system is thought to comply with the IEEE standard.

    EMAX    (output) INTEGER
            The largest exponent before overflow

    RMAX    (output) REAL
            The largest machine floating-point number.

   =====================================================================
*/

    /* Table of constant values */
    static real c_b5 = 0.f;
    /* System generated locals */
    real r__1;
    /* Local variables */
    static integer lexp;
    static real oldy;
    static integer uexp, i;
    static real y, z;
    static integer nbits;
    static real recbas;
    static integer exbits, expsum, try;

/*     First compute LEXP and UEXP, two powers of 2 that bound
       abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
       approximately to the bound that is closest to abs(EMIN).
       (EMAX is the exponent of the required number RMAX).
*/

    lexp = 1;
    exbits = 1;
    while ((try = lexp << 1) <= -(*emin)) {
        lexp = try;
        ++exbits;
    }
    if (lexp == -(*emin)) {
        uexp = lexp;
    } else {
        uexp = try;
        ++exbits;
    }

/*     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
       than or equal to EMIN. EXBITS is the number of bits needed to
       store the exponent.
*/

    if (uexp + *emin > -lexp - *emin) {
        expsum = lexp << 1;
    } else {
        expsum = uexp << 1;
    }

    /* EXPSUM is the exponent range, approximately equal to EMAX - EMIN + 1 . */
    *emax = expsum + *emin - 1;
    nbits = exbits + 1 + *p;

/*     NBITS is the total number of bits needed to store a floating-point number. */

    if (nbits % 2 == 1 && *beta == 2) {
/*        Either there are an odd number of bits used to store a
          floating-point number, which is unlikely, or some bits are
          not used in the representation of numbers, which is possible,
          (e.g. Cray machines) or the mantissa has an implicit bit,
          (e.g. IEEE machines, Dec Vax machines), which is perhaps the
          most likely. We have to assume the last alternative.
          If this is true, then we need to reduce EMAX by one because
          there must be some way of representing zero in an implicit-bit
          system. On machines like Cray, we are reducing EMAX by one
          unnecessarily.
*/
        --(*emax);
    }

    if (*ieee) {
        /* Assume we are on an IEEE machine which reserves one exponent for infinity and NaN. */
        --(*emax);
    }

/*  Now create RMAX, the largest machine number, which should be equal to (1.0 - BETA**(-P)) * BETA**EMAX . */
/*  First compute 1.0 - BETA**(-P), being careful that the result is less than 1.0 . */

    recbas = 1.f / *beta;
    z = *beta - 1.f;
    y = 0.f;
    for (i = 1; i <= *p; ++i) {
        z *= recbas;
        if (y < 1.f) {
            oldy = y;
        }
        y = slamc3_(&y, &z);
    }
    if (y >= 1.f) {
        y = oldy;
    }

    /* Now multiply by BETA**EMAX to get RMAX. */
    for (i = 1; i <= *emax; ++i) {
        r__1 = y * *beta;
        y = slamc3_(&r__1, &c_b5);
    }

    *rmax = y;
} /* slamc5_ */
