/* lapack/util/ieeeck.f -- translated by f2c (version 20050501).
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

/*<       INTEGER          FUNCTION IEEECK( ISPEC, ZERO, ONE ) >*/
integer ieeeck_(integer *ispec, real *zero, real *one)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    real nan1, nan2, nan3, nan4, nan5, nan6, neginf, posinf, negzro, newzro;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1998 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            ISPEC >*/
/*<       REAL               ONE, ZERO >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  IEEECK is called from the ILAENV to verify that Infinity and */
/*  possibly NaN arithmetic is safe (i.e. will not trap). */

/*  Arguments */
/*  ========= */

/*  ISPEC   (input) INTEGER */
/*          Specifies whether to test just for inifinity arithmetic */
/*          or whether to test for infinity and NaN arithmetic. */
/*          = 0: Verify infinity arithmetic only. */
/*          = 1: Verify infinity and NaN arithmetic. */

/*  ZERO    (input) REAL */
/*          Must contain the value 0.0 */
/*          This is passed to prevent the compiler from optimizing */
/*          away this code. */

/*  ONE     (input) REAL */
/*          Must contain the value 1.0 */
/*          This is passed to prevent the compiler from optimizing */
/*          away this code. */

/*  RETURN VALUE:  INTEGER */
/*          = 0:  Arithmetic failed to produce the correct answers */
/*          = 1:  Arithmetic produced the correct answers */

/*     .. Local Scalars .. */
/*<    >*/
/*     .. */
/*     .. Executable Statements .. */
/*<       IEEECK = 1 >*/
    ret_val = 1;

/*<       POSINF = ONE / ZERO >*/
    posinf = *one / *zero;
/*<       IF( POSINF.LE.ONE ) THEN >*/
    if (posinf <= *one) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       NEGINF = -ONE / ZERO >*/
    neginf = -(*one) / *zero;
/*<       IF( NEGINF.GE.ZERO ) THEN >*/
    if (neginf >= *zero) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       NEGZRO = ONE / ( NEGINF+ONE ) >*/
    negzro = *one / (neginf + *one);
/*<       IF( NEGZRO.NE.ZERO ) THEN >*/
    if (negzro != *zero) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       NEGINF = ONE / NEGZRO >*/
    neginf = *one / negzro;
/*<       IF( NEGINF.GE.ZERO ) THEN >*/
    if (neginf >= *zero) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       NEWZRO = NEGZRO + ZERO >*/
    newzro = negzro + *zero;
/*<       IF( NEWZRO.NE.ZERO ) THEN >*/
    if (newzro != *zero) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       POSINF = ONE / NEWZRO >*/
    posinf = *one / newzro;
/*<       IF( POSINF.LE.ONE ) THEN >*/
    if (posinf <= *one) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       NEGINF = NEGINF*POSINF >*/
    neginf *= posinf;
/*<       IF( NEGINF.GE.ZERO ) THEN >*/
    if (neginf >= *zero) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       POSINF = POSINF*POSINF >*/
    posinf *= posinf;
/*<       IF( POSINF.LE.ONE ) THEN >*/
    if (posinf <= *one) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }




/*     Return if we were only asked to check infinity arithmetic */

/*<    >*/
    if (*ispec == 0) {
        return ret_val;
    }

/*<       NAN1 = POSINF + NEGINF >*/
    nan1 = posinf + neginf;

/*<       NAN2 = POSINF / NEGINF >*/
    nan2 = posinf / neginf;

/*<       NAN3 = POSINF / POSINF >*/
    nan3 = posinf / posinf;

/*<       NAN4 = POSINF*ZERO >*/
    nan4 = posinf * *zero;

/*<       NAN5 = NEGINF*NEGZRO >*/
    nan5 = neginf * negzro;

/*<       NAN6 = NAN5*0.0 >*/
    nan6 = nan5 * (float)0.;

/*<       IF( NAN1.EQ.NAN1 ) THEN >*/
    if (nan1 == nan1) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       IF( NAN2.EQ.NAN2 ) THEN >*/
    if (nan2 == nan2) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       IF( NAN3.EQ.NAN3 ) THEN >*/
    if (nan3 == nan3) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       IF( NAN4.EQ.NAN4 ) THEN >*/
    if (nan4 == nan4) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       IF( NAN5.EQ.NAN5 ) THEN >*/
    if (nan5 == nan5) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       IF( NAN6.EQ.NAN6 ) THEN >*/
    if (nan6 == nan6) {
/*<          IEEECK = 0 >*/
        ret_val = 0;
/*<          RETURN >*/
        return ret_val;
/*<       END IF >*/
    }

/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* ieeeck_ */

#ifdef __cplusplus
        }
#endif
