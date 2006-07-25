/* slamch-test.c -- Written by Peter Vanroose, 9 November 2003 */
#include "v3p_netlib.h"
#include <stdio.h>

int main(void)
{
    printf("eps  = %g\n", v3p_netlib_slamch_("E", 1));
    printf("sfmin= %g\n", v3p_netlib_slamch_("S", 1));
    printf("base = %g\n", v3p_netlib_slamch_("B", 1));
    printf("prec = %g\n", v3p_netlib_slamch_("P", 1));
    printf("t    = %g\n", v3p_netlib_slamch_("N", 1));
    printf("rnd  = %g\n", v3p_netlib_slamch_("R", 1));
    printf("emin = %g\n", v3p_netlib_slamch_("M", 1));
    printf("rmin = %g\n", v3p_netlib_slamch_("U", 1));
    printf("emax = %g\n", v3p_netlib_slamch_("L", 1));
    printf("rmax = %g\n", v3p_netlib_slamch_("O", 1));
    return 0;
}
