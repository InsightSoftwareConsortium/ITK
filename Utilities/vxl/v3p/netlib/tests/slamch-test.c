/* slamch-test.c -- Written by Peter Vanroose, 9 November 2003 */
#include "../f2c.h"
#include "../netlib.h"
#include <stdio.h>

int main(void)
{
    printf("eps  = %g\n", slamch_("E"));
    printf("sfmin= %g\n", slamch_("S"));
    printf("base = %g\n", slamch_("B"));
    printf("prec = %g\n", slamch_("P"));
    printf("t    = %g\n", slamch_("N"));
    printf("rnd  = %g\n", slamch_("R"));
    printf("emin = %g\n", slamch_("M"));
    printf("rmin = %g\n", slamch_("U"));
    printf("emax = %g\n", slamch_("L"));
    printf("rmax = %g\n", slamch_("O"));
    return 0;
}
