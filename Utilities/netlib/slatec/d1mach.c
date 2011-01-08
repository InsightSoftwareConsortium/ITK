/** d1mach - determine machine precision.  Instead of using the
 * original fortran version (or f2c'd version), use this C code
 * instead. This is code is actually in a comment in the original
 * fortran code.
 *
 * Note, that by using this C code, d1mach is now thread safe,
 * whereas, the f2c code was not.
 *
 */


/* Standard C source for D1MACH */
#include <stdio.h>
#include <float.h>
#include <math.h>
#include "v3p_f2c_original.h"
double d1mach_(integer *i)
{
  switch(*i)
    {
    case 1: return DBL_MIN;
    case 2: return DBL_MAX;
    case 3: return DBL_EPSILON/FLT_RADIX;
    case 4: return DBL_EPSILON;
    case 5: return log10((double)FLT_RADIX);
    }
  fprintf(stderr, "invalid argument: d1mach(%ld)\n", (long) *i);
  return 0; /* some compilers demand return values */
}
